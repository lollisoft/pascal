(* ------------------------------------------------------ *)
(*                      XHEAP.PAS                         *)
(*  Diese Unit ermîglicht es gro·en Programmen, die einen *)
(*  sehr gierigen Speicherbedarf haben, einen privaten,   *)
(*  erweiterten Heap zu benutzen. Dieser Heap wird vom    *)
(*  EMS genommem; steht dieses nicht zur VerfÅgung oder   *)
(*  reicht es immer noch nicht aus, so wird auch die      *)
(*  Platte als Heap verwendet. Das Programm arbeitet in   *)
(*  jedem Fall nur mit den dynamischen Daten (Zeigern).   *)
(*     (c) 1991 Dipl.Ing. O. Grossklaus & DMV-Verlag      *)
(* ------------------------------------------------------ *)
{$F+,I-}
UNIT XHeap;

INTERFACE

USES
  Dos;

CONST
  AbsMaxEMSBlocks  : BYTE = 8;     { abs. Max. an 64K-Blk (EMS)    }
  AbsMaxFileBlocks : BYTE = 8;     { abs. Max. an 64K-Blk (HDD)    }

  SwapBufferSize   = 65500;        { nur relevant, wenn kein EMS,  }
                                   { also alles im HEAP(std.65500) }
  MaxLLEntrys      =  1000;        { Anzahl der LochListenEintrÑge }
  NormSize         = TRUE;         { normalisiere die Grî·e        }
  BestFit          = TRUE;
  UseNHeapForSBuf  = TRUE;         { normalen Heap mitbenutzen     }

TYPE
  BlockType     = (EMSType, HDType);      { Typ des 64K-Blocks     }
  XtendedPtr    = RECORD                  { EMS-Pointer-Typ        }
                    P       : POINTER;    { anpassen mit TypeCast  }
                    BlockNr : WORD;       { BlockNr. des Speichers }
                  END;
  SwapBufferPtr = ^SwpBuffer;         { Block zum Arbeiten auf HDD }

  SwpBuffer     = ARRAY [1..SwapBufferSize] of BYTE;

  LLEntry       = RECORD                  { Lochlisteneintrag      }
                    Offset : WORD;
                    Size   : WORD;
                  END;
  LLPtr         = ^LochLst;               { Lochliste              }
  LochLst       = ARRAY [1..MaxLLEntrys] of LLEntry;
  DescTypePtr   = ^DescType;              { Block-Identifier       }
  DescType      = RECORD
                    BlockNr : WORD;       { EMS/HDD-Block          }

  (* ---------------------------------------------------- *)
  (* Zur Geschwindigkeitssteigerung kann das folgende     *)
  (* Flag aktiviert werden. Es mu· aber bei Benutzung der *)
  (* Speicherblîcke "von Hand" gesetzt werden, damit      *)
  (* dieser Block gespeichert wird.                       *)
  (* ---------------------------------------------------- *)
{.$DEFINE SpeedUp}

{$IFDEF SpeedUp}
                    Modified : BOOLEAN;   { wurde es geÑndert ?    }
{$ENDIF}

                    Typ      : BlockType; { Blocktyp: EMS oder HDD }
                    BlkUsed  : LONGINT;   { # Benutzungen          }
                    AnzPtr   : WORD;      { # Zeiger auf den Block }
                  END;

VAR
  MMUse        : BOOLEAN;          { extended MemoryManagement Use }
  CurrentBlock : WORD;             { aktueller EMS/HDD-Block       }
  Descriptor   : DescTypePtr;      { Block-Beschreibung            }
  LochListe    : LLPtr;            { Lochliste eines Blocks        }
  EMSError     : WORD;      { EMS-Fehler nach EMS-Aufruf?          }
  EMSBlocks    : WORD;      { Wieviele EMSBlks sind gerade benutzt }
  FileBlocks   : WORD;      { dto. fÅr Datei-Blîcke                }


  FUNCTION MemAvailX(Size : WORD) : BOOLEAN;
    (* Testroutine, ob Åberhaupt noch Speicher vorhanden *)

  PROCEDURE GetMemX(VAR P; Size : WORD);
    (* Speicheranforderungsroutine *)

  PROCEDURE FreeMemX(VAR P; Size : WORD);
    (* Speicherfreigaberoutine *)

  PROCEDURE BlendeBlockEin(Nr : WORD);
    (* Einblenderoutine eines bestimmten Blocks *)

(* ------------------------------------------------------ *)
  
IMPLEMENTATION

VAR
  EMSInt            : POINTER ABSOLUTE 0:$19C;  { Interrupt fÅr EMS }
  Regs              : Registers;

  ExitSave          : POINTER; { Exit-Routine zum AufrÑumen EMS/HDD }

  SwapFile          : FILE OF SwpBuffer;  { Datei fÅr Auslagerungen }

  SwapBufferPresent : BOOLEAN;       { gibt's zum Swappen Speicher? }
  SwapBuffer        : SwapBufferPtr; { Zeiger auf einen 64K-Block   }
  EMSAvail          : BOOLEAN;       { Ist EMS da?                  }
  EMSHandle         : WORD;          { verwendeter EMSHandle        }

                                     { LochListen-Update-Routinen   }
  ClearMemory : ARRAY[0..3] OF
                  PROCEDURE(Pt : XtendedPtr; Size : WORD);


  PROCEDURE EMSCall;
    (* Aufruf des EMS-Treibers *)
  BEGIN
    Intr($67, Regs);
    EMSError := Regs.AH;
  END;

  FUNCTION EMSAvailable : BOOLEAN;
    (* Feststellen, ob es Åberhaupt EMS gibt *)
  BEGIN
    EMSAvailable := FALSE;
    IF EMSInt <> NIL THEN BEGIN
      IF BYTE(EMSInt^) <> $CF THEN BEGIN                  { IRET }
                                { Ist EMS Åberhaupt installiert? }
        Regs.AH := $40;
        EMSCall;
        IF EMSError = 0 THEN BEGIN
          Regs.AH := $42;       { hole Anzahl freier 16K Blîcke  }
          EMSCall;
          EMSAvailable := (EMSError = 0) AND (Regs.BX >= 4);
        END;
      END;
    END;
  END;

  PROCEDURE GetWorkBufferEMS;
    (* Arbeitsbuffer im EMS festlegen *)
  VAR
    i : BYTE;
  BEGIN
    Regs.AH := $43;                { reserviere Blîcke           }
    Regs.BX := 4;                  { einen 64K Block reservieren }
    EMSCall;                       { reserviere die 16K Blîcke   }
    EMSHandle := Regs.DX;          { Handle zurÅck               }
    FOR i := 0 to 3 DO BEGIN       { EMSBlock 0 einblenden       }
      Regs.AX := $4400 + i;
      Regs.BX := i;
      Regs.DX := EMSHandle;
      EMSCall;
    END;
  END;

  FUNCTION GetPageFrameSeg : WORD;
    (* Anfangsadresse des Arbeitsbuffers erfragen *)
  BEGIN
    GetPageFrameSeg := 0;
    IF EMSAvail THEN BEGIN
      Regs.AH := $41;
      EMSCall;
      IF EMSError = 0 THEN
        GetPageFrameSeg := Regs.BX;
    END;
  END;

  PROCEDURE InitAll;
    (* Alle Variablen und Strukturen initialisieren *)
  BEGIN
    MMUse             := TRUE;   { Benutze diese Verwaltung            }
    EMSBlocks         := 0;      { Keine Blocks bis jetzt              }
    FileBlocks        := 0;      { dto.                                }
    EMSHandle         := 0;      { Handlenummer fÅr EMS                }
    EMSError          := 0;      { EMS-Fehler                          }
    SwapBuffer        := NIL;    { kein Swapbuffer prÑsent             }
    SwapBufferPresent := FALSE;  { ... }
    Descriptor        := NIL;    { keine Swapbuffer-Beschreibung aktiv }
    LochListe         := NIL;                { keine Lochliste aktiv   }
    CurrentBlock      := $FFFF;              { kein aktiver Block      }
    EMSAvail          := EMSAvailable;       { schau mal nach ...      }
    IF EMSAvail THEN BEGIN
                                      { GetWorkBuffer im EMS PageFrame }
      GetWorkBufferEMS;
      SwapBuffer := Ptr(GetPageFrameSeg, 0);
    END ELSE BEGIN
                                      { GetWorkBuffer im normalen HEAP }
      IF (MaxAvail>=SwapBufferSize) AND UseNHeapForSBuf THEN
        GetMem(SwapBuffer, SwapBufferSize)
      ELSE
        MMUse := FALSE;       { keine Mîglichkeit, die Verw. zu nutzen }
    END;
    IF MMUse THEN BEGIN
                         { Festlegung der Blockdescriptoren und der LL }
      Descriptor        := DescTypePtr(SwapBuffer);
      LochListe         := Ptr(Seg(Descriptor^), SizeOf(DescType));
      SwapBufferPresent := TRUE;
      FillChar(SwapBuffer^, SwapBufferSize, #0);     { Blank putzen    }
      Assign(SwapFile, 'SWAP.!!!');                  { SwapFile îffnen }
      Rewrite(SwapFile);
      IF IOResult <> 0 THEN;
    END;
  END;

  PROCEDURE SaveCurrentFileBlock;
    (* Den aktuellen 64K-Block auf Datei sichern (wenn nîtig) *)
  BEGIN
    IF (FileBlocks <> 0)
{$IFDEF SpeedUp}
        AND Descriptor^.Modified
{$ENDIF}
                                  THEN BEGIN
      Seek(SwapFile, Descriptor^.BlockNr-EMSBlocks-1);
      IF IOResult <> 0 THEN;
{$IFDEF SpeedUp}
      Descriptor^.Modified := FALSE;
{$ENDIF}
      Write(SwapFile, SwapBuffer^);
      IF IOResult <> 0 THEN;
    END;
  END;

  PROCEDURE PositionFile(Place : WORD);
    (* Dateizeiger auf den durch PLACE angegebenen Block setzen *)
  BEGIN
    IF Place <= FileSize(SwapFile) THEN
      Seek(SwapFile, Place);
    IF IOResult <> 0 THEN;
  END;

  PROCEDURE BlendeBlockEin(Nr : WORD);
    (* aktuellen Block speichern und NR-Block einblenden *)
  VAR
    i : BYTE;
  BEGIN
    IF CurrentBlock <> Nr THEN BEGIN
                                { ist der Blk schon eingeblendet? }
      IF (Nr <= EMSBlocks) AND EMSAvail THEN      { Block aus EMS }
        FOR i := 0 to 3 DO BEGIN
          Regs.AX := $4400 + i;     { Block (16K) 0..3 einblenden }
          Regs.BX := Nr * 4 + i;
          Regs.DX := EMSHandle;
          EMSCall;
        END ELSE BEGIN                            { Block von HDD }
          IF EMSAvail THEN            { FileWorkBuffer einblenden }
            BlendeBlockEin(0);
          SaveCurrentFileBlock;       { aktuellen Block speichern }
          IF Nr > 0 THEN
            PositionFile(Pred(Nr - EMSBlocks));
          IF FilePos(SwapFile) <> FileSize(SwapFile) THEN
            IF Descriptor^.BlockNr <> (Nr - EMSBlocks) THEN
              Read(SwapFile, SwapBuffer^);             { einlesen }
          IF IOResult <> 0 THEN;
        END;
      CurrentBlock := Nr;                   { Currentblock setzen }
    END;
  END;

  PROCEDURE InitCurrentBlock(Nr : WORD);
    (* neuen Block initialisieren *)
  BEGIN
    FillChar(SwapBuffer^, SwapBufferSize, #0);     { Blank putzen }
    WITH Descriptor^ DO BEGIN
      BlockNr := Nr;                         { BlockNummer setzen }
{$IFDEF SpeedUp}
      Modified := TRUE;
{$ENDIF}
      IF Nr > EMSBlocks THEN
        Typ := HDType                           { Typ setzen }
      ELSE
        Typ := EMSType;
    END;
    WITH LochListe^[1] DO BEGIN       { Lochliste initialisieren }
      Offset := SizeOf(DescType) + SizeOf(LochLst);
      Size := SwapBufferSize - Offset;
    END;
    IF Nr > EMSBlocks THEN
      CurrentBlock := Nr;
    Descriptor^.BlkUsed := 0;
    Descriptor^.AnzPtr  := 0;
  END;

  PROCEDURE NormalizeSize(VAR Size : WORD);
    (* Grî·e anpassen auf das nÑchste acht-fache *)
  BEGIN
    IF NormSize THEN
      Size := 8 + (8 * (Pred(Size) DIV 8));
  END;

  FUNCTION MemAvailX;
    (* Testen, ob in irgendeinem Block noch die angeforderte *)
    (* Menge an Speicher vorhanden ist                       *)
  VAR
    BlockCount : WORD;

    FUNCTION CheckLLEntrys : BOOLEAN;
      (* Lochlisten-Check auf genÅgend gro·en Eintrag *)
    VAR
      LLCount : WORD;
    BEGIN
      CheckLLEntrys := TRUE;
      LLCount := 1;
      WHILE (LochListe^[LLCount].Size > 0) AND
            (LLCount <= MaxLLEntrys) DO BEGIN
        IF LochListe^[LLCount].Size >= Size THEN
          Exit;
        Inc(LLCount);
      END;
      CheckLLEntrys := FALSE;
    END;

  BEGIN
    NormalizeSize(Size);
    IF (Size = 0) OR
       (Size > (SwapBufferSize -
              SizeOf(DescType) -
              SizeOf(LochLst))) THEN BEGIN       { ungÅltig, also }
      MemAvailX := FALSE;                        { raus hier!     }
      Exit;
    END;
    MemAvailX := TRUE;
                { erst mal EMS testen, weil es am schnellsten ist }
    IF EMSAvail THEN BEGIN
                { Ist im CurrentBlock (EMS) noch Platz ?...       }
      IF (EMSBlocks > 0) and(CurrentBlock <= EMSBlocks) THEN
        IF CheckLLEntrys THEN Exit;               { es gibt Platz }

           { ...Nein! ist in irgendeinem Block (EMS) was frei ?...}
      BlockCount := 1;
      WHILE BlockCount <= EMSBlocks DO BEGIN
        BlendeBlockEin(BlockCount);      { alle EMS-Blîcke testen }
        IF CheckLLEntrys THEN Exit;      { es gibt Platz          }

        INC(BlockCount);
      END;

                      {... Nein! ist Åberhaupt noch EMS frei ?... }
      Regs.AH := $42;             { hole Anzahl freier 16K Blîcke }
      EMSCall;
      IF EMSBlocks >= AbsMaxEMSBlocks THEN       { "Schallgrenze" }
        Regs.BX := 0;
      IF (Regs.BX >= 4) THEN BEGIN
                               { es gibt noch Åber 64K freies EMS }
        Regs.AH := $51;
        Regs.BX := (EMSBlocks * 4) + 8;         { neuen 64K Block }
        Regs.DX := EMSHandle;
        EMSCall;
        IF (Regs.BX = (EMSBlocks * 4) + 8) AND
           (EMSError = 0) THEN BEGIN
          INC(EMSBlocks);              { reservieren hat geklappt }
          BlendeBlockEin(EMSBlocks);   { mache den Blk aktuell    }
          InitCurrentBlock(EMSBlocks); { Initialisiere Blk        }
          Exit;                        { Ergebnis: Speicher da!   }
        END;
      END;
    END;

        { EMSWorkBuffer einblenden, und alle Plattenblîcke testen }
    IF EMSAvail THEN BEGIN
      BlendeBlockEin(0);
      CurrentBlock := Descriptor^.BlockNr;
    END;

       { ...Nein! ist auf dem aktuellen Plattenblock was frei?... }
    IF FileBlocks <> 0 THEN BEGIN
      IF CheckLLEntrys THEN
        Exit;               { es gibt ein real existierendes Loch }

       { ...Nein! ist auf irgendeinem HDD-Block was frei ?...     }
      BlockCount := 1;
      WHILE BlockCount <= FileBlocks DO BEGIN
                                        { alle Blîcke durchtesten }
        BlendeBlockEin(EMSBlocks + BlockCount);
        IF CheckLLEntrys THEN
          Exit;                    { ein Block mit Platz gefunden }
        INC(BlockCount);
      END;
    END;
            { ...Nein! ist Åberhaupt auf der Platte was frei ?... }
    IF FileBlocks <> 0 THEN
      SaveCurrentFileBlock;           { evtl. alten Block sichern }

    IF FileBlocks < AbsMaxFileBlocks THEN BEGIN
                                 { nÑchsten Plattenblock erzeugen }
      InitCurrentBlock(EMSBlocks + Succ(FileBlocks));
      Seek(SwapFile, FileSize(SwapFile));
{$IFDEF SpeedUp}
      Descriptor^.Modified := FALSE;
{$ENDIF}
      Write(SwapFile, SwapBuffer^);
      IF IOResult = 0 THEN BEGIN
                      { es gibt auf der Platte einen freien Block }
        INC(FileBlocks);
        Exit;
      END ELSE
                  { Vorletzten Block einblenden, wenn Platte voll }
        BlendeBlockEin(Pred(CurrentBlock));
    END;
         { ...Nein! also: DEFINITIV KEIN SPEICHER! }
    IF (CurrentBlock = 0) AND
       ((EMSBlocks + FileBlocks) > 0) THEN
      BlendeBlockEin(1);
    MemAvailX := FALSE;
  END;

  PROCEDURE ClearLLEntry(LC : WORD);
    (* Lochlisteneintrag entfernen *)
  BEGIN
    Move(LochListe^[Succ(LC)], LochListe^[LC],
        (MaxLLEntrys-LC)*SizeOf(LLEntry));
    LochListe^[MaxLLEntrys].Size := 0;
    LochListe^[MaxLLEntrys].Offset := 0;
  END;

  PROCEDURE SortLL;
    (* Sortieren der Lochliste nach Grî·e fÅhrt dazu, da· immer *)
    (* das am besten passende Loch genommen wird                *)
  VAR
     LLCount : WORD;
     Tmp     : LLEntry;
     i       : WORD;
  BEGIN                                        { Bubble-Sort }
    LLCount := 2;
    WHILE (LLCount <= MaxLLEntrys) AND
          (LochListe^[LLCount].Size > 0) DO BEGIN
      IF LochListe^[LLCount].Size <
         LochListe^[Pred(LLCount)].Size THEN BEGIN
        i := LLCount;
        WHILE (I > 1) AND                    { Swap-Eintrag }
            (LochListe^[I].Size <
             LochListe^[Pred(I)].Size) DO BEGIN
          Tmp := LochListe^[Pred(I)];
          LochListe^[Pred(I)] := LochListe^[I];
          LochListe^[I] := Tmp;
          DEC(i);
        END;
      END;
      INC(LLCount);
    END;
  END;

  PROCEDURE GetMemX;
    (* Speicher anfordern.Diese Routine DARF nur nach MemAvailX *)
    (* aufgerufen werden, damit der richtige Block eingeblendet *)
    (* ist.                                                     *)
  VAR
    Pt      : XtendedPtr ABSOLUTE P;    { TypeCasting des Zeigers }
    LLCount : WORD;
  BEGIN
    NormalizeSize(Size);
    Pt.P := NIL;
    Pt.BlockNr := 0;
    IF Size = 0 THEN Exit;
             { Block ist eingeblendet und hat Speicher !!! }
    LLCount := 1;
    WHILE (LochListe^[LLCount].Size < Size) AND
          (LLCount <= MaxLLEntrys) DO
      Inc(LLCount);
    IF LLCount > MaxLLEntrys THEN
      RunError(203);             { falsche XHEAP-Anforderung }
    Pt.P := Ptr(Seg(SwapBuffer^), LochListe^[LLCount].Offset);
    Pt.BlockNr := CurrentBlock;
    IF CurrentBlock = 0 THEN
      inline($cc);
                                { Locheintrag aktualisieren }
    Dec(LochListe^[LLCount].Size, Size);
    Inc(LochListe^[LLCount].Offset, Size);
    IF LochListe^[LLCount].Size = 0 THEN
      ClearLLEntry(LLCount);    { Leeren Locheintrag lîschen }
{$IFDEF SpeedUp}
    Descriptor^.Modified := TRUE;
{$ENDIF}
    inc(Descriptor^.AnzPtr);
    inc(Descriptor^.BlkUsed);
    IF BestFit THEN SortLL;
  END;

  FUNCTION ExistLochOver(Pt : XtendedPtr; Size : WORD) : BYTE;
    (* Gibt es Åber dem freizugebenden Loch ein weiteres,   *)
    (* das zu einem gro·en Loch zusammengefa·t werden kann? *)
  VAR
    LLCount : WORD;
  BEGIN
    ExistLochOver := 0;
    LLCount := 1;
    WHILE (LochListe^[LLCount].Size > 0) AND
          (LLCount <= MaxLLEntrys) DO BEGIN
      IF Ofs(Pt.P^) + Size = LochListe^[LLCount].Offset THEN BEGIN
        ExistLochOver := 2;      { Index fÅr PROCEDURE-ARRAY }
        Exit;
      END;
      Inc(LLCount);
    END;
    IF LLCount > MaxLLEntrys THEN         { Fehlerbehandlung }
      RunError(204);
  END;

  FUNCTION ExistLochUnder(Pt : XtendedPtr) : BYTE;
    (* dto. aber unterhalb ein Loch frei *)
  VAR
    LLCount : WORD;
  BEGIN
    ExistLochUnder := 0;
    LLCount := 1;
    WHILE (LochListe^[LLCount].Size > 0) AND
          (LLCount <= MaxLLEntrys) DO BEGIN
      WITH LochListe^[LLCount] DO
        IF Offset + Size = Ofs(Pt.P^) THEN BEGIN
          ExistLochUnder := 1;   { Index fÅr PROCEDURE-ARRAY }
          Exit;
        END;
      Inc(LLCount);
    END;
    IF LLCount > MaxLLEntrys THEN         { Fehlerbehandlung }
      RunError(204);
  END;

  PROCEDURE Clear0(Pt : XtendedPtr; Size : WORD);
    (* einfache Speicherfreigabe *)
  VAR
    LLCount : WORD;
  BEGIN
    LLCount := 1;    { suche den nÑchsten freien Locheintrag }
    WHILE (LochListe^[LLCount].Size <> 0) AND
          (LLCount <= MaxLLEntrys) DO
      Inc(LLCount);
    IF LLCount > MaxLLEntrys THEN
      RunError(204);
                               { markiere Speicher als FREI }
    LochListe^[LLCount].Offset := Ofs(Pt.P^);
    LochListe^[LLCount].Size := Size;
  END;

  PROCEDURE Clear1(Pt : XtendedPtr; Size : WORD);
    (* aktuelle Freigabe mit Loch darunter *)
  VAR
    LLCount : WORD;
  BEGIN
    LLCount := 1;          { suche den passenden Locheintrag }
    WHILE (LochListe^[LLCount].Offset +
           LochListe^[LLCount].Size <> Ofs(Pt.P^)) AND
           (LLCount <= MaxLLEntrys) DO
      Inc(LLCount);
    IF LLCount > MaxLLEntrys THEN
      RunError(204);
    Inc(LochListe^[LLCount].Size, Size);{ Eintrag vergrî·ern }
  END;

  PROCEDURE Clear2(Pt : XtendedPtr; Size : WORD);
    (* Freigabe mit Loch darÅber *)
  VAR
    LLCount : WORD;
  BEGIN
    LLCount := 1;              { finde passenden Locheintrag }
    WHILE (Ofs(Pt.P^)+Size <> LochListe^[LLCount].Offset) AND
          (LLCount <= MaxLLEntrys) DO
      Inc(LLCount);
    IF LLCount > MaxLLEntrys THEN
      RunError(204);
    Dec(LochListe^[LLCount].Offset, Size);{ Eintrag anpassen }
    Inc(LochListe^[LLCount].Size, Size);
  END;

  PROCEDURE Clear3(Pt : XtendedPtr; Size : WORD);
    (* Freigabe mit Loch darÅber UND darunter *)
  VAR
    LochUnder,
    LochOver : WORD;
  BEGIN
    LochUnder := 1;                    { finde Loch darunter }
    WHILE (LochListe^[LochUnder].Offset +
           LochListe^[LochUnder].Size <> Ofs(Pt.P^)) AND
           (LochUnder <= MaxLLEntrys) DO
      Inc(LochUnder);
    LochOver := 1;                      { finde Loch darÅber }
    WHILE (Ofs(Pt.P^)+Size <> LochListe^[LochOver].Offset) AND
          (LochOver <= MaxLLEntrys) DO
      Inc(LochOver);
                              { aktualisiere die Lochliste }
    LochListe^[LochUnder].Size := LochListe^[LochUnder].Size +
                                  LochListe^[LochOver].Size +
                                  Size;
    ClearLLEntry(LochOver);     { Leeren Locheintrag lîschen }
  END;

  PROCEDURE FreeMemX;
    (* Freigabe des angeforderten Speichers *)
  VAR
    Pt : XtendedPtr ABSOLUTE P;    { TypeCasting des Zeigers }
  BEGIN
    NormalizeSize(Size);
    IF Size = 0 THEN Exit;
    BlendeBlockEin(Pt.BlockNr);           { Block einblenden }
    IF Pt.P = NIL THEN                    { Fehlerbehandlung }
      RunError(204);
                              { trickiger PROCEDURE-Aufruf }
    ClearMemory[ExistLochUnder(Pt) +
                ExistLochOver(Pt,Size)](Pt, Size);
    Pt.P := NIL;
    Pt.BlockNr := 0;
{$IFDEF SpeedUp}
    Descriptor^.Modified := TRUE;
{$ENDIF}
    dec(Descriptor^.AnzPtr);
    IF BestFit THEN SortLL;
  END;

  PROCEDURE ExitEMS;
  BEGIN
    IF EMSAvail THEN BEGIN                 { gebe EMS wieder frei }
      Regs.AH := $45;
      Regs.DX := EMSHandle;
      EMSCall;
    END ELSE                              { gebe HEAP wieder frei }
      IF SwapBuffer <> NIL THEN
        FreeMem(SwapBuffer, SwapBufferSize);
    Close(SwapFile);                 { lîsche die Swap-Datei }
    IF IOResult <> 0 THEN;
    Erase(SwapFile);
    IF IOResult <> 0 THEN;
    ExitProc := ExitSave;
  END;


BEGIN

  (* Initialisierungsteil *)

  ExitSave       := ExitProc;
  ExitProc       := @ExitEMS;
  InitAll;                      { Variablen initialisieren }
  ClearMemory[0] := Clear0;     { PROCEDURE-Zeiger setzen  }
  ClearMemory[1] := Clear1;
  ClearMemory[2] := Clear2;
  ClearMemory[3] := Clear3;
END.
(* ------------------------------------------------------ *)
(*                  Ende von XHEAP.PAS                    *)
