(* ------------------------------------------------------ *)
(*                      MEMTEST.PAS                       *)
(* Dieses ist KEINE Super-Demo, sondern nur ein Testprg.  *)
(* Dieses Program zeigt, wie man die UNIT XHeap anwendet. *)
(* Es zeigt graphisch die 64K-Blîcke und die Loch-        *)
(* listenverwaltung. Es wird ein zufÑllig-gro·es StÅck    *)
(* Speicher angefordert und wieder freigegeben.           *)
(* Man kann mit Tastendruck den Test abbrechen, wobei zum *)
(* Schlu· ein sauber aufgerÑumter Speicher Åbrigbleiben   *)
(* wird (oder soll).                                      *)
(*                                                        *)
(*     (c) 1991 Dipl.Ing. O. Grossklaus & DMV-Verlag      *)
(* ------------------------------------------------------ *)
{$M 16384,0,655360}
{$A-,B-,D+,E-,F-,I-,L+,N-,O-,R+,S+}

PROGRAM MemTest;

USES
  Graph, XHeap, Crt, Dos;

CONST
  MaxE        = 7500;     { Maximale Anzahl der zu belegenden XZeiger }
  MaxTakeSize = 2500;     { Maximalgrî·e einer Anforderung            }

  ID : ARRAY [BlockType] OF STRING [3] = ('EMS', 'HDD');

TYPE
  BufferPtr   = ^Buffer;
  Buffer      = ARRAY [1..1000] of CHAR;

  LinePtrType = RECORD
                  Ptr    : BufferPtr;        { jeder beliebige Typ       }
                  PageNr : WORD;             { Blocknummer des Speichers }
                END;

  LineSize    = RECORD
                  XPtr : LinePtrType;                          { XZeiger }
                  Size : WORD;            { Grî·e der Anforderung merken }
                END;

VAR
  MaxY       : INTEGER;
  f          : TEXT;
  i          : WORD;
  Max        : WORD;
  Size       : WORD;
                      { Alle Zeiger werden hier fÅr den Test gespeichert }
  DemoPtrs   : ARRAY [1..MaxE] OF LineSize;
  AbsMaxLLEs : WORD;


  PROCEDURE ShowEms;
    (* den aktuellen Stand des aktuellen 64K-Blocks zeigen *)
  VAR
    M     : REAL;
    i     : BYTE;
    Block : BYTE;

    PROCEDURE ShowBlock;
      (* den Block selbst zeigen *)
    VAR
      X1, Y1,
      X2, Y2  : INTEGER;
      LLCount : WORD;
    BEGIN
      X1 := Pred(Descriptor^.BlockNr) * 40;
      X2 := X1 + 30;
                                  { Platz fÅr den Descriptor }
      Y1 := MaxY;
      Y2 := MaxY - Round(SizeOf(Descriptor^) * M);
      SetFillStyle(SolidFill, Yellow);
      Bar(X1, Y1, X2, Y2);
                         { Platz fÅr die LochListe (benutzt) }
      LLCount := 1;
      WHILE LochListe^[LLCount].Size <> 0 DO
        INC(LLCount);
      Y1 := Y2 - 1;
      Y2 := MaxY - Round(SizeOf(Descriptor^) * M) -
                   Round(4 * LLCount * M);
      IF Y2 > Y1 THEN
        Y2 := Pred(Y1);
      SetFillStyle(SolidFill, DarkGray);
      Bar(X1, Y1, X2, Y2);
                       { Platz fÅr die LochListe (unbenutzt) }
      Y1 := Y2 - 1;
      Y2 := MaxY - Round(SizeOf(Descriptor^) * M) -
                   Round(4 * MaxLLEntrys * M);
      SetFillStyle(SolidFill, LightGray);
      Bar(X1, Y1, X2, Y2);
                                     { alles erstmal besetzt }
      Y1 := Y2 - 1;
      Y2 := 0;
      SetFillStyle(SolidFill, Red);
      Bar(X1, Y1, X2, Y2);
                                         { Zeige alle Lîcher }
      IF Descriptor^.Typ = EMSType THEN
        SetFillStyle(SolidFill, Green)
      ELSE
        SetFillStyle(SolidFill, White);
      LLCount := 1;
      WHILE LochListe^[LLCount].Size > 0 DO BEGIN
        X2 := X1 + 30;
        WITH LochListe^[LLCount] DO BEGIN
          Y1 := MaxY - Round(Offset * M);
          Y2 := MaxY - Round((Offset + Size) * M);
        END;
        Bar(X1, Y1, X2, Y2);
        INC(LLCount);
      END;
      LLCount := 0;
      REPEAT
        INC(LLCount);
      UNTIL LochListe^[LLCount].Size = 0;
      DEC(LLCount);
      IF LLCount > AbsMaxLLEs THEN
        AbsMaxLLEs := LLCount;
    END;

    PROCEDURE ShowWindow;
      (* Zusatzinformationen anzeigen *)
    CONST
      Rect : ARRAY[1..4] of PointType =
             ((X :  1; Y :  1),
              (X : 69; Y :  1),
              (X : 69; Y : 75),
              (X :  1; Y : 75));
    VAR
      Dummy : STRING;
    BEGIN
      IF (Descriptor^.BlockNr < 3) THEN
        Exit;
      SetColor(Black);
      SetFillStyle(SolidFill, Black);
      FillPoly(SizeOf(Rect) div SizeOf(PointType), Rect);
      SetColor(White);
      WITH Descriptor^ DO
        OutTextXY(3, 3, 'Blk:' + ID[Typ]);        { Blocktyp }
      Str(Size, Dummy);
      OutTextXY(3, 11, 'Sze:' + Dummy);{ letzte angef. Grî·e }
      Str(EMSBlocks, Dummy);
      OutTextXY(3, 19, 'EMS:' + Dummy);  { Anzahl EMS-Blîcke }
      Str(FileBlocks, Dummy);
      OutTextXY(3, 27, 'HDD:' + Dummy); { Anzahl File-Blîcke }
      Str(Max, Dummy);
      OutTextXY(3, 35, 'Max:' + Dummy);    { Benutze XZeiger }
      Str(AbsMaxLLEs, Dummy);
      OutTextXY(3, 43, 'LLE:' + Dummy);{ Anzahl LocheintrÑge }
      Str(Descriptor^.BlockNr, Dummy);
      OutTextXY(3, 51, 'BNr:' + Dummy);        { Blocknummer }
      Str(Descriptor^.BlkUsed, Dummy);
      OutTextXY(3, 59, 'BLu:' + Dummy); { Anzahl Benutzungen }
      Str(Descriptor^.AnzPtr, Dummy);
      OutTextXY(3, 67, 'Apt:' + Dummy);      { Anzahl Zeiger }
    END;

  BEGIN                     { mehr geht nicht auf den Screen }
    M := MaxY / 65535;                     { Skalierungsfaktor }
    IF Descriptor^.BlockNr < 17 THEN
      ShowBlock;
    ShowWindow;
  END;

  PROCEDURE Init;
    (* Graphik initialisieren (ohne viel TAM-TAM) *)
  VAR
    GD, GM : INTEGER;
  BEGIN
    GD := Detect;
    InitGraph(GD, GM, '');
  END;

  FUNCTION LLCheck : BOOLEAN;
    (* LochlisteneintrÑge auf Korrektheit ÅberprÅfen *)
  VAR
    LLCount : WORD;
    Dummy   : BOOLEAN;
  BEGIN
    Dummy := TRUE;
    { Erster Eintr.beinhaltet die Gesamtmenge freien Speichers }
    IF (LochListe^[1].Size <>
       (SwapBufferSize-(SizeOf(DescType)+SizeOf(LochLst)))) OR
       (LochListe^[1].Offset <>
       (SizeOf(DescType) + SizeOf(LochLst))) THEN BEGIN
      Writeln(f,#13,#10,'    1 Size : ',
              LochListe^[1].Size:6,
              ' Offset : ',
              LochListe^[1].Offset);
      Writeln(#13,#10,'    1 Size : ',
              LochListe^[1].Size:6,
              ' Offset : ',
              LochListe^[1].Offset);
      Dummy := FALSE;
    END;
                   { alle anderen LocheintrÑge mÅssen 0 sein }
    FOR LLCount := 2 TO MaxLLEntrys DO
      IF (LochListe^[LLCount].Size <> 0) OR
         (LochListe^[LLCount].Offset <> 0) THEN BEGIN
        Writeln(f,LLCount:5,
                ' Size : ',
                LochListe^[1].Size:6,
                ' Offset : ',
                LochListe^[1].Offset);
        Writeln(LLCount:5,
                ' Size : ',
                LochListe^[1].Size:6,
                ' Offset : ',
                LochListe^[1].Offset);
        Dummy := FALSE;
      END;
    LLCheck := Dummy;
  END;


BEGIN
  AbsMaxLLEs := 0;              { Gesamtzahl der benutzten LocheintrÑge }
  ClrScr;
  Randomize;
  Init;                                         { Graphik bereitstellen }
  MaxY := GetMaxY;
  I    := 1;
  Max  := 0;

                                       { XZeiger auf NIL initialisieren }
  FillChar(DemoPtrs, SizeOf(DemoPtrs), #0);
  Size := 1 + Random(MaxTakeSize);

                                                { Belegen des Speichers }
  WHILE MemAvailX(Size) AND
        (NOT KeyPressed) AND
        (I <= MaxE) DO BEGIN
    GetMemX(DemoPtrs[I].XPtr, Size);
    DemoPtrs[I].Size := Size;
    ShowEms;               { Zeige den EMS/HDD-Zustand des akt. Blockes }
    Inc(I);
    Inc(Max);
    Size := 1 + Random(MaxTakeSize);
  END;

               { Freigeben und Neubelegen einzelner beliebiger Speicher }
  WHILE NOT KeyPressed DO BEGIN
    I := Random(Max) + 1;
    IF DemoPtrs[I].XPtr.Ptr <> NIL THEN
      FreeMemX(DemoPtrs[I].XPtr, DemoPtrs[I].Size);
    Size := 1 + Random(MaxTakeSize);
    IF MemAvailX(Size) THEN BEGIN
      GetMemX(DemoPtrs[I].XPtr, Size);
      DemoPtrs[I].Size := Size;
    END;
    ShowEms;
  END;
                                   { Freigeben aller SpeicherstÅckchen }
  FOR I := 1 TO Max DO BEGIN
    IF DemoPtrs[I].XPtr.Ptr <> NIL THEN
      FreeMemX(DemoPtrs[I].XPtr, DemoPtrs[I].Size);
    ShowEms;
  END;
  Delay(3000);
  CloseGraph;

              { PrÅfroutinen, ob die Verwaltung korrekt gearbeitet hat }
  Assign(f,'Logbuch.txt');
  Rewrite(f);
  FOR I := 1 TO (EMSBlocks + FileBlocks) DO BEGIN
    BlendeBlockEin(I);
    WITH Descriptor^ DO BEGIN
      Write(f,'Block ',
             BlockNr:3,
             ' (',
             ID[Typ],
             ') ',
             AnzPtr :3,                 { Mu· zum Schlu· immer 0 sein }
             ' ',
             BlkUsed :6 );                                { Statistik }
      Write('Block ',
             BlockNr:3,
             ' (',
             ID[Typ],
             ') ',
             AnzPtr :3,                 { Mu· zum Schlu· immer 0 sein }
             ' ',
             BlkUsed :6 );                                { Statistik }
    END;
    IF LLCheck THEN BEGIN
      WriteLn(f, '  LL OK');        { Verwaltung der Blk hat geklappt }
      WriteLn(   '  LL OK');        { Verwaltung der Blk hat geklappt }
    END ELSE BEGIN
      WriteLn(f, '  ALARM');                      { Verwaltungsfehler }
      WriteLn(   '  ALARM');                      { Verwaltungsfehler }
    END;
  END;
  Close(f);
END.
(* ---------------------------------------------------------------- *)
(*                    Ende von MEMTEST.PAS                          *)
