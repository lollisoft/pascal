(*$A-,B-,D+,E+,F+,I-,L+,N+,O-,R-,S-,V-*)
UNIT StackDmp;

INTERFACE

USES
  StrTools, Dos;

TYPE
  TDumpProc = PROCEDURE (VAR D: TEXT);

VAR
  DumpMore: TDumpProc;

PROCEDURE Dump (VAR F: TEXT; VAR B; MaxBytes: INTEGER);

IMPLEMENTATION

CONST
  MoreStack = 2000;

{**** Date, Time and Version stamp. (c) 1990 by EMS. For Pascal! ****}
CONST Version =       4;    { 17.11.1990 19: 8 }

TYPE
  TBytes = ARRAY[1..MaxInt] OF BYTE;
  PStack = ^TStack;
  TStack = RECORD
    BP,IP,CS: WORD;
    Bytes: TBytes;
  END;

FUNCTION NearCall (CS,IP: WORD) : BOOLEAN;
  BEGIN
    NearCall := (MemW[CS:IP-4] <> $E80E) AND (Mem[CS:IP-5] <> $9A)
                AND (MemW[CS:IP-3] <> $5DFF);
  END;

FUNCTION Previous (BP: WORD) : POINTER;
  BEGIN
    Previous := Ptr(SSeg,BP);
  END;

VAR                               (* Globale Variable                       *)
  SavedExit: POINTER;             (* Alter Wert von ExitProc                *)
  SavedSSeg: WORD;                (* gesicherte Wert vom SSeg muá stimmen   *)
  FName: STRING;                  (* Wird bei jedem Init erzeugt            *)
  DumpFile: TEXT;
  inChar: STRING;

PROCEDURE Dump (VAR F: TEXT; VAR B; MaxBytes: INTEGER);
  VAR
    Count: INTEGER;
    Bytes: TBytes ABSOLUTE B;
  BEGIN
    Count := 1;
    inChar := '';
    WHILE Count <= MaxBytes DO BEGIN
      IF (Count MOD 16 = 1) THEN BEGIN
        Writeln(F, ' ', inChar);
        Write(F, '     ',ToHex(Ofs(B)+Count-1,4),' ');
        inChar := '';
      END;
      Write(F, ' ', ToHex(Bytes[Count],2));
      IF Bytes[Count] IN [0..31,127,255]
       THEN inChar := inChar + '.'
       ELSE inChar := inChar + Chr(Bytes[Count]);
      Inc(Count);
    END;
    IF Count MOD 16 = 1
     THEN Writeln(F, ' ', inChar)
     ELSE Writeln(F, ' ':3*(16-((Count-1) MOD 16))+1, inChar);
  END;

PROCEDURE StackDump; FAR;         (* Hier geht es zur Sache                 *)
  VAR
    BasePtr: PStack;
    IOErr: WORD;
  FUNCTION CPUBP: WORD;           (* Ermitteln des aktuellen BasePointer    *)
    INLINE( $89/ $E8);            (*    MOV   AX,BP                         *)
  BEGIN
    StackLimit := StackLimit - MoreStack;
    ExitProc := SavedExit;
    IF (ErrorAddr <> NIL) OR (ExitCode <> 0) THEN BEGIN
      IOErr := IOResult;
      Assign(DumpFile, FName);
      Append(DumpFile);
      IF IOResult = 0
       THEN WriteLn(DumpFile, Chars('Í',78))
       ELSE Rewrite(DumpFile);
      IF (ExitCode <> 0)
       THEN Write(DumpFile, 'Runtime error ', ExitCode);
      IF ErrorAddr <> NIL
       THEN Writeln(DumpFile, ' at ',ToHex(Seg(ErrorAddr^),4), ':', ToHex(Ofs(ErrorAddr^),4), ' ')
       ELSE Writeln(DumpFile);
      IF IOErr <> 0 THEN Writeln(DumpFile, 'Previously IOResult: ',IOErr);
      Writeln(DumpFile,'File: ',FExpand(ParamStr(0)));
      IF SSeg <> SavedSSeg
       THEN Writeln(DumpFile,'Stacksegment ist ge„ndert. Kein Stackdump!')
       ELSE BEGIN
        Writeln(DumpFile, 'StackDump: ', Chars('Ä',64));
        BasePtr := Ptr(SSeg,CPUBP);
        WHILE  (Ofs(BasePtr^)+1 <> BasePtr^.BP) AND (BasePtr^.BP <> 0)
         DO WITH BasePtr^ DO BEGIN
          inChar := '';
          IF NearCall(CS,IP)
           THEN BEGIN
            Write(DumpFile, 'near', ':', ToHex(IP,4), ' ');
            Dump(DumpFile, CS, BP-Ofs(BasePtr^)-4);
           END
           ELSE BEGIN
            Write(DumpFile, ToHex(CS-PrefixSeg-$10,4), ':', ToHex(IP,4), ' ');
            Dump(DumpFile, Bytes, BP-Ofs(BasePtr^)-6);
          END;
          BasePtr := Previous(BP);
        END;
      END;
      Flush(DumpFile);
      DumpMore(DumpFile);
      Close(DumpFile);
    END;
    StackLimit := StackLimit + MoreStack;
  END;

PROCEDURE Dummy (VAR D: TEXT); FAR;
  BEGIN
    Writeln(D, '--- End of Dump ---');
  END;

PROCEDURE InitUnit;               (* Initialisierungsteil der Unit          *)
  VAR
    W1,W2,W3,W4,W5: WORD;
  BEGIN
    DumpMore := Dummy;
    StackLimit := StackLimit + MoreStack;
    SavedSSeg := SSeg;
    SavedExit := ExitProc;
    ExitProc := @StackDump;
    GetDate(W3,W2,W1,W4);         (* Ab hier wird der Dateiname ermittelt   *)
    GetTime(W3,W4,W5,W5);         (* fr die Dumpdatei: 31032259.DMP        *)
    FName := LONGINTToSTRING(W1,2,10)+LONGINTToSTRING(W2,2,10)+LONGINTToSTRING(W3,2,10)+LONGINTToSTRING(W4,2,10)+'.DMP';
    WHILE System.Pos(' ',FName) > 0
     DO FName[System.Pos(' ',FName)] := '0';
  END;

BEGIN
  InitUnit;
END.
(*$S+*) (* Switch stackchecking on for all other units to be compiled       *)
