UNIT NameReg;

INTERFACE

USES StackDmp, Objects;

TYPE

  PNameRec = ^TNameRec;
  TNameRec = RECORD
    VmtLink, Size: Word;
    Name: PString;
    Next: PNameRec;
  END;

  PVmtTypeRec = ^TVmtTypeRec;
  TVmtTypeRec = RECORD
    VmtOfs: Word;
  END;

VAR
  Root: PNameRec;
  DumpNamesOnError: BOOLEAN;

FUNCTION MaxNameLen: INTEGER;

PROCEDURE AddNameRec(ANameRec: PNameRec);

PROCEDURE AddObjName(ObjPtr: POINTER; Name: STRING);

FUNCTION GetObjectName(ObjPtr: POINTER): PString;

FUNCTION GetVmtName(VmtLink: WORD): PString;

FUNCTION NewNameRec (VmtLink, Size: Word; Name: String; Next: PNameRec): PNameRec;

IMPLEMENTATION

CONST
  NotRegistered: STRING = 'not registered';

VAR
  MaxLen: INTEGER;

FUNCTION MaxNameLen: INTEGER;
  BEGIN
    MaxNameLen := MaxLen;
  END;

VAR Dump: TDumpProc;

PROCEDURE DumpNames(VAR F: TEXT); FAR;
  VAR Scan: PNameRec;
  BEGIN
    IF DumpNamesOnError THEN BEGIN
      Scan := Root;
      WHILE Scan <> Nil DO BEGIN
        Writeln(F, Scan^.Name^, '':MaxLen-Length(Scan^.Name^), Scan^.Size:5);
        Scan := Scan^.Next;
      END;
      Flush(F);
    END;
    Dump(F);
  END;

PROCEDURE AddNameRec (ANameRec: PNameRec);
VAR P: PNameRec;
BEGIN
  IF (ANameRec^.Name <> Nil) AND (Length(ANameRec^.Name^) > MaxLen)
   THEN MaxLen := Length(ANameRec^.Name^);
  P := Root;
  WHILE (P <> Nil) AND (P^.VmtLink <> ANameRec^.VmtLink) DO P := P^.Next;
  IF P = Nil
   THEN BEGIN  { There are no more doubles }
    ANameRec^.Next := Root;
    Root := ANameRec;
   END
   ELSE BEGIN  { New Name on the Object? }
    DisposeStr(P^.Name);
    P^.Name := ANameRec^.Name;
    Dispose(ANameRec);
  END;
END;

PROCEDURE AddObjName(ObjPtr: POINTER; Name: STRING);
  VAR NameRec: PNameRec;
  BEGIN
    New(Namerec);
    NameRec^.Name := NewStr(Name);
    NameRec^.VmtLink := PVmtTypeRec(ObjPtr)^.VmtOfs;
    AddNameRec(NameRec);
  END;

FUNCTION NewNameRec (VmtLink, Size: Word; Name: String; Next: PNameRec): PNameRec;
VAR P: PNameRec;
BEGIN
  New(P);
  IF Length(Name) > MaxLen THEN MaxLen := Length(Name);
  P^.VmtLink := VmtLink;
  P^.Size := Size;
  P^.Name := NewStr(Name);
  P^.Next := Next;
  NewNameRec := P;
END;

FUNCTION GetObjectName(ObjPtr: POINTER): PString;
VAR P: PNameRec;
BEGIN
  GetObjectName := GetVmtName(PVmtTypeRec(ObjPtr)^.VmtOfs);
END;

FUNCTION GetVmtName(VmtLink: WORD): PString;
VAR P: PNameRec;
BEGIN
  P := Root;
  WHILE (P <> NIL) AND (P^.VmtLink <> VmtLink) DO
    P := P^.Next;
  IF P <> NIL
   THEN GetVmtName := P^.Name
   ELSE GetVmtName := @NotRegistered;
END;

BEGIN
  MaxLen := Length(NotRegistered);
  Root := Nil;
  Dump := DumpMore;
  DumpMore := DumpNames;
  DumpNamesOnError := False;
END.
