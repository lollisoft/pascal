UNIT Inputs;

INTERFACE

USES
  Dialogs, Drivers, Objects, Views;

TYPE
  PValidInputLine = ^TValidInputLine;
  TValidInputLine = OBJECT(TInputLine)
    MinLen: INTEGER;
    CONSTRUCTOR Init (R: TRect; aMin, aMax: INTEGER);
    CONSTRUCTOR Load (VAR S: TStream);
    PROCEDURE Store (VAR S: TStream);
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

  PHexInputLine = ^THexInputLine;
  THexInputLine = OBJECT(TValidInputLine)
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

  PValInputLine = ^TValInputLine;
  TValInputLine = OBJECT(TValidInputLine)
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

  PIntInputLine = ^TIntInputLine;
  TIntInputLine = OBJECT(TValInputLine)
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

  PRealInputLine = ^TRealInputLine;
  TRealInputLine = OBJECT(TValInputLine)
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

CONST

  RValidInputLine: TStreamRec = (
    ObjType: 2901;
    VmtLink: Ofs(TypeOf(TValidInputLine)^);
    Load:    @TValidInputLine.Load;
    Store:   @TValidInputLine.Store
  );

  RValInputLine: TStreamRec = (
    ObjType: 2902;
    VmtLink: Ofs(TypeOf(TValInputLine)^);
    Load:    @TValInputLine.Load;
    Store:   @TValInputLine.Store
  );

  RHexInputLine: TStreamRec = (
    ObjType: 2903;
    VmtLink: Ofs(TypeOf(THexInputLine)^);
    Load:    @THexInputLine.Load;
    Store:   @THexInputLine.Store
  );

  RIntInputLine: TStreamRec = (
    ObjType: 2904;
    VmtLink: Ofs(TypeOf(TIntInputLine)^);
    Load:    @TIntInputLine.Load;
    Store:   @TIntInputLine.Store
  );

  RRealInputLine: TStreamRec = (
    ObjType: 2905;
    VmtLink: Ofs(TypeOf(TRealInputLine)^);
    Load:    @TRealInputLine.Load;
    Store:   @TRealInputLine.Store
  );


PROCEDURE RegisterInputs;

IMPLEMENTATION

CONSTRUCTOR TValidInputLine.Init (R: TRect; aMin, aMax: INTEGER);
  BEGIN
    TInputLine.Init(R, aMax);
    MinLen := aMin;
  END;

CONSTRUCTOR TValidInputLine.Load (VAR S: TStream);
  BEGIN
    TInputLine.Load(S);
    S.Read(MinLen, 2);
  END;

PROCEDURE TValidInputLine.Store (VAR S: TStream);
  BEGIN
    TInputLine.Store(S);
    S.Write(MinLen, 2);
  END;

FUNCTION TValidInputLine.Valid (Command: WORD): BOOLEAN;
  BEGIN
    Select;
    SelectAll(True);
    {$B-}
    Valid := (Command <> cmCancel) AND (Data <> Nil) AND
             (Length(Data^) >= MinLen) AND (Length(Data^) <= MaxLen);
  END;

FUNCTION THexInputLine.Valid (Command: WORD): BOOLEAN;
  FUNCTION isHex(S: STRING): BOOLEAN;
    CONST LegalChars = '0123456789ABCDEFabcdef';
    VAR I: INTEGER;
    BEGIN
      I := 1;
      WHILE (I <= Length(S)) AND (Pos(S[I], LegalChars)>0) DO Inc(I);
      isHex := I > Length(S);
    END;
  BEGIN
    IF TValidInputLine.Valid(Command)
     THEN Valid := (Command <> cmCancel) AND (Data <> Nil) AND isHex(Data^)
     ELSE Valid := False;
  END;

FUNCTION TValInputLine.Valid (Command: WORD): BOOLEAN;
  VAR R: REAL;
      S: STRING;
      I: INTEGER;
  BEGIN
    IF TValidInputLine.Valid(Command)
     THEN BEGIN
       IF Data <> Nil THEN BEGIN
                        S := Data^;
                        IF Pos(',',S) > 0 THEN S[Pos(',',S)] := '.';
                        Val(S, R, I);
                      END
                      ELSE I := 1;
       Valid := (Command <> cmCancel) AND (I = 0);
     END
     ELSE Valid := False;
  END;

FUNCTION TIntInputLine.Valid (Command: WORD): BOOLEAN;
  BEGIN
    IF TValInputLine.Valid(Command)
     THEN Valid := (Command <> cmCancel) AND
                   (Pos('.',Data^) = 0) AND (Pos(',',Data^) = 0) AND
                   (Pos('E',Data^) = 0) AND (Pos('e',Data^) = 0)
     ELSE Valid := False;
  END;

FUNCTION TRealInputLine.Valid (Command: WORD): BOOLEAN;
  BEGIN
    IF TValInputLine.Valid(Command)
     THEN Valid := (Command <> cmCancel) AND
                   ((Pos('.',Data^) <> 0) OR (Pos(',',Data^) <> 0) OR
                    (Pos('E',Data^) <> 0) OR (Pos('e',Data^) <> 0))
     ELSE Valid := False;
  END;

PROCEDURE RegisterInputs;
  BEGIN
    RegisterType(RValidInputLine);
    RegisterType(RValInputLine);
    RegisterType(RIntInputLine);
    RegisterType(RRealInputLine);
    RegisterType(RHexInputLine);
  END;

END.