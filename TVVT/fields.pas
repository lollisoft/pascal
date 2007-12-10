(********************************************************************)
(*                           FIELDS.PAS                             *)
(*                                                                  *)
(*       Die Unit FIELDS aus \tvdemos ins deutsche �bertragen       *)
(*                                                                  *)
(*         Copyright (c) 1990 by Borland International              *)
(********************************************************************)
UNIT Fields;

INTERFACE

USES Objects, Drivers, Dialogs, Views, MsgBox;

TYPE
  pKeyInputLine = ^tKeyInputLine;
  tKeyInputLine = OBJECT (tInputLine)
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

  pNumInputLine = ^tNumInputLine;
  tNumInputLine = OBJECT (tInputLine)
    Min: LONGINT;
    Max: LONGINT;

    CONSTRUCTOR Init (VAR Bounds: tRect;
                      aMaxLen:    INTEGER;
                      aMin, aMax: LONGINT);
    CONSTRUCTOR Load (VAR S: tStream);
    FUNCTION DataSize: WORD; VIRTUAL;
    PROCEDURE GetData (VAR Rec); VIRTUAL;
    PROCEDURE SetData (VAR Rec); VIRTUAL;
    PROCEDURE Store (VAR S: tStream);
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
  END;

PROCEDURE RegisterFields;

CONST
  rKeyInputLine: tStreamRec =
    (ObjType: 10060;
     VmtLink: Ofs (TypeOf (tKeyInputLine)^);
     Load:    @tKeyInputLine.Load;
     Store:   @tKeyInputLine.Store);
  rNumInputLine: tStreamRec =
    (ObjType: 10061;
     VmtLink: Ofs (TypeOf (tNumInputLine)^);
     Load:    @tNumInputLine.Load;
     Store:   @tNumInputLine.Store);

IMPLEMENTATION

(* ================================================================ *)
(*                            tKeyInputLine                         *)
(* ================================================================ *)
(* tKeyInputLine ist ein Nachkomme von tInputLine, der eine leere   *)
(* Eingabe nicht zul�sst, sondern �ber eine MessageBox den Benutzer *)
(* informiert, dass er das Feld doch bitte ausf�llen soll. Vorher   *)
(* selektiert Valid das eigene Objekt, damit der Benutzer weiss,    *)
(* welches Feld er ausf�llen muss.                                  *)
(* ---------------------------------------------------------------- *)
FUNCTION tKeyInputLine.Valid (Command: WORD): BOOLEAN;
  VAR Ok: BOOLEAN;
BEGIN
  Ok := TRUE;
  IF (Command <> cmCancel) AND (Command <> cmValid) THEN BEGIN
    IF Data^ = '' THEN BEGIN
      Select;
      MessageBox ('Dieses Feld darf nicht leer sein !',
                  NIL, mfError + mfOkButton);
      Ok := FALSE;
    END;
  END;
  IF Ok THEN Valid := tInputLine.Valid (Command)
        ELSE Valid := FALSE;
END;

(* ================================================================ *)
(*                           tNumInputLine                          *)
(* ================================================================ *)
(* tNumInputLine nimmt ganzzahlige Eingaben entgegen, die im        *)
(* Bereich aMin bis aMax liegen m�ssen. aMaxLen gibt die maximale   *)
(* L�nge der Zahl an (z.B. 2: nur zweistellige Eingaben erlaubt).   *)
(* ---------------------------------------------------------------- *)
CONSTRUCTOR tNumInputLine.Init (VAR Bounds: tRect;
                                aMaxLen:    INTEGER;
                                aMin, aMax: LONGINT);
BEGIN
  tInputLine.Init (Bounds, aMaxLen);
  Min := aMin;  Max := aMax;
END;

CONSTRUCTOR tNumInputLine.Load (VAR S: tStream);
BEGIN
  tInputLine.Load (S);
  S.Read (Min, SizeOf (LongInt) * 2);
END;

FUNCTION tNumInputLine.DataSize: WORD;
BEGIN
  DataSize := SizeOf (LongInt);
END;

PROCEDURE tNumInputLine.GetData (VAR Rec);
  VAR Code: INTEGER;
BEGIN
  Val (Data^, Longint (Rec), Code);
END;

PROCEDURE tNumInputLine.Store (VAR S: tStream);
BEGIN
  tInputLine.Store (S);
  S.Write (Min, SizeOf (Longint) * 2);
END;

PROCEDURE tNumInputLine.SetData (VAR Rec);
  VAR S: STRING [12];
BEGIN
  Str (Longint (Rec), Data^);
  SelectAll (True);
END;

FUNCTION tNumInputLine.Valid (Command: WORD): BOOLEAN;
  VAR
    Code:   INTEGER;
    Value:  LONGINT;
    Params: ARRAY [0..1] OF LONGINT;
    Ok:     BOOLEAN;
BEGIN
  Ok := TRUE;
  IF (Command <> cmCancel) AND (Command <> cmValid) THEN BEGIN
    IF Data^ = '' THEN Data^ := '0';
    Val (Data^, Value, Code);
    IF (Code <> 0) OR (Value < Min) OR (Value > Max) THEN BEGIN
      Select;
      Params [0] := Min;
      Params [1] := Max;
      MessageBox ('Die Zahl muss im Bereich von %D bis %D liegen.',
                  @Params, mfError + mfOkButton);
      SelectAll (TRUE);
      Ok := FALSE;
    END;
  END;
  IF Ok THEN Valid := tInputLine.Valid (Command)
  ELSE Valid := FALSE;
END;

PROCEDURE RegisterFields;
BEGIN
  RegisterType (rKeyInputLine);
  RegisterType (rNumInputLine);
END;

END.

(* ---------------------------------------------------------------- *)
(*                                FIELDS.PAS                        *)
(* ---------------------------------------------------------------- *)
