{************************************************}
{                                                }
{    Beispielprogramm fÅr TSmartInputLine        }
{                                                }
{************************************************}

{$X+}

PROGRAM Smart;

USES
  Dos, Objects, Drivers, Memory, Views, Menus,
  Dialogs, StdDlg, App, ConstCol;

CONST
  cmSmartDialog = 456;

TYPE
  PSmartApp = ^TSmartApp;
  TSmartApp = OBJECT(TApplication)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
  END;

  PSmartInputLine = ^TSmartInputLine;
  TSmartInputLine = OBJECT(TInputLine)
    Value: LongInt;
    FUNCTION Valid(Command: Word): Boolean; VIRTUAL;
    FUNCTION DataSize: Word; VIRTUAL;
    PROCEDURE GetData(VAR Rec); VIRTUAL;
    PROCEDURE SetData(VAR Rec); VIRTUAL;
  END;


{ TSmartInputLine }

FUNCTION TSmartInputLine.Valid(Command: Word): Boolean;
VAR
  Error: Integer;
  isValid: Boolean;
BEGIN
  IF (Command = cmCancel) OR
     (Data^ = '') THEN isValid := True
  ELSE BEGIN
    IF ConstCollection <> NIL THEN
      Value := ConstCollection^.GetValue(Data^)
    ELSE Value := $FFFF;
    IF Value = $FFFF THEN BEGIN
      Val(Data^, Value, Error);
      isValid := Error = 0;
    END ELSE isValid := True;
  END;
  Valid := isValid;
  IF NOT isValid THEN Select;
END;

FUNCTION TSmartInputLine.DataSize: Word;
BEGIN
  DataSize := 2;
END;


PROCEDURE TSmartInputLine.GetData(VAR Rec);
BEGIN
  Move(Value, Rec, SizeOf(Word));
END;


PROCEDURE TSmartInputLine.SetData(VAR Rec);
BEGIN
  Move(Rec, Value, SizeOf(Word));
END;


{ TSmartApp }

PROCEDURE TSmartApp.HandleEvent(VAR Event: TEvent);

PROCEDURE SmartDialog;
VAR
  D: PDialog;
  R: TRect;
  Control: PView;
  S: PSmartInputLine;
BEGIN
  R.Assign(2,2,30,12);
  D := New(PDialog, Init(R, ''));
  R.Assign(6,7,16,9);
  Control := New(PButton, Init(R, 'O~K~', cmOK, bfdefault));
  D^.Insert(Control);
  R.Assign(3,4,20,5);
  S := New(PSmartInputLine, Init(R, 30));
  D^.Insert(S);
  DeskTop^.ExecView(D);
  Dispose(D);
END;

BEGIN
  TApplication.HandleEvent(Event);
  CASE Event.What OF
    evCommand:
      BEGIN
        CASE Event.Command OF
          cmSmartDialog: SmartDialog;
        ELSE
          Exit;
        END;
        ClearEvent(Event);
      END;
  END;
END;

PROCEDURE TSmartApp.InitMenuBar;
VAR
  R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', 100, NewMenu(
      NewItem('~S~mart Dialog', '', kbNoKey, cmSmartDialog, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext, NIL))),
      NIL))));
END;

VAR
  SmartApp: TSmartApp;

BEGIN
  SmartApp.Init;
  SmartApp.Run;
  SmartApp.Done;
END.
