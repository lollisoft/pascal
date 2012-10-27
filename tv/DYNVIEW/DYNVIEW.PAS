{************************************************}
{                                                }
{        Beispielprogramm fÅr dynamische         }
{        View-Objekte in einem Dialog            }
{                                                }
{************************************************}

{$X+}

PROGRAM Sample;

USES
  Dos, Objects, Drivers, Memory, Views, Menus,
  Dialogs, App;

CONST
  cmDialog = 1234;
  cmValueChanged = 5678;

TYPE
  PSampleApp = ^TSampleApp;
  TSampleApp = OBJECT(TApplication)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
  END;

  PDynamicButtons = ^TDynamicButtons;
  TDynamicButtons = OBJECT(TRadioButtons)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

  PDynamicLabel = ^TDynamicLabel;
  TDynamicLabel = OBJECT(TLabel)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

  PDynamicInputLine = ^TDynamicInputLine;
  TDynamicInputLine = OBJECT(TInputLine)
    CONSTRUCTOR Init(VAR Bounds: TRect; AMaxLen: Integer);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;


{ TDynamicInputLine }

PROCEDURE TDynamicInputLine.HandleEvent(VAR Event: TEvent);
CONST
  First: Boolean = True;
  Save: STRING[40] = '';
  NotAvailString = '<not available>';
TYPE
  PWord = ^Word;
BEGIN
  IF First THEN BEGIN
    First := False;
    Data^ := NotAvailString;
  END;
  TInputLine.HandleEvent(Event);
  IF (Event.What = evBroadcast) AND
     (Event.Command = cmValueChanged) THEN BEGIN
    IF PWord(Event.InfoPtr)^ AND 2 <> 0 THEN BEGIN
      Options := Options OR ofSelectable;
        Data^ := Save;
    END
    ELSE BEGIN
      Options := Options AND NOT ofSelectable;
      IF Data^ <> NotAvailString THEN Save := Data^;
      Data^ := NotAvailString;
    END;
    DrawView;
  END;
END;


CONSTRUCTOR TDynamicInputLine.Init(VAR Bounds: TRect; AMaxLen: Integer);
BEGIN
  TInputLine.Init(Bounds, AMaxLen);
  EventMask := EventMask OR evBroadcast;
  Options := Options AND NOT ofSelectable;
END;


{ TDynamicButtons }

PROCEDURE TDynamicButtons.HandleEvent(VAR Event: TEvent);
VAR
  Temp: Word;
BEGIN
  Temp := Value;
  TRadioButtons.HandleEvent(Event);
  IF Value <> Temp THEN BEGIN
    Temp := Value;
    Message(Owner, evBroadcast, cmValueChanged, @Temp);
  END;
END;


{ TDynamicLabel }

PROCEDURE TDynamicLabel.HandleEvent(VAR Event: TEvent);
BEGIN
  IF (Link <> NIL) AND (Link^.Options AND ofSelectable = 0) THEN
    Exit;
  TLabel.HandleEvent(Event);
END;


{ TSampleApp }

PROCEDURE TSampleApp.HandleEvent(VAR Event: TEvent);

PROCEDURE DynamicDialog;
VAR
  D: PDialog;
  I: PDynamicInputLine;
  B: PDynamicButtons;
  Control: PView;
  L: PDynamicLabel;
  R: TRect;
BEGIN
  R.Assign(2,2,35,16);
  D := New(PDialog, Init(R, 'Dynamic Dialog'));
  R.Assign(8,2,23,5);
  B := New(PDynamicButtons, Init(R,
    NewSItem('~A~lways',
    NewSItem('~H~ardware',
    NewSItem('~C~ondition', NIL)))));
  R.Assign(5,8,28,9);
  I := New(PDynamicInputLine, Init(R, 20));
  R.Assign(5,7,15,8);
  L := New(PDynamicLabel, Init(R, '~C~ondition', I));
  R.Assign(6,11,14,13);
  Control := New(PButton, Init(R, 'O~k~', cmOK, bfDefault));
  D^.Insert(Control);
  R.Assign(16,11,26,13);
  Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
  D^.Insert(Control);
  D^.Insert(B);
  D^.Insert(I);
  D^.Insert(L);
  DeskTop^.ExecView(D);
  Dispose(D);
END;

BEGIN
  TApplication.HandleEvent(Event);
  CASE Event.What OF
    evCommand:
      BEGIN
        CASE Event.Command OF
          cmDialog: DynamicDialog;
        ELSE
          Exit;
        END;
        ClearEvent(Event);
      END;
  END;
END;

PROCEDURE TSampleApp.InitMenuBar;
VAR
  R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', 100, NewMenu(
      NewItem('~D~ialog', '', kbF8, cmDialog, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      NIL))), NIL))));
END;

VAR
  SampleApp: TSampleApp;

BEGIN
  SampleApp.Init;
  SampleApp.Run;
  SampleApp.Done;
END.
