{*****************************************************}
{                                                     }
{       Einfaches Telekommunikationsprogramm          }
{                                                     }
{*****************************************************}

PROGRAM VisionTerminal;

USES
  Crt, Dos, ComIO, VTDlg, DialDir, VtTYPE, Viewer,
  App, Dialogs, Drivers, Menus, OBJECTs, StdDlg, Views;

TYPE
  TVterm = OBJECT(TApplication)
    ComWindow: PComWindow;
    CONSTRUCTOR Init;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    FUNCTION GetLoadPath: DirStr;
    PROCEDURE ViewFile;
    PROCEDURE Idle; VIRTUAL;
    PROCEDURE InitStatusLine; VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
  END;


{ TVterm }

CONSTRUCTOR TVterm.Init;
VAR Event: TEvent;
BEGIN
  TApplication.Init;
  ComWindow := New(PComWindow, Init);
  Desktop^.Insert(ComWindow);
END;

PROCEDURE TVterm.InitStatusLine;
VAR R: TRect;
BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit',   kbAltX, cmQuit,
      NewStatusKey('~F3~ Open Log',  kbF3, cmOpenLogFile,
      NewStatusKey('~F5~ Zoom',      kbF5, cmZoom,
      NewStatusKey('~F6~ Next',      kbF6, cmNext,
      NewStatusKey('~Alt-F3~ Close', kbAltF3, cmClose,
      NewStatusKey('~F10~ Menu',      kbF10, cmMenu,
      NIL)))))),
    NIL)
  ));
END;

PROCEDURE TVterm.InitMenuBar;
VAR R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('~O~pen Log', 'F3', kbF3, cmOpenLogFile, hcNoContext,
      NewItem('~C~lose Log', '', 0, cmCloseLogFile, hcNoContext,
      NewLine(
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      NIL))))),
    NewSubMenu('~P~hone', hcNoContext, NewMenu(
      NewItem('~D~ial Directory', 'Alt-D', kbAltD, cmDialDlg, hcNoContext,
      NewItem('~H~ang up phone', 'Alt-H', kbAltH, cmDoHangUp, hcNoContext,
      NIL))),
    NewSubMenu('~S~etup', hcNoContext, NewMenu(
      NewItem('~L~ine Settings', '', 0, cmLineSet, hcNoContext,
      NewItem('Local ~E~cho', '', 0, cmSetLocalEcho, hcNoContext,
      NIL))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      NewItem('~N~ext', 'F6', kbF6, cmNext, hcNoContext,
      NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
      NewItem('~T~ile', '', 0, cmTile, hcNoContext,
      NewItem('~V~iew File', '', 0, cmViewFile, hcNoContext,
      NIL))))),
    NIL)))
  ))));
END;

PROCEDURE TVterm.ViewFile;
VAR
  D: PFileDialog;
  FileName: PathStr;
  W: PWindow;
BEGIN
  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'Open a File',
    '~N~ame', fdOpenButton, 100))));
  IF D <> NIL THEN
  BEGIN
    IF Desktop^.ExecView(D) <> cmCancel THEN
    BEGIN
      D^.GetFileName(FileName);
      W := PWindow(ValidView(New(PFileWindow,Init(FileName))));
      IF W <> NIL THEN Desktop^.Insert(W);
    END;
    Dispose(D, Done);
  END;
END;

PROCEDURE TVterm.Idle;
{ wir verwenden Leerlauf, um Zeichen aus dem Puffer zu lesen }
VAR S: STRING;
BEGIN
  TApplication.Idle;
  IF ComWindow^.Com.Avail THEN
  BEGIN
    S := '';
    WHILE ComWindow^.Com.Avail DO
    BEGIN
      S := S + Char(ComWindow^.Com.Get);
      IF Length(S) = 255 THEN
      BEGIN
        Write(ComWindow^.Win, S);
        S := '';
      END;
    END;
    Write(ComWindow^.Win, S);
  END;
END;

FUNCTION TVterm.GetLoadPath: DirStr;
VAR
  D: DirStr;
  N: NameStr;
  E: ExtStr;
BEGIN
  FSplit(ParamStr(0), D, N, E);
  GetLoadPath := D;
END;

PROCEDURE TVterm.HandleEvent(VAR Event: TEvent);

PROCEDURE Tile;
VAR
  R: TRect;
BEGIN
  Desktop^.GetExtent(R);
  Desktop^.Tile(R);
END;

PROCEDURE HangUp;
BEGIN
  ComWindow^.Com.PutString('+++');
  Delay(1500);
  ComWindow^.Com.PutString('ATH0'^M);
END;

BEGIN
  TApplication.HandleEvent(Event);
  IF Event.What = evCommand THEN
  BEGIN
    CASE Event.Command OF
      cmSetLocalEcho:
        ComWindow^.SetLocalEcho;
      cmLineSet:
        ComWindow^.SetLineDialog;
      cmDialDlg:
        DialDialog(GetLoadPath+cDialDirName);
      cmDoHangUp:
        HangUp;
      cmOpenLogFile:
        ComWindow^.OpenLogFile;
      cmCloseLogFile:
        ComWindow^.CloseLogFile;
      cmViewFile:
        ViewFile;
      cmTile:
        Tile;
      ELSE Exit;
    END;
    ClearEvent(Event);
  END;
END;

VAR
    VTerm: TVterm;
BEGIN
  VTerm.Init;
  VTerm.Run;
  VTerm.Done;
END.
