PROGRAM UseHelp;
(* Programm zur Demonstration der kontextsensitiven Hilfe von Vision *)

{$X+}

USES App, Drivers, Dialogs, Gadgets, Menus, MsgBox, Objects, StdDlg, Views,
     (* Units fÅr die Hilfefunktion *)
     Dos, HelpFile, HelpText;

CONST
  CommandBase = 6100;
  cmNothing   = CommandBase + 1;
  cmAbout     = CommandBase + 2;

TYPE
  TMyProgram  = OBJECT (TApplication)
     Heap : PHeapView;
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE About;
     PROCEDURE InitMenuBar; VIRTUAL;
     FUNCTION GetPalette: PPalette; VIRTUAL;
     PROCEDURE GetEvent (VAR Event : TEvent); VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
     PROCEDURE Idle; VIRTUAL;
   END;

VAR
  MyProgram  : TMyProgram;

function CalcHelpName: PathStr;
var
  EXEName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
begin
  if Lo(DosVersion) >= 3 then EXEName := ParamStr(0)
  else EXEName := FSearch('usehelp.EXE', GetEnv('PATH'));
  FSplit(EXEName, Dir, Name, Ext);
  if Dir[Length(Dir)] = '\' then Dec(Dir[0]);
  CalcHelpName := FSearch('HELPtext.HLP', Dir);
end;

PROCEDURE TMyProgram.About;
var
  D: PDialog;
  Control: PView;
  R: TRect;
begin
  R.Assign(0, 0, 40, 9);
  D := New(PDialog, Init(R, 'Programminformation'));
  with D^ do
  begin
    Options := Options or ofCentered;

    R.Grow(-1, -1);
    Dec(R.B.Y, 3);
    Insert(New(PStaticText, Init(R,
      #13 +
      ^C'Turbo Vision Demo'#13 +
      #13 +
      ^C'fÅr die integrierte Hilfefunktion'#13)));

    R.Assign(15, 6, 25, 8);
    Insert(New(PButton, Init(R, 'O~K', cmOk, bfDefault)));
  end;
  if ValidView(D) <> nil then
  begin
    Desktop^.ExecView(D);
    Dispose(D, Done);
  end;
END;

FUNCTION TMyProgram.GetPalette: PPalette;
CONST
  CNewColor = CColor + CHelpColor;
  CNewBW    = CBlackWhite + CHelpBlackWhite;
  CNewMono  = CMonochrome + CHelpMonochrome;
  P : ARRAY [apColor..apMonochrome] OF String[Length(CNewColor)] =
              (CNewColor, CNewBW, CNewMono);
BEGIN
  GetPalette := @P[AppPalette];
END;

PROCEDURE TMyProgram.GetEvent (VAR Event : TEvent);
VAR
  W: PWindow;
  HFile: PHelpFile;
  HelpStrm: PDosStream;
CONST
  HelpInUse: Boolean = False;
BEGIN
  TApplication.GetEvent(Event);
  case Event.What of
    evCommand:
      if (Event.Command = cmHelp) and not HelpInUse then
      begin
        HelpInUse := True;
        HelpStrm := New(PDosStream, Init(CalcHelpName, stOpenRead));
        HFile := New(PHelpFile, Init(HelpStrm));
        if HelpStrm^.Status <> stOk then
        begin
          MessageBox('Could not open help file.', nil, mfError + mfOkButton);
          Dispose(HFile, Done);
        end
        else
        begin
          W := New(PHelpWindow,Init(HFile, GetHelpCtx));
          if ValidView(W) <> nil then
          begin
            ExecView(W);
            Dispose(W, Done);
          end;
          ClearEvent(Event);
        end;
        HelpInUse := False;
      end;
    evMouseDown:
      if Event.Buttons <> 1 then Event.What := evNothing;
  end;end;

PROCEDURE TMyProgram.HandleEvent (VAR Event : TEvent);
BEGIN
  TApplication.HandleEvent (Event);
  IF (Event.What = evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing : ;
      cmAbout   : About;
      ELSE EXIT;
    END;
  END;
  ClearEvent (Event);
END (* TMyProgram.HandleEvent *);

PROCEDURE TMyProgram.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;

CONSTRUCTOR TMyProgram.Init;
VAR
  R : TRect;
BEGIN
  TApplication.Init;
  RegisterHelpFile;
  GetExtent(R);
  R.A.X := R.B.X - 9;
  R.B.Y := R.A.Y + 1;
  Heap := New(PHeapView, Init(R));
  Insert(Heap);
END (* TMyProgram.Init *);

PROCEDURE TMyProgram.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R, NewStatusDef (0, $FFFF,
                   NewStatusKey ('Demo fÅr die Hilfe-Funktion ',0,0,
                   NewStatusKey ('~Alt-X~ zum Verlassen',kbAltX,cmQuit,
                   NewStatusKey ('', kbF10, cmMenu,
                   NewStatusKey ('~F1~ Hilfe', kbF1, cmHelp,
                   nil)))),
                nil)));
END (* TMyProgram.InitStatusLine *);

PROCEDURE TMyProgram.InitMenuBar;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PMenuBar, Init(R, NewMenu(
    NewSubMenu('~'#240'~', hcNoContext, NewMenu(
      NewItem('öber dieses ~P~rogramm', '', kbNoKey, cmAbout, hcNoContext,
      NewLine(
      NewItem('Nichts', '', kbNoKey, cmNothing, hcNoContext,
      nil)))),
    NewSubMenu('~M~enue 1', 100, NewMenu(
      NewItem('~A~uswahl 1.1', '', 0, cmNothing, hcNoContext,
      nil)),
    NewSubMenu('M~e~nue 2', 100, NewMenu(
      NewItem('~A~uswahl 2.1', '', 0, cmNothing, hcNoContext,
      NewItem('A~u~swahl 2.2', '', 0, cmNothing, hcNoContext,
      nil))),
    NewSubMenu('Me~n~ue 3', 100, NewMenu(
      NewItem('~A~uswahl 3.1', '', 0, cmNothing, hcNoContext,
      NewLine(
      NewItem('A~u~swahl 3.2', '', 0, cmNothing, hcNoContext,
      NewLine(
      NewItem('Au~s~wahl 3.3', '', 0, cmNothing, hcNoContext,
      nil)))))),
    NewSubMenu('~H~ilfe', hcNoContext, NewMenu(
      NewItem('~H~ilfe Åber Hilfe', '', 0, cmHelp, hcNoContext,
      NewItem('~I~nhaltsÅbersicht', '', 0, cmHelp, hcContents,
      nil))),
    nil))))))));
END (* TMyProgram.InitMenuBar *);


BEGIN
  MyProgram.Init;
  MyProgram.Run;
  MyProgram.Done;
END (* MyProgram *).

