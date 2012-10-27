PROGRAM Hint;
(* Programm zur Demonstration der Hints in Vision *)

USES App, Drivers, Dialogs, Gadgets, Memory, Menus, MsgBox, Objects, StdDlg, Views,
     (* Units fÅr die Hilfefunktion *)
     HintStat;

CONST
  cmNothing   = 100;

(* Definition der Hilfekontexte fÅr die Hints *)
  hcThisHint = 09;
  hcSystem   = 10;
  hcSExit    = 11;
  hcFile     = 20;
  hcFOpen    = 21;
  hcFNew     = 22;
  hcEdit     = 30;
  hcERestore = 31;
  hcECut     = 32;
  hcCompile  = 40;
  hcCCompile = 41;
  hcCMake    = 42;
  hcCBuild   = 43;

TYPE
  TMyProgram  = OBJECT (TApplication)
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE InitMenuBar; VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
   END;

VAR
  MyProgram  : TMyProgram;

PROCEDURE TMyProgram.HandleEvent (VAR Event : TEvent);
BEGIN
  TApplication.HandleEvent (Event);
  IF (Event.What = evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing : ;
      ELSE EXIT;
    END;
  END;
  ClearEvent (Event);
END (* TMyProgram.HandleEvent *);

CONSTRUCTOR TMyProgram.Init;
VAR
  R : TRect;
BEGIN
  TApplication.Init;
END (* TMyProgram.Init *);

PROCEDURE TMyProgram.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PHintStatusLine, Init(R, NewStatusDef (0, $FFFF,
                   NewStatusKey ('~Alt-X~ zum Verlassen',kbAltX,cmQuit,
                   NewStatusKey ('', kbF10, cmMenu,
                   nil)),
                nil)));
END (* TMyProgram.InitStatusLine *);

PROCEDURE TMyProgram.InitMenuBar;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PMenuBar, Init(R, NewMenu(
    NewSubMenu('~'#240'~', hcSystem, NewMenu(
      NewItem('Hinweise', '', 0, cmNothing, hcThisHint,
      NewItem('E~x~it', '', kbAltX, cmQuit, hcSExit,
      nil))),
    NewSubMenu('~F~ile', hcFile, NewMenu(
      NewItem('~O~pen', '', 0, cmNothing, hcFOpen,
      NewItem('~N~ew', '', 0, cmNothing, hcFNew,
      nil))),
    NewSubMenu('~E~dit', hcEdit, NewMenu(
      NewItem('~R~estore Line', '', 0, cmNothing, hcERestore,
      NewItem('~C~ut', '', 0, cmNothing, hcECut,
      nil))),
    NewSubMenu('~C~ompile', hcCompile, NewMenu(
      NewItem('~C~ompile', '', 0, cmNothing, hcCCompile,
      NewItem('~M~ake', '', 0, cmNothing, hcCMake,
      NewItem('~B~uild', '', 0, cmNothing, hcCBuild,
      nil)))),
    nil)))))));
END (* TMyProgram.InitMenuBar *);

BEGIN
  MyProgram.Init;
  MyProgram.Run;
  MyProgram.Done;
END (* MyProgram *).
