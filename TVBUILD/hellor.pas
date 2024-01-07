(* ---------------------------------------------------------------- *)
(*                              HELLOR.PAS                          *)
(*                                                                  *)
(* Eine leicht modifizierte Fassung von HELLO aus \tvdemos, die     *)
(* eine von TOOL erstellte Datei als Include-Datei einbindet und    *)
(* annimmt, dass diese eine Prozedur namens DemoDialog enthÑlt.     *)
(* Dieser Dialog wird ausgefÅhrt, wenn im "MenÅ" Hello der Punkt    *)
(* Demo gewÑhlt wird.                                               *)
(*                                                                  *)
(*                  (c) 1992 by R.Reichert & DMV-Verlag             *)
(* ---------------------------------------------------------------- *)
PROGRAM HelloResource;

USES Objects, Drivers, Views, Menus,
     Dialogs, App, Editors;

CONST
  cmDemo = 100;
  RezName= 'C:\tp\tool\DEMODLGR.REZ';

TYPE
  pHelloApp = ^tHelloApp;
  tHelloApp = OBJECT (tApplication)
    CONSTRUCTOR Init;
    PROCEDURE HandleEvent (VAR Event: tEvent);                VIRTUAL;
    PROCEDURE InitMenuBar;                                    VIRTUAL;
    PROCEDURE InitStatusLine;                                 VIRTUAL;
  END;

  pProtectedStream = ^tProtectedStream;
  tProtectedStream = OBJECT (tBufStream)
    PROCEDURE Error (Code, Info: INTEGER);                    VIRTUAL;
  END;

VAR
  ToolDemo:  tHelloApp;
  RezFile:   tResourceFile;
  RezStream: pStream;

(* ---------------------------------------------------------------- *)
(*                           tProtectedStream                       *)
(* ---------------------------------------------------------------- *)
PROCEDURE tProtectedStream.Error (Code, Info: INTEGER);
BEGIN
  RunError (255);
END;

(* ---------------------------------------------------------------- *)
(*                              tHelloApp                           *)
(* ---------------------------------------------------------------- *)
CONSTRUCTOR tHelloApp.Init;
BEGIN
  RezStream := New (pProtectedStream,
                    Init (RezName, stOpenRead, 4096));
  RezFile.Init (RezStream);

  RegisterObjects;
  RegisterViews;
  RegisterMenus;
  RegisterEditors;
  RegisterDialogs;

  tApplication.Init;
END;

PROCEDURE tHelloApp.HandleEvent(var Event: TEvent);
  {$I c:\tp\tool\DEMODLGR.PAS }
BEGIN
  tApplication.HandleEvent (Event);
  IF (Event.What = evCommand) AND
     (Event.Command = cmDemo) THEN BEGIN
    DemoDialogDialog;
    ClearEvent (Event);
  END;
END;

PROCEDURE tHelloApp.InitMenuBar;
  VAR R: tRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (pMenuBar, Init (R, NewMenu (
    NewSubMenu ('~H~ello', hcNoContext, NewMenu (
      NewItem ('~D~emo...','', 0, cmDemo, hcNoContext,
      NewLine (
      NewItem ('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
    NIL)))),
  NIL))));
END;

PROCEDURE tHelloApp.InitStatusLine;
  VAR R: TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y-1;
  StatusLine := New (pStatusLine, Init (R,
    NewStatusDef (0, $FFFF,
      NewStatusKey ('', kbF10, cmMenu,
      NewStatusKey ('~Alt-X~ Exit', kbAltX, cmQuit,
    NIL)),
  NIL)));
END;

BEGIN
  ToolDemo.Init;
  ToolDemo.Run;
  ToolDemo.Done;
END.

(* ---------------------------------------------------------------- *)
(*                           Ende von HELLO.PAS                     *)
(* ---------------------------------------------------------------- *)

