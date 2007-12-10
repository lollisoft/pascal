(* ------------------------------------------------------ *)
(*                       FWDEMO.PAS                       *)
(*                  Demo zu TFileWindow                   *)
(*        (c) 1991 Raimond Reichert & DMV-Verlag          *)
(* ------------------------------------------------------ *)
{$M 16384, 0, 600000}

PROGRAM tFileWindowDemo;

USES Dos, Drivers, Objects, Views, Menus, App,
     FileWin, GadGets, MsgBox;

CONST
  cmAbout = 4100;

TYPE
  pFileDemoApp = ^tFileDemoApp;
  tFileDemoApp = OBJECT (tFileApplication)
    Heap        : pHeapView;
    Clock       : pClockView;

    CONSTRUCTOR Init;
    PROCEDURE   InitScreen;                           VIRTUAL;
    PROCEDURE   InitStatusLine;                       VIRTUAL;
    PROCEDURE   InitMenuBar;                          VIRTUAL;
    PROCEDURE   Idle;                                 VIRTUAL;
    PROCEDURE   HandleEvent(VAR Event : tEvent);      VIRTUAL;
  END;


  CONSTRUCTOR tFileDemoApp.Init;
  VAR
    R : tRect;
  BEGIN
    tFileApplication.Init;

    GetExtent(R);
    Dec(R.B.X);
    R.A.X := R.B.X - 9;
    R.A.Y := R.B.Y - 1;
    Heap  := New(pHeapView, Init(R));
    Insert(ValidView(Heap));

    GetExtent(R);
    R.A.X := R.B.X - 9;
    R.B.Y := R.A.Y + 1;
    Clock := New(pClockView, Init(R));
    Insert(ValidView(Clock));
  (*  Message(@Self, evCommand, cmOpenFileWin, NIL); *)
  END;

(* ------------------------------------------------------ *)
(* InitScreen zeigt, wie erweiterte Textmodi genutzt wer- *)
(* den k”nnen. ABER ACHTUNG: Der hier verwendete Modi ist *)
(* Tseng ET3000-spezifisch. Wer HIMODE nutzen will, muá   *)
(* den entsprechenden Mode und dessen Aufl”sung selbst    *)
(* einsetzen.                                             *)
(* ------------------------------------------------------ *)
  PROCEDURE tFileDemoApp.InitScreen;
  VAR
    Regs : Registers;
    s    : STRING;
  BEGIN
    s := ParamStr(1);
    IF UpString(s) = ('/HIMODE') THEN BEGIN
      Regs.AH := $00;
      Regs.AL := $2A;
      Intr($10, Regs);
      ScreenHeight :=  40;
      ScreenWidth  := 100;
    END ELSE
      tFileApplication.InitScreen;
  END;

  PROCEDURE tFileDemoApp.InitStatusLine;
  VAR
    R : tRect;
  BEGIN
    GetExtent(R);
    R.A.Y := R.B.Y - 1;
    StatusLine := New(pStatusLine, Init(R,
      NewStatusDef(0, $FFFF,
        NewStatusKey(' ~Alt-X~ Exit ', kbAltX, cmQuit,
        NewStatusKey(' ~Alt-V~erzeichnisfenster ',
                      kbAltV, cmEnterNewDir,
        NewStatusKey(' ~Alt-F3~ Schlieáen ',
                      kbAltF3, cmClose,
      NIL))),
    NIL)));
  END;

  PROCEDURE tFileDemoApp.InitMenuBar;
  VAR
    R : tRect;
  BEGIN
    GetExtent(R);
    R.B.Y := R.A.Y+1;
    MenuBar := New(pMenuBar, Init (R, NewMenu(
      NewSubMenu(' ~'#240'~ ', hcNoContext, NewMenu(
        NewItem('~š~ber FWDEMO', '',
                kbNoKey, cmAbout, hcNoContext,
        NewLine(
        NewItem('Neue ~A~rbeitsfl„che', '',
                kbNoKey, cmClearDesk, hcNoContext,
      NIL)))),
      NewSubMenu(' ~D~atei ', hcNoContext, NewMenu(
        NewItem('Neues ~V~erzeichnis', 'Alt-V',
                kbAltV, cmEnterNewDir, hcNoContext,
        NewItem('Neue ~S~uchmaske', 'Alt-M',
                kbAltM, cmEnterNewMask, hcNoContext,
        NewLine(
        NewItem('~D~OS Shell', '',
                kbNoKey, cmDosShell, hcNoContext,
        NewItem('B~e~enden', 'Alt-X',
                kbAltX, cmQuit, hcNoContext,
      NIL)))))),
      NewSubMenu(' ~F~enster ', hcNoContext, NewMenu(
        NewItem('~B~ewegen','Ctrl-F5',
                kbCtrlF5, cmResize, hcNoContext,
        NewItem('~V~ergr”áern', 'F5',
                kbF5, cmZoom, hcNoContext,
        NewItem('~N~„chstes', 'F6',
                kbF6, cmNext, hcNoContext,
        NewItem('~S~chlieáen', 'Alt-F3',
                kbAltF3, cmClose, hcNoContext,
        NewItem('~T~ile', '', kbNoKey, cmTile, hcNoContext,
        NewItem('C~a~scade', '',
                kbNoKey, cmCascade, hcNoContext,
      NIL))))))),
      NewSubMenu(' ~E~instellung ', hcNoContext, NewMenu(
        NewItem('~U~mschalten neues Fenster', 'F8',
                kbF8, cmToggleNewWin, hcNoContext,
      NIL)),
    NIL)))))));
  END;

  PROCEDURE tFileDemoApp.Idle;
  BEGIN
    tFileApplication.Idle;
    Clock^.Update;
    Heap^.Update;
  END;

  PROCEDURE tFileDemoApp.HandleEvent(VAR Event : tEvent);
  BEGIN
    tFileApplication.HandleEvent(Event);
    IF (Event.What = evCommand) AND
       (Event.Command = cmAbout) THEN
      MessageBox(^C + 'Demo zu tFileWindow '+ ^m + ^m +
                 ^C + '(c) 1991 R.Reichert & DMV-Verlag',
                 NIL, mfInformation + mfOkButton);
  END;

VAR
  MyApp : tFileDemoApp;

BEGIN
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
END.
(* ------------------------------------------------------ *)
(*                 Ende von FILEWIN.PAS                   *)
