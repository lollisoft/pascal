PROGRAM XMenueBar;
{$X+}
(* Dieses Programm benutzt einen TXmsStream um den Men�baum abzuspeichern *)
(* Dieser TXmsStream wird in den Methoden PutMenu und GetMenu angewendet. *)

USES Dos, App, Drivers, Dialogs, Gadgets, Memory, Menus, Objects, StdDlg,
     Views, XmsStm;

CONST
  cmNothing   = 5101;
  cmDisableM3 = 5102;
  cmEnableM3  = 5103;
  cmBel       = 5104;
  cmStoreMenu = 5105;
  cmLoadMenu  = 5106;
  cmM31       = 131;
  cmM32       = 132;
  cmM33       = 133;
  cmM34       = 134;

  cmKeyFocused = 5110;
  cmKeyDblClick= 5111;

  M3Commands : TCommandSet = [cmM31, cmM32, cmM33,cmM34];

  hcMenue1    = 1100;
  hc11        = 1110;
  hcMenue2    = 1200;
  hc21        = 1210;
  hc22        = 1220;
  hc221       = 1221;
  hc222       = 1222;
  hcEnde      = 1299;
  hcMenue3    = 1300;
  hc31        = 1310;
  hc32        = 1320;
  hc33        = 1330;
  hc34        = 1340;
  hcTricks    = 1400;
  hcTDisable  = 1410;
  hcTEnable   = 1420;

TYPE
  TMenueBarApp      = OBJECT (TApplication)
     Heap: PHeapView;
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE InitMenuBar; VIRTUAL;
     PROCEDURE DisableM3;
     PROCEDURE EnableM3;
     PROCEDURE PutMenu;
     PROCEDURE GetMenu;
     PROCEDURE Idle; VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
  END;

VAR
  MyMenueBarApp  : TMenueBarApp;

(******************************** TMenueBarApp *****************************)

CONSTRUCTOR TMenueBarApp.Init;
VAR
  R: TRect;
BEGIN
  TApplication.Init;
  RegisterMenus;
  GetExtent (R);
  R.Assign (R.B.X-9, R.B.Y-1, R.B.X-1, R.B.Y);
  Heap := New (PHeapView, Init(R));
  Insert(Heap);
END (* TMenueBarApp.Init *);

PROCEDURE TMenueBarApp.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R,
     NewStatusDef (0, hcMenue1-1,
        NewStatusKey ('Hier r�hrt sich ja �berhaupt nichts !', 0, 0,
        NewStatusKey ('', kbF10, cmMenu,
        NewStatusKey ('', kbAltF9, cmLoadMenu,
        NewStatusKey ('~Alt-X~ zum Beenden des Programms', kbAltX, cmQuit,
        nil)))),
     NewStatusDef (hcMenue1, hcMenue1,
        NewStatusKey ('Nix wie raus hier', 0, 0,
        nil),
     NewStatusDef (hcMenue1, hcMenue2-1,
        NewStatusKey ('~E~mpty Program', 0, 0,
        NewStatusKey ('~Alt-X~ to leave', kbAltX, cmQuit,
        NewStatusKey ('', kbF10, cmMenu,
        nil))),
     NewStatusDef (hcMenue2, hcMenue3-2,
        NewStatusKey ('Sie befinden Sich jetzt im Men� 2', 0, 0,
        nil),
     NewStatusDef (hcEnde, hcEnde,
        NewStatusKey ('Jetzt ist aber wirklich Schlu�', 0, 0,
        nil),
     NewStatusDef (hcMenue3, hcTricks-1,
        NewStatusKey ('Dies sind die Men�punkte, die verschwinden werden', 0, 0,
        nil),
     NewStatusDef (hcTricks, hcTEnable,
        NewStatusKey ('Und in dieser Abteilung sehen Sie,', 0, 0,
        NewStatusKey ('wie man Men�punkte verschwinden l��t', 0, 0,
        nil)),
     NewStatusDef (hcTEnable+1, $FFFF,
        NewStatusKey ('~L~eeres Programm', 0, 0,
        NewStatusKey ('~Alt-X~ zum Verlassen', kbAltX, cmQuit,
        NewStatusKey ('', kbF10, cmMenu,
        nil))),
  nil))))))))));
END (* TMenueBarApp.InitStatusLine *);

PROCEDURE TMenueBarApp.InitMenuBar;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PMenuBar, Init(R, NewMenu(
    NewSubMenu('Menue ~1~', hcMenue1, NewMenu(
      NewItem('~B~eenden', 'Alt-X', kbAltX, cmQuit, hc11,
      nil)),
    NewSubMenu('Menue ~2~', hcMenue2, NewMenu(
      NewItem('~B~ei mir piepst''s', '', 0, cmBel, hc21,
      NewSubMenu('~H~ier geht''s weiter', hc22, NewMenu(
        NewItem('~W~ohin denn ?', '', 0, cmNothing, hc221,
        NewItem('~N~a hierher !', '', 0, cmNothing, hc222,
        NewSubMenu('~D~as geht wirklich zu weit', hcMenue2, NewMenu(
          NewItem('~I~n der Tat !', '', 0, cmBel, hcMenue2,
          NewSubMenu('~o~der doch nicht ?', hcMenue2, NewMenu(
            NewItem('~E~inen hab ich noch...', '...', 0, cmBel, hcMenue2,
            NewSubMenu('~J~etzt aber kommt das Ende', hcMenue2, NewMenu(
              NewItem('~Ende~', '', 0, cmQuit, hcEnde,
              nil)),
            nil))),
          nil))),
        nil)))),
      nil))),
    NewSubMenu('Menue ~3~', hcMenue3, NewMenu(
      NewItem('~D~iese Kommandos', '', 0, cmM31, hc31,
      NewItem('~k~�nnen auf Bedarf', '', 0, cmM32, hc32,
      NewItem('~a~ktiviert oder', '', 0, cmM33, hc33,
      NewItem('~d~eaktiviert werden.', '', 0, cmM34, hc34,
      nil))))),
    NewSubMenu('~T~ricks', hcTricks, NewMenu(
      NewItem('~D~eaktivieren von Men� 3', '', 0, cmDisableM3, hcTDisable,
      NewItem('~A~ktivieren von Men� 3', '', 0, cmEnableM3, hcTEnable,
      NewItem('Abspeichern des Men�baumes', '', 0, cmStoreMenu, hcNoContext,
      nil)))),
    nil)))))));
END (* TMenueBarApp.InitMenuBar *);

PROCEDURE TMenueBarApp.DisableM3;
BEGIN
  DisableCommands (M3Commands);
END;

PROCEDURE TMenueBarApp.EnableM3;
BEGIN
  EnableCommands (M3Commands);
END;

PROCEDURE TMenueBarApp.PutMenu;
VAR
  TmpMenuBar : PMenuBar;
  R : TRect;
  S : TXmsStream;
BEGIN
  S.Init;
  S.Put (MenuBar);

  GetExtent (R);
  R.B.Y := R.A.Y +1;
  TmpMenuBar := New (PMenuBar, Init(R, NewMenu(
    NewSubMenu ('~E~rstes Men�', hcNoContext, NewMenu(
    NewItem('Load Menu', '', 0, cmLoadMenu, hcNoContext, nil)), nil))));

  (* Nur wenn kein Streamfehler aufgetreten ist, Men�leiste wechseln *)
  IF (S.ErrorInfo = stOk) THEN
  BEGIN
     MenuBar^.Menu^.Items := TmpMenuBar^.Menu^.Items;
     MenuBar^.DrawView;
  END;

  S.Done;
END;

PROCEDURE TMenueBarApp.GetMenu;
VAR
  S : TXmsStream;
  TmpMenuBar : PMenuBar;
BEGIN
  (* Hier ist nur der Aufruf von TDosStream.Init (MenuFile,stCreate) durch *)
  (* TXmsStream.Init zu ersetzen.                                          *)
  S.Init;

  TmpMenuBar := PMenuBar(S.Get);
  IF (S.ErrorInfo = stOk) THEN
  BEGIN
    MenuBar^.Menu^.Items := TmpMenuBar^.Menu^.Items;
    MenuBar^.DrawView;
  END ELSE Write(#7);
  S.Done;
END;

PROCEDURE TMenueBarApp.HandleEvent (VAR Event : TEvent);
VAR
  C : WORD;
BEGIN
  TApplication.HandleEvent (Event);
  IF (Event.What = evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing  : ;
      cmBel: Write(#7);
      cmM31, cmM32, cmM33 : Write(#7);
      cmDisableM3: DisableM3;
      cmEnableM3: EnableM3;
      cmStoreMenu: PutMenu;
      cmLoadMenu: GetMenu;
      ELSE EXIT;
    END;
  END;
  ClearEvent (Event);
END (* TMenueBarApp.HandleEvent *);

PROCEDURE TMenueBarApp.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;

BEGIN
  MyMenueBarApp.Init;
  MyMenueBarApp.Run;
  MyMenueBarApp.Done;
END (* MenueBarApp *).

