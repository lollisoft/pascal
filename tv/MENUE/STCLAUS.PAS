PROGRAM StClaus;

USES App, Drivers, Dialogs, Gadgets, Memory, Menus, MsgBox, Objects, StdDlg, Views;

CONST
  cmNothing   = 101;
  cmBel       = 102;
  cm1Auswahl1 = 103;
  cm2Auswahl1 = 104;
  cm2Auswahl2 = 105;
  cm3Auswahl1 = 106;
  cm3Auswahl2 = 107;
  cm3Auswahl3 = 108;

TYPE
  TMakroApp = OBJECT (TApplication)
     Heap : PHeapView;
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE InitMenuBar; VIRTUAL;
     PROCEDURE GetEvent (VAR Event: TEvent); VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
     PROCEDURE Idle; VIRTUAL;
  END;

VAR
  MyMakroApp  : TMakroApp;
  Menu        : PMenu;
  LocalMenuOpen : BOOLEAN;

CONSTRUCTOR TMakroApp.Init;
VAR
  R: TRect;
BEGIN
  TApplication.Init;
  (* room for further initialization *)
  GetExtent(R);
  R.A.X := R.B.X - 8;
  R.B.Y := R.A.Y + 1;
  Heap := New(PHeapView, Init(R));
  Insert(Heap);
  Menu := NewMenu(NewSubMenu('Santa Claus', 0, NewMenu(
      NewSubMenu('Men ~1~', cmNothing, NewMenu(
        NewItem('Auswahl ~1~', '', 0, cmBel, hcNoContext,
        nil)),
      NewSubMenu('Men ~2~', cmBel, NewMenu(
        NewItem('Auswahl ~1~', '', 0, cm2Auswahl1, hcNoContext,
        NewItem('Auswahl ~2~', '', 0, cm2Auswahl2, hcNoContext,
        nil))),
      NewSubMenu('Men ~3~', 1100, NewMenu(
        NewItem('Auswahl ~1~', '', 0, cm3Auswahl1, hcNoContext,
        NewItem('Auswahl ~2~', '', 0, cm3Auswahl2, hcNoContext,
        NewItem('E~x~it', '', 0, cmQuit, hcNoContext,
        nil)))),
      nil)))),
    nil));
  LocalMenuOpen := FALSE;
  EventMask := EventMask OR evBroadcast;
END;

PROCEDURE TMakroApp.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;

PROCEDURE TMakroApp.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R,
                   NewStatusDef (0, $FFFF,
                      NewStatusKey ('~Alt-X~ zum Verlassen', kbAltX, cmQuit,
                      NewStatusKey ('', kbF10, cmMenu,
                   nil)),
                nil)));
END (* TMakroApp.InitStatusLine *);

PROCEDURE TMakroApp.InitMenuBar;
VAR
  R : TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PMenuBar, Init(R, NewMenu(
  NewSubMenu('~Weihnachtsmann~', 0, NewMenu(
    NewSubMenu('Men ~1~', 10, NewMenu(
      NewItem('~S~election 1.1', '', 0, cmBel, hcNoContext,
      nil)),
    NewSubMenu('Men ~2~', 10, NewMenu(
      NewItem('~S~election 2.1', '', 0, cmNothing, hcNoContext,
      NewItem('S~e~lection 2.2', '', 0, cmNothing, hcNoContext,
      nil))),
    NewSubMenu('Men ~3~', 10, NewMenu(
      NewItem('~S~election 3.1', '', 0, cm3Auswahl1, hcNoContext,
      NewItem('S~e~lection 3.2', '', 0, cmNothing, hcNoContext,
      NewItem('E~x~it', '', 0, cmQuit, hcNoContext,
      nil)))),
    nil)))),
  nil))));
END (* TMakroApp.InitMenuBar *);

PROCEDURE TMakroApp.GetEvent (VAR Event: TEvent);
VAR
  Code : WORD;
  NewEvent: TEvent;
  Box: PMenuBox;
  R: TRect;
  MP: TPoint;
BEGIN
  TApplication.GetEvent (Event);
  IF ((Event.What = evMouseDown) AND (Event.Buttons = mbRightButton)
    AND NOT LocalMenuOpen)
  THEN BEGIN
    LocalMenuOpen := TRUE;
    (* hole Mausposition *)
    MP := Event.Where;
    IF (MP.X > 70) THEN MP.X := 70;
    IF (MP.Y > 15) THEN MP.Y := 15;
    ClearEvent(Event);
    R.Assign (MP.X, MP.Y, MP.X+11, MP.Y+2);
    Box := New (PMenuBox, Init(R, Menu, NIL));
    Code := ExecView(Box);
    Dispose (Box);
    LocalMenuOpen := FALSE;
    NewEvent.What := evBroadcast;
    NewEvent.Command := Code;
    PutEvent (NewEvent);
  END;
END;

PROCEDURE TMakroApp.HandleEvent (VAR Event : TEvent);
VAR
  C : WORD;
BEGIN
  TApplication.HandleEvent (Event);
  IF ((Event.What = evCommand) OR (Event.What = evBroadcast)) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing : ;
      cm2Auswahl1, cm2Auswahl2, cm3Auswahl2 : Write(#7);
      cm3Auswahl1: c:= MessageBox ('This is a test', NIL, mfOkButton OR mfInformation);
      cmBel: Write(#7);
      ELSE EXIT;
    END;
  END;
  ClearEvent (Event);
END (* TMakroApp.HandleEvent *);

BEGIN
  MyMakroApp.Init;
  MyMakroApp.Run;
  MyMakroApp.Done;
END (* Makro *).

