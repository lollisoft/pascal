PROGRAM DynamicMenueTest;

USES App, DynMenue, Drivers, Dialogs, Gadgets, Memory, Menus, Objects,
     StdDlg, Views;

CONST
  cmNothing   = 101;
  cmPutIn     = 102;
  cmPutItem   = 103;
  cmNavigate  = 104;
  cmMakeM2    = 105;
  cmRemoveM2  = 106;
  cmDeleteMe  = 107;
  cmDispose   = 108;
  MenuID      : String = '3';

TYPE
  TDynTest = OBJECT (TApplication)
     Heap : PHeapView;
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE InitMenuBar; VIRTUAL;
     PROCEDURE Idle; VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
  END;

VAR
  MyDynTest: TDynTest;
  P2       : PMenuItem;

PROCEDURE TDynTest.HandleEvent (VAR Event : TEvent);
VAR
  C : WORD;
  p3: PMenuItem;
  StartAt: PMenu;
BEGIN
  TApplication.HandleEvent (Event);
  IF (Event.What = evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing: ;
      cmPutItem: BEGIN
        StartAt := MenuBar^.Menu;
        p3 := NewItem('Freie Auswahl', '', 0, cmNothing, hcNoContext, nil);
        PDynamicMenuBar(MenuBar)^.InsertItemIntoGivenMenu (2, StartAt, p3);
        MenuBar^.DrawView;
      END;
      cmPutIn: BEGIN
        StartAt := MenuBar^.Menu;
        P2 := NewSubMenu('MenÅ ~'+MenuId+'~', 1100, NewMenu(
          NewItem('Auswahl '+MenuID+'.~1~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl '+MenuID+'.~2~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl '+MenuID+'.~3~', '', 0, cmNothing, hcNoContext,
        nil)))),nil);
        MenuID := CHR(ORD(MenuID[1])+1);
        PDynamicMenuBar(MenuBar)^.InsertItem (1, 1, StartAt, P2);
        MenuBar^.DrawView;
      END;
      cmNavigate: BEGIN
        p3 := NewItem('Freie Auswahl', '', 0, cmNothing, hcNoContext, nil);
        StartAt := PDynamicMenuBar(MenuBar)^.Navigate (1,2, MenuBar^.Menu);
        PDynamicMenuBar(MenuBar)^.InsertItem (3,4, StartAt, p3);
        MenuBar^.DrawView;
      END;
      cmMakeM2 : BEGIN
        StartAt := MenuBar^.Menu;
        P2 := NewSubMenu('Menue ~Two~', 1100, NewMenu(
          NewItem('Auswahl ~1~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl ~2~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl ~3~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl ~4~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl ~5~', '', 0, cmNothing, hcNoContext,
          NewItem('Auswahl ~6~', '', 0, cmNothing, hcNoContext,
        nil))))))),nil);
        PDynamicMenuBar(MenuBar)^.InsertItemIntoGivenMenu (2, StartAt, P2);
        MenuBar^.DrawView;
      END;
      cmRemoveM2: BEGIN
        StartAt := MenuBar^.Menu;
        p3 := PDynamicMenuBar(MenuBar)^.RemoveItem (2, 0, StartAt);
        MenuBar^.DrawView;
      END;
      cmDeleteMe: BEGIN
        StartAt := MenuBar^.Menu;
        p3 := PDynamicMenuBar(MenuBar)^.RemoveItemFromGivenMenu (0, StartAt);
        MenuBar^.DrawView;
      END;
      cmDispose: BEGIN
        StartAt := MenuBar^.Menu;
        p3 := PDynamicMenuBar(MenuBar)^.RemoveItem (1,0, StartAt);
        PDynamicMenuBar(MenuBar)^.DeleteItem (p3);
        MenuBar^.DrawView;
      END;
      ELSE EXIT;
    END;
  END;
  ClearEvent (Event);
END (* TDynTest.HandleEvent *);

CONSTRUCTOR TDynTest.Init;
VAR
  R: TRect;
BEGIN
  TApplication.Init;
  GetExtent (R);
  R.Assign (R.B.X-9, R.B.Y-1, R.B.X-1, R.B.Y);
  Heap := New(PHeapView, Init(R));
  Insert (Heap);
END (* TDynTest.Init *);

PROCEDURE TDynTest.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R,
                   NewStatusDef (0, $FFFF,
                      NewStatusKey ('~Alt-X~ to leave', kbAltX, cmQuit,
                      NewStatusKey ('', kbF10, cmMenu,
                   nil)),
                nil)));
END (* TDynTest.InitStatusLine *);

PROCEDURE TDynTest.InitMenuBar;
VAR
  R : TRect;
 BEGIN
  GetExtent (R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PDynamicMenuBar, Init(R, NewMenu(
    NewSubMenu('~M~enue 0', 100, NewMenu(
      NewItem('Insert ~M~enue', '', 0, cmPutIn, hcNoContext,
      NewItem('Insert ~I~tem', '', 0, cmPutItem, hcNoContext,
      NewItem('~G~enerate Menue Two', '', 0, cmMakeM2, hcNoContext,
      NewItem('~R~emove Menue Two', '', 0, cmRemoveM2, hcNoContext,
      NewItem('~T~ry Navigate', '', 0, cmNavigate, hcNoContext,
      NewItem('~D~elete Me...', '', 0, cmDeleteMe, hcNoContext,
      NewItem('Dispose Really...', '', 0, cmDispose, hcNoContext,
      NewItem('E~x~it', 'Alt-X', 0, cmQuit, hcNoContext,
      nil))))))))),
    NewSubMenu('Menue ~1~', 100, NewMenu(
      NewItem('~Really Nothing...~', '', 0, cmNothing, hcNoContext,
      NewItem('Item 1.~1~', '', 0, cmNothing, hcNoContext,
      NewSubMenu('Item 1.~2~', 100, NewMenu(
        NewItem ('Item 1.2.~0~', '', 0, cmNothing, hcNoContext,
        NewItem ('Item 1.2.~1~', '', 0, cmNothing, hcNoContext,
        NewItem ('Item 1.2.~2~', '', 0, cmNothing, hcNoContext,
        NewSubMenu('Item 1.2.~3~', 100, NewMenu(
          NewItem ('Item 1.2.3.~0~', '', 0, cmNothing, hcNoContext,
          NewItem ('Item 1.2.3.~1~', '', 0, cmNothing, hcNoContext,
          NewItem ('Item 1.2.3.~2~', '', 0, cmNothing, hcNoContext,
          NewItem ('Item 1.2.3.~3~', '', 0, cmNothing, hcNoContext,
          NewItem ('Item 1.2.3.~4~', '', 0, cmNothing, hcNoContext,
        nil)))))),
      nil))))),
      nil)))),
    nil)))));
END (* TDynTest.InitMenuBar *);

PROCEDURE TDynTest.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;

BEGIN
  MyDynTest.Init;
  MyDynTest.Run;
  MyDynTest.Done;
END (* DynamicMenueTest *).

