UNIT DynMenue;

INTERFACE

USES Menus;

TYPE
  PDynamicMenuBar  = ^TDynamicMenuBar;
  TDynamicMenuBar  = OBJECT (TMenuBar)
     FUNCTION  Navigate (MenuNo, Position: WORD; Start: PMenu): PMenu;
     PROCEDURE InsertItemIntoGivenMenu
                (Position: WORD; Start: PMenu; Item: PMenuItem);
     PROCEDURE InsertItem
                (MenuNo, Position: WORD; Start: PMenu; Item: PMenuItem);
     FUNCTION  RemoveItemFromGivenMenu
                (Position: WORD; Start: PMenu): PMenuItem;
     FUNCTION  RemoveItem
                (MenuNo, Position: WORD; Start: PMenu): PMenuItem;
     PROCEDURE DeleteItem (What: PMenuItem);
  END;

IMPLEMENTATION

USES App;

CONST
  NILPointer = 204;

(* RunError(204) l”st einen Laufzeitfehler 'invalid pointer operation' aus *)

FUNCTION NavigateSubMenu (Position: WORD; Start: PMenu): PMenu;
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF (Start = NIL) THEN RunError (NILPointer);
  Counter := 1;
  p := Start^.Items;
  WHILE ((p^.Next <> NIL) AND (Counter <= Position)) DO
  BEGIN
    p := p^.Next;
    INC(Counter);
  END;
  NavigateSubMenu := p^.SubMenu;
END;

FUNCTION TDynamicMenuBar.Navigate
          (MenuNo, Position: WORD; Start: PMenu): PMenu;
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF (Start = NIL) THEN RunError (NILPointer);
  BEGIN
    Counter := 1;
    p := Start^.Items;
    WHILE ((p^.Next <> NIL) AND (Counter <= MenuNo)) DO
    BEGIN
      p := p^.Next;
      INC(Counter);
    END;
    IF (Position = 0)
      THEN Navigate := p^.SubMenu
      ELSE Navigate := NavigateSubMenu (Position, p^.SubMenu);
  END;
END;

(****************************** Insert ************************************)

PROCEDURE InsertAtTop (p: PMenu; Item: PMenuItem);
BEGIN
  Item^.Next := p^.Items;
  p^.Items := Item;
END;

PROCEDURE TDynamicMenuBar.InsertItemIntoGivenMenu
          (Position: WORD; Start: PMenu; Item: PMenuItem);
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF ((Start = NIL) OR (Item = NIL)) THEN RunError (NILPointer);
  IF (Position = 0)
  THEN InsertAtTop (Start, Item)
  ELSE BEGIN
    Counter := 1;
    p := Start^.Items;
    WHILE ((p^.Next <> NIL) AND (Counter < Position)) DO
    BEGIN
      p := p^.Next;
      INC(Counter);
    END;
    Item^.Next := p^.Next;
    p^.Next := Item;
  END;
END;

PROCEDURE TDynamicMenuBar.InsertItem
          (MenuNo, Position: WORD; Start: PMenu; Item: PMenuItem);
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF ((Start = NIL) OR (Item = NIL)) THEN RunError (NILPointer);
  IF (MenuNo = 0) THEN
  BEGIN
    InsertItemIntoGivenMenu (Position, MenuBar^.Menu^.Items^.SubMenu, Item);
  END
  ELSE BEGIN
    Counter := 1;
    p := Start^.Items;
    WHILE ((p^.Next <> NIL) AND (Counter <= MenuNo)) DO
    BEGIN
      p := p^.Next;
      INC(Counter);
    END;
    InsertItemIntoGivenMenu (Position, p^.SubMenu, Item);
  END;
END;

(****************************** Remove ************************************)

FUNCTION RemoveTop (Start: PMenu): PMenuItem;
VAR
  p : PMenuItem;
BEGIN
  p := Start^.Items;
  Start^.Items := p^.Next;
  RemoveTop := p;
END;

FUNCTION TDynamicMenuBar.RemoveItemFromGivenMenu
         (Position: WORD; Start: PMenu): PMenuItem;
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF (Start = NIL) THEN RunError (NILPointer);
  IF (Position = 0)
  THEN RemoveItemFromGivenMenu := RemoveTop (Start)
  ELSE BEGIN
    Counter := 1;
    p := Start^.Items;
    WHILE ((p^.Next <> NIL) AND (Counter < Position)) DO
    BEGIN
      p := p^.Next;
      INC(Counter);
    END;
    IF (p^.Next <> NIL) THEN
    BEGIN
      RemoveItemFromGivenMenu := p^.Next;
      p^.Next := p^.Next^.Next;
    END ELSE RunError (NILPointer);
  END;
END;

FUNCTION TDynamicMenuBar.RemoveItem
         (MenuNo, Position: WORD; Start: PMenu): PMenuItem;
VAR
  p: PMenuItem;
  Counter: WORD;
BEGIN
  IF (Start = NIL) THEN RunError (NILPointer);
  IF (MenuNo = 0) THEN
  BEGIN
    RemoveItem := RemoveItemFromGivenMenu (Position, MenuBar^.Menu^.Items^.SubMenu);
  END
  ELSE BEGIN
    Counter := 1;
    p := Start^.Items;
    WHILE ((p^.Next <> NIL) AND (Counter <= MenuNo)) DO
    BEGIN
      p := p^.Next;
      INC(Counter);
    END;
    RemoveItem := RemoveItemFromGivenMenu (Position, p^.SubMenu);
  END;
END;

(****************************** Delete ************************************)

PROCEDURE TDynamicMenuBar.DeleteItem (What: PMenuItem);
BEGIN
  IF (What <> NIL)
    THEN Dispose (What)
    ELSE RunError (NILPointer);
END;

END (* DynMenue *).

