{$X+}
UNIT WorkDlg;

INTERFACE

USES
  Templ,
  BaseDlg, Strings,
  App, Dialogs, Drivers, Objects, Views;

TYPE

  PWorkDialog = ^TWorkDialog;
  TWorkDialog = OBJECT (TBaseDialog)
    CONSTRUCTOR Init (Rect: TRect);
    PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
    FUNCTION SubViewAt (Where: TPoint): PView;
  END;

CONST
   cmEditItem = 2240;

  RWorkDialog: TStreamRec = (
    ObjType: 2821;
    VmtLink: Ofs(TypeOf(TWorkDialog)^);
    Load:    @TWorkDialog.Load;
    Store:   @TWorkDialog.Store
  );

PROCEDURE RegisterWorkDlg;

IMPLEMENTATION

CONSTRUCTOR TWorkDialog.Init (Rect: TRect);
  BEGIN
    TBaseDialog.Init(Rect, '');
    Flags := (Flags OR wfMove OR wfGrow) AND NOT wfClose;
  END;

PROCEDURE TWorkDialog.HandleEvent (VAR Event: TEvent);
  VAR R: TRect;
      View: PView;
      Point: TPoint;
      Item: PNamedView;
      Template: PViewTemplate;

  FUNCTION DroppedInDialog(Template: PViewTemplate): BOOLEAN;
    VAR DialogRect: TRect;
    BEGIN
      DroppedInDialog := False;
      GetBounds(DialogRect);
      IF DialogRect.Contains(Template^.Origin) THEN BEGIN
        DeskTop^.Delete(Template);
        Template^.Delete(Template^.Client);
        DeskTop^.MakeGlobal(Template^.Origin, Template^.Origin);
        MakeLocal(Template^.Origin, Template^.Client^.Origin);
        WITH Template^
         DO Views^.Insert(New(PNamedView, Init( Name^.GetStr,Client)));
        Dispose(Template, Done);
        DeleteAll;
        InsertAll;
        Draw;
        DroppedInDialog := True;
      END;
    END;

  FUNCTION BroadCastHandled: BOOLEAN;
    BEGIN
      BroadCastHandled := False;
      CASE Event.Command OF
        cmViewDropped: IF NOT DroppedInDialog(Event.InfoPtr) THEN Exit;
        ELSE Exit;
      END;
      BroadCastHandled := True;
    END;

  BEGIN
    IF (Event.What = evMouseDown) AND Event.Double THEN BEGIN
      MakeLocal(Event.Where, Point);
      View := SubViewAt(Point);
      IF (View <> Nil) AND (View <> PView(Frame)) THEN BEGIN
        Delete(View);                       { remove from Dialog     }
        IF TypeOf(View^) = TypeOf(TLabel)
         THEN PLabel(View)^.Link := Nil
         ELSE Views^.RemoveLinksTo(View);
        MakeGlobal(View^.Origin, View^.Origin);
        DeskTop^.MakeLocal(View^.Origin, View^.Origin);
        Item := Views^.At(Views^.ViewIndexOf(View));
        Views^.AtDelete(Views^.ViewIndexOf(View));
        Template := New ( PViewTemplate,
                            Init ( View, View^.Size, View^.Size,
                                   Item^.Name^.GetStr));
        Item^.View := Nil;
        Dispose(Item, Done);
        DeskTop^.Insert(Template);
        ClearEvent(Event);
      END;
    END;
    TBaseDialog.HandleEvent(Event);
    CASE Event.What OF
      evBroadCast: IF NOT BroadCastHandled THEN Exit;
      ELSE Exit;
    END;
    ClearEvent(Event);
  END;

FUNCTION TWorkDialog.SubViewAt (Where: TPoint): PView;
  FUNCTION ContainsPoint (P: PView): BOOLEAN; FAR;
    VAR Bounds: TRect;
    BEGIN
      P^.GetBounds(Bounds);
      ContainsPoint := (P^.State AND sfVisible <> 0) AND Bounds.Contains(Where);
    END;
  BEGIN
    SubViewAt := FirstThat(@ContainsPoint);
  END;

PROCEDURE RegisterWorkDlg;
  BEGIN
    RegisterType(RWorkDialog);
  END;

END.