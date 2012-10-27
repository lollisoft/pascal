UNIT UseDlg;

INTERFACE

USES
  BaseDlg, Strings,
  Dialogs, Drivers, Objects, Views;

TYPE

  PUseDialog = ^TUseDialog;
  TUseDialog = OBJECT (TBaseDialog)
    CONSTRUCTOR Init(R: TRect; aTitle: STRING; SomeViews: PViewCollection);
    FUNCTION GetItem (Name: STRING; VAR Item): BOOLEAN;
    FUNCTION SetItem (Name: STRING; VAR Item): BOOLEAN;
    FUNCTION ItemSize (Name: STRING): INTEGER;
  END;

CONST

  RUseDialog: TStreamRec = (
  ObjType: 2811;
  VmtLink: Ofs(TypeOf(TUseDialog)^);
  Load:    @TUseDialog.Load;
  Store:   @TUseDialog.Store
);

PROCEDURE RegisterUseDlg;

IMPLEMENTATION

CONSTRUCTOR TUseDialog.Init ( R: TRect; aTitle: STRING;
                              SomeViews: PViewCollection);
  BEGIN
    TBaseDialog.Init(R, aTitle);
    IF SomeViews <> Nil THEN BEGIN
      DeleteAll;
      Dispose(Views, Done);
      Views := SomeViews;
    END;
    InsertAll;
  END;

FUNCTION TUseDialog.GetItem (Name: STRING; VAR Item): BOOLEAN;
  VAR Index: INTEGER;
  BEGIN
    Index := Views^.NameIndexOf(Name);
    GetItem := Index >= 0;
    PNamedView(Views^.At(Index))^.View^.GetData(Item);
  END;

FUNCTION TUseDialog.SetItem (Name: STRING; VAR Item): BOOLEAN;
  VAR Index: INTEGER;
  BEGIN
    Index := Views^.NameIndexOf(Name);
    SetItem := Index >= 0;
    PNamedView(Views^.At(Index))^.View^.SetData(Item);
  END;

FUNCTION TUseDialog.ItemSize (Name: STRING): INTEGER;
  VAR Index: INTEGER;
  BEGIN
    Index := Views^.NameIndexOf(Name);
    IF Index < 0
     THEN ItemSize := -1
     ELSE ItemSize := PNamedView(Views^.At(Index))^.View^.DataSize;
  END;

PROCEDURE RegisterUseDlg;
  BEGIN
    RegisterType(RUseDialog);
  END;

END.