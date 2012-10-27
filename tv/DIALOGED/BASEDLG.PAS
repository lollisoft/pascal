{$X+}
UNIT BaseDlg;

INTERFACE

USES
  Strings,
  Dialogs, Objects, Views;

TYPE

  PViewCollection = ^TViewCollection;
  TViewCollection = OBJECT(TSortedCollection)
    FUNCTION Compare (Key1, Key2: POINTER): INTEGER; VIRTUAL;
    FUNCTION ViewIndexOf (View: PView): INTEGER;
    FUNCTION NameIndexOf (Name: STRING): INTEGER;
    PROCEDURE RemoveLinksTo (View: PView);
    PROCEDURE Insert (Item: POINTER); VIRTUAL;
  END;

  PNamedView = ^TNamedView;
  TNamedView = OBJECT(TObject)
    Name: PStr;
    View: PView;
    CONSTRUCTOR Init (aName: STRING; aView: PView);
    CONSTRUCTOR Load (VAR S: TStream);
    PROCEDURE Store (VAR S: TStream);
    DESTRUCTOR Done; VIRTUAL;
  END;

  PBaseDialog = ^TBaseDialog;
  TBaseDialog = OBJECT (TDialog)
    Views: PViewCollection;
    CONSTRUCTOR Init (R: TRect; aTitle: STRING);
    CONSTRUCTOR Load (VAR S: TStream);
    PROCEDURE Store (VAR S: TStream);
    PROCEDURE InsertAll;
    PROCEDURE DeleteAll;
    DESTRUCTOR Done; VIRTUAL;
  END;

CONST

  RViewCollection: TStreamRec = (
    ObjType: 2801;
    VmtLink: Ofs(TypeOf(TViewCollection)^);
    Load:    @TViewCollection.Load;
    Store:   @TViewCollection.Store
  );

  RNamedView: TStreamRec = (
    ObjType: 2802;
    VmtLink: Ofs(TypeOf(TNamedView)^);
    Load:    @TNamedView.Load;
    Store:   @TNamedView.Store
  );

  RBaseDialog: TStreamRec = (
  ObjType: 2803;
  VmtLink: Ofs(TypeOf(TBaseDialog)^);
  Load:    @TBaseDialog.Load;
  Store:   @TBaseDialog.Store
);

PROCEDURE RegisterBaseDlg;

IMPLEMENTATION

CONSTRUCTOR TBaseDialog.Init (R: TRect; aTitle: STRING);
  BEGIN
    TDialog.Init(R, aTitle);
    Views := New(PViewCollection, Init(20,10));
  END;

CONSTRUCTOR TBaseDialog.Load (VAR S: TStream);
  BEGIN
    TDialog.Load(S);
    Views := PViewCollection(S.Get);
    InsertAll;
  END;

PROCEDURE TBaseDialog.Store (VAR S: TStream);
  BEGIN
    DeleteAll;
    TDialog.Store(S);
    S.Put(Views);
    InsertAll;
  END;

PROCEDURE TBaseDialog.DeleteAll;
  VAR I: INTEGER;
  BEGIN
    FOR I := 0 TO Views^.Count-1 DO Delete(PNamedView(Views^.At(I))^.View);
  END;

PROCEDURE TBaseDialog.InsertAll;
  VAR I: INTEGER;
  BEGIN
    FOR I := 0 TO Views^.Count-1 DO Insert(PNamedView(Views^.At(I))^.View);
  END;

DESTRUCTOR TBaseDialog.Done;
  BEGIN
    DeleteAll;
    Dispose(Views, Done);
    TDialog.Done;
  END;

FUNCTION TViewCollection.Compare (Key1, Key2: POINTER): INTEGER;
  VAR Item1: PNamedView ABSOLUTE Key1;
      Item2: PNamedView ABSOLUTE Key2;
  BEGIN
    IF Item1^.View^.Origin.Y < Item2^.View^.Origin.Y
     THEN Compare := -1
     ELSE IF Item1^.View^.Origin.Y > Item2^.View^.Origin.Y
      THEN Compare := 1
      ELSE IF Item1^.View^.Origin.X < Item2^.View^.Origin.X
       THEN Compare := -1
       ELSE IF Item1^.View^.Origin.X > Item2^.View^.Origin.X
        THEN Compare := 1
        ELSE Compare := 0;
  END;

FUNCTION TViewCollection.ViewIndexOf (View: PView): INTEGER;
  VAR I: INTEGER;
      Found: BOOLEAN;
  BEGIN
    I := 0;
    Found := False;
    WHILE NOT Found AND (I < Count) DO BEGIN
      Found := PNamedView(At(I))^.View = View;
      IF NOT Found THEN Inc(I);
    END;
    IF Found THEN ViewIndexOf := I
             ELSE ViewIndexOf := -1;
  END;

FUNCTION TViewCollection.NameIndexOf (Name: STRING): INTEGER;
  VAR I: INTEGER;
      Found: BOOLEAN;
  BEGIN
    I := 0;
    Found := False;
    WHILE NOT Found AND (I < Count) DO BEGIN
      Found := PNamedView(At(I))^.Name^.GetStr = Name;
      IF NOT Found THEN Inc(I);
    END;
    IF Found THEN NameIndexOf := I
             ELSE NameIndexOf := -1;
  END;

PROCEDURE TViewCollection.RemoveLinksTo (View: PView);
  VAR I: INTEGER;
      P: PView;
  BEGIN
    FOR I := 0 TO Count-1 DO BEGIN
      P := PNamedView(At(I))^.View;
      IF (TypeOf(P^) = TypeOf(TLabel)) AND (PLabel(P)^.Link = View)
       THEN PLabel(P)^.Link := Nil;
    END;
  END;

PROCEDURE TViewCollection.Insert (Item: POINTER);
  VAR I: INTEGER;
      P: PNamedView;
  BEGIN
    TSortedCollection.Insert(Item);
    Search(Item,I);
    P := PNamedView(Item);
    IF TypeOf(P^.View^) = TypeOf(TLabel)
     THEN BEGIN
      IF I+1 < Count THEN PLabel(P^.View)^.Link := PNamedView(At(I+1))^.View;
     END
     ELSE IF I > 0 THEN BEGIN
      P := PNamedView(At(I-1));
      IF (I-1 >= 0) AND (TypeOf(P^.View^) = TypeOf(TLabel))
         AND (PLabel(P^.View)^.Link = Nil)
       THEN PLabel(P^.View)^.Link := PNamedView(Item)^.View;
    END;
  END;

CONSTRUCTOR TNamedView.Init (aName: STRING; aView: PView);
  BEGIN
    TObject.Init;
    Name := New(PStr, Init(aName));
    View := aView;
  END;

CONSTRUCTOR TNamedView.Load (VAR S: TStream);
  BEGIN
    TObject.Init;
    Name := PStr(S.Get);
    View := PView(S.Get);
  END;

PROCEDURE TNamedView.Store (VAR S: TStream);
  BEGIN
    S.Put(Name);
    S.Put(View);
  END;

DESTRUCTOR TNamedView.Done;
  BEGIN
    Dispose(Name, Done);
    IF View <> Nil THEN Dispose(View, Done);
    TObject.Done;
  END;

PROCEDURE RegisterBaseDlg;
  BEGIN
    RegisterType(RBaseDialog);
    RegisterType(RViewCollection);
    RegisterType(RNamedView);
  END;

END.