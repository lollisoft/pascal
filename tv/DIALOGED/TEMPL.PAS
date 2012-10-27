{$X+}

UNIT Templ;
{
  implements a move and resize template for TViews to be resized and
  moved around the DeskTop
}

INTERFACE

USES
  Drivers, Objects, Views, Dialogs,
  Strings;

CONST
  cmViewDropped = 10001;

TYPE
  PViewTemplate = ^TViewTemplate;
  TViewTemplate = OBJECT(TGroup)
    Client: PView;
    MinSize,
    MaxSize: TPoint;
    Name: PStr;
    CONSTRUCTOR Init(aClient: PView;
                     aMinSize, aMaxSize: TPoint;
                     aName: STRING);
    CONSTRUCTOR Load(VAR S: TStream);
    PROCEDURE Store(VAR S: TStream);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    FUNCTION GetPalette: PPalette; VIRTUAL;
    FUNCTION Valid (Command: WORD): BOOLEAN; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    DESTRUCTOR Done; VIRTUAL;
  END;

CONST
  RViewTemplate: TStreamRec = (
    ObjType: 2304;
    VmtLink: Ofs(TypeOf(TViewTemplate)^);
    Load:    @TViewTemplate.Load;
    Store:   @TViewTemplate.Store
  );

PROCEDURE RegisterTempl;

IMPLEMENTATION

CONSTRUCTOR TViewTemplate.Init( aClient: PView; aMinSize, aMaxSize: TPoint;
                                aName: STRING);
VAR R: TRect;
BEGIN
  aClient^.GetBounds(R);
  TGroup.Init(R);
  MaxSize := aMaxSize;
  MinSize := aMinSize;
  Options := Options OR ofTopselect;
  DragMode := dmDragGrow;
  Client := aClient;
  Client^.Origin.X := 0;
  Client^.Origin.Y := 0;
  Client^.Size := Size;
  Insert(Client);
  Name := New(PStr, Init(aName));
END;

CONSTRUCTOR TViewTemplate.Load(VAR S: TStream);
  BEGIN
    TGroup.Load(S);
    Client := Last;
    S.Read(MaxSize, SizeOf(MaxSize));
    S.Read(MinSize, SizeOf(MinSize));
    Name := PStr(S.Get);
  END;

PROCEDURE TViewTemplate.Store(VAR S: TStream);
  BEGIN
    TGroup.Store(S);
    S.Write(MaxSize, SizeOf(MaxSize));
    S.Write(MinSize, SizeOf(MinSize));
    S.Put(Name);
  END;

PROCEDURE TViewTemplate.Draw;
BEGIN
  Client^.Origin.X := 0;
  Client^.Origin.Y := 0;
  Client^.Size := Size;
  Client^.Draw;
END;

FUNCTION TViewTemplate.GetPalette: PPalette;
CONST CTemplate: String[Length(CDialog)] = CDialog;
BEGIN
  GetPalette := @CTemplate;
END;

FUNCTION TViewTemplate.Valid (Command: WORD): BOOLEAN;
  BEGIN
    Valid := True;
  END;

PROCEDURE TViewTemplate.HandleEvent(VAR Event: TEvent);
VAR MouseInTemplate,
    PointInDeskTop: TPoint;
    R: TRect;
    OldPos: TPoint;
    ResizeMode: Boolean;
    Limits: TRect;

BEGIN
  GetBounds(R);
  Owner^.MakeLocal(Event.Where, PointInDeskTop);
  IF (Event.What = evMouseDown) AND (R.Contains(PointInDeskTop)) THEN BEGIN
    Select;
    MakeLocal(Event.Where, MouseInTemplate);
    OldPos := Origin;
    ResizeMode := (MouseInTemplate.X+1 = Size.X) AND
                  (MouseInTemplate.Y+1 = Size.Y);


    {Owner^.MakeLocal(Event.Where, Event.Where);}
    Owner^.GetExtent(Limits);
    IF ResizeMode THEN
      DragView(Event, dmDragGrow, Limits, MinSize, MaxSize)
    ELSE
      DragView(Event, dmDragMove, Limits, MinSize, MaxSize);
    DrawView;
    ClearEvent(Event);
    Message(TopView, evBroadcast, cmViewDropped, @Self);
  END;
  TGroup.HandleEvent(Event);
END;

DESTRUCTOR TViewTemplate.Done;
  BEGIN
    Dispose(Name, Done);
    TGroup.Done;
  END;

PROCEDURE RegisterTempl;
  BEGIN
    RegisterType(RViewTemplate);
  END;

END.