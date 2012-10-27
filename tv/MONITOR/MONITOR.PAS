UNIT Monitor;

INTERFACE

USES
  Dos,
  DosTools, Strings,
  IDXFile,
  InstTree, NameReg, Tree, StrTools,
  App, Dialogs, Drivers, Objects, Views;

CONST
  cmDelete           = 2100;
  hcInstance         = 2100;

PROCEDURE InstanceMonitor;

IMPLEMENTATION

TYPE
  PInstanceTree = ^TInstanceTree;
  TInstanceTree = OBJECT (TDialog)
    CONSTRUCTOR Init(VAR R: TRect; VAR Tree: PObjNameCollection);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    FUNCTION GetHelpCtx: WORD; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    DESTRUCTOR Done; VIRTUAL;
  PRIVATE
    TreeView: PTreeViewer;
    Cur, AppData: PObjEntry;
  END;

VAR
  Index: TIndexFile;

CONSTRUCTOR TInstanceTree.Init(VAR R: TRect; VAR Tree: PObjNameCollection);
  VAR Control: PView;
  BEGIN
    TDialog.Init(R, 'Ownerhierachy - Snapshot');
    GetExtent(R);
    R.Assign(R.A.X+2, R.A.Y+1, R.B.X-2, R.B.Y-15);
    New(TreeView, Init(R, Tree));
    Insert(TreeView);

    GetExtent(R);
    R.Assign(R.A.X+2, R.B.Y-14, R.A.X+19, R.B.Y-2);
    Control := New(PCheckBoxes, Init(R,
      NewSItem('sfVisible',
      NewSItem('sfCursorVis',
      NewSItem('sfCursorIns',
      NewSItem('sfShadow',
      NewSItem('sfActive',
      NewSItem('sfSelected',
      NewSItem('sfFocused',
      NewSItem('sfDragging',
      NewSItem('sfDisabled',
      NewSItem('sfModal',
      NewSItem('sfDefault',
      NewSItem('sfExposed', Nil))))))))))))));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+21, R.B.Y-14, R.A.X+40, R.B.Y-4);
    Control := New(PCheckBoxes, Init(R,
      NewSItem('ofSelectable',
      NewSItem('ofTopSelect',
      NewSItem('ofFirstClick',
      NewSItem('ofFramed',
      NewSItem('ofPreProcess',
      NewSItem('ofPostProcess',
      NewSItem('ofBuffered',
      NewSItem('ofTileable',
      NewSItem('ofCenterX',
      NewSItem('ofCenterY', Nil))))))))))));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+42, R.B.Y-14, R.A.X+59, R.B.Y-4);
    Control := New(PCheckBoxes, Init(R,
      NewSItem('evMouseDown',
      NewSItem('evMouseUp',
      NewSItem('evMouseMove',
      NewSItem('evMouseAuto',
      NewSItem('evKeyDown',
      NewSItem('?????????',
      NewSItem('?????????',
      NewSItem('?????????',
      NewSItem('evCommand',
      NewSItem('evBroadCast', Nil))))))))))));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-14, R.A.X+78, R.B.Y-13);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+60, R.B.Y-14, R.A.X+70, R.B.Y-13);
    Control := New(PLabel, Init(R, 'Origin.X:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-13, R.A.X+78, R.B.Y-12);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+60, R.B.Y-13, R.A.X+70, R.B.Y-12);
    Control := New(PLabel, Init(R, 'Origin.Y:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-11, R.A.X+78, R.B.Y-10);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+62, R.B.Y-11, R.A.X+70, R.B.Y-10);
    Control := New(PLabel, Init(R, 'Size.X:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-10, R.A.X+78, R.B.Y-9);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+62, R.B.Y-10, R.A.X+70, R.B.Y-9);
    Control := New(PLabel, Init(R, 'Size.Y:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-8, R.A.X+78, R.B.Y-7);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+61, R.B.Y-8, R.A.X+70, R.B.Y-7);
    Control := New(PLabel, Init(R, 'HelpCtx:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-7, R.A.X+78, R.B.Y-6);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+62, R.B.Y-7, R.A.X+70, R.B.Y-6);
    Control := New(PLabel, Init(R, 'SizeOf:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+71, R.B.Y-5, R.A.X+78, R.B.Y-4);
    Control := New(PInputLine, Init(R, 5));
    Insert(Control);
    GetExtent(R);
    R.Assign(R.A.X+61, R.B.Y-5, R.A.X+70, R.B.Y-4);
    Control := New(PLabel, Init(R, 'VmtLink:', Control));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+20, R.B.Y-3, R.A.X+30, R.B.Y-1);
    Control := New(PButton, Init(R, 'O~k~ay', cmOk, bfNormal));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+32, R.B.Y-3, R.A.X+42, R.B.Y-1);
    Control := New(PButton, Init(R, '~C~ancel', cmCancel, bfDefault));
    Insert(Control);

    GetExtent(R);
    R.Assign(R.A.X+44, R.B.Y-3, R.A.X+54, R.B.Y-1);
    Control := New(PButton, Init(R, '~D~elete', cmDelete, bfNormal));
    Insert(Control);

    SelectNext(False);
  END;

PROCEDURE TInstanceTree.HandleEvent(VAR Event: TEvent);
  BEGIN
    TDialog.HandleEvent(Event);
    CASE Event.What OF
      evBroadCast: BEGIN
        IF Event.Command = cmEntryFocusedCount
         THEN BEGIN
          GetData(Cur^.State);
          IF INTEGER(Event.InfoPtr^) = -1
           THEN Cur := AppData
           ELSE Cur := TreeView^.ItemCol^.At(INTEGER(Event.InfoPtr^));
          SetData(Cur^.State);
          DrawView;
        END;
      END;
      evCommand: CASE Event.Command OF
        cmDelete: IF Cur^.View^.Owner <> Nil
          THEN Cur^.View^.Owner^.Delete(Cur^.View);
      END;
    END;
  END;

FUNCTION TInstanceTree.GetHelpCtx: WORD;
  BEGIN
    GetHelpCtx := hcInstance;
  END;

PROCEDURE TInstanceTree.Draw;
  VAR I: INTEGER;
      AView: PView;
      Buf: TDrawBuffer;
  BEGIN
    TDialog.Draw;
    AView := Cur^.View;
    WHILE AView^.GetPalette = Nil DO AView := AView^.Owner;
    FOR I := Length(AView^.GetPalette^) DOWNTO 1 DO
      MoveChar(Buf, 'X', AView^.GetColor(I), I);
    WriteLine(2,Size.Y-15,Length(AView^.GetPalette^),1,Buf);
  END;

DESTRUCTOR TInstanceTree.Done;
  BEGIN
    Dispose(AppData, Done);
    TDialog.Done;
  END;

PROCEDURE InstanceMonitor;
  VAR Tree: PObjNameCollection;
      Dialog: PInstanceTree;
      P: PObjEntry;
      I: INTEGER;
      R: TRect;
  PROCEDURE SetValues(P: PObjEntry);
    PROCEDURE SetValue(S: STRING; VAR Value: INTEGER);
      VAR W, E: INTEGER;
      BEGIN
        Val(S, W, E);
        IF E = 0 THEN Value := W;
      END;
    BEGIN
      P^.State := P^.View^.State;
      P^.Options := P^.View^.Options;
      P^.EventMask := P^.View^.EventMask;
      SetValue(P^.Origin[1], P^.View^.Origin.X);
      SetValue(P^.Origin[2], P^.View^.Origin.Y);
      SetValue(P^.Size[1], P^.View^.Size.X);
      SetValue(P^.Size[2], P^.View^.Size.Y);
    END;
  BEGIN
    Tree := GenerateTree;
    New(P, Init(NewStr(GetObjectName(Application)^), 0, Application));
    DeskTop^.GetExtent(R);
    New(Dialog, Init(R, Tree));
    Dialog^.AppData := P;
    Dialog^.Cur := P;
    Dialog^.SetData(Dialog^.AppData^.State);
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      Dialog^.GetData(Dialog^.Cur^.State);
      SetValues(Dialog^.AppData);
      FOR I := 0 TO Dialog^.TreeView^.ItemCol^.Count-1
       DO SetValues(PObjEntry(Dialog^.TreeView^.ItemCol^.At(I)));
      Application^.ReDraw;
    END;
    Dispose(Dialog, Done);
  END;

FUNCTION UniqueName: STRING;
  VAR Y, Mo, D, Dummy,
      H, Mi, S, S100: WORD;
  BEGIN
    GetDate(Y, Mo, D, Dummy);
    GetTime(H, Mi, S, S100);
    UniqueName := LONGINTToSTRING(D, 2, 10)+'.'+
                  LONGINTToSTRING(Mo, 2, 10)+'.'+
                  LONGINTToSTRING(Y, 4, 10)+' '+
                  LONGINTToSTRING(H, 2, 10)+':'+
                  LONGINTToSTRING(Mi, 2, 10)+':'+
                  LONGINTToSTRING(S, 2, 10)+':'+
                  LONGINTToSTRING(S100, 2, 10);
  END;

END.