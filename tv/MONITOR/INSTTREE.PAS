UNIT InstTree;

INTERFACE

USES Tree, App, Views, Objects, NameReg, Dialogs;

TYPE

  PObjEntry = ^TObjEntry;
  TObjEntry = OBJECT(TEntry)
    View: PView;
    State, Options, EventMask: WORD;
    Origin, Size: ARRAY[1..2] OF STRING[5];
    HelpCtx, SizeOf, VmtLink: STRING[5];
    CONSTRUCTOR Init(AName: PString; ALevel: BYTE; AView: PView);
  END;

  PObjNameCollection = ^TObjNameCollection;
  TObjNameCollection = OBJECT (TItemCollection)
    FUNCTION GetRootString: PString; VIRTUAL;
    FUNCTION GetItemWidth: INTEGER; VIRTUAL;
    FUNCTION GetFocusString (Where: TPoint): PString; VIRTUAL;
  END;

FUNCTION GenerateTree: PObjNameCollection;

IMPLEMENTATION

CONSTRUCTOR TObjEntry.Init(AName: PString; ALevel: BYTE; AView: PView);
  BEGIN
    TEntry.Init(AName, ALevel);
    View := AView;
    State := AView^.State;
    Options := AView^.Options;
    EventMask := AView^.EventMask;
    Str(AView^.Origin.X, Origin[1]);
    Str(AView^.Origin.Y, Origin[2]);
    Str(AView^.Size.X, Size[1]);
    Str(AView^.Size.Y, Size[2]);
    Str(AView^.HelpCtx, HelpCtx);
    Str(System.SizeOf(AView^), SizeOf);
    Str(PVmtTypeRec(AView)^.VmtOfs,VmtLink);
  END;

FUNCTION TObjNameCollection.GetRootString: PString;
  BEGIN
    GetRootString := GetObjectName(Application);
  END;

FUNCTION TObjNameCollection.GetItemWidth: INTEGER;
  BEGIN
    GetItemWidth := MaxNameLen;
  END;

FUNCTION TObjNameCollection.GetFocusString (Where: TPoint): PString;
  BEGIN
    GetFocusString := Nil;
  END;

FUNCTION GenerateTree: PObjNameCollection;
  VAR ObjNameCollection: PObjNameCollection;
      Main: PView;
  PROCEDURE CollectViews(View: PView; Level: BYTE);
    VAR Entry: PObjEntry;
        Scan: PView;
    BEGIN
      IF View <> Nil THEN BEGIN
        IF Level > 0 THEN BEGIN
          New(Entry, Init(NewStr(GetObjectName(View)^), Level, View));
          ObjNameCollection^.InsertWithLine(Entry);
        END;
        IF PGroup(View)^.Last^.Owner = PGroup(View)
         THEN CollectViews(PGroup(View)^.Last, Level+1);
        Scan := View;
        View := View^.Next;
        IF View <> Nil THEN WHILE View <> Scan DO BEGIN
          IF Level > 0 THEN BEGIN
            New(Entry, Init(NewStr(GetObjectName(View)^), Level, View));
            ObjNameCollection^.InsertWithLine(Entry);
          END;
          IF PGroup(View)^.Last^.Owner = PGroup(View)
           THEN CollectViews(PGroup(View)^.Last, Level+1);
          View := View^.Next;
        END;
      END;
    END;
  BEGIN
    New(ObjNameCollection, Init(20,1));
    CollectViews(Application, 0);
    ObjNameCollection^.ComputeLinkSym;
    GenerateTree := ObjNameCollection;
  END;

END.