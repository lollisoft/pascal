{$X+}
UNIT Strings;

INTERFACE

USES
  App, Dialogs, Drivers, IDXFile, Views, StdDlg, StrTools, Objects;

TYPE PStr = ^TStr;
     TStr = OBJECT (TObject)
       CONSTRUCTOR Init (S: STRING);
       PROCEDURE SetStr (S: STRING);
       FUNCTION GetStr: STRING;
       CONSTRUCTOR Load (VAR S: TStream);
       PROCEDURE Store (VAR S: TStream);
       DESTRUCTOR Done; VIRTUAL;
     PRIVATE
       P: PString;
     END;

     PStrInputLine = ^TStrInputLine;
     TStrInputLine = OBJECT (TInputLine)
       CONSTRUCTOR Init (VAR Bounds: TRect; AMaxLen: INTEGER);
       PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
     END;

     PStrCollection = ^TStrCollection;
     TStrCollection = OBJECT (TSortedCollection)
       FUNCTION Compare (Key1, Key2: POINTER): INTEGER; VIRTUAL;
       PROCEDURE FreeItem (Item: POINTER); VIRTUAL;
       FUNCTION GetItem (VAR S: TStream): POINTER; VIRTUAL;
       PROCEDURE PutItem (VAR S: TStream; Item: POINTER); VIRTUAL;
     END;

     PStrList = ^TStrList;
     TStrList = OBJECT (TSortedListBox)
       CONSTRUCTOR Init (VAR Bounds: TRect; AList: PStrCollection;
                         AScrollBar: PScrollBar);
       PROCEDURE FocusItem (Item: Integer); VIRTUAL;
       FUNCTION GetText(Item: Integer; MaxLen: Integer): String; VIRTUAL;
       FUNCTION GetKey (VAR S: STRING): POINTER; VIRTUAL;
       PROCEDURE HandleEvent(var Event: TEvent); VIRTUAL;
       DESTRUCTOR Done; VIRTUAL;
     END;

CONST brFocused = 2000;
      RStr: TStreamRec = (
        ObjType: 2001;
        VmtLink: Ofs(TypeOf(TStr)^);
        Load: @TStr.Load;
        Store: @TStr.Store);

FUNCTION SelectName (aFile: PObject; Save: BOOLEAN): STRING;

PROCEDURE RegisterStrings;

IMPLEMENTATION

{------------------------------- TStr --------------------------------}
CONSTRUCTOR TStr.Init (S: STRING);
  BEGIN
    TObject.Init;
    SetStr (S);
  END;

PROCEDURE TStr.SetStr (S: STRING);
  VAR I: INTEGER;
  BEGIN
    IF P <> Nil THEN FreeMem(P, Length(P^)+1);
    I := Length(S);
    WHILE S[I] = ' ' DO Dec(I);
    S[0] := Chr(I);
    IF Length(S) = 0
     THEN P := Nil
     ELSE BEGIN
      GetMem(P, Length(S)+1);
      P^ := S;
    END;
  END;

FUNCTION TStr.GetStr: STRING;
  BEGIN
    IF P = Nil
     THEN GetStr := ''
     ELSE GetStr := P^;
  END;

CONSTRUCTOR TStr.Load (VAR S: TStream);
  BEGIN
    TObject.Init;
    P := S.ReadStr;
  END;

PROCEDURE TStr.Store (VAR S: TStream);
  BEGIN
    S.WriteStr(P);
  END;

DESTRUCTOR TStr.Done;
  BEGIN
    SetStr('');
    TObject.Done;
  END;

{-------------------------------- TStrInputLine ----------------------------}
CONSTRUCTOR TStrInputLine.Init (VAR Bounds: TRect; AMaxLen: INTEGER);
  BEGIN
    TInputLine.Init (Bounds, AMaxLen);
    EventMask := EventMask OR evBroadCast;
  END;

PROCEDURE TStrInputLine.HandleEvent (VAR Event: TEvent);
  BEGIN
    TInputLine.HandleEvent (Event);
    IF (Event.What = evBroadCast) AND (Event.Command = brFocused) AND
       (State AND sfSelected = 0) THEN Data^ := STRING(Event.InfoPtr^);
  END;

{-------------------------------- TStrCollection ---------------------------}
FUNCTION TStrCollection.Compare (Key1, Key2: POINTER): INTEGER;
  BEGIN
    IF DownStr(PStr(Key1)^.GetStr) < DownStr(PStr(Key2)^.GetStr)
     THEN Compare := -1
     ELSE IF DownStr(PStr(Key1)^.GetStr) = DownStr(PStr(Key2)^.GetStr)
      THEN Compare := 0
      ELSE Compare := 1;
  END;

PROCEDURE TStrCollection.FreeItem (Item: POINTER);
  BEGIN
    Dispose(PStr(Item), Done);
  END;

FUNCTION TStrCollection.GetItem (VAR S: TStream): POINTER;
  BEGIN
    GetItem := S.Get;
  END;

PROCEDURE TStrCollection.PutItem (VAR S: TStream; Item: POINTER);
  BEGIN
    S.Put(Item);
  END;

{-------------------------------- TStrList ---------------------------------}
CONSTRUCTOR TStrList.Init (VAR Bounds: TRect; AList: PStrCollection;
                           AScrollBar: PScrollBar);
  BEGIN
    TSortedListBox.Init(Bounds, 1, AScrollBar);
    NewList(AList);
  END;


DESTRUCTOR TStrList.Done;
  BEGIN
    Dispose(List, Done);
    TSortedListBox.Done;
  END;

PROCEDURE TStrList.FocusItem (Item: Integer);
  VAR S: STRING;
  BEGIN
    TSortedListBox.FocusItem(Item);
    S := PStr(List^.At(Item))^.GetStr;
    Message(TopView, evBroadcast, brFocused, @S);
  END;

FUNCTION TStrList.GetText(Item: Integer; MaxLen: Integer): String;
  BEGIN
    GetText := Copy(PStr(List^.At(Item))^.GetStr,1,MaxLen);
  END;

FUNCTION TStrList.GetKey (VAR S: STRING): POINTER;
  BEGIN
    GetKey := New(PStr, Init(S));
  END;

PROCEDURE TStrList.HandleEvent(var Event: TEvent);
  BEGIN
    IF (Event.What = evMouseDown) and (Event.Double)
     THEN BEGIN
      Event.What := evCommand;
      Event.Command := cmOK;
      PutEvent(Event);
      ClearEvent(Event);
     END
     ELSE TSortedListBox.HandleEvent(Event);
  END;

{-------------------------------- TStrDialog -------------------------------}
TYPE PStrDialog = ^TStrDialog;
     TStrDialog = OBJECT(TDialog)
       StrItem: PStrInputLine;
       StrList: PStrList;
       CONSTRUCTOR Init (VAR Bounds: TRect; List: PStrCollection; IL: BOOLEAN);
     END;

CONSTRUCTOR TStrDialog.Init (VAR Bounds: TRect; List: PStrCollection; IL: BOOLEAN);
  VAR Control: PView;
      R: TRect;
  BEGIN
    TDialog.Init(Bounds, '');

    IF IL THEN BEGIN
      GetExtent(R);
      R.Assign( 2, 2, R.B.X-2, 3);
      Control := New(PInputLine, Init(R, 80));
      Insert(Control);
    END;

    GetExtent(R);
    R.Assign ( 3, R.B.Y-5, R.B.X-3, R.B.Y-4);
    Control := New(PScrollBar, Init(R));
    GetExtent(R);
    R.Assign ( 3, R.B.Y-15, R.B.X-3, R.B.Y-5);
    StrList := New(PStrList, Init(R, List, PScrollBar(Control)));
    Insert(StrList);
    Insert(Control);

    GetExtent(R);
    R.Assign( 2, R.B.Y-3, 12, R.B.Y-1);
    Control := New(PButton, Init(R, 'O~k~', cmOk, bfDefault));
    Insert(Control);

    GetExtent(R);
    R.Assign( 18, R.B.Y-3, 28, R.B.Y-1);
    Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
    Insert(Control);

    SelectNext(False);
  END;

FUNCTION RezContents (RezFile: PResourceFile): PStrCollection;
  VAR List: PStrCollection;
      I: INTEGER;
  BEGIN
    List := New(PStrCollection, Init(20,1));
    FOR I := 0 TO RezFile^.Count-1
     DO List^.Insert(New(PStr, Init(RezFile^.KeyAt(I))));
    RezContents := List;
  END;

FUNCTION IDXContents (IDXFile: PIndexFile): PStrCollection;
  VAR List: PStrCollection;
      I: INTEGER;
  BEGIN
    List := New(PStrCollection, Init(20,1));
    FOR I := 0 TO IDXFile^.Count-1
     DO List^.Insert(New(PStr, Init(IDXFile^.KeyAt(I))));
    IDXContents := List;
  END;

FUNCTION SelectName (aFile: PObject; Save: BOOLEAN): STRING;
  TYPE TLReturn = RECORD
         Name: STRING[80];
         List: PStrList;
         Item: INTEGER;
       END;
       TSReturn = RECORD
         List: PStrList;
         Item: INTEGER;
       END;
  VAR Dialog: PStrDialog;
      List: PStrCollection;
      SReturn: TSReturn;
      LReturn: TLReturn;
      I: INTEGER;
      S: STRING;
      R: TRect;
  BEGIN
    IF TypeOf(aFile^) = TypeOf(TIndexFile)
     THEN List := IDXContents(PIndexFile(aFile))
     ELSE IF TypeOf(aFile^) = TypeOf(TResourceFile)
      THEN List := RezContents(PResourceFile(aFile))
      ELSE RunError(255);
    IF Save THEN R.Assign(15,1,63,20) ELSE R.Assign(15,1,63,18);
    Dialog := New(PStrDialog, Init(R, List, Save));
    IF DeskTop^.ExecView(Dialog) <> cmCancel
     THEN BEGIN
      IF Save THEN Dialog^.GetData(LReturn)
              ELSE Dialog^.GetData(SReturn);
      IF Save
       THEN IF LReturn.Name = ''
        THEN S := PStr(List^.At(LReturn.Item))^.GetStr
        ELSE S := LReturn.Name
       ELSE S := PStr(List^.At(SReturn.Item))^.GetStr
     END
     ELSE S := '';
    Dispose(Dialog, Done);
    SelectName := S;
  END;

PROCEDURE RegisterStrings;
  BEGIN
    RegisterType(RStr);
  END;

END.