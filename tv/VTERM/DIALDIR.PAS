{*******************************************************}
{                                                       }
{   Implementiert den Dialog zum Abspeichern der        }
{   Eintr„ge mit Namen, Telefonnummer und Parametern    }
{                                                       }
{*******************************************************}

UNIT DialDir;

{$X+}

INTERFACE

USES Dos;

PROCEDURE DialDialog(DialFileName: ComStr);

IMPLEMENTATION

USES
  VTDlg, VtType,MsgBox, App, Objects, Dialogs,
  Drivers, Memory, StdDlg, Views, Tools;

CONST
  MaxDialEntry = 35;
  BufferSize = 2048;
  cBase = 1000;
  cmDial = cBase;
  cmInsert = cBase+1;
  cmEdit = cBase+2;
  cmDelete = cBase+3;

TYPE
  PNameInputLine = ^TNameInputLine;
  TNameInputLine = OBJECT(TInputLine)
    Collection: PSortedCollection;
    CONSTRUCTOR Init(VAR Bounds: TRect; AMaxLen: Integer;
                     ACollection: PSortedCollection);
    FUNCTION Valid(Command: Word): Boolean; VIRTUAL;
  END;

  PDialCollection = ^TDialCollection;
  TDialCollection = OBJECT(TSortedCollection)
    FUNCTION Compare(Key1, Key2: Pointer): Integer; VIRTUAL;
  END;

  PDialList = ^TDialList;
  TDialList = OBJECT(TSortedListBox)
    CONSTRUCTOR Init(VAR R: TRect; FName: ComStr; ASCrollBar: PScrollBar);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION GetText(Item, MaxLen: Integer): STRING; VIRTUAL;
    FUNCTION GetKey(VAR S: STRING): Pointer; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event:  TEvent); VIRTUAL;
  END;

  PDialDialog = ^TDialDialog;
  TDialDialog = OBJECT(TDialog)
    DialFile: ComStr;
    DialList: PDialList;
    CONSTRUCTOR Init(ADialFile: ComStr);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

CONST
  RDialCollection : TStreamRec = (
    ObjType: 1001;
    VmtLink: Ofs(TypeOf(TDialCollection)^);
    Load:    @TDialCollection.Load;
    Store:   @TDialCollection.Store
  );


{ TNameInputLine }

CONSTRUCTOR TNameInputLine.Init(VAR Bounds: TRect; AMaxLen: Integer;
                                ACollection: PSortedCollection);
BEGIN
  TInputLine.Init(Bounds, AMaxLen);
  Collection := ACollection;
END;

FUNCTION TNameInputLine.Valid(Command: Word): Boolean;
VAR
  Buf: TDialEntry;
  Idx: Integer;
BEGIN
  Valid := True;
  IF (Command <> cmCancel) AND (Command <> 0) THEN
  BEGIN
    Idx := 0;
    Buf.Name := Data^;
    IF Collection^.Search(@Buf, Idx) THEN
    BEGIN
      MessageBox('already exits',NIL,mfWarning);
      Select;
      Valid := False;
    END
  END;
END;


{ TDialCollection }

FUNCTION TDialCollection.Compare(Key1, Key2: Pointer): Integer;
BEGIN
  IF UpStr(TDialEntry(Key1^).Name) = UpStr(TDialEntry(Key2^).Name) THEN
    Compare:=0
  ELSE
    IF UpStr(TDialEntry(Key1^).Name) > UpStr(TDialEntry(Key2^).Name) THEN
      Compare := 1
    ELSE
      Compare := -1;
END;


{ TDialList }

CONSTRUCTOR TDialList.Init(VAR R: TRect; FName: ComStr;
                           ASCrollBar: PScrollBar);
VAR
  S: TBufStream;
  P: PDialCollection;
BEGIN
  TSortedListBox.Init(R, 1, AScrollBar);
  S.Init(FName, stOpen, BufferSize);
  IF S.Status = stInitError THEN
  BEGIN
    P := New(PDialCollection, Init(MaxDialEntry, 2));
    S.Init(FName, stCreate, BufferSize);
    S.Put(P);
    S.Done;
    NewList(P);
  END
  ELSE BEGIN
    P := PDialCollection(S.Get);
    IF P = NIL THEN
    BEGIN
      P := New(PDialCollection, Init(MaxDialEntry, 2));
      S.Init(FName, stCreate, BufferSize);
      S.Put(P);
    END;
    NewList(P);
    S.Done;
  END;
END;

DESTRUCTOR TDialList.Done;
BEGIN
  IF List <> NIL THEN Dispose(List, Done);
  TSortedListBox.Done;
END;

FUNCTION TDialList.GetText(Item, MaxLen: Integer): STRING;
CONST
  BaudRateStrings: ARRAY[0..4] OF STRING =
                    (' 300ú',
                     '1200ú',
                     '2400ú',
                     '4800ú',
                     '9600ú');
  DataBitsStrings: ARRAY[0..1] OF STRING =
                    ('7ú',
                     '8ú');
  ParityStrings: ARRAY[0..2] OF STRING =
                  ('Nú',
                   'Eú',
                   'Oú');
  StopBitsStrings: ARRAY[0..1] OF STRING =
                   ('1 ',
                    '2 ');
VAR
  Buf: PDialEntry;
  S: STRING;

PROCEDURE TrimToLength(VAR S: STRING; Len: Byte);
BEGIN
  IF Length(S) > Len THEN
    S[0] := Char(Len)
  ELSE
    WHILE Length(S) < Len DO S := S + ' ';
END;

BEGIN
  IF (List <> NIL) AND (List^.Count > 0) THEN
    Buf := PDialEntry(List^.At(Item))
  ELSE
  BEGIN
    GetText := '';
    Exit;
  END;
  S := Buf^.Name;
  TrimToLength(S, 36);
  S := S + Buf^.Number;
  TrimToLength(S, 48);
  S := S + BaudRateStrings[Buf^.LineSettings.Baud];
  S := S + DataBitsStrings[Buf^.LineSettings.DataBits];
  S := S + ParityStrings[Buf^.LineSettings.Parity];
  S := S + StopBitsStrings[Buf^.LineSettings.StopBits];
  GetText := S;
END;

FUNCTION TDialList.GetKey(VAR S: STRING): Pointer;
CONST
  Buf: TDialEntry = ();
BEGIN
  S := UpStr(S);
  Buf.Name := S;
  GetKey := @Buf;
END;

PROCEDURE TDialList.HandleEvent(VAR Event:  TEvent);
BEGIN
  IF (Event.What = evMouseDown) AND Event.Double THEN
  BEGIN
    Event.What := evCommand;
    Event.Command := cmDial;
    PutEvent(Event);
    ClearEvent(Event);
  END
  ELSE TSortedListBox.HandleEvent(Event);
END;


{ TDialDialog }

CONSTRUCTOR TDialDialog.Init(ADialFile: ComStr);
VAR
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(0, 0, cDListLen+3, 17);
  TDialog.Init(R, 'Dialing Directory');
  Options := Options OR ofCentered;
  DialFile := ADialFile;
  R.Assign(3, 2, 7, 3);
  Control := New(PStaticText, Init(R, 'Name') );
  Insert(Control);
  R.Assign(39, 2, 45, 3);
  Control := New(PStaticText, Init(R, 'Number') );
  Insert(Control);
  R.Assign(51, 2, 61, 3);
  Control := New(PStaticText, Init(R, 'Parameters') );
  Insert(Control);
  { Lister }
  R.Assign(cDListLen, 3, cDListLen+1, 13);
  Control := New(PScrollBar, Init(R));
  R.Assign(2, 3, cDListLen, 13);
  DialList := New(PDialList, Init(R, DialFile, PScrollBar(Control)) );
  Insert(DialList);
  Insert(Control);
  { Buttons }
  R.Assign(3, 14, 14, 16);
  Control := New(PButton, Init(R, '~D~ial', cmDial, bfDefault + bfLeftJust) );
  Insert(Control);
  R.Assign(16, 14, 26, 16);
  Control := New(PButton, Init(R, '~I~nsert', cmInsert, bfNormal + bfLeftJust) );
  Insert(Control);
  R.Assign(28, 14, 38, 16);
  Control := New(PButton, Init(R, 'De~l~ete', cmDelete, bfNormal + bfLeftJust) );
  Insert(Control);
  R.Assign(40, 14, 50, 16);
  Control := New(PButton, Init(R, '~E~dit', cmEdit, bfNormal + bfLeftJust) );
  Insert(Control);
  R.Assign(52, 14, 62, 16);
  Control := New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal + bfLeftJust) );
  Insert(Control);
  SelectNext(False);
END;

PROCEDURE TDialDialog.HandleEvent(VAR Event: TEvent);

FUNCTION InputDlg(VAR Item: TDialEntry; Title: STRING): Boolean;
VAR
  D: PDialog;
  Control: PView;
  Command: Word;
  R: TRect;
  Help:
    RECORD
      LineSettings: TLineSettings;
      Value: LongInt;
    END;
BEGIN
  InputDlg := False;
  R.Assign(0, 0, 46, 18);
  D := New(PDialog, Init(R, Title));
  D^.Options := D^.Options OR ofCentered;
  { Input Lines }
  R.Assign(12, 2, 14+cDNamLen, 3);
  Control := New(PNameInputLine, Init(R, cDNamLen,
                 PSortedCollection(DialList^.List)) );
  D^.Insert(Control);
  R.Assign(3, 2, 10, 3);
  Control := New(PLabel, Init(R, '~N~ame', Control));
  D^.Insert(Control);
  R.Assign(12, 3, 14+cDNumLen, 4);
  Control := New(PInputLine, Init(R, cDNumLen) );
  D^.Insert(Control);
  R.Assign(3, 3, 11, 4);
  Control := New(PLabel, Init(R, 'n~U~mber', Control));
  D^.Insert(Control);
  { Radio Buttons }
  R.Assign(4, 7, 15, 9);
  Control := New(PRadioButtons, Init(R,
    NewSItem('COM ~1~',
    NewSItem('COM ~2~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(3, 6, 13, 7);
  Control := New(PLabel, Init(R, '~P~ort', Control));
  D^.Insert(Control);
  R.Assign(4, 11, 15, 16);
  Control := New(PRadioButtons, Init(R,
    NewSItem('300',
    NewSItem('1200',
    NewSItem('2400',
    NewSItem('4800',
    NewSItem('9600',
    NIL)))))
  ));
  D^.Insert(Control);
  R.Assign(3, 10, 10, 11);
  Control := New(PLabel, Init(R, '~B~aud', Control));
  D^.Insert(Control);
  R.Assign(18, 7, 28, 9);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~7~',
    NewSItem('~8~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(17, 6, 28, 7);
  Control := New(PLabel, Init(R, '~D~ata Bits', Control));
  D^.Insert(Control);
  R.Assign(18, 11, 28, 14);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~N~one',
    NewSItem('~E~ven',
    NewSItem('~O~dd',
    NIL)))
  ));
  D^.Insert(Control);
  R.Assign(17, 10, 28, 11);
  Control := New(PLabel, Init(R, 'p~A~rity', Control));
  D^.Insert(Control);
  D^.Insert(Control);
  R.Assign(31, 6, 42, 7);
  Control := New(PLabel, Init(R, '~S~top Bits', Control));
  D^.Insert(Control);
  R.Assign(31, 7, 42, 9);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~1~',
    NewSItem('~2~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(31, 11, 42, 13);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~Y~es',
    NewSItem('~N~o',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(30, 10, 41, 11);
  Control := New(PLabel, Init(R, '~L~ocal echo', Control));
  D^.Insert(Control);
  R.Assign(19, 15, 29, 17);
  Control := New(PButton, Init(R, 'O~k~', cmOK, bfDefault));
  D^.Insert(Control);
  R.Assign(32, 15, 42, 17);
  Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
  D^.Insert(Control);
  D^.SelectNext(False);
  IF LowMemory THEN
  BEGIN
    Dispose(D, Done);
    MessageBox('Out of memory',NIL,mfError);
    DoneMemory;
  END ELSE
  BEGIN
    D^.SetData(Item.Name);
    Command := DeskTop^.ExecView(D);
    IF Command = cmOK THEN
    BEGIN
      D^.GetData(Item.Name);
      InputDlg := True;
    END;
    Dispose(D, Done);
  END;
END;

PROCEDURE Save;
VAR
  S: TBufStream;
BEGIN
  S.Init(DialFile, stCreate, BufferSize);
  S.Put(DialList^.List);
  S.Done;
END;

PROCEDURE InsertEntry;
VAR
  P: PDialEntry;
  PD: PCollection;
BEGIN
  IF DialList^.List^.Count < MaxDialEntry THEN
  BEGIN
    P := New(PDialEntry, Init);
    WITH P^.LineSettings DO BEGIN
      Port := 0;
      Baud := 2;
      DataBits := 1;
      Parity := 0;
      StopBits := 0;
    END;
    IF InputDlg(P^, 'New Entry') THEN
    BEGIN
      PD := DialList^.List;
      PD^.Insert(P);
      DialList^.List := NIL;
      DialList^.NewList(PD);
      Save;
      DialList^.FocusItem(DialList^.List^.IndexOf(P));
      DialList^.Drawview;
    END
    ELSE Dispose(P, Done);
    END
  ELSE MessageBox('Limit reached',NIL,mfError);
  DialList^.Select;
END;

PROCEDURE DeleteEntry;
VAR
  Pos: Integer;
  PD: PCollection;
  C: Word;
BEGIN
  IF DialList^.List^.Count > 0 THEN
  BEGIN
    C := MessageBox('Are you sure ?',NIL, mfInformation + mfYesNoCancel);
    IF C = cmYes THEN
    BEGIN
      Pos := DialList^.Focused;
      PD := DialList^.List;
      PD^.AtDelete(DialList^.Focused);
      DialList^.List := NIL;
      DialList^.NewList(PD);
      Save;
      IF Pos >= DialList^.List^.Count THEN
      BEGIN
        IF DialList^.TopItem > 0 THEN Dec(DialList^.TopItem);
        Dec(Pos);
      END;
      IF Pos >= 0 THEN DialList^.FocusItem(Pos);
      DialList^.Drawview;
    END
  END;
  DialList^.Select;
END;

PROCEDURE EditEntry;
VAR
  DialEntry: PDialEntry;
  Help: TDialEntry;
BEGIN
  Help.Init;
  IF DialList^.List^.Count = 0 THEN Exit;
  Help := TDialEntry(DialList^.List^.AT(DialList^.Focused)^);
  DialList^.List^.AtDelete(DialList^.Focused);
  IF InputDlg(Help, 'Edit') THEN BEGIN
    DialEntry := New(PDialEntry, Init);
    DialEntry^ := Help;
    DialList^.List^.Insert(DialEntry);
    DialList^.FocusItem(DialList^.List^.IndexOf(DialEntry));
    Save;
  END
  ELSE BEGIN
    DialEntry := New(PDialEntry, Init);
    DialEntry^ := Help;
    DialList^.List^.Insert(DialEntry);
  END;
  DialList^.Drawview;
  DialList^.Select;
END;

BEGIN
  TDialog.HandleEvent(Event);
  IF Event.What = evCommand THEN
    CASE Event.Command OF
      cmDial:
        IF DialList^.List^.Count > 0 THEN BEGIN
          EndModal(Event.Command);
          ClearEvent(Event);
          Message(Desktop, evBroadcast, cmDoDial, DialList^.List^.AT(DialList^.Focused));
        END;
      cmInsert:
        InsertEntry;
      cmEdit:
        EditEntry;
      cmDelete:
        DeleteEntry;
    END;
END;

PROCEDURE DialDialog(DialFileName: ComStr);
VAR
  P: PDialDialog;
BEGIN
  P := New(PdialDialog, Init(DialFileName));
  Desktop^.ExecView(p);
  Dispose(p, Done);
END;

BEGIN
  RegisterType(RDialCollection);
END.
