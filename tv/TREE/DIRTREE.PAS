{************************************************}
{                                                }
{   TDirTreeViewer stellt ein View-Objekt zur    }
{   Anzeige hierarischer Verzeichnisstrukturen   }
{   zur VerfÅgung.                               }
{                                                }
{************************************************}

{$B-,X+,V-}

UNIT DirTree;

INTERFACE

USES
  Dos, Objects, Crt, StdDlg, Dialogs, Views, Drivers,
  App, MsgBox, Tree, Tools;

VAR
  MyStream: TBufStream;

CONST
  TCDInfoFile = '\TCD.INI';
  {
   schreibe die Verzeichnis-Information in das Root-Verzeichnis
   des jeweiligen Laufwerks
  }
  cmDirTreeBase = 1010;
  cmFileFocused = cmDirTreeBase + 1;
  cmNewDrive = cmDirTreeBase + 2;
  cmRescan = cmDirTreeBase + 3;
  cmSelectFile = cmDirTreeBase + 4;
  cmChangeDir = cmDirTreeBase + 5;
  cmLookForFileListBox = cmDirTreeBase + 6;
  cmChDir = cmDirTreeBase + 7;
  cmShowCurDir = cmDirTreeBase + 8;

  cdChangeButton = $0001;
  cdSelectButton = $0002;

TYPE
  PDirItemCollection = ^TDirItemCollection;
  TDirItemCollection = OBJECT(TItemCollection)
    Drive: Char;
    PROCEDURE GetAllDirs(ReadThisDrive: STRING);
    FUNCTION GetRootString: PString; VIRTUAL;
    FUNCTION GetFocusString(Where: TPoint): PString; VIRTUAL;
    FUNCTION GetItemWidth: Integer; VIRTUAL;
    FUNCTION GetRootCursorPos: Integer; VIRTUAL;
    CONSTRUCTOR Load(VAR S: TStream);
    PROCEDURE Store(VAR S: TStream);
  END;

  PDirTreeViewer = ^TDirTreeViewer;
  TDirTreeViewer = OBJECT(TTreeViewer)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

  PPathViewer = ^TPathViewer;
  TPathViewer = OBJECT(TView)
    PathName: PathStr;
    FileName: STRING[12];
    CONSTRUCTOR Init(VAR Bounds: TRect);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION DataSize: Word; VIRTUAL;
    PROCEDURE GetData(VAR Rec); VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    FUNCTION GetPalette: PPalette; VIRTUAL;
  END;

  PCdDialog = ^TCdDialog;
  TCdDialog = OBJECT(TDialog)
    theOptions: Word;
    ThePathView: PPathViewer;
    CONSTRUCTOR Init(AOptions: Word);
    FUNCTION CurrentDrive: Char;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

  PDrivecol = ^TDriveCol;
  TDriveCol = OBJECT(TStringCollection)
    CONSTRUCTOR Init;
    FUNCTION DriveValid(C: Char): Boolean;
  END;

  PDriveListBox = ^TDriveListBox;
  TDriveListBox = OBJECT(TListBox)
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE SelectItem(Item: Integer); VIRTUAL;
  END;

  PFileListBox = ^TFileListBox;
  TFileListBox = OBJECT(TSortedListBox)
    AFileCol: PStringCollection;
    CONSTRUCTOR Init(VAR Bounds: TRect; AScrollBar: PScrollBar);
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE SelectItem(Item: Integer); VIRTUAL;
    FUNCTION GetKey(VAR S: STRING): Pointer; VIRTUAL;
    PROCEDURE ReadFiles(Dir: PathStr);
    PROCEDURE FocusItem(Item: Integer); VIRTUAL;
  END;

CONST
  RDirItemCollection: TStreamRec = (
     ObjType: MyStreamBase + 1;
     VmtLink: Ofs(TypeOf(TDirItemCollection)^);
     Load:    @TDirItemCollection.Load;
     Store:   @TDirItemCollection.Store
  );

IMPLEMENTATION

PROCEDURE RegisterTypes;
BEGIN
  RegisterType(RDirItemCollection);
END;


{ TFileListBox }

CONSTRUCTOR TFileListBox.Init(VAR Bounds: TRect; AScrollBar: PScrollBar);
BEGIN
  TSortedListBox.Init(Bounds, 1, AScrollBar);
  Options := Options AND NOT ofSelectable;
END;

DESTRUCTOR TFileListBox.Done;
BEGIN
  IF List <> NIL THEN Dispose(List, Done);
  TSortedListBox.Done;
END;

PROCEDURE TFileListBox.FocusItem(Item: Integer);
VAR
  S: STRING[12];
  P: Integer;
BEGIN
  TSortedListBox.FocusItem(Item);
  S := PString(List^.At(Item))^;
  P := Pos(' ', S);
  IF P <> 0 THEN BEGIN
    WHILE Pos(' ', S) <> 0 DO System.Delete(S, Pos(' ', S), 1);
    System.Insert('.', S , P);
  END;
  Message(TopView, evBroadcast, cmFileFocused, @S);
END;

PROCEDURE TFileListBox.HandleEvent(VAR Event: TEvent);
BEGIN
  TSortedListBox.HandleEvent(Event);
  IF Event.What = evBroadCast THEN
  CASE Event.Command OF
    cmSelectItem:
      BEGIN
        AFileCol := New(PStringCollection, Init(20,5));
        ReadFiles(PString(Event.InfoPtr)^);
        NewList(AFileCol);
        Options := Options OR ofSelectable;
        Select;
      END;
    cmNewDrive,
    cmEntryFocused:
      BEGIN
        NewList(NIL);
        Options := Options AND NOT ofSelectable;
      END;
    cmLookForFileListBox:
      IF (Options AND ofSelectable) <> 0 THEN
        ClearEvent(Event);
  END;
END;

PROCEDURE TFileListBox.ReadFiles(Dir: PathStr);
CONST
  FindAttr = ReadOnly + Archive;
VAR
  SR: SearchRec;
  S: STRING[12];
  CurPath: PathStr;

  FUNCTION FillWithBlanks(S: STRING): STRING;
  VAR
    P: Integer;
  BEGIN
    P := Pos('.', S);
    IF P <> 0 THEN BEGIN
      System.Delete(S, P, 1);
      WHILE Length(S) < 12 DO System.Insert(' ', S, P);
    END;
    FillWithBlanks := S;
  END;

BEGIN
  IF Dir[Length(Dir)] <> '\'THEN
    CurPath := Dir + '\*.*'
  ELSE CurPath := Dir + '*.*';
  FindFirst(CurPath, FindAttr, SR);
  WHILE DosError = 0 DO
  BEGIN
    IF SR.Attr AND Directory = 0 THEN BEGIN
      S := FillWithBlanks(SR.Name);
      AFileCol^.Insert(NewStr(S));
    END;
    FindNext(SR);
  END;
END;

FUNCTION TFileListBox.GetKey(VAR S: STRING): Pointer;
CONST
  Name: STRING[8] = '';
BEGIN
  IF Pos('.',S) <> 0 THEN Name := Copy(S, 1, Pos('.',S)-1)
  ELSE Name := S;
  Name := UpStr(Name);
  GetKey := @Name;
END;

PROCEDURE TFileListBox.SelectItem(Item: Integer);
BEGIN
  Message(Owner, evCommand, cmSelectFile, @Self);
END;


{ TDriveListBox }

DESTRUCTOR TDriveListBox.Done;
BEGIN
  IF List <> NIL THEN Dispose(List, Done);
  TListBox.Done;
END;

PROCEDURE TDriveListBox.SelectItem(Item: Integer);
VAR
  S: STRING[2];
BEGIN
  S := GetText(Item, 2);
  Message(Owner, evBroadCast, cmNewDrive, @S[1]);
END;


{ TPathViewer }

DESTRUCTOR TPathViewer.Done;
BEGIN
  TView.Done;
END;

CONSTRUCTOR TPathViewer.Init(VAR Bounds: TRect);
BEGIN
  TView.Init(Bounds);
  EventMask := EventMask OR evBroadCast;
END;

PROCEDURE TPathViewer.Draw;
VAR
  C: Byte;
  B: TDrawBuffer;
  P: PathStr;
BEGIN
  C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  IF PathName[Length(PathName)] = '\' THEN
    P := PathName + FileName
  ELSE IF Length(FileName) <> 0 THEN
         P := PathName + '\' + FileName
       ELSE P := PathName;
  MoveStr(B, P,C);
  WriteLine(0, 0, Size.X, 1, B);
END;

PROCEDURE TPathViewer.HandleEvent(VAR Event: TEvent);
BEGIN
  TView.HandleEvent(Event);
  IF Event.What = evBroadCast THEN BEGIN
    CASE Event.Command OF
      cmEntryFocused:
        BEGIN
          PathName := PathStr(Event.InfoPtr^);
          FileName := '';
        END;
      cmFileFocused:
        FileName := PString(Event.InfoPtr)^;
    END;
    DrawView;
  END;
END;

PROCEDURE TPathViewer.GetData(VAR Rec);
VAR
  P: STRING;
BEGIN
  IF PathName[Length(PathName)] = '\' THEN
    P := PathName + FileName
  ELSE IF Length(FileName) <> 0 THEN
         P := PathName + '\' + FileName
       ELSE P := PathName;
  Move(P, Rec, DataSize);
END;

FUNCTION TPathViewer.DataSize: Word;
BEGIN
  DataSize := SizeOf(PathName)+SizeOf(FileName);
END;

FUNCTION TPathViewer.GetPalette: PPalette;
CONST
  P: STRING[Length(CInputLine)] = CInputLine;
BEGIN
  GetPalette := @P;
END;


{ TDriveCol }

CONSTRUCTOR TDriveCol.Init;
VAR
  C: Char;
BEGIN
  TStringCollection.Init(4,1);
  FOR C := 'A' TO 'Z' DO
    IF C >= 'C' THEN
      IF DriveValid(C) THEN Insert(NewStr(C+':')) ELSE
    ELSE Insert(NewStr(C+':'));
END;

FUNCTION TDriveCol.DriveValid(C: Char): Boolean; ASSEMBLER;
ASM
  MOV	DL,C
  MOV	AH,36H
  SUB	DL,'A'-1
  INT	21H
  INC	AX
  JE	@@2
@@1:
  MOV	AL,1
@@2:
END;


{ TCdDialog }

CONSTRUCTOR TCdDialog.Init(aOptions: Word);
VAR
  ADirItemCol: PDirItemCollection;
  ADirTreeViewer: PDirTreeViewer;
  ADriveCol: PDriveCol;
  AFileListBox: PFileListBox;
  ADriveListBox: PDriveListBox;
  AFileScrollBar,
  ADriveScrollBar: PScrollBar;
  AButton : PButton;
  F: TPoint;
  Bounds, R, R2, R4: TRect;
  CurrentDriveIndex: Integer;
  ButtonOrigin: TPoint;
  ButtonLength: Integer;
  ButtonRect: TRect;

  FUNCTION HasDriveLetter(S: PString): Boolean; FAR;
  BEGIN
    HasDriveLetter := S^[1] = CurrentDrive;
  END;

  PROCEDURE FixButtonRect(VAR R: TRect; VAR P: TPoint; L: Integer);
  BEGIN
    R.A.X := P.X; R.A.Y := P.Y;
    R.B.X := P.X + L;
    R.B.Y := P.Y + 2;
    Inc(P.X, L+2);
  END;

BEGIN
  Bounds.Assign(1,1,72,21);
  TDialog.Init(Bounds, 'VISION''s CD');
  theOptions := aOptions;
  ButtonOrigin.X := Bounds.A.X+10;
  ButtonOrigin.Y := Bounds.B.Y-4;
  Bounds.Assign(Bounds.A.X+8,Bounds.A.Y+1,Bounds.A.X+51,Bounds.A.Y+12);
  R := Bounds;
  ButtonLength := 10;
  FixButtonRect(ButtonRect, ButtonOrigin, ButtonLength);
  AButton := New(PButton, Init(ButtonRect, '~R~escan', cmRescan, bfNormal));
  Insert(AButton);
  IF theOptions AND cdSelectButton <> 0 THEN BEGIN
    ButtonLength := 15;
    FixButtonRect(ButtonRect, ButtonOrigin, ButtonLength);
    AButton := New(PButton, Init(ButtonRect, '~S~elect File', cmSelectFile, bfNormal));
    Insert(AButton);
  END;
  IF theOptions AND cdChangeButton <> 0 THEN BEGIN
    ButtonLength := 14;
    FixButtonRect(ButtonRect, ButtonOrigin, ButtonLength);
    AButton := New(PButton, Init(ButtonRect, 'Change ~D~ir', cmChangeDir, bfDefault));
    Insert(AButton);
  END;
  ButtonLength := 10;
  FixButtonRect(ButtonRect, ButtonOrigin, ButtonLength);
  AButton := New(PButton, Init(ButtonRect, 'Cancel', cmCancel, bfNormal));
  Insert(AButton);
  MyStream.Init(TCDInfoFile, stOpenRead, 512);
  IF MyStream.Status <> 0 THEN BEGIN
    ADirItemCol := New(PDirItemCollection, Init(20,5));
    ADirItemCol^.GetAllDirs(CurrentDrive+':\');
    ADirItemCol^.Drive := CurrentDrive;
    ADirItemCol^.ComputeLinkSym;
    MyStream.Init(TCDInfoFile, stCreate, 512);
    MyStream.Put(ADirItemCol);
  END
  ELSE ADirItemCol := PDirItemCollection(MyStream.Get);
  MyStream.Done;
  ADirTreeViewer := New(PDirTreeViewer, Init(Bounds,ADirItemCol));
  Insert(ADirTreeViewer);
  Bounds.Assign(Bounds.A.X-6, Bounds.B.Y+1, Bounds.B.X+16, Bounds.B.Y+2);
  ThePathView := New(PPathViewer, Init(Bounds));
  Insert(ThePathView);
  F.X := ADirTreeViewer^.FocusedLevel;
  F.Y := ADirTreeViewer^.FocusedLine;
  ADirTreeViewer^.FocusItem(F);
  R.Assign(R.B.X+1, R.A.Y, R.B.X+15, R.B.Y);
  R2.Assign(R.B.X, R.A.Y, R.B.X+1, R.B.Y);
  AFileScrollBar := New(PScrollBar, Init(R2));
  R2.Assign(Bounds.A.X+4,R.A.Y,Bounds.A.X+5,R.B.Y);
  ADriveScrollBar := New(PScrollBar, Init(R2));
  R2.Assign(R2.A.X-4,R2.A.Y,R2.A.X,R2.B.Y);
  ADriveListBox := New(PDriveListBox, Init(R2,1,ADriveScrollBar));
  ADriveCol := New(PDriveCol, Init);
  CurrentDriveIndex :=
      ADriveCol^.IndexOf(ADriveCol^.FirstThat(@HasDriveLetter));
  ADriveListBox^.NewList(ADriveCol);
  ADriveListBox^.FocusItem(CurrentDriveIndex);
  AFileListBox := New(PFileListBox, Init(R,AFileScrollBar));
  Insert(AFileListBox);
  Insert(AFileScrollBar);
  Insert(ADriveListBox);
  Insert(ADriveScrollBar);
  ADirTreeViewer^.Select;
  Message(ADirTreeViewer, evBroadcast, cmShowCurDir, NIL);
  Inc(Origin.X, 3);
END; { of TCdDialog.Init }

FUNCTION TCdDialog.CurrentDrive: Char; ASSEMBLER;
ASM
  MOV	AH,19H
  Int	21H
  ADD	AL,'A'
END;

PROCEDURE TCdDialog.HandleEvent(VAR Event: TEvent);
TYPE
  PWord = ^Word;
VAR
  E: TEvent;
  P: Pointer;
BEGIN
  IF Event.What = evCommand THEN
  CASE Event.Command OF
    cmRescan:
      BEGIN
        Message(@Self, evBroadCast, cmRescan, NIL);
        ClearEvent(Event);
      END;
    cmChangeDir:
      BEGIN
        {$I-}
        ChDir(ThePathView^.PathName);
        {$I+}
        IF IOResult <> 0 THEN
          MessageBox('Directory does not exist', NIL, mfOKButton+mfError)
        ELSE
          EndModal(cmChDir);
      END;
    cmSelectFile:
      BEGIN
        P := Message(TopView, evBroadCast,
              cmLookForFileListBox, NIL);
        P := Ptr(DSeg, PWord(P)^);
        IF (P = TypeOf(TFileListBox)) AND
           (theOptions AND cdSelectButton <> 0) THEN BEGIN
          E.What := evCommand;
          E.Command := cmOK;
          PutEvent(E);
        END;
     END;
  END;
  TDialog.HandleEvent(Event);
END;


{ TDirTreeViewer }

PROCEDURE TDirTreeViewer.HandleEvent(VAR Event: TEvent);
VAR
    F: TPoint;
    ADrive: Char;
    S, D: PathStr;
    W: TPoint;
    L: Integer;
    Found: Boolean;
BEGIN
  TTreeViewer.HandleEvent(Event);
  CASE Event.What OF
    evBroadCast:
    IF (Event.Command = cmNewDrive) OR
       (Event.Command = cmRescan) OR
       (Event.Command = cmShowCurDir) THEN BEGIN
      IF (Char(Event.InfoPtr^) <> PDirItemCollection(ItemCol)^.Drive) OR
         (Event.Command = cmRescan) OR
         (Event.Command = cmShowCurDir) THEN BEGIN
        IF Event.Command <> cmShowCurDir THEN BEGIN
          IF Event.Command <> cmRescan THEN
            ADrive := Char(Event.InfoPtr^)
          ELSE ADrive := PDirItemCollection(ItemCol)^.Drive;
          Dispose(ItemCol, Done);
          MyStream.Init(ADrive+':'+
                        TCDInfoFile, stOpenRead, 512);
          IF (MyStream.Status <> 0) OR (Event.Command = cmRescan) THEN BEGIN
            MyStream.Done;
            ItemCol := New(PDirItemCollection, Init(20,5));
            PDirItemCollection(ItemCol)^.GetAllDirs(
              ADrive+':\');
            ItemCol^.ComputeLinkSym;
            MyStream.Init(ADrive+':'+
                          TCDInfoFile, stCreate, 512);
            PDirItemCollection(ItemCol)^.Drive := ADrive;
            IF MyStream.Status = 0 THEN MyStream.Put(ItemCol);
          END
          ELSE ItemCol := PDirItemCollection(MyStream.Get);
          MyStream.Done;
        END;

        L := 0;
        Found := False;
        GetDir(Ord(PDirItemCollection(ItemCol)^.Drive)-Ord('A')+1, D);
        WHILE NOT Found AND (L <= ItemCol^.Count-1) DO BEGIN
          W.X := PEntry(ItemCol^.At(L))^.Level;
          W.Y := PEntry(ItemCol^.At(L))^.Line;
          S := PDirItemCollection(ItemCol)^.GetFocusString(W)^;
          Found := D = S;
          Inc(L);
        END;
        IF Found THEN
          F := W
        ELSE BEGIN
          F.X := 0;
          F.Y := 1;
        END;

        FocusedLevel := F.X;
        FocusedLine := F.Y;
        StartLevel := 0;
        StartLine := 1;

        RedrawView := NoRedraw;
        FocusItem(F);
        AdjustView;
        DrawView;
      END;
    END;
  END; { of CASE }
END;


{ TDirItemCollection }

CONSTRUCTOR TDirItemCollection.Load(VAR S: TStream);
BEGIN
  TItemCollection.Load(S);
  S.Read(Drive, SizeOf(Char));
END;

PROCEDURE TDirItemCollection.Store(VAR S: TStream);
BEGIN
  TItemCollection.Store(S);
  S.Write(Drive, SizeOf(Char));
END;

FUNCTION TDirItemCollection.GetRootString: PString;
CONST
  R:STRING[4]=' \  ';
BEGIN
  GetRootString := @R;
END;

FUNCTION TDirItemCollection.GetFocusString(Where: TPoint): PString;
CONST
  F: PathStr = '';
VAR
  P: PEntry;
  L: Integer;
  S: PathStr;
  CurLevel: Byte;

  FUNCTION IsItThisItem(P: PEntry): Boolean; FAR;
  BEGIN
    IsItThisItem := (P^.Level = Where.X) AND (P^.Line = Where.Y);
  END;

BEGIN
  IF (Where.X = 0) AND (Where.Y = 1) THEN BEGIN
    F := Drive+':'+'\';
    GetFocusString := @F;
    Exit;
  END;
  P := PEntry(FirstThat(@IsItThisItem));
  S := P^.Name^;
  CurLevel := P^.Level;
  FOR L := IndexOf(P) DOWNTO 1 DO BEGIN
    P := At(L-1);
    IF P^.Level < CurLevel THEN BEGIN
      CurLevel := P^.Level;
      S := P^.Name^+'\'+S;
    END;
  END;
  F := Drive+':'+'\'+S;
  GetFocusString := @F;
END;

FUNCTION TDirItemCollection.GetRootCursorPos: Integer;
BEGIN
  GetRootCursorPos := 1;
END;

FUNCTION TDirItemCollection.GetItemWidth: Integer;
BEGIN
  GetItemWidth := 12;
END;

PROCEDURE TDirItemCollection.GetAllDirs(ReadThisDrive: STRING);

  PROCEDURE ReadSubDir(Path: PathStr; Level: Byte);
  VAR
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
    Search: SearchRec;
  BEGIN
    Inc(Level);
    FSplit(Path, Dir, Name, Ext);
    FindFirst(Dir+'*.*', AnyFile, Search);
    WHILE DosError = 0 DO BEGIN
      IF (Search.Attr = Directory) AND (Search.Name <> '.')
                                   AND (Search.Name <> '..') THEN BEGIN
        InsertWithLine(PEntry(New(PEntry,
                 Init(NewStr(Search.Name),Level))));
        ReadSubDir(Dir+Search.Name+'\'+Name+Ext, Level);
      END;
      FindNext(Search);
    END;
  END;

BEGIN
  ReadSubDir(ReadThisDrive, 0);
END;

BEGIN
  RegisterTypes;
  {
   registriere alle Objekt-Typen, die abgespeichert werden mÅssen
  }
END. { of DirTree }