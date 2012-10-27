{************************************************}
{                                                }
{   TTreeViewer stellt ein generisches View-     }
{   Objekt zur Anzeige hierarischer Strukturen   }
{   zur VerfÅgung.                               }
{                                                }
{************************************************}

{$X+}

UNIT Tree;

INTERFACE

USES
  Dos, Objects, Views, Drivers, Tools;

TYPE
  PItemCollection = ^TItemCollection;
  TItemCollection = OBJECT(TCollection)
    PROCEDURE InsertWithLine(Item: Pointer);
    PROCEDURE ComputeLinkSym;
    FUNCTION IndexOf(Item: Pointer): Integer; VIRTUAL;
    FUNCTION LevelFoundBelow(WhichLevel: Byte; Index: Integer): Boolean;
    FUNCTION ValidRange(I: Integer): Boolean;
    FUNCTION BuildLine(Line: Integer): STRING;
    FUNCTION GetRootString: PString; VIRTUAL;
    FUNCTION GetFocusString(Where: TPoint): PString; VIRTUAL;
    FUNCTION GetItemWidth: Integer; VIRTUAL;
    FUNCTION GetRootCursorPos: Integer; VIRTUAL;
  END;

  PEntry = ^TEntry;
  TEntry = OBJECT(TObject)
    Name:  PString;
    Level: Byte;
    Line: Integer;
    LinkSymbol: Char;
    CONSTRUCTOR Init(N: PString; L: Byte);
    DESTRUCTOR Done; VIRTUAL;
    CONSTRUCTOR Load(VAR S: TStream);
    PROCEDURE Store(VAR S: TStream);
  END;

  PTreeViewer = ^TTreeViewer;
  TTreeViewer = OBJECT(TView)
    FocusedLevel: Byte;
    FocusedLine: Integer;
    StartLevel: Byte;
    StartLine: Integer;
    ItemCol: PItemCollection;
    IncSearchString: STRING[20];
    RedrawView: (IncView, DecView, NoRedraw);
    CONSTRUCTOR Init(VAR Bounds: TRect; AnItemCol: PItemCollection);
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE WhereIsMouseFocus(Mouse: TPoint; VAR FocusLocation: TPoint);
    FUNCTION GetPalette: PPalette; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE FocusItem(FocusLocation: TPoint);
    PROCEDURE SetState(AState: Word; Enable: Boolean); VIRTUAL;
    FUNCTION GetFocused: PEntry;
    FUNCTION XStartPos(Level: Byte): Integer;
    PROCEDURE IncSearch(C: Char);
    PROCEDURE AdjustView;
    PROCEDURE IncStartLevel;
    PROCEDURE DecStartLevel;
    PROCEDURE IncStartLine;
    PROCEDURE DecStartLine;
  END;

CONST
  kbSpace = $3920;
  MyStreamBase = 200;
  cmMyBase = 1000;
  cmEntryFocused = cmMyBase + 1;
  cmSelectItem   = cmMyBase + 2;
  cmEntryFocusedCount = cmMyBase + 3;

  REntry: TStreamRec = (
    ObjType: MyStreamBase + 0;
    VmtLink: Ofs(TypeOf(TEntry)^);
    Load:    @TEntry.Load;
    Store:   @TEntry.Store
   );

IMPLEMENTATION


{ TTreeViewer }

DESTRUCTOR TTreeViewer.Done;
BEGIN
  IF ItemCol <> NIL THEN Dispose(ItemCol, Done);
  TView.Done;
END;

CONSTRUCTOR TTreeViewer.Init(VAR Bounds: TRect;
                             AnItemCol: PItemCollection);
BEGIN
  TView.Init(Bounds);
  Options := Options OR (ofSelectable + ofFirstClick);
  EventMask := EventMask OR evBroadCast;
  FocusedLevel := 0;
  FocusedLine := 1;
  StartLevel := 0;
  StartLine := 1;
  RedrawView := NoRedraw;
  ItemCol := AnItemCol;
  ShowCursor;
END;

PROCEDURE TTreeViewer.SetState(AState: Word; Enable: Boolean);
BEGIN
  TView.SetState(AState, Enable);
  IF AState AND (sfSelected + sfActive) <> 0 THEN
    DrawView;
END;

FUNCTION TTreeViewer.GetFocused: PEntry;

  FUNCTION IsThisDir(P: PEntry): Boolean; FAR;
  BEGIN
    IsThisDir := (P^.Line = FocusedLine) AND (P^.Level = FocusedLevel);
  END;

BEGIN
  GetFocused := ItemCol^.FirstThat(@IsThisDir);
END;

PROCEDURE TTreeViewer.Draw;
VAR
  RootWidth,
  Width: Integer;
  B: TDrawBuffer;
  NormalColor,
  FocusedColor,
  SelectedColor: Byte;
  I, StartCol,
  FocusLen,
  FocusStart: Integer;
  S,P: STRING;

  FUNCTION StripLines(S: STRING): STRING;
  VAR L: Integer;
  BEGIN
    FOR L := 1 TO Length(S) DO IF S[L] = 'ƒ' THEN S[L] := ' ';
    StripLines := S;
  END;

BEGIN
  RootWidth := Length(ItemCol^.GetRootString^)+1;
  Width := ItemCol^.GetItemWidth;
  NormalColor := GetColor(1);
  FocusedColor := GetColor(3);
  SelectedColor := GetColor(4);
  StartCol := 1;
  IF StartLevel = 1 THEN StartCol := RootWidth
  ELSE IF StartLevel > 0 THEN StartCol := (StartLevel-1)*Width+RootWidth;
  FOR I := 0 TO Size.Y-1 DO
  BEGIN
    MoveChar(B, ' ', NormalColor, Size.X);
    IF ItemCol^.BuildLine(StartLine+I) <> 'ERROR' THEN BEGIN
      P := ItemCol^.BuildLine(StartLine+I);
      S := Copy(P, StartCol, Size.X);
      MoveStr(B, S, NormalColor);
      IF StartLine+I = FocusedLine THEN BEGIN
        IF FocusedLevel = 0 THEN BEGIN
          FocusLen := RootWidth;
          FocusStart := 1;
        END ELSE BEGIN
          FocusLen := Width+1;
          FocusStart := (FocusedLevel-1)*(Width+2)+RootWidth+2;
        END;
        Dec(FocusStart, StartCol-1);
        IF FocusStart <= 0 THEN
	  RedrawView := DecView
        ELSE IF FocusStart+FocusLen-1 > Size.X THEN
          RedrawView := IncView
        ELSE RedrawView := NoRedraw;
        WHILE FocusStart <= 0 DO BEGIN
          Inc(FocusStart);
          Dec(FocusLen);
        END;
        IF FocusedLevel <> 0 THEN
          SetCursor(FocusStart+Length(IncSearchString),I)
        ELSE SetCursor(ItemCol^.GetRootCursorPos,I);
        IF State AND (sfSelected + sfActive) = (sfSelected + sfActive)
        THEN MoveStr(B[FocusStart-1], StripLines(Copy(S,FocusStart,FocusLen)),FocusedColor)
        ELSE BEGIN
          IF FocusedLevel > 0 THEN BEGIN
            Inc(FocusStart);
            FocusLen := Length(GetFocused^.Name^);
          END
          ELSE FocusLen := Length(ItemCol^.GetRootString^);
          MoveStr(B[FocusStart-1], Copy(S,FocusStart,FocusLen),SelectedColor)
        END;
      END;
    END;
    WriteLine(0, I, Size.X, 1, B);
  END;
END;

FUNCTION TTreeViewer.XStartPos(Level: Byte): Integer;
BEGIN
  XStartPos := Ord(Level<>0)*(Level-1)*(ItemCol^.GetItemWidth+2)+
               (Length(ItemCol^.GetRootString^)+2)*Ord(Level>0);
END;

PROCEDURE TTreeViewer.WhereIsMouseFocus(Mouse: TPoint; VAR FocusLocation: TPoint);
VAR
  RealX,
  SearchLevel: Byte;
  ThisLine,
  CurIndex: Integer;
  P: PEntry;
  Found: Boolean;

  FUNCTION FocusLen(Level: Byte): Byte;
  BEGIN
    IF Level = 0 THEN
      FocusLen := Length(ItemCol^.GetRootString^)+1
    ELSE FocusLen := ItemCol^.GetItemWidth+1;
  END;

  FUNCTION HasThisLine(P: PEntry): Boolean; FAR;
  BEGIN
    HasThisLine := P^.Line = ThisLine;
  END;

BEGIN
  IF ItemCol^.Count = 0 THEN BEGIN
    FocusLocation.Y := 0;
    Exit;
  END;
  IF Mouse.Y+StartLine > PEntry(ItemCol^.At(ItemCol^.Count-1))^.Line THEN BEGIN
    FocusLocation.Y := 0; { keine gÅltige Position }
    Exit;
  END ELSE FocusLocation.Y := Mouse.Y+StartLine;
  RealX := XStartPos(StartLevel)-2*Ord(StartLevel<>0)+Mouse.X;
  SearchLevel := 0;
  WHILE RealX >= XStartPos(SearchLevel) DO Inc(SearchLevel);
  Dec(SearchLevel);
  IF RealX > XStartPos(SearchLevel)+FocusLen(SearchLevel)-1 THEN BEGIN
    FocusLocation.Y := 0; { wir befinden uns irgendwo dazwischen }
    Exit;
  END;
  IF (SearchLevel = 0) AND (FocusLocation.Y = 1) THEN BEGIN
    FocusLocation.X := 0;
    Exit;
  END;
  ThisLine := FocusLocation.Y;
  P := ItemCol^.FirstThat(@HasThisLine);
  CurIndex := ItemCol^.IndexOf(P);
  Found := False;
  WHILE (CurIndex <= ItemCol^.Count-1) AND
        (PEntry(ItemCol^.At(CurIndex))^.Line = ThisLine) AND
        NOT Found DO BEGIN
    Found := SearchLevel = PEntry(ItemCol^.At(CurIndex))^.Level;
    Inc(CurIndex);
  END;
  IF Found THEN BEGIN
    Dec(CurIndex);
    FocusLocation.X := PEntry(ItemCol^.At(CurIndex))^.Level;
  END ELSE FocusLocation.Y := 0;
END;

PROCEDURE TTreeViewer.FocusItem(FocusLocation: TPoint);
VAR
  Index: Integer;
BEGIN
  FocusedLevel := FocusLocation.X;
  FocusedLine := FocusLocation.Y;
  Message(Owner, evBroadCast, cmEntryFocused,
          ItemCol^.GetFocusString(FocusLocation));
  Index := ItemCol^.IndexOf(GetFocused);
  Message(Owner, evBroadCast, cmEntryFocusedCount, @Index);
  DrawView;
END;

PROCEDURE TTreeViewer.AdjustView;
BEGIN
  IF FocusedLine >= StartLine + Size.Y THEN BEGIN
    WHILE FocusedLine >= StartLine + Size.Y DO IncStartLine;
    IncStartLine;
    IncStartLine;
  END;
  WHILE XStartPos(FocusedLevel) > XStartPos(StartLevel) + Size.X -
        ItemCol^.GetItemWidth DO
    IncStartLevel;
  IF FocusedLine < StartLine THEN BEGIN
    WHILE FocusedLine < StartLine DO DecStartLine;
    DecStartLine;
    DecStartLine;
  END;
  IF XStartPos(FocusedLevel) < XStartPos(StartLevel) THEN
    DecStartLevel;
END;

PROCEDURE TTreeViewer.HandleEvent(VAR Event: TEvent);
CONST
  MouseAutosToSkip = 1;
VAR
  Mouse, FocusLocation: TPoint;
  OldFocus: TPoint;
  Count: Integer;
  F: TPoint;
  CurIndex, CI: Integer;

BEGIN
  TView.HandleEvent(Event);
  CASE Event.What OF
    evMouseDown:
    BEGIN
      IncSearchString := '';
      OldFocus.Y := FocusedLine;
      OldFocus.X := FocusedLevel;
      MakeLocal(Event.Where, Mouse);
      WhereIsMouseFocus(Mouse,FocusLocation);
      Count := 0;
      REPEAT
        IF ((FocusLocation.X <> OldFocus.X) OR (FocusLocation.Y <> OldFocus.Y))
           AND (FocusLocation.Y <> 0) THEN BEGIN
          FocusItem(FocusLocation);
          WHILE RedrawView <> NoRedraw DO
          CASE RedrawView OF
            IncView: BEGIN
	               IncStartLevel;
                       FocusItem(FocusLocation);
                     END;
            DecView: BEGIN
	               DecStartLevel;
                       FocusItem(FocusLocation);
                     END;
          END;
          OldFocus := FocusLocation;
        END ELSE IF FocusLocation.Y = 0 THEN FocusItem(OldFocus);
        MakeLocal(Event.Where, Mouse);
        IF MouseInView(Event.Where) THEN BEGIN
          WhereIsMouseFocus(Mouse,FocusLocation);
          IF FocusLocation.Y = 0 THEN FocusLocation := OldFocus;
        END ELSE BEGIN
	  IF Event.What = evMouseAuto THEN Inc(Count);
	  IF Count = MouseAutosToSkip THEN
	  BEGIN
	    Count := 0;
            IF Mouse.Y < 0 THEN DecStartLine
	    ELSE IF Mouse.Y >= Size.Y THEN IncStartLine;
            IF Mouse.X < 0 THEN DecStartLevel
            ELSE IF Mouse.X >= Size.X THEN IncStartLevel;
	  END;
          DrawView;
        END;
      UNTIL NOT MouseEvent(Event, evMouseMove + evMouseAuto);
      FocusItem(Focuslocation);
      ClearEvent(Event);
      IF Event.Double THEN BEGIN
        Message(Owner, evBroadCast, cmSelectItem,
                ItemCol^.GetFocusString(FocusLocation));
      END;
    END;
    evKeyDown:
      BEGIN
        IF Event.CharCode = #0 THEN IncSearchString := '';
        {
         lîsche den Suchstring, sobald eine Nicht-ASCII-Zeichen-Taste
         gedrÅckt wurde, um beim Suchen neu aufzusetzen
        }
        CurIndex := ItemCol^.IndexOf(GetFocused);
        FocusLocation.X := FocusedLevel;
        FocusLocation.Y := FocusedLine;
        CASE Event.KeyCode OF
          kbHome:
            BEGIN
              F.X := 0;
              F.Y := 1;
              FocusItem(F);
            END;
          kbDown:
            BEGIN
              CI := CurIndex + 1;
              WHILE (CI <= ItemCol^.Count-1) AND (CI > 0) AND
                    (PEntry(ItemCol^.At(CI))^.Level <>
                    PEntry(ItemCol^.At(CurIndex))^.Level)
                 DO Inc(CI);
              IF (CI <= ItemCol^.Count-1) AND (CI > 0) AND
                 (PEntry(ItemCol^.At(CI))^.Level =
                 PEntry(ItemCol^.At(CurIndex))^.Level)
                THEN BEGIN
                  F.X := PEntry(ItemCol^.At(CI))^.Level;
                  F.Y := PEntry(ItemCol^.At(CI))^.Line;
                  FocusItem(F);
                END
                ELSE IF CurIndex < ItemCol^.Count-1 THEN BEGIN
                  F.X := PEntry(ItemCol^.At(CurIndex+1))^.Level;
                  F.Y := PEntry(ItemCol^.At(CurIndex+1))^.Line;
                  FocusItem(F);
                END;
              ClearEvent(Event);
            END;
          kbUp:
            BEGIN
              CI := CurIndex - 1;
              WHILE (CI >= 0) AND
                    (PEntry(ItemCol^.At(CI))^.Level <>
                    PEntry(ItemCol^.At(CurIndex))^.Level)
                 DO Dec(CI);
              IF (CI >=0) AND
                 (PEntry(ItemCol^.At(CI))^.Level =
                 PEntry(ItemCol^.At(CurIndex))^.Level)
                THEN BEGIN
                  F.X := PEntry(ItemCol^.At(CI))^.Level;
                  F.Y := PEntry(ItemCol^.At(CI))^.Line;
                  FocusItem(F);
                END
                ELSE IF CurIndex >= 1 THEN BEGIN
                  F.X := PEntry(ItemCol^.At(CurIndex-1))^.Level;
                  F.Y := PEntry(ItemCol^.At(CurIndex-1))^.Line;
                  FocusItem(F);
                END ELSE BEGIN
                  F.X := 0;
                  F.Y := 1;
                  FocusItem(F);
                END;
              ClearEvent(Event);
            END;
          kbRight:
            BEGIN
              IF CurIndex < ItemCol^.Count-1 THEN BEGIN
                  F.X := PEntry(ItemCol^.At(CurIndex+1))^.Level;
                  F.Y := PEntry(ItemCol^.At(CurIndex+1))^.Line;
                  FocusItem(F);
               END;
              ClearEvent(Event);
            END;
          kbLeft:
            BEGIN
              IF (CurIndex >= 0) AND
                 (PEntry(ItemCol^.At(CurIndex))^.Level > 1) THEN BEGIN
                 CI := CurIndex - 1;
                 WHILE (CI >= 0) AND
                    (PEntry(ItemCol^.At(CI))^.Level <> FocusedLevel-1)
                     DO Dec(CI);
                 IF CI >= 0 THEN BEGIN
                   F.X := PEntry(ItemCol^.At(CI))^.Level;
                   F.Y := PEntry(ItemCol^.At(CI))^.Line;
                 END;
                 FocusItem(F);
               END
               ELSE BEGIN
                 F.X := 0;
                 F.Y := 1;
                 FocusItem(F);
               END;
              ClearEvent(Event);
            END;
          kbSpace:
            BEGIN
              Message(Owner, evBroadCast, cmSelectItem,
                      ItemCol^.GetFocusString(FocusLocation));
              ClearEvent(Event);
            END;
         ELSE
           CASE UpCase(Event.CharCode) OF
             'A'..'Z',
             '0'..'9',
             'é','ô','ö',
             '·','.',
             #8:
               BEGIN
                 IncSearch(Event.CharCode);
                 ClearEvent(Event);
               END;
           END;
      END; { of CASE }
      AdjustView;
      DrawView;
    END;
  END; { of CASE }
END;

PROCEDURE TTreeViewer.IncStartLevel;
VAR
  MaxLevel: Byte;

  PROCEDURE GetMaxLevel(P: PEntry); FAR;
  BEGIN
    IF P^.Level > MaxLevel THEN MaxLevel := P^.Level;
  END;

BEGIN
  MaxLevel := 0;
  IF ItemCol^.Count <> 0 THEN
    ItemCol^.ForEach(@GetMaxLevel)
  ELSE MaxLevel := 0;
  IF StartLevel < MaxLevel THEN Inc(StartLevel);
END;

PROCEDURE TTreeViewer.DecStartLevel;
BEGIN
  IF StartLevel > 0 THEN Dec(StartLevel);
END;

PROCEDURE TTreeViewer.IncStartLine;
VAR
  MaxLine: Byte;

  PROCEDURE GetMaxLine(P: PEntry); FAR;
  BEGIN
    IF P^.Line > MaxLine THEN MaxLine := P^.Line;
  END;

BEGIN
  MaxLine := 1;
  IF ItemCol^.Count <> 0 THEN
    ItemCol^.ForEach(@GetMaxLine)
  ELSE MaxLine := 1;
  IF StartLine < MaxLine THEN Inc(StartLine);
END;

PROCEDURE TTreeViewer.DecStartLine;
BEGIN
  IF StartLine > 1 THEN Dec(StartLine);
END;

FUNCTION TTreeViewer.GetPalette: PPalette;
CONST
  P: STRING[Length(CListViewer)] = CListViewer;
BEGIN
  GetPalette := @P;
END;

PROCEDURE TTreeViewer.IncSearch(C: Char);
VAR
  P: PEntry;
  F: TPoint;

  FUNCTION Matches(P: PEntry): Boolean; FAR;
  VAR
    S: STRING[12];
  BEGIN
    S := Copy(P^.Name^, 1, Length(IncSearchString));
    Matches := (S = IncSearchString)
  END;

BEGIN
  CASE UpCase(C) OF
    'A'..'Z',
    '0'..'9',
    'é','ô','ö',
    '·','.', #8:
      BEGIN
        IF (C = #8) AND (Length(IncSearchString) > 0)
           THEN Dec(Byte(IncSearchString[0]))
           ELSE IncSearchString := IncSearchString + C;
        IncSearchString := UpStr(IncSearchString);
        P := ItemCol^.FirstThat(@Matches);
        IF (P <> NIL) AND (IncSearchString <> '') THEN BEGIN
          F.X := P^.Level;
          F.Y := P^.Line;
          FocusItem(F);
        END
        ELSE BEGIN
          IF Length(IncSearchString) > 0 THEN
             Dec(Byte(IncSearchString[0]));
          IF Length(IncSearchString) = 0 THEN BEGIN
            F.X := 0;
            F.Y := 1;
            FocusItem(F);
          END;
        END;
      END;
  END;
END;


{ TEntry }

DESTRUCTOR TEntry.Done;
BEGIN
  DisposeStr(Name);
  TObject.Done;
END;

CONSTRUCTOR TEntry.Init(N: PString; L: Byte);
BEGIN
  Name := N;
  Level := L;
END;

CONSTRUCTOR TEntry.Load(VAR S: TStream);
BEGIN
  Name := S.ReadStr;
  S.Read(Level, 4);
END;

PROCEDURE TEntry.Store(VAR S: TStream);
BEGIN
  S.WriteStr(Name);
  S.Write(Level, 4);
END;


{ TItemCollection }

FUNCTION TItemCollection.GetRootCursorPos: Integer;
BEGIN
  GetRootCursorPos := 0;
END;

FUNCTION TItemCollection.GetRootString: PString;
BEGIN
  Abstract;
END;

FUNCTION TItemCollection.GetFocusString(Where: TPoint): PString;
BEGIN
  Abstract;
END;

FUNCTION TItemCollection.GetItemWidth: Integer;
BEGIN
  Abstract;
END;

FUNCTION TItemCollection.BuildLine(Line: Integer): STRING;
VAR
  S: STRING;
  L, FillPos: Integer;
  LowestLevelInLine,
  IndexOfLowestLevel: Byte;

  PROCEDURE InsertBars;
  VAR L: Integer;
  BEGIN
    IF LowestLevelInLine = 1 THEN Exit;
    FOR L := LowestLevelInLine-1 DOWNTO 1 DO
      IF LevelFoundBelow(L, IndexOfLowestLevel) THEN
       S[(L-1)*(GetItemWidth+2)+(Length(GetRootString^)+2)] := '≥';
  END;

  PROCEDURE FillInString(Source: STRING; VAR S: STRING; Index: Integer);
  ASSEMBLER;
  ASM
    PUSH	DS
    LES	DI,S
    LDS	SI,Source
    MOV	CX,[SI]
    XOR	CH,CH
    Inc	SI
    MOV	AX,Index
    ADD	DI,AX
    CLD
    REP	MOVSB
    POP	DS
  END;

  PROCEDURE CutLinesOff(VAR S: STRING; FromIndex: Integer; Max: Integer);
  ASSEMBLER;
  ASM
    LES         DI,S
    MOV         AX,FromIndex
    ADD         DI,AX
    MOV         AL,'ƒ'
    CLD
    MOV         CX,Max
    Inc         CX
    MOV         SI,DI
    REPNE	SCASB
    MOV         AL,' '
    CMP         AL,ES:[DI-01]
    JE          @@1
    MOV         AX,DI
    SUB         AX,SI
    MOV         BX,MAX
    Inc         BX
    SUB         BX,AX
    XCHG	CX,BX
    MOV         AL,' '
    Dec         DI
    REP         STOSB
@@1:
  END;

  FUNCTION PadWithLines(S: STRING; Len: Integer): STRING;
  BEGIN
    IF Length(S) < Len THEN S := 'ƒ' + S;
    WHILE Length(S) < Len DO S := S + 'ƒ';
    PadWithLines := S;
  END;

BEGIN
  FillChar(S,256,#32);
  S[0]:=Char(MaxViewWidth);
  L := 0;
  FillPos := 1;
  IF Count = 0 THEN BEGIN
    IF Line = 1 THEN BuildLine := GetRootString^
    ELSE BuildLine := '';
    Exit;
  END;
  IF PEntry(At(Count-1))^.Line < Line THEN BEGIN
    BuildLine := 'ERROR';
    Exit;
  END;
  WHILE (L <= Count-1) AND (PEntry(At(L))^.Line < Line) DO Inc(L);
  IF L <= Count-1 THEN BEGIN
    LowestLevelInLine := PEntry(At(L))^.Level;
    IndexOfLowestLevel := L;
  END;
  WHILE (L <= Count-1) AND (PEntry(At(L))^.Line = Line) DO BEGIN
    FillPos := (PEntry(At(L))^.Level-1)*(GetItemWidth+2)+(Length(GetRootString^)+2);
    FillInString(PEntry(At(L))^.LinkSymbol,S,FillPos);
    Inc(FillPos);
    FillInString(PadWithLines(PEntry(At(L))^.Name^,GetItemWidth+1),S,FillPos);
    Inc(L);
  END;
  IF Line = 1 THEN BEGIN
    FillInString(GetRootString^,S,1);
    FillInString('ƒ',S,Length(GetRootString^)+1);
  END;
  InsertBars;
  CutLinesOff(S,FillPos+1,GetItemWidth);
  BuildLine := S;
END;

FUNCTION TItemCollection.ValidRange(I: Integer): Boolean;
BEGIN
  ValidRange := (I <= Count-1) AND (I >= 0);
END;

FUNCTION TItemCollection.LevelFoundBelow(WhichLevel: Byte; Index: Integer): Boolean;
VAR
  L: Integer;
BEGIN
  L := Index;
  WHILE (L <= Count-1) AND (PEntry(At(L))^.Level > WhichLevel) DO Inc(L);
  LevelFoundBelow := (L <= Count-1) AND (PEntry(At(L))^.Level = WhichLevel);
END;

PROCEDURE TItemCollection.InsertWithLine(Item: Pointer);
BEGIN
  Insert(Item);
  IF Count = 1 THEN PEntry(Item)^.Line := 1
  ELSE IF PEntry(At(IndexOf(Item)-1))^.Level <
          PEntry(Item)^.Level THEN
	    PEntry(Item)^.Line := PEntry(At(IndexOf(Item)-1))^.Line
        ELSE PEntry(Item)^.Line := PEntry(At(IndexOf(Item)-1))^.Line + 1;
END;

PROCEDURE TItemCollection.ComputeLinkSym;
CONST
  BrotherDirAbove = '¿';
  BrotherDirBelow = '¬';
  NoBrotherDirs   = 'ƒ';
  BothBrotherDirs = '√';

  FUNCTION HasBrother(Upward: Boolean; Index: Integer): Boolean;
  VAR
    BreakLevel: Byte;
    Found: Boolean;
    I: Integer;
    Step: ShortInt;

  BEGIN
    HasBrother := False;
    IF Upward THEN Step := 1
              ELSE Step := -1;
    Found := False;
    I := Index;
    BreakLevel := PEntry(At(Index))^.Level;
    WHILE NOT Found AND (BreakLevel <= PEntry(At(I))^.Level) DO BEGIN
      IF ValidRange(I + Step) THEN I := I + Step
        ELSE Exit;
      IF PEntry(At(I))^.Level = BreakLevel THEN Found := True;
    END;
    HasBrother := Found;
  END;

  FUNCTION HasBrotherAbove(Index: Integer): Boolean;
  BEGIN
    HasBrotherAbove := HasBrother(False,Index);
  END;

  FUNCTION HasBrotherBelow(Index: Integer): Boolean;
  BEGIN
    HasBrotherBelow := HasBrother(True,Index);
  END;

VAR
  L: Integer;
  Sym: Char;

BEGIN
  FOR L := 0 TO Count-1 DO BEGIN
    IF HasBrotherAbove(L) THEN
      IF HasBrotherBelow(L) THEN Sym := BothBrotherDirs
                            ELSE Sym := BrotherDirAbove
    ELSE IF HasBrotherBelow(L) THEN Sym := BrotherDirBelow
                               ELSE Sym := NoBrotherDirs;
    PEntry(At(L))^.LinkSymbol := Sym;
  END;
END;

FUNCTION TItemCollection.IndexOf(Item: Pointer): Integer;
BEGIN
  IF Item = NIL THEN IndexOf := -1
  ELSE IndexOf := TCollection.IndexOf(Item);
END;

BEGIN
  RegisterType(REntry);
END. { of Tree }
