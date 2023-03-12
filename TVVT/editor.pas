{$S-}

UNIT Editor;



INTERFACE



USES

  StrTools, App, Memory, Dialogs, Drivers, Objects, Views,

  Strings,
  TVBCmds;





CONST







  cmEdExit          = 0;   { Editor beendet, nichts sichern, nicht l”schen }

  cmEdSave          = 1;   { Block sichern, nicht l”schen                  }

  cmEdSaveAndExit   = 2;   { Editor beendet, Block sichern und l”schen     }

  cmEdRead          = 3;   { Editor ben”tigt Text                          }

  cmEdSend          = 4;   { Hier kommt der Text fr den Editor            }

  cmEdSaveAs        = 5;

  cmEdSendText:WORD = 5432;{ Editor Inter View Command                     }

  CMemoEdit: STRING[2] = #26#27;



TYPE

  PEdit = ^TEdit;





  TextModes = (StopAtEnd, Block);

  PEditorWindow = ^TEditorWindow;

  TEditorWindow = OBJECT (TWindow)

    Editor: PEdit;

    CONSTRUCTOR Init( Bounds: TRect;

                      WindowNo: WORD;

                      WindowName: STRING;

                      Anchor: PCollection;

                      cmEdBase: WORD);

    Procedure Close; Virtual;

    Procedure NewTitle(Titel: String);

(*    Procedure SetState(AState: Word; Enable: Boolean); virtual;*)

  END;

  PLineEditor = ^TLineEditor;

  TLineEditor = OBJECT (TEditorWindow)

    CONSTRUCTOR Init( Bounds: TRect;

                      WindowNo: WORD;

                      WindowName: STRING;

                      Anchor: PCollection;

                      ALineLen: INTEGER;

                      cmEdBase: WORD);

  END;





  TEdit = OBJECT (TScroller)

    overwrite: BOOLEAN;      { Modus ist berschreiben/einfgen         }

    cmBase, WdwNo: WORD;

    TheLineLen: INTEGER;

    CONSTRUCTOR Init( Bounds: TRect;

                      AHScrollBar,AVScrollBar: PScrollBar;

                      AnAnchor: PCollection;

                      cmEdBase, WNo: WORD);

    CONSTRUCTOR Load (VAR S: TStream);

    PROCEDURE Store (VAR S: TStream);

    PROCEDURE SetData (VAR Rec); VIRTUAL;

    PROCEDURE GetData (VAR Rec); VIRTUAL;

    FUNCTION DataSize: WORD; VIRTUAL;

    PROCEDURE DeleteBlock;

    PROCEDURE GetBlock( VAR P: PCollection );

    PROCEDURE PutBlock(Block: PCollection; At: TPoint);

    PROCEDURE StartMark(P: TPoint);

    PROCEDURE EndMark(P: TPoint);

    PROCEDURE CorrectMark(VAR P: TPoint);

    FUNCTION BeforeBlock (P: TPoint): BOOLEAN;

    FUNCTION InBlock (P: TPoint): BOOLEAN;

    FUNCTION AfterBlock (P: TPoint): BOOLEAN;

    FUNCTION BlockDefined: BOOLEAN;

    FUNCTION MaxLines: INTEGER;

    FUNCTION GetLine(LineNo: INTEGER): STRING;

    FUNCTION GetIndent(LineNo: INTEGER): INTEGER;

    FUNCTION IsClipBoard: Boolean;

    PROCEDURE SetLine(LineNo: INTEGER; ALine: STRING);

    PROCEDURE DeleteLine(LineNo: INTEGER);

    PROCEDURE InsertLine(LineNo: INTEGER; ALine: STRING);

    PROCEDURE CombineLines(LineNo: INTEGER);

    PROCEDURE CutLineAt(LineNo, Pos: INTEGER);

    PROCEDURE Draw; VIRTUAL;

    PROCEDURE HandleEvent ( VAR Event: TEvent ); VIRTUAL;

    FUNCTION LineLen(LineNo: INTEGER): INTEGER; VIRTUAL;

    FUNCTION Mode(LineNo: INTEGER): TextModes; VIRTUAL;

    DESTRUCTOR Done; VIRTUAL;

  PRIVATE

    ReDrawLine, WasShift, Save: BOOLEAN;

    LastChar: CHAR;

    Anchor: PCollection;

    Blk: TRect;

    FUNCTION GetWindowNo: INTEGER;

  END;

  PLineEdit = ^TLineEdit;

  TLineEdit = OBJECT (TEdit)

    CONSTRUCTOR Init( Bounds: TRect;

                      AHScrollBar,AVScrollBar: PScrollBar;

                      AnAnchor: PCollection;

                      ALineLen: INTEGER;

                      cmEdBase, WNo: WORD);

    FUNCTION Mode(LineNo: INTEGER): TextModes; VIRTUAL;

  END;

  PMemoEdit = ^TMemoEdit;

  TMemoEdit = OBJECT (TEdit)

    FUNCTION GetPalette: PPalette; VIRTUAL;

    DESTRUCTOR Done; VIRTUAL;

  END;



   PMyMemoEdit = ^TMyMemoEdit;

   TMyMemoEdit = OBJECT (TEdit)

     DESTRUCTOR Done; VIRTUAL;

   END;







CONST



  Clipboard: PEdit = nil;





  REdit: TStreamRec = (

    ObjType: 2100;

    VmtLink: Ofs(TypeOf(TEdit)^);

    Load:    @TEdit.Load;

    Store:   @TEdit.Store

  );

  RLineEdit: TStreamRec = (

    ObjType: 2101;

    VmtLink: Ofs(TypeOf(TLineEdit)^);

    Load:    @TLineEdit.Load;

    Store:   @TLineEdit.Store

  );

  RMemoEdit: TStreamRec = (

    ObjType: 2102;

    VmtLink: Ofs(TypeOf(TMemoEdit)^);

    Load:    @TMemoEdit.Load;

    Store:   @TMemoEdit.Store

  );

  RMyMemoEdit: TStreamRec = (

    ObjType: 2105;

    VmtLink: Ofs(TypeOf(TMemoEdit)^);

    Load:    @TMemoEdit.Load;

    Store:   @TMemoEdit.Store

  );

  REditor: TStreamRec = (

    ObjType: 2103;

    VmtLink: Ofs(TypeOf(TEditorWindow)^);

    Load:    @TEditorWindow.Load;

    Store:   @TEditorWindow.Store

  );

  RLineEditor: TStreamRec = (

    ObjType: 2104;

    VmtLink: Ofs(TypeOf(TLineEditor)^);

    Load:    @TLineEditor.Load;

    Store:   @TLineEditor.Store

  );





PROCEDURE RegisterEditor;



IMPLEMENTATION



CONSTRUCTOR TEdit.Init( Bounds: TRect;

                            AHScrollBar,AVScrollBar: PScrollBar;

                            AnAnchor: PCollection;

                            cmEdBase, WNo: WORD);

  BEGIN

    TScroller.Init(Bounds, AHScrollBar, AVScrollBar);

    GrowMode := gfGrowHiX + gfGrowHiY;

    ShowCursor;

    IF AnAnchor = Nil THEN AnAnchor := New(PCollection, Init (20, 1));

    IF AnAnchor^.Count = 0 THEN AnAnchor^.Insert( New( PStr, Init('')));

    Anchor := AnAnchor;

    overwrite := False;

    Save := False;

    cmBase := cmEdBase;

    WdwNo := WNo;

    TheLineLen := 70;

    Blk.Assign(0,0,0,0);

    EventMask := EventMask OR evBroadCast;

    Options := Options OR ofPostProcess;

    SetLimit(250, Anchor^.Count);

  END;



CONSTRUCTOR TEdit.Load (VAR S: TStream);

  BEGIN

    TScroller.Load(S);

    S.Read(overwrite, SizeOf(BOOLEAN));

    S.Read(cmBase, 3*SizeOf(WORD));

    S.Read(ReDrawLine, 3*SizeOf(BOOLEAN));

    S.Read(LastChar, SizeOf(CHAR));

    Anchor := PCollection(S.Get);

    S.Read(Blk, SizeOf(TRect));

  END;



PROCEDURE TEdit.Store (VAR S: TStream);

  BEGIN

    TScroller.Store(S);

    S.Write(overwrite, SizeOf(BOOLEAN));

    S.Write(cmBase, 3*SizeOf(WORD));

    S.Write(ReDrawLine, 3*SizeOf(BOOLEAN));

    S.Write(LastChar, SizeOf(CHAR));

    S.Put(Anchor);

    S.Write(Blk, SizeOf(TRect));

  END;



PROCEDURE TEdit.GetData ( VAR Rec);

  BEGIN

    Move(Anchor, Rec, SizeOf(Anchor));

  END;



PROCEDURE TEdit.SetData (VAR Rec);

  BEGIN

    Dispose(Anchor, Done);

    Move(Rec, Anchor, SizeOf(Anchor));

  END;



FUNCTION TEdit.DataSize: WORD;

  BEGIN

    DataSize := SizeOf(Anchor);

  END;



FUNCTION TEdit.IsClipboard: Boolean;

begin

  IsClipboard := Clipboard = @Self;

end;







PROCEDURE TEdit.DeleteBlock;

  VAR

    I: INTEGER;

    W1, W2: STRING;

  BEGIN

    W1 := Copy(PStr(Anchor^.At(Blk.A.Y))^.GetStr, 1, Blk.A.X);

    W2 := Copy(PStr(Anchor^.At(Blk.B.Y))^.GetStr, Blk.B.X+1, 255);

    FOR I := Blk.A.Y TO Blk.B.Y-1 DO Anchor^.AtFree(Blk.A.Y);

    PStr(Anchor^.At(Blk.A.Y))^.SetStr(W1+W2);

    SetLimit(250, Anchor^.Count);

    Blk.B := Blk.A;

  END;



PROCEDURE TEdit.GetBlock ( VAR P: PCollection);

  VAR I: INTEGER;

  BEGIN

    P := New(PCollection, Init(20,1));

    IF Blk.A.Y = Blk.B.Y

     THEN P^.AtInsert(0, New(PStr, Init(Copy(PStr(Anchor^.At(Blk.A.Y))^.GetStr, Blk.A.X+1, Blk.B.X-Blk.A.X))))

     ELSE BEGIN

      P^.AtInsert(0, New(PStr, Init(Copy(PStr(Anchor^.At(Blk.A.Y))^.GetStr, Blk.A.X+1, 255))));

      FOR I := 1 TO Blk.B.Y-Blk.A.Y-1 DO

       P^.AtInsert(I, New(PStr, Init(PStr(Anchor^.At(Blk.A.Y+I))^.GetStr)));

      P^.AtInsert(Blk.B.Y-Blk.A.Y, New(PStr, Init(Copy(PStr(Anchor^.At(Blk.B.Y))^.GetStr, 1, Blk.B.X))));

    END;

  END;



PROCEDURE TEdit.PutBlock(Block: PCollection; At: TPoint);

  VAR

    W1, W2: STRING;

    I: INTEGER;

  BEGIN

    WITH At DO BEGIN

      W1 := GetLine(Y+Delta.Y);

      W2 := Copy(W1, X+Delta.X+1,255);

      Delete(W1, X+Delta.X+1, 255);

      W1 := W1 + Chars(' ',X+Delta.X-Length(W1));

      IF Block^.Count=1

       THEN PStr(Anchor^.At(Y+Delta.Y))^.SetStr(W1+PStr(Block^.At(Block^.Count-1))^.GetStr+W2)

       ELSE BEGIN

        PStr(Anchor^.At(Y+Delta.Y))^.SetStr(PStr(Block^.At(Block^.Count-1))^.GetStr+W2);

        FOR I := Block^.Count-2 DOWNTO 1 DO

         Anchor^.AtInsert(Y+Delta.Y, New(PStr, Init(PStr(Block^.At(I))^.GetStr)));

        Anchor^.AtInsert(Y+Delta.Y, New(PStr, Init(W1+PStr(Block^.At(0))^.GetStr)));

      END;

    END;

    SetLimit(250, Anchor^.Count);

  END;



PROCEDURE TEdit.StartMark(P: TPoint);

  BEGIN

    Blk.A.X := P.X+Delta.X;

    Blk.A.Y := P.Y+Delta.Y;

    CorrectMark(Blk.A);

  END;



PROCEDURE TEdit.EndMark(P: TPOINT);

  BEGIN

    Blk.B.X := P.X+Delta.X;

    Blk.B.Y := P.Y+Delta.Y;

    CorrectMark(Blk.B);

  END;



PROCEDURE TEdit.CorrectMark(VAR P: TPoint);

  BEGIN

    IF P.Y >= Anchor^.Count THEN P.Y := Anchor^.Count-1;

    IF P.Y < 0 THEN P.Y := 0;

    IF P.X > Length(GetLine(P.Y)) THEN P.X := Length(GetLine(P.Y));

    IF P.X < 0 THEN P.X := 0;

  END;



FUNCTION TEdit.BeforeBlock (P: TPoint): BOOLEAN;

  BEGIN

    WITH Blk.A DO

     BeforeBlock := (P.Y < Y) OR ((P.Y = Y) AND (P.X < X));

  END;



FUNCTION TEdit.InBlock (P: TPoint): BOOLEAN;

  BEGIN

    InBlock := NOT BeforeBlock(P) AND NOT AfterBlock(P);

    { Das ist doch selbstdokumentierender Code oder? }

  END;



FUNCTION TEdit.AfterBlock (P: TPoint): BOOLEAN;

  BEGIN

    WITH Blk.B DO

     AfterBlock := (P.Y > Y) OR ((P.Y = Y) AND (P.X >= X));

  END;



FUNCTION TEdit.BlockDefined: BOOLEAN;

  BEGIN

    BlockDefined := (Blk.A.Y <> Blk.B.Y) OR

      ((Blk.A.Y=Blk.B.Y) AND (Blk.A.X <> Blk.B.X));

  END;



FUNCTION TEdit.MaxLines: INTEGER;

  BEGIN

    MaxLines := Anchor^.Count;

  END;



FUNCTION TEdit.GetLine(LineNo: INTEGER): STRING;

  BEGIN

    GetLine := PStr(Anchor^.At(LineNo))^.GetStr;

  END;



FUNCTION TEdit.GetIndent(LineNo: INTEGER): INTEGER;

  VAR WorkStr: STRING;

      I: INTEGER;

  BEGIN

    WorkStr := GetLine(LineNo);

    I := 1;

    WHILE (WorkStr[I] = ' ') DO Inc(I);

    If I > Length(WorkStr) then I := Length(WorkStr) + 1;

    GetIndent := I-1;

  END;



PROCEDURE TEdit.SetLine(LineNo: INTEGER; ALine: STRING);

  VAR OldLine: STRING;

      I, Diff: INTEGER;

  BEGIN

    IF (Blk.B.Y = LineNo) OR (Blk.A.Y = LineNo)

     THEN OldLine := GetLine(LineNo);

    Diff := Length(ALine)-Length(OldLine);

    PStr(Anchor^.At(LineNo))^.SetStr(ALine);

    IF (Blk.B.Y = LineNo) OR (Blk.A.Y = LineNo) THEN BEGIN

      I := 1;

      WHILE (I<=Length(OldLine)) AND (I<=Length(ALine))

         AND (OldLine[I] = ALine[I]) DO Inc(I);

      IF (Blk.A.Y = LineNo) AND (Blk.A.X > I-1) THEN Inc(Blk.A.X, Diff);

      IF (Blk.B.Y = LineNo) AND (Blk.B.X > I-1) THEN Inc(Blk.B.X, Diff);

    END;

    CorrectMark(Blk.A);

    CorrectMark(Blk.B);

  END;



PROCEDURE TEdit.InsertLine(LineNo: INTEGER; ALine: STRING);

  BEGIN

    IF (Anchor^.Count < Anchor^.Limit) OR

       ((Anchor^.Limit < 16380) AND (Anchor^.Delta > 0)) THEN BEGIN

      Anchor^.AtInsert(LineNo, New(PStr, Init(ALine)));

      IF LineNo <= Blk.A.Y THEN Inc(Blk.A.Y);

      IF LineNo <= Blk.B.Y THEN Inc(Blk.B.Y);

      SetLimit(250, Anchor^.Count);

    END;

  END;



PROCEDURE TEdit.DeleteLine(LineNo: INTEGER);

  BEGIN

    Anchor^.AtFree(LineNo);

    IF LineNo < Blk.A.Y

     THEN Dec(Blk.A.Y)

     ELSE IF LineNo = Blk.A.Y THEN Blk.A.X := 0;

    IF LineNo < Blk.B.Y

     THEN Dec(Blk.B.Y)

     ELSE IF LineNo = Blk.B.Y THEN Blk.B.X := 0;

    SetLimit(250, Anchor^.Count);

  END;



PROCEDURE TEdit.CombineLines(LineNo: INTEGER);

  VAR Bl: TRect;

      Len: INTEGER;

  BEGIN

    Bl := Blk;

    Len := Length(GetLine(LineNo));

    SetLine(LineNo, GetLine(LineNo)+GetLine(LineNo+1));

    IF LineNo+1=Bl.A.Y THEN BEGIN

      Blk.A.Y := Bl.A.Y-1;

      Blk.A.X := Bl.A.X+Len;

    END;

    IF LineNo+1=Bl.B.Y THEN BEGIN

      Blk.B.Y := Bl.B.Y-1;

      Blk.B.X := Bl.B.X+Len;

    END;

    DeleteLine(LineNo+1);

  END;



PROCEDURE TEdit.CutLineAt(LineNo, Pos: INTEGER);

  VAR WorkStr: STRING;

      Bl: TRect;

      Blanks: INTEGER;

  BEGIN

    Bl := Blk;

    Blanks := GetIndent(LineNo);

    WorkStr := Copy(GetLine(LineNo), Pos+1, Length(GetLine(LineNo)));

    SetLine(LineNo, Copy(GetLine(LineNo), 1, Pos));

    IF Pos > Blanks

     THEN WorkStr := Chars(' ',GetIndent(LineNo)) + WorkStr

     ELSE WorkStr := Chars(' ',Blanks-Pos) + Copy(WorkStr, Blanks-Pos+1, Length(WorkStr));

    InsertLine(LineNo+1, WorkStr);

    IF LineNo=Bl.A.Y

     THEN BEGIN

      IF LineNo=Bl.B.Y

       THEN BEGIN

        IF Bl.B.X>Pos THEN BEGIN

         Blk.B.Y := Bl.B.Y+1;

         Blk.B.X := Bl.B.X-Pos+Blanks;

        END;

        IF Bl.A.X>Pos THEN BEGIN

         Blk.A.Y := Bl.A.Y+1;

         Blk.A.X := Bl.A.X-Pos+Blanks;

        END;

       END

       ELSE IF Bl.A.X>Pos THEN BEGIN

        Blk.A.X := Bl.A.X-Pos+Blanks;

        Blk.A.Y := Bl.A.Y+1;

      END

     END

     ELSE IF (LineNo=Bl.B.Y) AND (Bl.B.X>Pos) THEN BEGIN

      Blk.B.X := Bl.B.X-Pos+Blanks;

      Blk.B.Y := Bl.B.Y+1;

    END;

  END;



PROCEDURE TEdit.Draw;

  VAR

    Bl: TRect;

    Norm, High: BYTE;

    B: TDrawBuffer;

    I: INTEGER;

  PROCEDURE DrawLine(I: INTEGER);

    BEGIN

      WITH Delta DO BEGIN

        IF BlockDefined

         THEN BEGIN

          IF (Y+I >= Bl.A.Y) AND (Y+I < Bl.B.Y)

           THEN MoveChar(B, ' ', High, Size.X)

           ELSE MoveChar(B, ' ', Norm, Size.X);

          IF (I+Y = Bl.B.Y) AND (Bl.B.X-X > 0)

           THEN IF Bl.B.X-X < Size.X

            THEN MoveChar(B, ' ', High, Bl.B.X-X)

            ELSE MoveChar(B, ' ', High, Size.X);

          IF (I+Y = Bl.A.Y) AND (Bl.A.X-X > 0)

           THEN IF Bl.A.X-X < Size.X

            THEN MoveChar(B, ' ', Norm, Bl.A.X-X)

            ELSE MoveChar(B, ' ', Norm, Size.X);

         END

         ELSE MoveChar(B, ' ', Norm, Size.X);

        IF Anchor^.Count >  Delta.Y+I

         THEN MoveStr( B, Copy(GetLine(Y+I), X+1, Size.X), 0);

        Writeline(0, I, Size.X, 1, B);

      END;

    END;

  BEGIN

    IF (Blk.A.Y > Blk.B.Y) OR ((Blk.A.Y = Blk.B.Y) AND (Blk.A.X > Blk.B.X))

     THEN BEGIN

      Bl.A := Blk.B;

      Bl.B := Blk.A;

     END

     ELSE Bl := Blk;

    Norm := GetColor(1);

    High := GetColor(2);

    IF ReDrawLine

     THEN DrawLine(Cursor.Y)

     ELSE FOR I := 0 TO Size.Y-1 DO DrawLine(I);

    ReDrawLine := False;

  END;



TYPE

  POptions = ^TOptions;

  TOptions = OBJECT (TDialog)

    CONSTRUCTOR Init;

  END;



CONSTRUCTOR TOptions.Init;

  VAR

    R,B: TRect;

    Control: PView;

  BEGIN



    EventMask := EventMask OR evBroadCast;



    GetExtent(R);

    R.Assign(R.A.X+5, R.A.Y+1, R.A.X+26, R.A.Y+8);

    TDialog.Init(R,'Block to');



    GetExtent(R);

    B.Assign(R.A.X+13, R.A.Y+2,R.A.X+17, R.A.Y+3);

    Control := New(PInputLine, Init(B, 1));

    Insert(Control);

    B.Assign(R.A.X+2, R.A.Y+2, R.A.X+11, R.A.Y+3);

    Control := New(PLabel, Init(B, 'Window:', Control));

    Insert(Control);



    B.Assign( R.A.X+1, R.A.Y+4, R.A.X+8, R.A.Y+6);

    Control := New(PButton, Init(B, 'O~K~', cmOk, bfDefault));

    Insert(Control);



    B.Assign( R.A.X+12, R.A.Y+4, R.A.X+19, R.A.Y+6);

    Control := New(PButton, Init(B, 'Cancel', cmCancel, bfNormal));

    Insert(Control);



    SelectNext(False);



  END;



FUNCTION TEdit.GetWindowNo: INTEGER;

  VAR

    Err, WindowNo: INTEGER;

    WindowS: STRING[1];

    Option: POptions;

  BEGIN

    WindowNo := -1;

    WindowS := '';

    Option := New(POptions, Init);

    IF NOT LowMemory THEN BEGIN

      Option^.SetData(WindowS);

      IF DeskTop^.ExecView(Option) = cmOk THEN BEGIN

        Option^.GetData(WindowS);

        Val(WindowS, WindowNo, Err);

        IF Err <> 0 THEN WindowNo := -1;

      END;

    END;

    Dispose(Option, Done);

    GetWindowNo := WindowNo;

  END;



PROCEDURE TEdit.HandleEvent ( VAR Event: TEvent );

  VAR ReDraw, ShiftState: BOOLEAN;

      T: TPoint;



{ Allgemein verwendete Prozeduren und Funktionen -------------------- }

  PROCEDURE CheckForRedraw;

    BEGIN

      IF Cursor.X < 0 THEN BEGIN

        Redraw := True;

        Delta.X := Delta.X + Cursor.X; { Cursor.X ist negativ }

        SetCursor(0, Cursor.Y);

      END;

      IF Cursor.X >= Size.X THEN BEGIN

        Redraw := True;

        Delta.X := Delta.X + (Cursor.X-Size.X+1);

        SetCursor(Size.X-1, Cursor.Y);

      END;

      IF Cursor.Y < 0 THEN BEGIN

        Redraw := True;

        Delta.Y := Delta.Y + Cursor.Y; { Cursor.Y ist negativ }

        SetCursor(Cursor.X, 0);

      END;

      IF Cursor.Y >= Size.Y THEN BEGIN

        Redraw := True;

        Delta.Y := Delta.Y + (Cursor.Y-Size.Y+1);

        SetCursor(Cursor.X, Size.Y-1);

      END;

      IF Redraw THEN BEGIN

        ReDrawLine := False;

        DrawView;

        ScrollTo(Delta.X,Delta.Y);

      END;

    END;



  FUNCTION Cur: INTEGER;

    BEGIN

      Cur := Cursor.Y+Delta.Y;

    END;



  FUNCTION Prv: INTEGER;

    BEGIN

      Prv := Cur-1;

    END;



  FUNCTION Nxt: INTEGER;

    BEGIN

      Nxt := Cur+1;

    END;



  FUNCTION Max: INTEGER;

    BEGIN

      Max := Anchor^.Count-1;

    END;



  PROCEDURE ReformatParagraph;

    VAR

      S: STRING;

      I, L, L1: INTEGER;

    BEGIN

      I := Cur;

      WHILE (I<Anchor^.Count-1) AND

            (GetIndent(I)=GetIndent(I+1)) AND

            (Length(GetLine(I+1)) > 0) DO BEGIN

        WHILE (I < Anchor^.Count-1) AND

              (Length(GetLine(I)) < LineLen(I)) AND

              (GetIndent(I)=GetIndent(I+1)) AND

              (Length(GetLine(I+1)) > 0) DO BEGIN

          L := Length(GetLine(I));

          SetLine(I, GetLine(I)+' '+'R');

          CombineLines(I);

          S := GetLine(I);

          Delete(S,L+2,GetIndent(I)+1);

          SetLine(I,S);

        END;

        S := GetLine(I);

        L1 := LineLen(I)+1;

        IF L1 > Length(GetLine(I)) THEN Exit;

        WHILE (L1 > 0) AND (S[L1] <> ' ') DO Dec(L1);

        IF L1 = 0 THEN Exit;

        CutLineAt(I, L1);

        IF (Cursor.Y+Delta.Y=I) AND (Cursor.X+Delta.X>=L1+1)

         THEN SetCursor(GetIndent(I)+Cursor.X-L1, Cursor.Y+1);

        Inc(I);

      END;

      IF Length(GetLine(I)) >= LineLen(I) THEN BEGIN

        S := GetLine(I);

        L1 := LineLen(I);

        WHILE (L1 > 0) AND (S[L1] <> ' ') DO Dec(L1);

        IF L1 = 0 THEN Exit;

        WHILE (L1 > 0) AND (S[L1] = ' ') DO Dec(L1);

        IF L1 = 0 THEN Exit;

        CutLineAt(I, L1+1);

        IF (Cursor.Y+Delta.Y=I) AND (Cursor.X+Delta.X>=L1+1)

         THEN SetCursor(GetIndent(I)+Cursor.X-L1-1, Cursor.Y+1);

      END;

    END;



  PROCEDURE InsertChar(C: CHAR; Pos,Line: INTEGER);

    VAR S: STRING;

    BEGIN

      S := PStr(Anchor^.At(Line))^.GetStr;

      Insert(C,S,Pos+1);

      PStr(Anchor^.At(Line))^.SetStr(S);

      IF BlockDefined THEN BEGIN

        IF (Pos<Blk.A.X)  AND (Blk.A.Y=Line) THEN Dec(Blk.A.X);

        IF (Pos<Blk.B.X) AND (Blk.B.Y=Line) THEN Dec(Blk.B.X);

      END;

      Save := True

    END;



  PROCEDURE DeleteChar (Pos, Line: INTEGER);

    VAR S: STRING;

    BEGIN

      S := PStr(Anchor^.At(Line))^.GetStr;

      Delete(S,Pos+1,1);

      PStr(Anchor^.At(Line))^.SetStr(S);

      IF BlockDefined THEN BEGIN

        IF (Pos<Blk.A.X)  AND (Blk.A.Y=Line) THEN Dec(Blk.A.X);

        IF (Pos<Blk.B.X) AND (Blk.B.Y=Line) THEN Dec(Blk.B.X);

      END;

      ReDraw := True;

      Save := True

    END;



{ Ctrl-K Behandlungen ----------------------------------------------- }

  PROCEDURE HandleCtrlKB;

    BEGIN

      IF NOT BlockDefined THEN EndMark(Cursor);

      StartMark(Cursor);

      ReDraw := True;

    END;



  PROCEDURE HandleCtrlKK;

    BEGIN

      IF NOT BlockDefined THEN StartMark(Cursor);

      EndMark(Cursor);

      ReDraw := True;

    END;



  PROCEDURE HandleCtrlKV;

    VAR

      Block: PCollection;

      CursorAbs: TPoint;

      Len: INTEGER;

    BEGIN

      CursorAbs.X := Cursor.X+Delta.X;

      CursorAbs.Y := Cursor.Y+Delta.Y;

      IF BlockDefined AND NOT InBlock(CursorAbs) THEN WITH Cursor DO BEGIN

        GetBlock(Block);

        DeleteBlock;

        Len := Length(PStr(Block^.At(Block^.Count-1))^.GetStr);

        IF AfterBlock(CursorAbs)

         THEN IF Y+Delta.Y=Blk.B.Y

          THEN SetCursor(X-Len, Y-Block^.Count+1)

          ELSE SetCursor(X, Y-Block^.Count+1);

        PutBlock(Block,Cursor);

        IF Block^.Count = 1

         THEN Blk.Assign(X+Delta.X, Y+Delta.Y, X+Delta.X+Len, Y+Delta.Y)

         ELSE Blk.Assign(X+Delta.X, Y+Delta.Y, Len, Y+Delta.Y+Block^.Count-1);

        Dispose(Block, Done);

        ReDraw := True;

        Save := True

      END;

    END;



  PROCEDURE HandleCtrlKC;

    VAR

      Block: PCollection;

      Len: INTEGER;

    BEGIN

      IF BlockDefined THEN WITH Cursor DO BEGIN

        GetBlock(Block);

        PutBlock(Block,Cursor);

        Len := Length(PStr(Block^.At(Block^.Count-1))^.GetStr);

        IF Block^.Count = 1

         THEN Blk.Assign(X+Delta.X, Y+Delta.Y, X+Delta.X+Len, Y+Delta.Y)

         ELSE Blk.Assign(X+Delta.X, Y+Delta.Y, Len, Y+Delta.Y+Block^.Count-1);

        Dispose(Block, Done);

        Setlimit(250, Anchor^.Count);

        Redraw := True;

        Save := True

      END;

    END;



  PROCEDURE HandleCtrlKY;

    BEGIN

      IF BlockDefined THEN BEGIN

        DeleteBlock;

        SetCursor(Blk.B.X-Delta.X, Blk.B.Y-Delta.Y);

        Redraw := True;

        Setlimit(250, Anchor^.Count);

        Save := True

      END;

    END;



  PROCEDURE HandleCtrlKS;

    VAR

      P: PCollection;

      Event: TEvent;

      WNo: INTEGER;

    BEGIN

      WNo := GetWindowNo;

      IF WNo > 0 THEN BEGIN

        Event.What := evBroadCast;

        Event.Command := cmEdSendText+WNo;

        IF Not Blockdefined THEN Blk.Assign(0,0,Limit.X, Limit.Y-1);

        GetBlock(P);

        Event.InfoPtr := P;

        TopView^.HandleEvent(Event);

      END;

    END;



  PROCEDURE HandleCtrlKW;

    VAR P: PCollection;

    BEGIN

      Event.What := evBroadCast;

      Event.Command := cmBase+cmEdSave;

      IF NOT BlockDefined THEN Blk.Assign(0,0,Limit.X,Limit.Y-1);

      GetBlock(P);

      Event.InfoPtr := P;

      TopView^.HandleEvent(Event);

      Dispose(P, Done);

      Save := False

    END;



  PROCEDURE HandleCtrlKR;

    BEGIN

      Event.What := evBroadCast;

      Event.Command := cmBase+cmEdRead;

      TopView^.HandleEvent(Event);

    END;



  PROCEDURE HandleCtrlKQ;

    BEGIN

      Event.What := evBroadCast;

      Event.Command := cmBase+cmEdSaveandExit;

      TopView^.HandleEvent(Event);

    END;



  PROCEDURE HandleCtrlKI;

    VAR I: INTEGER;

    BEGIN

      IF BlockDefined THEN BEGIN

        FOR I := Blk.A.Y TO Blk.B.Y-1 DO InsertChar(' ',0,I);

        IF Blk.B.X > 0 THEN InsertChar(' ',0,Blk.B.Y);

        SetCursor(Blk.A.X-Delta.X, Blk.A.Y-Delta.Y);

        ReDraw := True;

        Save := True

      END;

    END;



  PROCEDURE HandleCtrlKU;

    VAR I: INTEGER;

        S: STRING;

    BEGIN

      IF BlockDefined THEN BEGIN

        FOR I := Blk.A.Y TO Blk.B.Y-1 DO WITH PStr(Anchor^.At(I))^ DO BEGIN

          S := GetStr;

          IF S[1] = ' ' THEN Delete(S,1,1);

          SetStr(S);

        END;

        IF Blk.B.X > 0 THEN WITH PStr(Anchor^.At(Blk.B.Y))^ DO BEGIN

         S := GetStr;

         IF S[1] = ' ' THEN Delete(S,1,1);

         SetStr(S);

         Dec(Blk.B.X);

        END;

        SetCursor(Blk.A.X-Delta.X, Blk.A.Y-Delta.Y);

        ReDraw := True;

        Save := True

      END;

    END;



  FUNCTION HandleCtrlK: BOOLEAN;

    BEGIN

      HandleCtrlK := False;

      CASE Event.CharCode OF

        ^B,'b','B':  HandleCtrlKB;

        ^K,'k','K':  HandleCtrlKK;

        ^S,'s','S':  HandleCtrlKS;

        ^V,'v','V':  HandleCtrlKV;

        ^Y,'y','Y':  HandleCtrlKY;

        ^C,'c','C':  HandleCtrlKC;

        ^U,'u','U':  HandleCtrlKU;

        ^I,'i','I':  HandleCtrlKI;

        ^Q,'q','Q':  HandleCtrlKQ;

        ^W,'w','W':  HandleCtrlKW;

        ^R,'r','R':  HandleCtrlKR;

        ELSE Exit;

      END;

      HandleCtrlK := True;

      LastChar := #0;

    END;

{ Q ----------------------------------------------------------------- }



  FUNCTION HandleCtrlQ: BOOLEAN;

    BEGIN

      HandleCtrlQ := True;

    END;



{ Behandlung aller anderen Tasten mit SHIFT ------------------------- }

  PROCEDURE HandleShiftKeys (VAR BlkA, BlkB: TPoint; NewX,NewY: INTEGER);

    VAR T: TPoint;

    BEGIN

      T.X := Cursor.X+Delta.X;

      T.Y := Cursor.Y+Delta.Y;

      CorrectMark(T);

      IF ShiftState THEN BEGIN

        IF NOT WasShift THEN BEGIN

          StartMark(Cursor);

          EndMark(Cursor);

        END;

        IF (BlkB.X = T.X) AND (BlkB.Y = T.Y)

         THEN BEGIN

          BlkB.X := NewX;

          BlkB.Y := NewY;

          CorrectMark(BlkB);

         END

         ELSE IF (BlkA.X = T.X) AND (BlkA.Y = T.Y)

          THEN BEGIN

           BlkA.X := NewX;

           BlkA.Y := NewY;

           CorrectMark(BlkA);

         END;

         ReDraw := True;

      END;

    END;



  FUNCTION HandleRightKey: BOOLEAN;

    BEGIN

      IF Delta.X+Cursor.X < Limit.X-1

       THEN BEGIN

        IF ShiftState

         THEN HandleShiftKeys(Blk.B, Blk.A, Cursor.X+Delta.X+1, Cursor.Y+Delta.Y);

        SetCursor(Cursor.X+1,Cursor.Y);

       END

       ELSE Write(^G);

      HandleRightKey := True;

    END;



  FUNCTION HandleLeftKey: BOOLEAN;

    BEGIN

      IF Cursor.X+Delta.X > 0 THEN BEGIN

        IF ShiftState

         THEN HandleShiftKeys(Blk.A, Blk.B, Cursor.X+Delta.X-1, Cursor.Y+Delta.Y);

        SetCursor(Cursor.X-1,Cursor.Y);

      END;

      HandleLeftKey := True;

    END;



  FUNCTION HandleDownKey: BOOLEAN;

    BEGIN

      IF Delta.Y+Cursor.Y < Anchor^.Count-1 THEN BEGIN

        IF ShiftState

         THEN HandleShiftKeys(Blk.A, Blk.B, Cursor.X+Delta.X, Delta.Y+Cursor.Y+1);

        SetCursor(Cursor.X,Cursor.Y+1);

      END;

      HandleDownKey := True;

    END;



  FUNCTION HandleUpKey: BOOLEAN;

    BEGIN

      IF Cursor.Y+Delta.Y > 0 THEN BEGIN

        IF ShiftState

         THEN HandleShiftKeys(Blk.A, Blk.B, Cursor.X+Delta.X, Cursor.Y+Delta.Y-1);

        SetCursor(Cursor.X,Cursor.Y-1);

      END;

      HandleUpKey := True;

    END;



  FUNCTION HandleHomeKey: BOOLEAN;

    BEGIN

      IF ShiftState THEN HandleShiftKeys(Blk.A, Blk.B, 0, Cursor.Y+Delta.Y);

      SetCursor(-Delta.X,Cursor.Y);

      HandleHomeKey := True;

    END;



  FUNCTION HandleEndKey: BOOLEAN;

    BEGIN

      IF ShiftState

       THEN HandleShiftKeys(Blk.A, Blk.B, Length(GetLine(Cur)), Cursor.Y+Delta.Y);

      SetCursor(Length(GetLine(Cur))-Delta.X, Cursor.Y);

      HandleEndKey := True;

    END;



  FUNCTION HandlePgUpKey: BOOLEAN;

    BEGIN

      IF Delta.Y+Cursor.Y < Size.Y-1

       THEN BEGIN

        HandleShiftKeys(Blk.B, Blk.A, Cursor.X+Delta.X, 0);

        SetCursor(Cursor.X, -Delta.Y);

        ReDraw := True;

       END

       ELSE BEGIN

        HandleShiftKeys(Blk.B, Blk.A, Cursor.X+Delta.X, Cursor.Y+Delta.Y-Size.Y+1);

        Dec(Delta.Y, Size.Y-1);

        Redraw := True;

      END;

      HandlePgUpKey := True;

    END;



  FUNCTION HandlePgDnKey: BOOLEAN;

    BEGIN

      IF Delta.Y+Cursor.Y+Size.Y >= Limit.Y

       THEN BEGIN

        HandleShiftKeys(Blk.B, Blk.A, Cursor.X+Delta.X, Limit.Y-1);

        SetCursor(Cursor.X, Limit.Y-1-Delta.Y)

       END

       ELSE BEGIN

        HandleShiftKeys(Blk.B, Blk.A, Cursor.X+Delta.X, Cursor.Y+Delta.Y+Size.Y-1);

        Inc(Delta.Y, Size.Y-1);

        Redraw := True;

      END;

      HandlePgDnKey := True;

    END;



{ Behandlung aller anderen Tasten ----------------------------------- }

  FUNCTION HandleBackKey: BOOLEAN;

    BEGIN

      IF Cursor.X + Delta.X > 0

       THEN BEGIN

        SetCursor(Cursor.X-1, Cursor.Y);

        DeleteChar(Delta.X+Cursor.X,Cur);

        ReDrawLine := True;

        DrawView;

        Save := True

       END

       ELSE IF Delta.Y+Cursor.Y > 0 THEN BEGIN

        SetCursor(Length(GetLine(Prv)), Cursor.Y-1);

        CombineLines(Cur);

        Redraw := True;

        Save := True

      END;

      HandleBackKey := True;

    END;



  FUNCTION HandleDelKey: BOOLEAN;

    BEGIN

      IF Delta.X+Cursor.X < Length(GetLine(Cur))

       THEN BEGIN

        DeleteChar(Cursor.X+Delta.X,Cur);

        ReDrawLine := True;

        DrawView;

        Save := True

       END

       ELSE IF Delta.Y+Cursor.Y < Anchor^.Count-1 THEN BEGIN

        SetLine(Cur,GetLine(Cur)+GetLine(Nxt));

        DeleteLine(Nxt);

        Redraw := True;

        Save := True

      END;

      HandleDelKey := True;

    END;



  FUNCTION HandleInsKey: BOOLEAN;

    BEGIN

      IF overwrite

       THEN NormalCursor

       ELSE BlockCursor;

      overwrite := NOT overwrite;

      HandleInsKey := True;

    END;



  FUNCTION HandleCtrlLeft: BOOLEAN;

    VAR

      WorkStr: STRING;

      Found: BOOLEAN;

      I,J: INTEGER;

    BEGIN

      I := Delta.X+Cursor.X;

      J := Delta.Y+Cursor.Y;

      WorkStr := PStr(Anchor^.At(J))^.GetStr;

      IF I > Length(WorkStr) THEN I := Length(WorkStr);

      REPEAT

        IF I > 0 THEN Dec(I);

        WHILE (I <= 0) AND (J > 0) DO BEGIN

          Dec(J);

          WorkStr := PStr(Anchor^.At(J))^.GetStr;

          I := Length(WorkStr)-1;

        END;

        Found := WorkStr[I+1] <> ' ';

      UNTIL Found OR ((I = 0) AND (J = 0));

      IF Found AND ((I>0) OR (J>0))

       THEN REPEAT

        Found := WorkStr[I+1] = ' ';

        IF Found

         THEN Inc(I)

         ELSE IF I > 0 THEN Dec(I);

       UNTIL Found OR (I = 0)

       ELSE Found := WorkStr[I+1] <> ' ';

      IF Found OR (I=0) THEN BEGIN

        HandleShiftKeys(Blk.B, Blk.A, I, J);

        SetCursor(I-Delta.X,J-Delta.Y);

      END;

      HandleCtrlLeft := True;

    END;



  FUNCTION HandleCtrlRight: BOOLEAN;

    VAR

      WorkStr: STRING;

      Found: BOOLEAN;

      I,J: INTEGER;

    BEGIN

      I := Delta.X+Cursor.X;

      J := Delta.Y+Cursor.Y;

      WorkStr := GetLine(J);

      Found := I > Length(WorkStr);

      IF NOT Found THEN REPEAT

        IF I < Length(WorkStr)-1

         THEN Inc(I)

         ELSE Found := True;

        IF NOT Found AND (Length(WorkStr) > 0) AND (J < Limit.Y)

         THEN Found := WorkStr[I+1] = ' ';

      UNTIL Found OR (J >= Limit.Y);

      IF Found

       THEN BEGIN

        Found := False;

        REPEAT

          IF I < Length(WorkStr)-1

           THEN Inc(I)

           ELSE BEGIN

            IF J < Anchor^.Count-1 THEN BEGIN

              Inc(J);

              I := 0;

              WorkStr := PStr(Anchor^.At(J))^.GetStr;

             END

             ELSE BEGIN

              HandleCtrlRight := True;

              Exit;

           END;

          END;

          IF Length(WorkStr) > 0

           THEN Found := WorkStr[I+1] <> ' ';

        UNTIL Found OR (J >= Limit.Y)

       END

       ELSE Found := WorkStr[I+1] <> ' ';

      IF Found OR (I=0) THEN BEGIN

        HandleShiftKeys(Blk.A, Blk.B, I, J);

        SetCursor(I-Delta.X,J-Delta.Y);

      END;

      HandleCtrlRight := True;

    END;



  FUNCTION HandleTabKey: BOOLEAN;

    VAR S, SC: STRING;

        I: INTEGER;

    BEGIN

      IF (Anchor^.Count > 1) AND (Cursor.Y+Delta.Y > 0) THEN BEGIN

        S := GetLine(Prv);

        I := Cursor.X+Delta.X+1;

        WHILE (I<=Length(S)) AND (S[I] <> ' ') DO BEGIN

          InsertChar(' ', I-1, Cur);

          Inc(I);

        END;

        WHILE (I<=Length(S)) AND (S[I] = ' ') DO BEGIN

          InsertChar(' ', I-1, Cur);

          Inc(I);

        END;

        SetCursor(I-Delta.X-1, Cursor.Y);

        ReDrawLine := True;

        DrawView;

      END;

      Save := True;

      HandleTabKey := True;

    END;



  FUNCTION HandleReturnKey: BOOLEAN;

    BEGIN

      IF NOT overwrite

       THEN BEGIN

        CutLineAt(Cur, Delta.X+Cursor.X);

        ScrollTo(0, Delta.Y);

        IF GetIndent(Nxt) >= Delta.X+Cursor.X

         THEN SetCursor(-Delta.X, Cursor.Y+1)

         ELSE SetCursor(GetIndent(Nxt)-Delta.X, Cursor.Y+1);

        Redraw := True;

        Save := True

       END

       ELSE IF Delta.Y+Cursor.Y < Anchor^.Count-1 THEN BEGIN

        ScrollTo(0, Delta.Y);

        SetCursor(GetIndent(Nxt), Cursor.Y+1);

      END;

      HandleReturnKey := True;

    END;



  FUNCTION HandleCtrlT: BOOLEAN;

    VAR S: STRING;

        Len, I, J: INTEGER;

    BEGIN

      Len := Length(GetLine(Cur));

      I := Cursor.X+Delta.X+1;

      IF (I>Len) AND (Delta.Y+Cursor.Y<Limit.Y-1) THEN BEGIN

        CombineLines(Cur);

        S := GetLine(Cur);

        FOR J := Len TO I DO BEGIN

          Insert(' ',S,Len+1);

          SetLine(Cur, S);

        END;

       END

       ELSE BEGIN

        S := GetLine(Cur);

        WHILE (I<=Length(S)) AND (S[I] <> ' ') DO Delete(S,I,1);

      END;

      WHILE (I<=Length(S)) AND (S[I] = ' ') DO Delete(S,I,1);

      SetLine(Cur, S);

      Redraw := True;

      Save := True;

      HandleCtrlT := True;

    END;



  FUNCTION HandleCtrlY: BOOLEAN;

    BEGIN

      IF Delta.Y+Cursor.Y < Anchor^.Count-1

       THEN DeleteLine(Cur)

       ELSE SetLine(Cur, '');

      Cursor.X := -Delta.X;

      Redraw := True;

      Save := True;

      HandleCtrlY := True;

    END;



  FUNCTION HandleChars: BOOLEAN;

    VAR WorkStr: STRING;

    BEGIN

      IF (Length(GetLine(Cur)) < LineLen(Cur)) OR (Mode(Cur) = Block)

       THEN BEGIN

        WorkStr := GetLine(Cur);

        WHILE Length(WorkStr) < Delta.X+Cursor.X DO WorkStr := WorkStr + ' ';

        IF overwrite AND (Length(WorkStr) > Delta.X+Cursor.X)

         THEN WorkStr[Delta.X+Cursor.X+1] := Event.CharCode

         ELSE Insert(Event.CharCode, WorkStr, Delta.X+Cursor.X+1);

        SetLine(Cur, WorkStr);

        Inc(Cursor.X);

        ReDraw := True;

       END

       ELSE Write(^G);

      IF Length(GetLine(Cur)) > LineLen(Cur) THEN BEGIN

        Redraw := True;

        ReformatParagraph;

      END;

      Save := True;

      HandleChars := True;

    END;



  FUNCTION HandleNormalKeys: BOOLEAN;

    BEGIN

      IF LastChar = ^K

       THEN HandleNormalKeys := HandleCtrlK

       ELSE IF LastChar = ^Q

        THEN HandleNormalKeys := HandleCtrlQ

        ELSE BEGIN

         CASE Event.CharCode OF

           #32..#255: HandleNormalKeys := HandleChars;

           #13:       HandleNormalKeys := HandleReturnKey;

           ^B:        BEGIN

                        ReformatParagraph;

                        Redraw := True;

                        Save := True;

                        HandleNormalKeys := True;

                      END;

           ^K,^Q:     HandleNormalKeys := True;

           ^T:        HandleNormalKeys := HandleCtrlT;

           ^Y:        HandleNormalKeys := HandleCtrlY;

           ELSE       HandleNormalKeys := False;

         END;

         LastChar := Event.CharCode;

      END;

    END;



  FUNCTION HandleOneKey: BOOLEAN;

    BEGIN

      HandleOneKey := True;

      WITH Cursor DO CASE Event.KeyCode OF

        kbCtrlPgUp:  SetCursor(X, -Delta.Y);

        kbCtrlPgDn:  SetCursor(X, Limit.Y-Delta.Y-1);

        kbBack:      HandleOneKey := HandleBackkey;

        kbDel:       HandleOneKey := HandleDelKey;

        kbIns:       HandleOneKey := HandleInsKey;

        kbTab:       HandleOneKey := HandleTabKey;

        ELSE         HandleOneKey := HandleNormalKeys;

      END;

      WasShift := False;

      ScrollTo(Delta.X, Delta.Y);

    END;



  FUNCTION HandleOneSpecKey: BOOLEAN;

    BEGIN

      HandleOneSpecKey := True;

      WITH Cursor DO CASE Event.KeyCode OF

        kbRight:     HandleOneSpecKey := HandleRightKey;

        kbLeft:      HandleOneSpecKey := HandleLeftKey;

        kbDown:      HandleOneSpecKey := HandleDownKey;

        kbUp:        HandleOneSpecKey := HandleUpKey;

        kbHome:      HandleOneSpecKey := HandleHomeKey;

        kbEnd:       HandleOneSpecKey := HandleEndKey;

        kbPgUp:      HandleOneSpecKey := HandlePgUpKey;

        kbPgDn:      HandleOneSpecKey := HandlePgDnKey;

        kbCtrlLeft:  HandleOneSpecKey := HandleCtrlLeft;

        kbCtrlRight: HandleOneSpecKey := HandleCtrlRight;

      END;

      WasShift := ShiftState;

      ScrollTo(Delta.X, Delta.Y);

    END;



  FUNCTION HandleAKey: BOOLEAN;

    BEGIN

      HandleAKey := True;

      WITH Cursor DO CASE Event.KeyCode OF

        kbRight,

        kbLeft,

        kbDown,

        kbUp,

        kbPgUp,

        kbPgDn,

        kbCtrlLeft,

        kbCtrlRight,

        kbHome,

        kbEnd:       HandleAKey := HandleOneSpecKey;

        kbBack,

        kbDel,

        kbIns,

        kbCtrlPgUp,

        kbCtrlPgDn,

        kbTab:       HandleAKey := HandleOneKey;

        ELSE         HandleAKey := HandleOneKey;

      END;

      ScrollTo(Delta.X, Delta.Y);

    END;



{ Handle Mouse / Keyboard / BroadCast ------------------------------- }

  FUNCTION HandleMouse: BOOLEAN;

    VAR P: TPoint;

    BEGIN

      MakeLocal(Event.Where, P);

      IF P.Y+Delta.Y < Anchor^.Count THEN SetCursor(P.X, P.Y);

      IF MouseEvent(Event, evMouseAuto+evMouseMove) THEN BEGIN

        IF NOT ShiftState THEN StartMark(P);

        WasShift := True;

        REPEAT

          MakeLocal(Event.Where, P);

          EndMark(P);

          ReDraw := True;

          CheckForRedraw;

        UNTIL NOT MouseEvent(Event, evMouseAuto+evMouseMove);

      END;

      HandleMouse := True;

    END;



  FUNCTION HandleBroadCast: BOOLEAN;

    VAR

      Len: INTEGER;

      P: PCollection;

    BEGIN

      IF ((Event.Command = cmEdSendText+WdwNo) OR

          (Event.Command = cmBase+cmEdSend) ) AND

         (Event.InfoPtr <> Nil)

       THEN BEGIN

        P := PCollection(Event.InfoPtr);

        PutBlock(P,Cursor);

        Len := Length(PStr(P^.At(P^.Count-1))^.GetStr);

        IF P^.Count = 1

         THEN Blk.Assign(Cursor.X+Delta.X, Cursor.Y+Delta.Y, Cursor.X+Delta.X+Len, Cursor.Y+Delta.Y)

         ELSE Blk.Assign(Cursor.X+Delta.X, Cursor.Y+Delta.Y, Len, Cursor.Y+Delta.Y+P^.Count-1);

        Dispose(P, Done);

        ReDraw := True;

        HandleBroadCast := True;

       END

       ELSE HandleBroadCast := False;

    END;

  Var P: pCollection;



  BEGIN {TEdit.HandleEvent}

    TScroller.HandleEvent(Event);

    ShiftState := Mem[$40:$17] AND (kbRightShift+kbLeftShift) <> 0;

    Redraw := False;

    IF NOT ShiftState AND

       ((Blk.A.Y>Blk.B.Y) OR ((Blk.A.Y=Blk.B.Y) AND (Blk.A.X>Blk.B.X)))

     THEN BEGIN

      T := Blk.B;

      Blk.B := Blk.A;

      Blk.A := T;

    END;

    If (Event.What = evKeyDown) and

       (Event.KeyCode = kbESC) then

    Begin

      Event.What := evCommand;

      Event.Command := cmClose;

      TScroller.HandleEvent(Event)

    End;

    CASE Event.What OF

      evMouseDown: IF NOT HandleMouse THEN EXIT;

      evKeyDown:   IF NOT HandleAKey THEN EXIT;

      evBroadCast: IF NOT HandleBroadCast THEN

                   Begin

                     If Event.Command = edBase + cmEdSave then

                     IF Save THEN BEGIN

                       IF NOT BlockDefined THEN Blk.Assign(0,0,Limit.X,Limit.Y-1);

                       GetBlock(P);

                       Event.InfoPtr := P;

                       Event.Command := cmBase + cmEdSave;

                       TopView^.HandleEvent(Event);

                       Dispose(P, Done);

                       Save := False;

                       Blk.Assign(0,0,0,0);

                     END

                     Else ClearEvent(Event);



                     If Event.Command = edBase + cmEdSaveAs then

                     Begin

                       IF NOT BlockDefined THEN Blk.Assign(0,0,Limit.X,Limit.Y-1);

                       GetBlock(P);

                       Event.InfoPtr := P;

                       Event.Command := cmBase + cmEdSaveAs;

                       TopView^.HandleEvent(Event);

                       Dispose(P, Done);

                       Save := False;

                       Blk.Assign(0,0,0,0);

                     End;



                     Exit

                   End;

      ELSE Exit;

    END;

    ClearEvent(Event);

    CheckForRedraw;

  END; {TEdit.HandleEvent}



FUNCTION TEdit.LineLen(LineNo: INTEGER): INTEGER;

  BEGIN

    LineLen := TheLineLen;

  END;



FUNCTION TEdit.Mode(LineNo: INTEGER): TextModes;

  BEGIN

    Mode := Block;

  END;



DESTRUCTOR TEdit.Done;

  VAR Event: TEvent;

      Info: ^String;

  BEGIN

    IF Save THEN BEGIN

        Event.What := evBroadCast;

        Event.Command := cmEdSaveAndExit+cmBase;

        Event.InfoPtr := Anchor;

        TopView^.HandleEvent(Event);

      END;

    Dispose(Anchor, Done);

    TScroller.Done;

  END;



(*Procedure TEditorWindow.SetState(AState: Word; Enable: Boolean);

Var R: TRect;

Begin

  TWindow.SetState(AState, Enable);

  

  If AState And sfSelected = 1 then

  Begin

    Write(chr(7));

    DeskTop^.GetExtent(R);

    R.Assign(R.A.X, R.A.Y, R.B.X, R.B.Y-2);

    DeskTop^.Cascade(R)

  End

End;*)



CONSTRUCTOR TEditorWindow.Init( Bounds: TRect;

                          WindowNo: WORD;

                          WindowName: STRING;

                          Anchor: PCollection;

                          cmEdBase: WORD);

  VAR

    Scroller: PEdit;

    VScrollBar,HScrollBar: PScrollBar;

  BEGIN

    IF WindowName = ''

     THEN TWindow.Init(Bounds, 'Editor', WindowNo)

     ELSE TWindow.Init(Bounds, WindowName, WindowNo);



    EventMask := EventMask OR evBroadCast;



    GetExtent(Bounds);

    WITH Bounds DO Assign(B.X-1,A.Y+1, B.X, B.Y-1);

    VScrollBar := New(PScrollBar, Init(Bounds));

    WITH VScrollBar^ DO Options := Options + ofPostProcess;



    GetExtent(Bounds);

    WITH Bounds DO Assign(A.X+2,B.Y-1, B.X-2, B.Y);

    HScrollBar := New(PScrollBar, Init(Bounds));

    WITH VScrollBar^ DO Options := Options + ofPostProcess;



    IF Anchor = Nil THEN Anchor := New(PCollection, Init (20, 1));

    IF Anchor^.Count = 0 THEN Anchor^.Insert( New( PStr, Init('')));



    GetExtent(Bounds);

    Bounds.Grow(-1,-1);

    Scroller := New( PEdit, Init(Bounds, HScrollBar, VScrollBar,

                     Anchor, cmEdBase, WindowNo));

    WITH Scroller^ DO Options := Options OR ofFramed;

    Scroller^.SetLimit(250, Anchor^.Count);

    Insert(Scroller);

    Editor := Scroller;

    Insert(VScrollBar);

    Insert(HScrollBar);



    Options := Options OR ofTileable;

  END;



Procedure TEditorWindow.NewTitle(Titel: String);

Begin

  If Title <> Nil then

    Title^ := Titel

  Else

  Begin

    New(Title);

    Title^ := Titel

  End;

  TWindow.ReDraw

End;



procedure TEditorWindow.Close;

begin

  if Editor^.IsClipboard then Hide else

  Begin

    Dec(WNo);

    TWindow.Close

  End

end;







CONSTRUCTOR TLineEditor.Init( Bounds: TRect;

                              WindowNo: WORD;

                              WindowName: STRING;

                              Anchor: PCollection;

                              ALineLen: INTEGER;

                              cmEdBase: WORD);

  VAR

    Scroller: PScroller;

    VScrollBar,HScrollBar: PScrollBar;

  BEGIN

    IF WindowName = ''

     THEN TWindow.Init(Bounds, 'Editor', WindowNo)

     ELSE TWindow.Init(Bounds, WindowName, WindowNo);



    EventMask := EventMask OR evBroadCast;



    GetExtent(Bounds);

    WITH Bounds DO Assign(B.X-1,A.Y+1, B.X, B.Y-1);

    VScrollBar := New(PScrollBar, Init(Bounds));

    WITH VScrollBar^ DO Options := Options + ofPostProcess;



    GetExtent(Bounds);

    WITH Bounds DO Assign(A.X+2,B.Y-1, B.X-2, B.Y);

    HScrollBar := New(PScrollBar, Init(Bounds));

    WITH VScrollBar^ DO Options := Options + ofPostProcess;



    GetExtent(Bounds);

    Bounds.Grow(-1,-1);

    Scroller := New( PLineEdit, Init(Bounds, HScrollBar, VScrollBar,

                     Anchor, ALineLen, cmEdBase, WindowNo));

    WITH Scroller^ DO Options := Options OR ofFramed;

    Scroller^.SetLimit(250, Anchor^.Count);

    Insert(Scroller);

    Insert(VScrollBar);

    Insert(HScrollBar);



    Options := Options OR ofTileable;

  END;



FUNCTION TMemoEdit.GetPalette: PPalette;

  BEGIN

    GetPalette := @CMemoEdit;

  END;



DESTRUCTOR TMemoEdit.Done;

  VAR Event: TEvent;

  BEGIN

    Dispose(Anchor, Done);

    TScroller.Done;

  END;



Destructor TMyMemoEdit.Done;

Begin

  TScroller.Done

End;









CONSTRUCTOR TLineEdit.Init( Bounds: TRect;

                            AHScrollBar,AVScrollBar: PScrollBar;

                            AnAnchor: PCollection;

                            ALineLen: INTEGER;

                            cmEdBase, WNo: WORD);

  BEGIN

    TEdit.Init(Bounds, AHScrollBar, AVScrollBar, AnAnchor, cmEdBase, WNo);

    TheLineLen := ALineLen;

  END;



FUNCTION TLineEdit.Mode (LineNo: INTEGER): TextModes;

  BEGIN

    Mode := StopAtEnd;

  END;



PROCEDURE RegisterEditor;

  BEGIN

    RegisterType(REdit);

    RegisterType(RMemoEdit);

    RegisterType(RMyMemoEdit);

    RegisterType(RLineEdit);

    RegisterType(REditor);

    RegisterType(RLineEditor);

  END;



END.

