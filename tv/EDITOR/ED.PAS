{$V-}
USES
  Dos,
  App, Drivers, Menus, Dialogs, Objects, StdDlg, Views,
  Colors, DosTools, Editor, StackDmp, Strings, StrTools;

CONST
  cmTile       = 101;
  cmCasCade    = 102;
  cmColors     = 103;
  edBase       = 1200;
  WNo: INTEGER = 1;

TYPE
  PMain = ^TMain;
  TMain = OBJECT (TApplication)
    Editor: ARRAY[1..9] OF RECORD
      Ed: PEditor;
      Name: STRING;
    END;
    CONSTRUCTOR Init;
    PROCEDURE NewEditor(Name: STRING);
    PROCEDURE InitMenuBar; VIRTUAL;
    PROCEDURE InitStatusLine; VIRTUAL;
    FUNCTION GetFile(Arg: STRING): PCollection;
    PROCEDURE SendFile(WindowNo, cmBase: WORD);
    PROCEDURE SaveFile ( WindowNo: WORD; P: PCollection );
    PROCEDURE HandleEvent ( VAR Event: TEvent ); VIRTUAL;
  END;

CONSTRUCTOR TMain.Init;
  VAR
    Dummy: STRING;
    Search: SearchRec;
    I: INTEGER;
    R: TRect;
    F: Text;
  BEGIN
    TApplication.Init;
    NewEditor('');
    I := 1;
    WNo := 2;
    WHILE I <= ParamCount DO BEGIN
      FindFirst(ParamStr(I), anyFile, Search);
      IF DosError <> 0 THEN BEGIN
        FindFirst(ParamStr(I)+'.*', anyFile, Search);
        IF DosError <> 0 THEN BEGIN
          Assign(F, ParamStr(I));
          Rewrite(F);
          Close(F);
          FindFirst(ParamStr(I), anyFile, Search);
        END;
      END;
      WHILE DosError = 0 DO BEGIN
        IF Search.Attr OR Archive = Archive THEN BEGIN
          NewEditor(LoadPath(ParamStr(I))+Search.Name);
        END;
        FindNext(Search);
      END;
      Inc(I);
    END;
    GetExtent(R);
    R.Assign(R.A.X, R.A.Y, R.B.X-2, R.B.Y-3);
    DeskTop^.Tile(R);
  END;

FUNCTION ReadFile(Name: STRING): PCollection;
  VAR
    P: PCollection;
    ALine: STRING;
    AFile: TEXT;
  BEGIN
    New(P, Init(20,1));
    Assign(AFile, Name);
    {$I-}Reset(AFile);{$I+}
    IF IOResult = 0 THEN BEGIN
      WHILE NOT EOF(AFile) DO BEGIN
        ReadLn(AFile, ALine);
        P^.AtInsert( P^.Count, New(PStr, Init ( ALine )));
      END;
      Close(AFile);
    END;
    ReadFile := P;
  END;

PROCEDURE TMain.NewEditor(Name: STRING);
  VAR
    R: TRect;
    Dir, FName, Ext: STRING;
  BEGIN
    R.Assign(0,0,79,22);
    FSplit(Name, Dir, FName, Ext);
    Ext := DownStr(Ext);
    IF Name = ''
      THEN Editor[WNo].Ed := New(PEditor, Init(R, WNo, 'Clipboard', Nil,
                                               edBase+10*WNo))
     ELSE IF (Ext = '.txt') THEN BEGIN 
       Editor[WNo].Ed := New(PEditor, Init(R, WNo, Name, ReadFile(Name),
                                           edBase+10*WNo));
       Editor[WNo].Name := Name;
      END
      ELSE IF (Ext<>'.exe') AND (Ext<>'.com') AND (Ext<>'.bak') THEN BEGIN
        Editor[WNo].Ed := New(PLineEditor, Init(R, WNo, Name, ReadFile(Name),
                                                128, edBase+10*WNo));
        Editor[WNo].Name := Name;
       END
       ELSE Exit;
    DeskTop^.Insert(Editor[WNo].Ed);
    Inc(WNo);
  END;

procedure TMain.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', 100, NewMenu(
      NewItem('~O~pen...', 'F3', kbF3, cmFileOpen, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext, nil))),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      NewItem('~R~esize/move','Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
      NewItem('~N~ext', 'F6', kbF6, cmNext, hcNoContext,
      NewItem('~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext,
      NewItem('~T~ile', '', kbNoKey, cmTile, hcNoContext,
      NewItem('C~o~lors', '', kbNoKey, cmColors, hcNoContext,
      NewItem('C~a~scade', '', kbNoKey, cmCascade, hcNoContext, nil)))))))), nil)))));
end;

PROCEDURE TMain.InitStatusLine;
  VAR R: TRect;
  BEGIN
    GetExtent(R);
    R.A.Y := R.B.Y-1;
    StatusLine := New(PStatusLine, Init(R,
      NewStatusDef(hcDragging, hcDragging,
        NewStatusKey('~'#24#25#26#27'~ Move', kbNoKey, 0,
        NewStatusKey('~Shift-'#24#25#26#27'~ Resize', kbNoKey, 0,
        NewStatusKey('~'#17#196#217'~ Done', kbNoKey, 0,
        NewStatusKey('~Esc~ Cancel', kbNoKey, 0, nil)))),
      NewStatusDef(0,$FFFF,
        NewStatusKey('~Alt-X~ Exit',    kbAltX, cmQuit,
      Nil),
    Nil)
    )));
  END;

PROCEDURE TMain.SaveFile(WindowNo: WORD; P: PCollection);
  VAR
    TF: TEXT;
    I: INTEGER;
  BEGIN
    IF WindowNo > 1 THEN BEGIN
      Assign(TF,Editor[WindowNo].Name);
      Rewrite(TF);
      FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);
      Close(TF);
    END;
  END;

FUNCTION TMain.GetFile(Arg: STRING): PCollection;
  VAR
    D: PFileDialog;
    FileName: PathStr;
    P: PCollection;
  BEGIN
    D := PFileDialog(ValidView(New(PFileDialog, Init(Arg, 'Laden',
                                                'File', 3, 12))));
    IF (D <> Nil) AND (Desktop^.ExecView(D) <> cmCancel) THEN BEGIN
      D^.GetFileName(FileName);
      GetFile := ReadFile(FileName);
     END
     ELSE GetFile := Nil;
  END;

PROCEDURE TMain.SendFile(WindowNo, cmBase: WORD);
  VAR Event: TEvent;
  BEGIN
    Event.What := evBroadCast;
    Event.Command := cmBase+WindowNo*10+cmEdSend;
    Event.InfoPtr := GetFile('*.*');
    TopView ^.HandleEvent(Event);
  END;

PROCEDURE TMain.HandleEvent( VAR Event: TEvent );
  VAR R: TRect;

  FUNCTION HandleBroadCast: BOOLEAN;
    VAR EDCommand, WindowNo: WORD;
    BEGIN
      IF Event.Command DIV 100 * 100 = edBase THEN BEGIN
        HandleBroadCast := True;
        WindowNo := Event.Command MOD 100 DIV 10;
        EDCommand := Event.Command MOD 10;
        CASE EDCommand OF
          cmEdSave: SaveFile(WindowNo, Event.InfoPtr);
          cmEdSaveAndExit: SaveFile(WindowNo, Event.InfoPtr);
          cmEdRead: SendFile(WindowNo,edBase);
          cmEdExit: Dispose(Editor[WindowNo].Ed, Done);
          ELSE HandleBroadCast := False;
        END;
       END
       ELSE HandleBroadCast := False;
    END;

procedure FileOpen;
var
  D: PFileDialog;
  FileName: PathStr;
  W: PWindow;
begin
  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'Open a File',
    '~N~ame', fdOpenButton, 100))));
  if D <> nil then
  begin
    if Desktop^.ExecView(D) <> cmCancel then
    begin
      D^.GetFileName(FileName);
      NewEditor(FileName);
    end;
    Dispose(D, Done);
  end;
end;

  BEGIN
    TApplication.HandleEvent( Event );
    CASE Event.What OF
      evCommand: BEGIN
        GetExtent(R);
        R.Assign(R.A.X, R.A.Y, R.B.X-2, R.B.Y-3);
        CASE Event.Command OF
          cmTile: DeskTop^.Tile(R);
          cmCascade: DeskTop^.Cascade(R);
          cmFileOpen: FileOpen;
          cmColors: ColorDialog;
        END;
      END;
      evBroadCast: IF NOT HandleBroadCast THEN Exit;
      ELSE Exit;
    END;
    ClearEvent(Event);
  END;

VAR
  Main: TMain;

BEGIN
  Main.Init;
  Main.Run;
  Main.Done;
END.
