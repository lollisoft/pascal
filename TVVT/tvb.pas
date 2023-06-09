Program TVB;
(*{$DEFINE DEMO}*)

{$M 65520,8192,655360}
{$X+,S+,O+,F+}
{$G-}
{$W+}
Uses App,
     TVBCmds,
     HelpCmds,
     HelpFile,
     Crt,
     Dos,
     Objects,
     Memory,
     GadGets,
     Drivers,
     Views,
     Menus,
     Dialogs,
     MsgBox,
     Editors,
   (* F�r Editor: *)
     Colorsel,
     Colors,
     Editor,
     Strings,
     StrTools,
     (*     Vid, *)

     CursCont,
     StdDlg,

(* Serienbrief: *)
     Ser1;



Const

    FileName           : String12 = '*.*';


Type

  ColorFiletyp         = File of TPalette;

  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Heap        : PHeapView;
    Editor: ARRAY[1..9] OF RECORD
      Ed: PEditorWindow;
      Name: STRING;
    END;
    Constructor Init;
    Destructor  Done;                           Virtual;
    Procedure   Idle;                           Virtual;
    
    Procedure   InitClipBoard;
    Procedure   FileOpen;
    PROCEDURE   NewEditor(Name: STRING);
    FUNCTION    GetFile(Arg: STRING): PCollection;
    PROCEDURE   SendFile(WindowNo, cmBase: WORD);
    PROCEDURE   SaveFile (WindowNo: WORD; P: PCollection );
    PROCEDURE   SaveFileAs (WindowNo: WORD; P: PCollection );

    procedure   LoadDesktop(var S: TStream);
    procedure   StoreDesktop(var S: TStream);

    Function    GetPalette: PPalette;           Virtual;
    Procedure   HandleEvent(Var Event: TEvent); Virtual;
    Procedure   InitMenuBar;                    Virtual;
    Procedure   InitStatusLine;                 Virtual;
    Procedure   OutOfMemory;                    Virtual;
  END;
    



var
    
    Laufwerk     : String[12];
    ColorFile    : ColorFiletyp;
    ColorDat     : PPalette;
    S            : TStream;


(* Editor: *)

    EDCommand, WindowNo: WORD;
    ClipWindow: PEditorWindow;

function CalcHelpName: PathStr;
var
  EXEName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
begin
  if Lo(DosVersion) >= 3 then EXEName := ParamStr(0)
  else EXEName := FSearch('TVDV.EXE', GetEnv('PATH'));
  FSplit(EXEName, Dir, Name, Ext);
  if Dir[Length(Dir)] = '\' then Dec(Dir[0]);
  CalcHelpName := FSearch('TVDVHELP.HLP', Dir);
end;

PROCEDURE ProgramminformationDialog;
  VAR
    R: tRect;
    View: pView;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (13, 2, 62, 20);
  Dialog := New (pDialog, Init (R, 'Programminformation'));

{$IFDEF DEMO}

  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVB f�r DOS - Testversion !')));

{$ELSE}

  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVB f�r DOS - Vollversion !')));

{$ENDIF}

  R.Assign (9, 4, 39, 5);
  Dialog^.Insert (New (pStaticText, Init (R, 'Editor mit Serienbrieffunktion')));
 
  R.Assign (12, 7, 34, 8);
  Dialog^.Insert (New (pStaticText, Init (R, 'Copyright (c) 1994 bei')));
 
  R.Assign (14, 10, 28, 11);
  Dialog^.Insert (New (pStaticText, Init (R, 'Lothar Behrens')));
 
  R.Assign (14, 11, 26, 12);
  Dialog^.Insert (New (pStaticText, Init (R, 'Quellenweg 1')));
 
  R.Assign (14, 12, 34, 13);
  Dialog^.Insert (New (pStaticText, Init (R, 'W-74889 Sinsheim Ad.')));
 
  R.Assign (14, 13, 26, 14);
  Dialog^.Insert (New (pStaticText, Init (R, '(07261) 4671')));
 
  R.Assign (18, 15, 28, 17);
  Dialog^.Insert (New (pButton, Init (R, ' ~O~K ', 10, 1)));
 
  Dialog^.SelectNext (FALSE);
 
  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;


procedure TDateiverApp.LoadDesktop(var S: TStream);
var
  P: PView;

procedure CloseView(P: PView); far;
begin
  Message(P, evCommand, cmClose, nil);
end;

begin
  if Desktop^.Valid(cmClose) then
  begin
    Desktop^.ForEach(@CloseView); { Clear the desktop }
    repeat
      P := PView(S.Get);
      Desktop^.InsertBefore(ValidView(P), Desktop^.Last);
    until P = nil;
  end;
end;

procedure TDateiverApp.StoreDesktop(var S: TStream);

procedure WriteView(P: PView); far;
begin
  if P <> Desktop^.Last then S.Put(P);
end;
begin
  Desktop^.ForEach(@WriteView);
  S.Put(nil);
end;



FUNCTION TDateiverApp.GetPalette: PPalette;
CONST
  CNewColor = CColor + CHelpColor;
  CNewBW    = CBlackWhite + CHelpBlackWhite;
  CNewMono  = CMonochrome + CHelpMonochrome;
  P : ARRAY [apColor..apMonochrome] OF String[Length(CNewColor)] =
              (CNewColor, CNewBW, CNewMono);
BEGIN
  GetPalette := @P[AppPalette];
END;


Constructor TDateiverApp.Init;
var
  H: Word;
  R: TRect;
  Dummy: STRING;
  Search: SearchRec;
  I: INTEGER;
  F: Text;
  S: String;
BEGIN
  RegisterHelpFile;
  RegisterEditor;
  RegisterColorsel;
  RegisterObjects;
  RegisterViews;
  RegisterMenus;
  RegisterDialogs;
  RegisterApp;

  TApplication.Init;
  
  ColorDat := Application^.GetPalette;
  Assign(ColorFile, GetFName + '.COL');
  If (LastMode <> Mono) and
     (LastMode <> BW80) then
  Begin
    {$I-}
    Reset(ColorFile);
    If IOResult = 0 then
    Begin
      Read(ColorFile, S);
      Close(ColorFile);
      ColorDat^ := S;
      DeskTop^.ReDraw
    End;
    {$I+}
  End;




  ASerienBriefData := New(PSerienBriefData, Init);
  New(AMemo);

  GetExtent(R);
  R.A.X := R.B.X - 9;
  R.B.Y := R.A.Y + 1;
  Heap := New(PHeapView, Init(R));
  Insert(Heap);

  ProgrammInformationDialog;

  InitClipBoard;
  I := 1;

  DisableCommands([cmSave, cmSaveAs])

END;

Destructor TDateiverApp.Done;
BEGIN
  ReWrite(ColorFile);
  Write(ColorFile, ColorDat^);
  Close(ColorFile);
  TApplication.Done
END;

(* F�r Editor: *)

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


PROCEDURE TDateiverApp.NewEditor(Name: STRING);
  VAR
    R: TRect;
    Dir, FName, Ext: STRING;
  BEGIN
    Inc(WNo);

    R.Assign(0,0,80,23);
    FSplit(Name, Dir, FName, Ext);
    Ext := DownStr(Ext);
    IF Name = ''
      THEN Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,
                                               edBase+10*WNo))
     ELSE IF (Ext = '.txt') THEN BEGIN 
       Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, Name, ReadFile(Name),
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
  END;

PROCEDURE TDateiverApp.InitClipBoard;
  VAR
    R: TRect;
    Dir, FName, Ext: STRING;
    Name: String;
  BEGIN
    Name := '';
    GetExtent(R);
    Dec(R.B.Y, 2);
    FSplit(Name, Dir, FName, Ext);
    Ext := DownStr(Ext);

    Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,
                                               edBase+10*WNo));

    ClipWindow := Editor[WNo].Ed;
    if ClipWindow <> nil then
      Clipboard := ClipWindow^.Editor;

    ClipWindow^.Close;
    DeskTop^.Insert(Editor[WNo].Ed);

  END;


PROCEDURE TDateiverApp.SaveFile(WindowNo: WORD; P: PCollection);
  VAR
    TF: TEXT;
    I: INTEGER;
  BEGIN
    IF WindowNo > 1 THEN BEGIN
      Assign(TF,Editor[WindowNo].Name);
      Rewrite(TF);
      Delay(2000);
      FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);
      Close(TF);
    END;
  END;

PROCEDURE TDateiverApp.SaveFileAs(WindowNo: WORD; P: PCollection);
  VAR
    TF: TEXT;
    I: INTEGER;
    D: PFileDialog;
    FileName: PathStr;

  BEGIN
    IF WindowNo > 1 THEN BEGIN
      D := PFileDialog(ValidView(New(PFileDialog, Init(Editor[WindowNo].Name, 'Datei speichern als',
        '~N~ame', fdCloseButton, 100))));
      If D <> Nil then
      Begin
        If Desktop^.ExecView(D) <> cmCancel then
        Begin
          D^.GetFileName(FileName);
          Editor[WindowNo].Name := FileName;
          Editor[WindowNo].Ed^.NewTitle(FileName);

          Assign(TF,Editor[WindowNo].Name);
          Rewrite(TF);
          FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);
          Close(TF);
        End;
        Dispose(D, Done)
      End;
    END;
  END;



FUNCTION TDateiverApp.GetFile(Arg: STRING): PCollection;
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

PROCEDURE TDateiverApp.SendFile(WindowNo, cmBase: WORD);
  VAR Event: TEvent;
  BEGIN
    Event.What := evBroadCast;
    Event.Command := cmBase+WindowNo*10+cmEdSend;
    Event.InfoPtr := GetFile('*.*');
    TopView ^.HandleEvent(Event);
  END;

procedure TDateiverApp.FileOpen;
var
  D: PFileDialog;
  FileName: PathStr;
  W: PWindow;
begin
  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', '�ffnen einer Datei',
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


(*****************************************************************)

Procedure TDateiverApp.HandleEvent(Var Event: TEvent);
Var R: TRect;

(* F�r Editor: *)

  FUNCTION HandleBroadCast: BOOLEAN;
    BEGIN
      IF Event.Command DIV 100 * 100 = edBase THEN BEGIN
        HandleBroadCast := True;
        WindowNo := Event.Command MOD 100 DIV 10;
        EDCommand := Event.Command MOD 10;
        CASE EDCommand OF
          cmEdSave: SaveFile(WindowNo, Event.InfoPtr);
          cmEdSaveAs: SaveFileAs(WindowNo, Event.InfoPtr);
          cmEdSaveAndExit: SaveFile(WindowNo, Event.InfoPtr);
          cmEdRead: SendFile(WindowNo,edBase);
          cmEdExit: Dispose(Editor[WindowNo].Ed, Done);
          ELSE HandleBroadCast := False;
        END;
        CASE Event.Command OF
          edBase + cmEdSave: SaveFile(WindowNo, Event.InfoPtr);
          edBase + cmEdSaveAs: SaveFileAs(WindowNo, Event.InfoPtr);
          ELSE HandleBroadCast := False;
        END;
       END
       ELSE HandleBroadCast := False;
    END;



procedure RetrieveDesktop;
var
  S: PStream;
begin
  S := New(PBufStream, Init('TVB.DSK', stOpenRead, 1024));
  if LowMemory then OutOfMemory
  else if S^.Status <> stOk then
    MessageBox('Could not open desktop file', nil, mfOkButton + mfError)
  else
  begin
    LoadDesktop(S^);
    if S^.Status <> stOk then
      MessageBox('Error reading desktop file', nil, mfOkButton + mfError);
  end;
  Dispose(S, Done);
end;

procedure SaveDesktop;
var
  S: PStream;
  F: File;
begin
  S := New(PBufStream, Init('TVB.DSK', stCreate, 1024));
  if not LowMemory and (S^.Status = stOk) then
  begin
    StoreDesktop(S^);
    if S^.Status <> stOk then
    begin
      MessageBox('Could not create ' + ParamStr(0) + '.DSK.',
                  nil, mfOkButton + mfError);
      {$I-}
      Dispose(S, Done);
      Assign(F, 'TVB.DSK');
      Erase(F);
      Exit;
    end;
  end;
  Dispose(S, Done);
end;




procedure ShowClip;
begin
  ClipWindow^.Select;
  ClipWindow^.Show;
end;

procedure ChangeDir;
BEGIN
  ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
  InitFiles
END;

{
Procedure Memory;
Var LMem : Longint;
    SMem,
    EMem,
    XMem : Word;
    PA,
    PT   : Word;


Function GetVersion: String;
Begin
  GetVersion := EMS.GetVersion
End;


BEGIN
  EMem := 0;
  XMem := 0;

  LMem := System.MemAvail;

  If EMS.TestEMMDriver then
    EMS.EMMPageCount(EMem, PT)

  Else EMem := 0;

  XMem := 0;
  If XMS.Result = 0 then
    XMem := XMS.MemAvail

  Else XMem := 0;



  MessageBox('          Speicherplatz:' +
             Chr(13) +
             Chr(13) +
             '   Konventionell  : ' + GetString(System.MemAvail) + ' Byte.' +
             Chr(13) +
             Chr(13) +
             '   EMS ' + GetVersion + '        : ' + GetString(PT) + ' Seiten.' +
             Chr(13) +
             Chr(13) +
             '   EMS Verf�gbar  : ' + GetString(EMem) + ' Seiten.' +
             Chr(13) +
             Chr(13) +
             '   XMS            : ' +
              GetString(XMem) + ' kByte.',
             Nil, mfInformation + mfOkButton);

END;
}



(* BEGIN von TDareiverApp.HandleEvent: *)

Const SM: String12 = '*.*'; (* SuchmaskenVoreinstellung *)

Var Regs: Registers;

BEGIN
  TApplication.HandleEvent(Event);



  case Event.What of
    evCommand:
    Begin
      GetExtent(R);
      R.Assign(R.A.X, R.A.Y, R.B.X, R.B.Y-2);

      case Event.Command of
        cmTile: DeskTop^.Tile(R);
        cmCascade             : DeskTop^.Cascade(R);
        cmColors              : Begin
                                  ColorDialog;
                                  DeskTop^.ReDraw
                                End;

        cmSaveDesktop         : SaveDesktop;
        cmRetrieveDesktop     : RetrieveDesktop;

        (* Serienbrief bearbeiten: *)

        cmBriefeDrucken       : SerienBriefeDruckenDialog;
        cmEditorSave          : Begin
                                  Event.What := evBroadCast;
                                  Event.Command := edBase + cmEdSave;
                                  PutEvent(Event);
                                  Exit
                                  (*IF NOT HandleBroadCast THEN
                                    ClearEvent(Event);*)
                                End;
        cmEditorSaveAs        : Begin
                                  Event.What := evBroadCast;
                                  Event.Command := edBase + cmEdSaveAs;
                                  PutEvent(Event);
                                  Exit
                                  (*IF NOT HandleBroadCast THEN
                                    ClearEvent(Event);*)
                                End;

        cmChangeDir           : ChangeDir;
        cmShowClip            : ShowClip;

        cmChangeDir           : ChangeDir;

        cmIfCursor            : BEGIN
                                  IfCursor := Not(IfCursor);
                                  CursorAus
                                END;

(* Editor: *)
        cmFileOpen            : FileOpen;
      else
        Exit
      END;
    End;
    evBroadCast: IF NOT HandleBroadCast THEN ClearEvent(Event);
  else
    Exit
  END;

  If WNo > 1 then EnableCommands ([cmEditorSave, cmEditorSaveAs])
  else            DisableCommands([cmEditorSave, cmEditorSaveAs]);

  ClearEvent(Event);
END;

Procedure TDateiverApp.InitMenuBar;
Var R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(

    NewSubMenu('~E~ditor',                                        hcEditor, NewMenu(
      NewItem('~L~aden...',             '',   kbNoKey, cmFileOpen,   hcEditor,
      NewItem('~S~peichern',            '',   kbNoKey, cmEditorSave,       hcEditor,
      NewItem('Speichern ~a~ls',        '',   kbNoKey, cmEditorSaveAs,     hcEditor,
      NewLine(
      NewItem('~C~lipboard',            '',   kbNoKey, cmShowClip,   hcEditor,
      NewItem('~V~erzeichnis wechseln', '',   kbNoKey, cmChangeDir,  hcNoContext,
      NewLine(
      NewItem('~E~nde',            'Alt-X',   kbAltX,  cmQuit,       hcNoContext,
      Nil))))))))),


    NewSubMenu('Ein~s~tellungen',                               hcEinstellungen, NewMenu(
      NewItem('~C~ursor',               '',   kbNoKey, cmIfCursor,   hcNoContext,
      NewItem('~F~arben',               '',   kbNoKey, cmColors,      hcNoContext,
      NewLine(
      NewItem('~S~ave desktop', '', kbNoKey, cmSaveDesktop, hcNoContext,
      NewItem('~R~etrieve desktop', '', kbNoKey, cmRetrieveDesktop, hcNoContext,
      NewLine(
      NewItem('~T~ile',                 '',   kbNoKey, cmTile,       hcNoContext,
      NewItem('C~a~scade',              '',   kbNoKey, cmCascade,    hcNoContext,
      nil))))))))),

    NewSubMenu('Serien~b~rief', hcNoContext, NewMenu(
      NewItem('Briefe ~d~rucken...',    '', kbNoKey, cmBriefeDrucken, hcNoContext,
      nil)),



    nil))))));
END;

Procedure TDateiverApp.InitStatusLine;
var
  R: TRect;
BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Ende', kbAltX, cmQuit,
      NewStatusKey('~F2~ Speichern', kbF2, cmEditorSave,
      NewStatusKey('~F3~ Laden', kbF3, cmFileOpen,
      NewStatusKey('~F10~ Menu', kbF10, cmMenu,
      nil)))),
    nil)));

END;

Procedure TDateiverApp.OutOfMemory;
BEGIN
  MessageBox('Nicht genug Speicher f�r diese Operation.',
    nil, mfError + mfOkButton);
END;

PROCEDURE TDateiverApp.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;



(* Hauptprogramm: *)

Var DateiverApp : TDateiverApp;



BEGIN

  Regs.AH := $03;
  Intr($10, Regs);
  CursorSave := Regs.CX;
  RegisterStrings;

  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done;

  OrgCursor

END.