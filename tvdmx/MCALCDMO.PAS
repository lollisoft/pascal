
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	MCALCDMO  --demo for MATHCALC			}
{							}
{       Copyright (c) 1994      Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Program MCALCDMO;

{$B-}	{ boolean complete evaluation OFF }
{$D+}	{ Debug information ON }
{$I-}	{ I/O checking OFF }
{$R-}	{ Range checking OFF }
{$S+}	{ Stack checking ON }
{$V-}	{ Strict var-string checking OFF }
{$X+}	{ Extended syntax ON }

uses
    Dos, Crt, Objects,
    Drivers, Memory, Views, Menus, Dialogs, HistList, App,
    Editors, MsgBox, StdDlg,
    RSet, DmxGizma, tvGizma, MCalc;

const
    ProgName	=  'MULTIEDIT';
    ProgStName	=  'MULTI~EDIT~';
    ProgVer	=  '1.0';
    ProgVerNum	=   0;
    CoprName	=  'R. Beck';
    CoprNote	=  'Copyright (c) 1995';
    ConfigFName	=  'EDIT.CFG';

    CfgFHdrTxt	=  'MULTIEDIT configuration file'^M^J^Z#0;
    ConfigFHdr	:  string[length(CfgFHdrTxt)] = CfgFHdrTxt;

    hlFilenames	=  100;
    hlChDir	=  200;

    HeapSize	=  32 * (1024 div 16);

    cmAbout	=  100;
    cmShowClip	=  110;
    cmSearch	=  120;
    cmPrint	=  130;

    { status line message commands }
    cmPromptMsg	=  1001;
    cmBlinkMsg	=  1002;

    { main help-contexts }
    hcDesktop	=  hcDragging + 1;
    hcDialogs	=  2000;
    hcEditMemo	=  3000;
    hcMenu	=  11000;

    { dialog help contexts }
    hcOK	=  hcDialogs +  1;
    hcCancel	=  hcDialogs +  2;
    hcYes	=  hcDialogs +  3;
    hcNo	=  hcDialogs +  4;
    hcHelp	=  hcDialogs +  5;
    hcChDirName =  hcDialogs +  6;
    hcChDirList =  hcDialogs +  7;
    hcChDirButton= hcDialogs +  8;
    hcChDirRevert= hcDialogs +  9;
    hcFileName	=  hcDialogs + 10;
    hcFileList	=  hcDialogs + 11;
    hcFileOpen	=  hcDialogs + 12;
    hcFileReplace= hcDialogs + 13;
    hcFileClear	=  hcDialogs + 14;

    { help contexts for System menu }
    hcSystem	=  hcMenu + 10;
    hcAbout	=  hcSystem + 1;
    hcMathCalc	=  hcSystem + 2;
    hcSound	=  hcSystem + 3;
    hcVideo	=  hcSystem + 4;

    { help contexts for File menu }
    hcFile	=  hcMenu + 20;

    { help contexts for Edit menu }
    hcEdit	=  hcMenu + 30;
    hcShowClip	=  hcEdit + 1;

    { help contexts for Search menu }
    hcSearch	=  hcMenu + 40;
    hcFind	=  hcSearch + 1;
    hcReplace	=  hcSearch + 2;
    hcSearchAgain= hcSearch + 3;

    { help contexts for Window menu }
    hcWindow	=  hcMenu + 60;
    hcUserScreen=  hcWindow + 1;


type
    PMyStatusLine  = ^TMyStatusLine;
    TMyStatusLine  =  OBJECT(TStatusLine)
      procedure Draw;  VIRTUAL;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      function	Hint(AHelpCtx: word) : string;  VIRTUAL;
    end;


    TApp	=  OBJECT(TAppA)
	SysDir		: pathstr;
      constructor Init;
      destructor  Done;  VIRTUAL;
      procedure GetEvent(var Event: TEvent);  VIRTUAL;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure InitStatusLine;  VIRTUAL;
      procedure LoadConfigFile;
      procedure StoreConfigFile;
      procedure WriteShellMsg;  VIRTUAL;
    end;

const
    PromptMsg	: pstring = nil;
    BlinkMsg	: boolean = FALSE;

var
    ClipWindow	: PEditWindow;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure PutKeyCode(KeyCode: word);
var  Event : TEvent;
begin
  Event.InfoPtr := Application;
  Event.What    := evKeyDown;
  Event.KeyCode := KeyCode;
  Application^.PutEvent(Event);
end;


procedure SetControlHelpCtx(Dialog: PDialog; Control: PButton);
{ Sets the HelpCtx for this view }
begin
  If (TypeOf(Control^) = TypeOf(TButton)) then
    begin
    Case Control^.Command of
      cmOK:		Control^.HelpCtx := hcOK;
      cmCancel:		Control^.HelpCtx := hcCancel;
      cmYes:		Control^.HelpCtx := hcYes;
      cmNo:		Control^.HelpCtx := hcNo;
      cmHelp:		Control^.HelpCtx := hcHelp;
      cmChangeDir:	Control^.HelpCtx := hcChDirButton;
      cmRevert:		Control^.HelpCtx := hcChDirRevert;
      cmFileOpen:	Control^.HelpCtx := hcFileOpen;
      cmFileReplace:	Control^.HelpCtx := hcFileReplace;
      cmFileClear:	Control^.HelpCtx := hcFileClear;
      end;
    end;
end;


function  GetFileName(ATitle: string; WildCard: pathstr) : pathstr;
var  Dialog : PFileDialog;
     FName  : pathstr;
    procedure SetHelpCtx(P: PButton);  far;
    begin
      SetControlHelpCtx(Dialog, P);
    end;
begin
  Dialog := New(PFileDialog, Init(WildCard, ATitle, '~N~ame', fdOpenButton, hlFilenames));
  If (Application^.ValidView(Dialog) <> nil) then
    begin
    Dialog^.FileName^.HelpCtx := hcFileName;
    Dialog^.FileList^.HelpCtx := hcFileList;
    Dialog^.ForEach(@SetHelpCtx);
    If (Desktop^.ExecView(Dialog) <> cmCancel) then
      Dialog^.GetFileName(FName)
     else
      FName := '';
    Dispose(Dialog, Done);
    end;
  GetFileName := FName;
end;


function OpenEditor(FileName: FNameStr; Visible: boolean) : PEditWindow;
var  P	: PWindow;
     R	: TRect;
     WN	: integer;
begin
  If (FileName = '') and Visible then Exit;
  If (FileName = #0) then FileName := '';
  AssignWinRect(R, 0,0);
  If Visible then WN := wnNextAvail else WN := 0;
  P := New(PEditWindow, Init(R, FileName, WN));
  If not Visible then P^.Hide;
  OpenEditor := PEditWindow(Application^.InsertWindow(P));
end;


  { ออ TMyStatusLine อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TMyStatusLine.Draw;
var  c : word;
     B : TDrawBuffer;
begin
  If (PromptMsg = nil) then
    inherited Draw
   else
    begin
    c := GetColor(1);
    If BlinkMsg then c := c or Blink;
    MoveChar(B, ' ', c, Size.X);
    MoveStr(B[1], PromptMsg^, c);
    WriteLine(0, 0, Size.X, 1, B);
    end;
end;


procedure TMyStatusLine.HandleEvent(var Event: TEvent);
begin
  If (Event.What = evMouseDown) and (PromptMsg <> nil) then
    begin
    ClearEvent(Event);
    Message(Application, evCommand, cmPromptMsg, nil);
    end
   else
    inherited HandleEvent(Event);
end;


function  TMyStatusLine.Hint(AHelpCtx: word) : string;
begin
  Case AHelpCtx of
    hcSystem:	   Hint := 'System commands';
    hcAbout:	   Hint := 'Show version and copyright information';
    hcSound:	   Hint := 'Toggle sound on/off';
    hcVideo:	   Hint := 'Toggle video mode';
    hcMathCalc:	   Hint := 'Open the calculator window';

    hcFile:	   Hint := 'File management commands';
    hcExit:	   Hint := 'Exit ' + ProgName;

    hcEdit:	   Hint := 'Cut-and-paste editing commands';
    hcShowClip:	   Hint := 'Open the Clipboard window';

    hcSearch:	   Hint := 'Search commands';
    hcFind:	   Hint := 'Search for text';
    hcReplace:	   Hint := 'Search for text and replace it with new text';
    hcSearchAgain: Hint := 'Repeat the last Find or Replace command';

    hcWindow:	   Hint := 'Arrange and manipulate windows';
    hcUserScreen:  Hint := 'Display the original user screen';

    hcOK:	   Hint := 'Accept the settings in this dialog box';
    hcCancel:	   Hint := 'Close the dialog box without making any changes';
    hcYes:	   Hint := 'Perform the action';
    hcNo:	   Hint := 'Do not perform the action';
    hcHelp:	   Hint := 'View a help screen about this dialog box';
    hcChDirName:   Hint := 'Enter drive and/or directory path';
    hcChDirList:   Hint := 'Use cursor keys to navigate through directory tree';
    hcChDirButton: Hint := 'Change to the specified directory';
    hcChDirRevert: Hint := 'Return to the original directory';
    hcFileName:    Hint := 'Enter directory path and file mask';
    hcFileList:    Hint := 'Type for incremental search: lower for files, upper for subdir''s';
    hcFileOpen:	   Hint := 'Select the specified file into a new editor window';
    hcFileReplace: Hint := 'Open the specified file into the active window';
    hcFileClear:   Hint := 'Clear the specified file';

    $FF00..$FFFF:  Hint := StdMenuHint(AHelpCtx);
   else 	   Hint := '';
    end;
end;


  { ออ TApp ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TApp.Init;
const  InitMsg	: string[15] = 'Initializing...';
       HelloMsg	: string[45] = 'See the Calculator option under the '#240' menu';
var  H	: word;
     R	: TRect;
     N	: NameStr;
     X	: ExtStr;
begin
  PrintStr(ProgName + ' ' + ProgVer + '  ' + CoprNote + '  ' + CoprName + ^M^J);
  MaxHeapSize := HeapSize;
  inherited Init;
  DeskTop^.HelpCtx := hcDesktop;
  Message(Application, evCommand, cmPromptMsg, @InitMsg);

  DisableCommands([cmSave, cmSaveAs,
		   cmCut, cmCopy, cmPaste, cmClear, cmUndo,
		   cmFind, cmReplace, cmSearchAgain]);

  EditorDialog := StdEditorDialog;
  ClipWindow := OpenEditor('', FALSE);
  If (ClipWindow <> nil) then
    begin
    Clipboard := ClipWindow^.Editor;
    Clipboard^.CanUndo := FALSE;
    end;

  GetDir(0, SysDir);
  If (length(SysDir) < sizeof(SysDir)) and (SysDir[length(SysDir)] <> '\') then
    SysDir := SysDir + '\';
  If (IOresult <> 0) or (FSearch(ConfigFName, SysDir) = '') then
    FSplit(ParamStr(0), SysDir,N,X);
  If (FSearch(ConfigFName, SysDir) <> '') then
    begin
    LoadConfigFile;
    end;
  InitMathCalc;
  Message(Application, evCommand, cmPromptMsg, nil);
  If (ParamCount > 0) then OpenEditor(ParamStr(1), TRUE);
  Message(Application, evCommand, cmBlinkMsg, @HelloMsg);
end;


destructor TApp.Done;
begin
  StoreConfigFile;
  InitMathCalc;
  inherited Done;
end;


procedure TApp.GetEvent(var Event: TEvent);
begin
  inherited GetEvent(Event);
  If (PromptMsg <> nil) and
     ((Event.What and (evKeyDown or evMouseDown) <> 0) or
      ((Event.What = evCommand) and
       (Event.Command in [cmValid..cmError, cmClose..cmPrev])))
   then
    Message(Application, evCommand, cmPromptMsg, nil);
end;


procedure TApp.HandleEvent(var Event: TEvent);

    procedure About;
    const Intro = ^C+ProgName+^M^M
		+ ^C+'Version '+ProgVer+^M^M
		+ ^C+CoprNote+^M^M
		+ ^C+CoprName;
    var  R	: TRect;
	 Dialog : PDialog;
    begin
      R.Assign(0, 0, 41, 13);
      Dialog := New(PDialog, Init(R, 'About'));
      With Dialog^ do
	begin
	Options := Options or ofCentered;
	R.Grow(-1,-2);
	Insert(New(PStaticText, Init(R, Intro)));
	R.Assign(16, Dialog^.Size.Y - 3, 26, Dialog^.Size.Y - 1);
	Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
	end;
      ExecuteDialog(Dialog, nil);
    end;

    procedure ChangeDir;
    var  D : PChDirDialog;
	procedure SetHelpCtx(P: PButton);  far;
	begin
	  SetControlHelpCtx(D, P);
	end;
    begin
      D := New(PChDirDialog, Init(cdNormal, hlChDir));
      D^.DirInput^.HelpCtx := hcChDirName;
      D^.DirList^.HelpCtx  := hcChDirList;
      D^.ForEach(@SetHelpCtx);
      If (ValidView(D) <> nil) then ExecuteDialog(D, nil);
    end;

    procedure CloseAll;
    begin
      If DeskTop^.Valid(cmCloseAll) then
	begin
	DeskTop^.Lock;
	While (DeskTop^.Current <> nil) and DeskTop^.Valid(cmReleasedFocus) do
	  DeskTop^.Current^.Free;
	DeskTop^.Unlock;
	end;
    end;

    procedure DoChime;
    begin
      If BeepOn then
	begin
	Sound(1047);
	Delay(50);
	If (Event.InfoPtr = nil) or (PTimeView(Event.InfoPtr)^.Min = 0) then
	  begin
	  NoSound;
	  Delay(50);
	  Sound(2094);
	  end
	 else
	  Sound(523);
	Delay(100);
	NoSound;
	end;
    end;

    procedure DoPromptMsg;
    begin
      PromptMsg := Event.InfoPtr;
      BlinkMsg := (Event.Command = cmBlinkMsg);
      StatusLine^.DrawView;
    end;

    procedure SaveAll;
      procedure SaveFile(P: PEditWindow);  far;
      begin
	If (TypeOf(P^) = TypeOf(TEditWindow)) and (P <> ClipWindow) and
	    P^.Editor^.Modified
	 then
	  Message(P, evCommand, cmSave, @Self);
      end;
    begin
      If DeskTop^.Valid(cmSaveAll) then DeskTop^.ForEach(@SaveFile);
    end;

    procedure ShowClip;
    begin
      ClipWindow^.Select;
      ClipWindow^.Show;
    end;

    procedure DoCalc;
    const  CalcMsg : string[55] = 'This calculator can use the Cut and Paste edit options';
    begin
      Message(Application, evCommand, cmPromptMsg, @CalcMsg);
      Message(MathCalc, evCommand, cmMathCalc, @Self);
    end;

begin
  inherited HandleEvent(Event);
  If (Event.What and evMessage <> 0) then
    begin
    Case Event.Command of
      cmAbout:		About;
      cmNew:		OpenEditor(#0, TRUE);
      cmOpen:		OpenEditor(GetFileName('Open a File', '*.*'), TRUE);
      cmSaveAll:	SaveAll;

      cmChangeDir:	ChangeDir;
      cmChime:		DoChime;
      cmCloseAll:	CloseAll;
      cmMathCalc:	DoCalc;
      cmBlinkMsg,cmPromptMsg:	DoPromptMsg;
      cmShowClip:	ShowClip;
     else		Exit;
      end;
    If (Event.What = evCommand) then ClearEvent(Event);
    end;
end;


procedure TApp.InitMenuBar;
var  R : TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~'#240'~',  hcSystem, NewMenu(
      NewItem('~A~bout...', '', kbNoKey, cmAbout, hcAbout,
      NewLine(
      NewItem('~C~alculator', '', kbNoKey, cmMathCalc, hcMathCalc,
      NewLine(
      NewSoundItem(hcSound,
      NewVideoItem(hcVideo,
      nil))))))),
    NewSubMenu('~F~ile', hcFile, NewMenu(
      StdFileMenuItems(
      nil)),
    NewSubMenu('~E~dit', hcEdit, NewMenu(
      StdEditMenuItems(
      NewLine(
      NewItem('~S~how clipboard', '', kbNoKey, cmShowClip, hcShowClip,
      nil)))),
    NewSubMenu('~S~earch', hcSearch, NewMenu(
      NewItem('~F~ind...', '', kbNoKey, cmFind, hcFind,
      NewItem('~R~eplace...', '', kbNoKey, cmReplace, hcReplace,
      NewItem('~S~earch again', '', kbNoKey, cmSearchAgain, hcSearchAgain,
      nil)))),
    NewSubMenu('~W~indow', hcWindow, NewMenu(
      StdWindowMenuItems(
      NewLine(
      NewItem('~U~ser screen', 'Alt+F5', kbAltF5, cmUserScreen, hcUserScreen,
      nil)))),
    nil))))))
  ));
end;


procedure TApp.InitStatusLine;
var  R	: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PMyStatusLine, Init(R,
    NewStatusDef(hcNoContext, hcNoContext,
      NewStatusKey('~Esc~ Cancel',	kbNoKey,cmCancel,
      nil),
    NewStatusDef(hcDragging, hcDragging,
      NewStatusKey('Dragging...',	kbNoKey,cmAbout,
      nil),
    NewStatusDef(hcDesktop, hcDialogs - 1,
      NewStatusKey(ProgStName+' ',	kbNoKey,cmAbout,
      NewStatusKey('~Alt+F3~ Close',	kbAltF3,cmClose,
      NewStatusKey('~F5~ Zoom',		kbF5,   cmZoom,
      NewStatusKey('~F6~ Next',		kbF6,   cmNext,
      NewStatusKey('~F10~ Menu',	kbF10,  cmMenu,
      nil))))),
    NewStatusDef(hcDialogs, hcEditMemo - 1,
      NewStatusKey('~Esc~ Cancel',	kbEsc,  cmCancel,
      nil),
    NewStatusDef(hcEditMemo, hcMenu - 1,
      NewStatusKey('~Esc~ Close',	kbEsc,  cmCancel,
      NewStatusKey('~F5~ Zoom',		kbF5,   cmZoom,
      StdStatusKeys(
      nil))),
    NewStatusDef(hcMenu, $FFFF,
      NewStatusKey(ProgStName+' ',	kbNoKey,cmAbout,
      nil),
    nil))))))
  ));
end;


procedure TApp.LoadConfigFile;
const  ConfigFError: string[24] = 'Error reading config file';
var  S	 : TBufStream;
     Vid : word;
     Snd : boolean;
begin
  S.Init(SysDir + ConfigFName, stOpenRead, 1024);
  S.Seek(length(ConfigFHdr));
  S.Read(Vid, sizeof(Vid));
  S.Read(Snd, sizeof(Snd));
  LoadHistory(S);
  If (S.Status = stOK) then
    begin
    If (Snd <> BeepOn) then Message(Application, evCommand, cmToggleSound, @Self);
    If (Vid <> ScreenMode) then Message(Application, evCommand, cmToggleVideo, @Self);
    end
   else
    Message(Application, evCommand, cmPromptMsg, @ConfigFError);
  S.Done;
end;


procedure TApp.StoreConfigFile;
const  ConfigFError: string[25] = 'Error writing config file';
var  S	: TBufStream;
begin
  S.Init(SysDir + ConfigFName, stCreate, 1024);
  S.Write(ConfigFHdr[1], length(ConfigFHdr));
  S.Write(ScreenMode, sizeof(ScreenMode));
  S.Write(BeepOn, sizeof(BeepOn));
  StoreHistory(S);
  If (S.Status <> stOK) then
    Message(Application, evCommand, cmPromptMsg, @ConfigFError);
  S.Done;
end;


procedure TApp.WriteShellMsg;
begin
  PrintStr(#27'[J'^M'   '^M'Type EXIT to return to '+ProgName+'...');
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

var  MyApp	: TApp;

Begin
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.
