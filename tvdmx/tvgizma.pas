
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	tvGIZMA   --Turbo Vision Accessories		}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit tvGIZMA;

{$D-,B-,O+,R-,V-,X+ }

interface

uses
    Dos, Crt, Objects, Drivers, Memory, Dialogs, Menus,
    HistList, Views, App, MsgBox, Buffers, RSet, DmxGizma;

const
    cmUserScreen	= cmDMX + 51;	{ invokes User Screen }
    cmToggleSound	= cmDMX + 52;	{ toggles BeepOn }
    cmToggleVideo	= cmDMX + 53;	{ toggles video mode }
    cmBeep		= cmDMX + 54;	{ beeps if BeepOn is TRUE }

    BeepOn   : boolean 	= TRUE;  { allows beeping from cmBeep event }

    SoundIndOn		= ' ON';	{ On & Off must be the same length }
    SoundIndOff		= 'OFF';
    VideoIndHi		= '43/50';	{ Hi & Low must be the same length }
    VideoIndLow		= '   25';

type
    PCursorDlg	= ^TCursorDlg;
    TCursorDlg	=  OBJECT (TDialog)
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
    end;


    PTimeView  = ^TTimeView;
    TTimeView  =  OBJECT (TView)
	Hour,Min  : word;
      constructor Init (var Bounds : TRect);
      procedure Draw;  VIRTUAL;
      procedure Update;  VIRTUAL;
    end;


    PAppA   = ^TAppA;
    TAppA   =  OBJECT (TProgram)
	Clock		: PTimeView;
        SoundInd	: pstring;
        VideoInd	: pstring;
      constructor Init;
      destructor  Done;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure Idle;  VIRTUAL;
      procedure InitClock;  VIRTUAL;
      function  NewSoundItem (AHelpCtx : word; ANext : PMenuItem) : PMenuItem;
      function  NewVideoItem (AHelpCtx : word; ANext : PMenuItem) : PMenuItem;
      procedure OutOfMemory;  VIRTUAL;
      private
	KeptScreen : PVideoBuf;
	Col,Row    : byte;
    end;


    PUserScreen  = ^TUserScreen;
    TUserScreen  =  OBJECT (TScroller)
      constructor Init (var Bounds : TRect; AHScrollBar,AVScrollBar : PScrollBar);
      procedure Draw;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      function  Valid (Command : word) : boolean;  VIRTUAL;
    end;


  function  SParam (S : pstring;  Next : pointer) : pointer;
  function  DParam (N : longint;  Next : pointer) : pointer;
    { accessories for FormatStr() and MessageBox() procedures }


  procedure AssignWinRect (var Bounds : TRect;  MaxX,MaxY : integer);
    { assigns a rectangle to cascade into the desktop }

  function  InsertLine (Dialog : PDialog;  Col,Row,Width,Max : integer;
			Fmt : boolean; ALabel : string; hlID : word) : PInputLine;
    { inserts a TInputLine view with (optional) history list }

  function  InsertText (Dialog : PDialog; Col,Row : integer; AText : string) : PView;
    { inserts a single-line standard text view }

  function  InsertView (Owner :PGroup; View :PView; Options :word) : pointer;
    { sets a view's options and inserts it into an owner }

  function  NewVarItem (Name, Param : TMenuStr; var Ind : pstring;
                        KeyCode, Command, AHelpCtx : word;
                        Next : PMenuItem) : PMenuItem;
    { creates a new menu item with a status indicator }

  function  NextWindowNumber  : integer;
    { finds an unused window number }

  procedure TrimDialog (Window : PWindow);
    { resizes a dialog window }


implementation


  { ออ Param Functions อออออออออออออออออออออออออออออออออออออออออออออออออออ }

const	iparmax			= 15;  { maximum number of parameters - 1 }
	ipar	: integer	= iparmax;

var	Apar	: array [0..iparmax] of pointer;


function  SParam (S : pstring;  Next : pointer) : pointer;
begin
  {$IFOPT R+ }
  If (ipar < 0) then RunError (201);
  {$ENDIF }
  If (Next = nil) then ipar := iparmax;
  Apar [ipar] := S;
  SParam := @Apar [ipar];
  Dec (ipar);
end;


function  DParam (N : longint;  Next : pointer) : pointer;
begin
  {$IFOPT R+ }
  If (ipar < 0) then RunError (201);
  {$ENDIF }
  If (Next = nil) then ipar := iparmax;
  Apar [ipar] := pointer (N);
  DParam := @Apar [ipar];
  Dec (ipar);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure AssignWinRect (var Bounds : TRect;  MaxX,MaxY : integer);
var P : PView;
begin
  DeskTop^.GetExtent (Bounds);
  If (MaxX <= 0) then MaxX := Bounds.B.X;
  If (MaxY <= 0) then MaxY := Bounds.B.Y;
  If (Bounds.B.X > MaxX) then Bounds.B.X := MaxX;
  If (Bounds.B.Y > MaxY) then Bounds.B.Y := MaxY;
  P := DeskTop^.Current;
  If (P^.Options and ofTileable = 0) then P := nil;
  If (P <> nil) then
    begin
    Bounds.Move (succ (P^.Origin.X), succ (P^.Origin.Y));
    If (Bounds.B.X > DeskTop^.Size.X) then Bounds.B.X := DeskTop^.Size.X;
    If (Bounds.B.Y > DeskTop^.Size.Y) then Bounds.B.Y := DeskTop^.Size.Y;
    If (Bounds.B.X - Bounds.A.X < MinWinSize.X) or
       (Bounds.B.Y - Bounds.A.Y < MinWinSize.Y) then
      begin
      If (MaxX >= DeskTop^.Size.X) then MaxX := pred (DeskTop^.Size.X);
      Bounds.A.X := 1;
      Bounds.A.Y := 0;
      Bounds.B.X := succ (MaxX);
      Bounds.B.Y := MaxY;
      end;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  InsertLine (Dialog : PDialog;  Col,Row,Width,Max : integer;
		      Fmt : boolean; ALabel : string;  hlID : word) : PInputLine;
var  i  : integer;
     R  : TRect;
     B  : PInputLine;
begin
  With Dialog^ do
    begin
    i  := succ (CStrLen (ALabel));
    R.Assign (Col, Row, Col + Width + 2, succ (Row));
    If (ALabel <> '') then
      begin
      If Fmt then R.Move (1, 1) else R.Move (i, 0);
      end;
    B  := New (PInputLine, Init (R, Max));
    Insert (B);
    If (hlID > 0) then
      begin
      R.A.X := R.A.X + Width + 2;
      R.B.X := R.A.X + 3;
      Insert (New (PHistory, Init (R, B, hlID)));
      end;
    If (ALabel <> '') then
      begin
      R.Assign (Col, Row, Col + i, succ (Row));
      Insert (New (PLabel, Init (R, ALabel, B)));
      end;
    end;
  InsertLine := B;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  InsertText (Dialog : PDialog; Col,Row : integer; AText : string) : PView;
var  R : TRect;
     B : PView;
begin
  With Dialog^ do
    begin
    R.Assign (Col, Row, Col + length (AText), succ (Row));
    B  := New (PStaticText, Init (R, AText));
    Insert (B);
    end;
  InsertText := B;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  InsertView (Owner :PGroup; View :PView; Options :word) : pointer;
begin
  If (View <> nil) then
    begin
    View^.Options := View^.Options or Options;
    If (Owner <> nil) then Owner^.Insert (View);
    end;
  InsertView := View;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  NewVarItem (Name, Param : TMenuStr; var Ind : pstring;
                      KeyCode, Command, AHelpCtx : word;
                      Next : PMenuItem) : PMenuItem;
var  P : PMenuItem;
begin
  P := NewItem (Name,Param, KeyCode,Command,AHelpCtx, Next);
  Ind := P^.Param;
  NewVarItem := P;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  NextWindowNumber  : integer;
var  wn : integer;

    function  UsedWN (P : PWindow) : boolean;  far;
    begin
      UsedWN := (P^.Number = wn) and (P <> PWindow (DeskTop^.Background))
    end;

begin
  wn := 0;
  Repeat Inc (wn) until (DeskTop^.FirstThat (@UsedWN) = nil);
  NextWindowNumber := wn;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TrimDialog (Window : PWindow);
var  B    : TRect;
     MinX : integer;

    procedure FindBounds (P : PView);  far;
    begin
      If (PFrame (P) <> Window^.Frame) and (P^.GetState (sfVisible)) then
        begin
        If (P^.Origin.X < MinX) then MinX := P^.Origin.X;
        If (P^.Options and ofCenterX <> 0) then P^.MoveTo (0, P^.Origin.Y);
        If (P^.Size.X + P^.Origin.X > B.B.X) then B.B.X := P^.Size.X + P^.Origin.X;
        If (P^.Size.Y + P^.Origin.Y > B.B.Y) then B.B.Y := P^.Size.Y + P^.Origin.Y;
        P^.GrowMode := 0;
        end;
    end;

    procedure ReCenter (P : PView);  far;
    begin
      If (P^.Options and ofCenterX <> 0) and (PFrame (P) <> Window^.Frame) and
         (Window^.Size.X > P^.Size.X) then
        P^.MoveTo (((Window^.Size.X - P^.Size.X) shr 1), P^.Origin.Y);
    end;

begin
  If (Window = nil) then Exit;
  B.Assign (0,0,10,0);
  If (Window^.Title <> nil) then B.B.X := 12 + length (Window^.Title^);
  MinX := 999;
  Window^.ForEach (@FindBounds);
  If (MinX = 999) then MinX := 2;
  B.B.X := B.B.X + MinX + 1;
  B.B.Y := B.B.Y + 1;
  If (B.B.X > Window^.Size.X) then B.B.X := Window^.Size.X;
  If (B.B.Y > Window^.Size.Y) then B.B.Y := Window^.Size.Y;
  Window^.GrowTo (B.B.X, B.B.Y);
  Window^.ForEach (@ReCenter);
  Window^.Options := Window^.Options or ofCentered;
  Window^.DrawView;
end;


  { ออ TCursorDlg ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TCursorDlg.HandleEvent (var Event : TEvent);
var  P : PView;
begin
  TDialog.HandleEvent (Event);
  If (Event.What = evKeyDown) and (Current <> nil) then
    begin
    P := Current;
    Case Event.KeyCode of
      kbUp,kbLeft,kbCtrlLeft:
	begin
	Repeat
	  P := P^.Next;
	Until (P^.Options and ofSelectable <> 0) and P^.GetState (sfVisible);
	end;
      kbDown,kbRight,kbCtrlRight:
	begin
	Repeat
	  P := P^.Prev;
	Until (P^.Options and ofSelectable <> 0) and P^.GetState (sfVisible);
	end;
     else	Exit;
      end;
    P^.Select;
    ClearEvent (Event);
    end;
end;


  { ออ TTimeView อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TTimeView.Init (var Bounds : TRect);
begin
  TView.Init (Bounds);
  Min := 99;
  Update;
end;


procedure TTimeView.Draw;
var  B : TDrawBuffer;
     C : word;
     H : word;
     A,Suffix : string;
begin
  Suffix := ' pm';
  H  := Hour mod 12;
  If (Hour < 12) then Suffix [2] := 'a';
  If (H = 0) then H := 12;
  Str ((H * 1000) + Min:5, A);
  A [3] := ':';
  A := A + Suffix;
  C := GetColor (2);
  MoveChar (B, ' ', C, Size.X);
  MoveStr (B, A, C);
  WriteLine (0, 0, Size.X, 1, B);
end;


procedure TTimeView.Update;
var  H,M,S,T : word;
begin
  GetTime (H,M,S,T);
  If (Hour <> H) or (Min <> M) then
    begin
    Hour := H;
    Min  := M;
    DrawView;
    end;
end;


  { ออ TAppA อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TAppA.Init;
var  P     : PVideoBuf;
     WX,WY : byte;
begin
  InitMemory;
  InitVideo;
  If (StartupMode = ScreenMode) then
    begin
    New (P);
    Move (ScreenBuffer^, P^, sizeof (P^));
    WX := WhereX;
    WY := WhereY;
    end
   else
    P := nil;
  InitEvents;
  InitSysError;
  InitHistory;
  TProgram.Init;
  KeptScreen := P;
  Col := WX;
  Row := WY;
  InitClock;
  Insert (Clock);
end;


destructor TAppA.Done;
begin
  If (Clock <> nil) then Dispose (Clock, Done);
  TProgram.Done;
  DoneHistory;
  DoneSysError;
  DoneEvents;
  DoneVideo;
  If (KeptScreen <> nil) then
    begin
    Move (KeptScreen^, ScreenBuffer^, sizeof (KeptScreen^));
    GotoXY (Col,Row);
    Dispose (KeptScreen);
    KeptScreen := nil;
    end;
  DoneMemory;
end;


procedure TAppA.HandleEvent (var Event : TEvent);
var  R : TRect;
     M : word;

    procedure DeskTopCommand;
    begin
      Desktop^.Lock;
      Desktop^.GetExtent (R);
      Case Event.Command of
        cmCascade:	Desktop^.Cascade (R);
        cmTile:		Desktop^.Tile (R);
        end;
      Message (Desktop, evBroadcast, cmDMX_FixSize, @Self);
      Desktop^.Unlock;
    end;

    procedure ShowUserScreen;
    var  Dialog : PDialog;
    begin
      GetExtent (R);
      Dialog := New (PDialog, Init (R, 'User Screen'));
      Dialog^.Insert (New (PUserScreen, Init (R, nil,nil)));
      If (ValidView (Dialog) <> nil) then
        begin
        ExecView (Dialog);
        Dispose (Dialog, Done);
        end;
    end;

    procedure DoBeep;
    begin
      If BeepOn then
        begin
        Sound (523);
        Delay (50);
        NoSound;
        end;
    end;

begin
  TProgram.HandleEvent (Event);
  If (Event.What = evCommand) then
    begin
    Case Event.Command of
      cmCascade,cmTile:		DeskTopCommand;
      cmBeep,cmDMX_WrongKey:	DoBeep;
      cmToggleSound:
	begin
	BeepOn := not BeepOn;
	If (SoundInd <> nil) then
	  begin
	  If BeepOn then SoundInd^ := SoundIndOn else SoundInd^ := SoundIndOff;
	  end;
	end;
      cmToggleVideo:
	begin
	M := ScreenMode xor smFont8x8;
	If (M and smFont8x8 = 0) then
	  begin
	  ShadowSize.X := 2;
	  If (VideoInd <> nil) then VideoInd^ := VideoIndLow;
	  end
	 else
	  begin
	  ShadowSize.X := 1;
	  If (VideoInd <> nil) then VideoInd^ := VideoIndHi;
	  end;
	SetScreenMode (M);
	end;
      cmUserScreen:	ShowUserScreen;
     else		Exit;
      end;
    ClearEvent (Event);
    end;
end;


procedure TAppA.Idle;
var  M : word;

    function  IsTileable (P: PView) : boolean;  far;
    begin
      IsTileable := (P^.Options and ofTileable <> 0) and P^.GetState (sfVisible);
    end;

begin
  TProgram.Idle;
  If (Desktop^.FirstThat (@IsTileable) <> nil) then
    EnableCommands ([cmTile, cmCascade])
   else
    DisableCommands ([cmTile, cmCascade]);
  If (VideoInd <> nil) then
    begin
    If (ScreenMode and smFont8x8 = 0) then
      VideoInd^ := VideoIndLow
     else
      VideoInd^ := VideoIndHi;
    end;
  If (Clock <> nil) then Clock^.Update;
end;


procedure TAppA.InitClock;
var  R : TRect;
begin
  GetExtent (R);
  Dec (R.B.X);
  R.A.X := R.B.X - 8;
  R.B.Y := R.A.Y + 1;
  Clock := New (PTimeView, Init (R));
end;


function  TAppA.NewSoundItem (AHelpCtx : word;  ANext : PMenuItem) : PMenuItem;
begin
  NewSoundItem := NewVarItem ('~S~ound', SoundIndOn, SoundInd, kbNoKey,
                              cmToggleSound, AHelpCtx, ANext);
end;


function  TAppA.NewVideoItem (AHelpCtx : word;  ANext : PMenuItem) : PMenuItem;
begin
  If HiResScreen then
    NewVideoItem := NewVarItem ('~V~ideo mode', VideoIndLow, VideoInd, kbNoKey,
                                cmToggleVideo, AHelpCtx, ANext)
   else
    NewVideoItem := ANext;
end;


procedure TAppA.OutOfMemory;
begin
  MessageBox ('Not enough memory for this operation.', nil, mfError + mfOKButton);
end;


  { ออ TUserScreen อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TUserScreen.Init (var Bounds : TRect; AHScrollBar,AVScrollBar : PScrollBar);
var  Width,Height : integer;
begin
  TScroller.Init (Bounds, AHScrollBar,AVScrollBar);
  Width  := 80;
  Height := 25;
  If (StartupMode in [0,1]) then Width := 40;
  SetCursor (pred (PAppA (Application)^.Col), pred (PAppA (Application)^.Row));
  If (PAppA (Application)^.KeptScreen = nil) then Height := 0;
  GrowMode := gfGrowHiX or gfGrowHiY;
  SetLimit (Width,Height);
end;


procedure TUserScreen.Draw;
var  i, Y : integer;
     B    : TDrawBuffer;
begin
  For Y := 0 to Size.Y - 1 do
    begin
    FillChar (B, sizeof (B), 0);
    i := Delta.Y + Y;
    If (i < Limit.Y) then
      Move (PAppA (Application)^.KeptScreen^[(i * Limit.X) + Delta.X], B, Limit.X shl 1);
    WriteLine (0, Y, Size.X, 1, B);
    end;
  If (Limit.Y > 0) then ShowCursor;
end;


procedure TUserScreen.HandleEvent (var Event : TEvent);
begin
  TScroller.HandleEvent (Event);
  If Owner^.GetState (sfModal) and (Event.What in [evKeyDown,evMouseDown]) then
    Owner^.EndModal (cmCancel);
end;


function  TUserScreen.Valid (Command : word) : boolean;
var  b : boolean;
begin
  b := TScroller.Valid (Command);
  If b and (Command = cmValid) and (PAppA (Application)^.KeptScreen = nil) then
    begin
    MessageBox ('User screen was not preserved.', nil, mfError + mfOKButton);
    b := FALSE;
    end;
  Valid := b;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
