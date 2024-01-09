
Program MEMOSHOP;

{$M 16384,16384,655360}
{$V-,X+,D-,B-,R- }

uses
    Objects, Drivers, Views, Menus, Dialogs, App, Editors,
    RSet, DmxGizma, tvDMX, tvGizma, StdDMX;

const
    MyLabels   =  ' [A]       [B]                                 [C] ';
    MyTemplate =  ' ssssssss| ssssssssssssssssssssssssssssssss|WW,WWW ';
	{ The template codes for the memo field are
	  appended during window initialization. }

    cmOpenWin  =  101;
    cmEditMemo =  102;

type
    TMyMemoBuf =  RECORD  { Memo Buffer }
	Length : word;
	Buffer : array[1..512] of char;
    end;

    TMyRec     =  RECORD
	A      : string[8];
	B      : string[32];
        C      : word;
	Memo   : TMyMemoBuf;
    end;


    TAppN   =  OBJECT(TAppA)
    end;

    TMyApp  =  OBJECT(TAppN)
      constructor Init;
      procedure HandleEvent(var Event : TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure OpenWindow;
    end;

var
    WorkWindow	:  array[0..139] of TMyRec;


procedure EditCurrentMemoField(MemoNum : integer);
var  Win  : PDialog;
     DMX  : PDmxEditor;
     Memo : PMemo;
     P	  : pointer;
     A	  : string[20];
     R	  : TRect;
     Len  : integer;
begin
  GetBlob(MemoNum, P, Len);
  DMX := Message(DeskTop, evCommand, cmDMX_RollCall, Application);
  If (DMX <> nil) and (DMX^.FieldSelected) and
     (P <> nil) and (Len > 2) then
    begin
    Dec(Len, 2);
    DMX^.Vidis := TRUE;
    R.Assign(0,0,60,14);
    Str(succ(DMX^.CurrentRecord):1, A);
    A	:= 'Memo for Record ' + A;
    Win := New(PDialog, Init(R, A));
    Win^.Options := Win^.Options or ofCentered;
    Win^.GetExtent(R);
    R.Grow(-1, -1);
    Memo  := New(PMemo, Init(R,
      Win^.StandardScrollBar(sbHorizontal),
      Win^.StandardScrollBar(sbVertical),
      nil,
      Len));
    Memo^.SetData(P^);
    Win^.Insert(Memo);
    If (Application^.ValidView(Win) <> nil) then
      begin
      DMX^.EvaluateField;
      DeskTop^.ExecView(Win);
      Memo^.GetData(P^);
      DMX^.ChangeMade;
      DMX^.SetupField;
      Dispose(Win, Done);
      end;
    DMX^.Vidis := FALSE;
    end;
end;


constructor TMyApp.Init;
begin
  TAppN.Init;
  OpenWindow;
end;


procedure TMyApp.HandleEvent(var Event : TEvent);
begin
  TAppN.HandleEvent(Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmOpenWin:   OpenWindow;
      cmEditMemo:  EditCurrentMemoField(1);
     else	   Exit;
      end;
    ClearEvent(Event);
    end;
end;


procedure TMyApp.InitMenuBar;
var  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~M~emoshop', hcNoContext, NewMenu(
      NewItem('~O~pen',      'F4', kbF4, cmOpenWin,  hcNoContext,
      NewItem('Edit ~M~emo', 'F2', kbF2, cmEditMemo, hcNoContext,
      NewLine(
      NewItem('e~X~it',  'Alt-X',  kbAltX, cmQuit, hcNoContext,
      nil))))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      NewItem('~S~ize/Move', 'Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom',      'F5', kbF5,    cmZoom,	hcNoContext,
      NewItem('~T~ile',      '',   kbNoKey, cmTile,    hcNoContext,
      NewItem('C~a~scade',   '',   kbNoKey, cmCascade,	hcNoContext,
      NewItem('~N~ext',      'F6', kbF6,    cmNext,    hcNoContext,
      NewItem('~P~revious', 'Shift-F6', kbShiftF6, cmPrev, hcNoContext,
      NewItem('~C~lose', 'Alt-F3',  kbAltF3, cmClose,	hcNoContext,
      nil)))))))),
    nil)))
  ));
end;


procedure TMyApp.OpenWindow;
var  R	: TRect;
     W	: PWindow;
     Template : string;
begin
  AssignWinRect(R, length(MyLabels) + 2, 0);
  Template := MyTemplate + InitBlobField(sizeof(TMyMemoBuf), accNormal, 0);
  New(W, Init(R, 'Work Window', wnNextAvail));
  With W^ do
    begin
    Options := Options or ofTileable; { must be tileable for AssignWinRect }
    GetExtent(R);
    R.Grow(-1,-1);
    Inc(R.A.Y, 2);
    Insert(New(PDmxEditor,
	Init(Template,
	      WorkWindow,
	      sizeof(WorkWindow),
	      R,
	      New(PDmxFLabels, InitInsert(W, MyLabels)),
	      New(PDmxRecInd, InitInsert(W, 10)),
	      W^.StandardScrollBar(sbHorizontal),
	      W^.StandardScrollBar(sbVertical)
	      )
	));
    end;
  DeskTop^.Insert(ValidView(W));
end;


var  MyApp	:  TMyApp;

Begin
  FillChar(WorkWindow, sizeof(WorkWindow), 0);
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.

