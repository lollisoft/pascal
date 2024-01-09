
{*******************************************************}
{							}
{	SIDETEST:  demo of SIDESHOE splits DMX view	}
{							}
{		** Experimental **			}
{							}
{*******************************************************}

Program SIDETEST;

{$V-,X+,D+,B-,R- }

uses
    Objects, Drivers, Views, Dialogs, Menus, App, MsgBox,
    RSet, DmxGizma, tvDMX, tvGizma, SideShoe, MainShoe;

const
    xLabels	=
	' String Field            +Real         Real      Word   Seg : Ofs      Real         Real         Real    ';
    xTemplate	=
	' ssssssssssssssssssss|RRRRRRR.ZZZR|($rr,rrr.zz)|WWWWW | HHHH:HHHH |($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)';

type
    PDmxInterior    = ^TDmxInterior;
    TDmxInterior    =  OBJECT(TDmxMain)  { main object from MAINSHOE.PAS }
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
    end;


    TMyApp	=  OBJECT(TAppA)
      constructor Init;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure OpenWindow;
    end;


var
    WorkWindow	:  array[1..8000] of byte;  { generic database }


  { 様 TDmxInterior 様様様様様様様様様様様様様様様様様様様様様様様様様様様 }


procedure TDmxInterior.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
end;


  { 様 TMyApp 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 }


constructor TMyApp.Init;
begin
  PreserveScreen := FALSE;
  inherited Init;
  MinWinSize.X := 40;
end;


procedure TMyApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmOpen:	OpenWindow;
     else	Exit;
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
    NewSubMenu('~Side~Shoe', hcNoContext, NewMenu(
      NewItem('~O~pen',  'F3', kbF3, cmOpen, hcNoContext,
      NewLine(
      NewSoundItem(hcNoContext,
      NewVideoItem(hcNoContext,
      NewLine(
      NewItem('e~X~it',  'Alt+X', kbAltX, cmQuit, hcNoContext,
      nil))))))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      NewItem('~S~ize/Move', 'Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom',      'F5', kbF5,    cmZoom,	hcNoContext,
      NewItem('~T~ile',      '',   kbNoKey, cmTile,	hcNoContext,
      NewItem('C~a~scade',   '',   kbNoKey, cmCascade,	hcNoContext,
      NewItem('~N~ext',      'F6', kbF6,    cmNext,	hcNoContext,
      NewItem('~P~revious', 'Shift-F6', kbShiftF6, cmPrev, hcNoContext,
      NewItem('~C~lose', 'Alt-F3',  kbAltF3, cmClose,	hcNoContext,
      nil)))))))),
    nil)))
  ));
end;


procedure TMyApp.OpenWindow;
var  R	 : TRect;
     W	 : PWindow;
     DMX : PDmxInterior;
begin
  AssignWinRect(R, length(xLabels) + 2, 0);  { assign window dimensions }
      { width of string xLabels plus two for the border; }
      { zero rows indicates extend to bottom of screen }

  New(W, Init(R, 'Work Window', wnNextAvail));
  With W^ do
    begin
    Options := Options or ofTileable; { must be tileable for AssignWinRect }
    GetExtent(R);		  { create new rectangle for editor object }
    R.Grow(-1,-1);			      { shrink -1 to avoid borders }
    Inc(R.A.Y, 2);			 { make room for TDmxLabels object }
    DMX := New(PDmxInterior, Init(xTemplate,		 { template string }
		WorkWindow,				    { working data }
		sizeof(WorkWindow),		    { size of working data }
		R,					{ view's rectangle }
		New(PDmxFLabels, InitInsert(W, xLabels)),   { label string }
		New(PDmxRecInd, InitInsert(W, 10)),    { indicator width }
		W^.StandardScrollBar(sbHandleKeyboard or sbHorizontal),
		W^.StandardScrollBar(sbHandleKeyboard or sbVertical)
		)
	    );
    Insert(DMX);  { main view must be inserted before you create Side }

  (*************************************************************)
    DMX^.Side := New(PDmxSide, Init(DMX, 2));  { split after two fields }
    Insert(DMX^.Side);
  (*************************************************************)
    end;
  InsertWindow(W);
end;


  { 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 }

var  MyApp	:  TMyApp;


Begin
  FillChar(WorkWindow, sizeof(WorkWindow), 0);

  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.
