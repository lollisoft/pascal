
Program WIDESHOP;

{$M 16384,16384,655360}
{$V-,X+,D-,B-,R- }

uses
    Objects, Drivers, Views, Menus, Dialogs, App,
    RSet, DmxGizma, tvDMX, tvGizma, tvDMXREP;

const
    xLabel1	=  '         [A]          [B]  [C]    [D]     [E]  ';

   { Note there was a bug found in the TDmxScroller.InitStruct() that causes
     problems with the last field in each sub-template line.  Until this is
     fixed, you can get around this problem by placing a #0 character at the
     end of each segment.  (This is only a problem if you extend a template
     with TSItemFields.)      >>>---------------------------------->  }
    xTemplate1	=  ' ssssssssssssssssssssบ[X]|BBB |WW,WWW |WW,WWW '#0;
                                                                 {    }

    { --each xTemplateX line ends with a #0 character. }
    xLabel2	=  '      [F]          [G]          [H]          [I]          [J]    ';
    xTemplate2	=  '|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)'#0;

    xLabel3	=  '      [K]          [L]          [M]          [N]          [O]    ';
    xTemplate3	=  '|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)'#0;

    xLabel4	=  '      [P]          [Q]          [R]          [S]          [T]    ';
    xTemplate4	=  '|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)'#0;

    xLabel5	=  '      [U]          [V]          [W]          [X]          [Y]          [Z]    ';
    xTemplate5	=  '|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)|($rr,rrr.zz)'#0;

    cmOpenWin	=  101;
    cmPrint	=  102;


type
    TMyRec	=  RECORD
	A						: string[20];
	B						: boolean;
	C						: byte;
	D,E						: word;
	F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z	: TRealNum;
    end;


    PDmxInterior    = ^TDmxInterior;
    TDmxInterior    =  OBJECT(TDmxEditor)

     { see documentation on tvDMX's virtual methods if you wish to
       modify your tvDMX view's behavior  }

    end;


    TAppN	=  OBJECT(TAppPrn)
    end;

    TMyApp	=  OBJECT(TAppN)
      constructor Init;
      procedure HandleEvent(var Event : TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure OpenWindow;
    end;


const
    NumRecords	=  100;
var
    MyData	:  array[0..NumRecords-1] of TMyRec;


  { ออ TDmxInterior ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


     { see documentation on tvDMX's virtual methods if you wish to
       modify your tvDMX view's behavior  }


  { ออ TMyApp ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


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
      cmOpenWin:	OpenWindow;
      cmPrint:		PrnCurrentDMX;
      cmPRN_NewPage:	PrnPageStart(Event);
      cmPRN_EndPage:	PrnPageEnd(Event);
      cmPRN_SetOptions:	PrnSetOptions(hcNoContext, hcNoContext, hcNoContext);
     else		Exit;
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
    NewSubMenu('tv~DMX~', hcNoContext, NewMenu(
      NewItem('~O~pen',  'F4', kbF4, cmOpenWin, hcNoContext,
      NewLine(
      NewSoundItem(hcNoContext,
      NewVideoItem(hcNoContext,
      NewLine(
      NewItem('e~X~it',  'Alt-X',  kbAltX, cmQuit, hcNoContext,
      nil))))))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      StdWindowMenuItems(
      NewLine(
      NewItem('~U~ser screen', 'Alt-F5', kbAltF5, cmUserScreen, hcNoContext,
      nil)))),
    NewSubMenu('~P~rint', hcNoContext, NewMenu(
      NewItem('~P~rint', 'F9', kbF9, cmPrint, hcNoContext,
      StdPrnMenuItems(hcNoContext,
      nil))),
    nil))))
  ));
end;


procedure TMyApp.OpenWindow;
var  R	: TRect;
     W	: PWindow;

    function  LabelView : PDmxLink;
    begin
      LabelView := New(PDmxMLabels, InitInsert(W,
			NewSItem(xLabel1,
			NewSItem(xLabel2,
			NewSItem(xLabel3,
			NewSItem(xLabel4,
			NewSItem(xLabel5,
				 nil)))))
			));
    end;

    function  Template : string;
    begin
      Template := InitTSItemFields(
			NewSItem(xTemplate1,
			NewSItem(xTemplate2,
			NewSItem(xTemplate3,
			NewSItem(xTemplate4,
			NewSItem(xTemplate5,
				 nil)))))
			);
    end;

begin
  AssignWinRect(R, 0,0);  { assign window dimensions }
  New(W, Init(R, 'Wide Window', wnNextAvail));
  With W^ do
    begin
    Options := Options or ofTileable; { must be tileable for AssignWinRect }
    GetExtent(R);		  { create new rectangle for editor object }
    R.Grow(-1,-1);			      { shrink -1 to avoid borders }
    Inc(R.A.Y, 2);			 { make room for TDmxLabels object }
    Insert(New(PDmxInterior,
	  Init(Template,			     { ...template strings }
		MyData,					    { working data }
		sizeof(MyData),			    { size of working data }
		R,					{ view's rectangle }
		LabelView,				{ ...label strings }
		New(PDmxRecInd, InitInsert(W, 10)),	 { indicator width }
		W^.StandardScrollBar(sbHandleKeyboard or sbHorizontal),
		W^.StandardScrollBar(sbHandleKeyboard or sbVertical)
		)
	));
    end;
  DeskTop^.Insert(ValidView(W));
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

var  MyApp	:  TMyApp;


Begin
  FillChar(MyData, sizeof(MyData), 0);
  MyData[0].A := 'Abigail Adams';
  MyData[1].A := 'Betty Boop';

  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.
