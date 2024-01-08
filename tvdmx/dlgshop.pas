
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	DLGSHOP --tvDMX dialog demo program		}
{	tvDMX	--data editing project (ver 1.5)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Program DLGSHOP;

{  This program demonstrates the use of tvDMX objects in dialog windows.
   There are two examples:  EditRecord and EditInvoice.

   EditRecord presents a standard dialog box similar to the one created
   in the program SAMPLES.PAS.  It uses the InsertField() function from
   unit StdDMX.PAS to position each field in the record.

   DeskTop^.GetExtent (R) initializes the window's rectangle as oversized,
   and the size is trimmed down and centered by the TrimDialog() procedure
   after all the component views are inserted.

   EditInvoice inserts several fields using InsertField(), but the largest
   section of the window is taken by a TDmxEditDlg view, which has many
   records.  TDmxEditDlg views are tvDMX objects which have been modified
   (via virtual methods) to operate within a dialog box.

   This program is not yet complete.  A future version may use methods
   similar to those in object TDmxPayroll (in program SAMPLES.PAS) that
   calculate several fields in the record.
 }

{$M 16384,8192,655360 }
{$V-,X+,D-,B-,R- }

uses
    Dos,
    Objects, Drivers, Views, Menus, Dialogs, App,
    RSet, DmxGizma, tvDMX, StdDMX, tvGizma;


const
    cmEditRec     =  101;
    cmInvoice     =  102;

    cmNoCmd       = 1000;

    hcMenus       = 1100;
    hcDeskTop     = 1200;
    hcDialogs     = 4000;


type
    TMyApp	=  OBJECT (TApplication)
      constructor Init;
      destructor  Done;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure InitStatusLine;  VIRTUAL;
      procedure EditRecord;   VIRTUAL;
      procedure EditInvoice;  VIRTUAL;
    end;


  { ออ TMyApp ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TMyApp.Init;
begin
  TApplication.Init;
  DeskTop^.HelpCtx := hcDeskTop;
end;


destructor TMyApp.Done;
begin
  TApplication.Done;
  PrintStr (#27'[J'^M'   '^M);  { clear screen with ANSI colors }
end;


procedure TMyApp.HandleEvent (var Event : TEvent);
begin
  TApplication.HandleEvent (Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmEditRec:  EditRecord;
      cmInvoice:  EditInvoice;
     else	  Exit;
      end;
    ClearEvent (Event);
    end;
end;


procedure TMyApp.InitMenuBar;
var  R: TRect;
begin
  GetExtent (R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New (PMenuBar, Init (R, NewMenu (
    NewSubMenu ('~D~ialogShop', hcMenus, NewMenu (
      NewItem ('~N~ew Record', '',   kbNoKey, cmEditRec, hcNoContext,
      NewItem ('~I~nvoice',    '',   kbNoKey, cmInvoice, hcNoContext,
      NewLine (
      NewItem ('e~X~it',  'Alt-X',   kbAltX,  cmQuit,  hcNoContext,
      nil))))),
    nil))
  ));
  MenuBar^.HelpCtx := hcMenus;
end;


procedure TMyApp.InitStatusLine;
var  R: TRect;
begin
  GetExtent (R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New (PStatusLine, Init (R,
    NewStatusDef (hcNoContext, hcDeskTop - 1,
      NewStatusKey ('~Dialog~Shop',     kbNoKey, cmNoCmd,
      nil),
    NewStatusDef (hcDeskTop, hcDialogs - 1,
      NewStatusKey ('~F10~ Menu',       kbF10,   cmMenu,
      nil),
    NewStatusDef (hcDialogs, $FFFF,
      NewStatusKey ('~Esc~ Cancel',     kbEsc,   cmCancel,
      NewStatusKey ('~Tab~ Next field', kbNoKey, cmNoCmd,
      NewStatusKey ('~Shift-Tab~ Previous', kbNoKey, cmNoCmd,
      nil))),
    nil)))
  ));
end;


procedure TMyApp.EditRecord;
var  R       : TRect;
     Dialog  : PCursorDlg;  { TDialog descendant }
     B       : PButton;
     P       : PView;
     Control : word;
     void    : word;
     Date    : DateTime;
begin
  With Date do
    begin
    GetDate (Year, Month, Day, void);
    GetTime (Hour, Min,   Sec, void);
    end;
  DeskTop^.GetExtent (R);
  Dialog := New (PCursorDlg, Init (R, 'Data Record'));
  With Dialog^ do
    begin
    HelpCtx  := hcDialogs;

    InsertField (Dialog, 5, 2, FALSE, '~N~ame:   ', ' SSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 4, FALSE, '~A~ddress:', ' SSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 6, FALSE, '~C~ity:   ', ' SSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 8, FALSE, '~S~tate:  ', ' SS');
    InsertField (Dialog, 5,10, FALSE, '~Z~ip:    ', ' #####');
    InsertField (Dialog, 9,12, TRUE,  '  ~D~ate     Time', fldDATETIME)^.SetData (Date);
    InsertField (Dialog,11,15, FALSE, '~D~eposit:', '($rrr,rrr.rr)');
    InsertField (Dialog,11,16, FALSE, '~B~alance:', '($rrr,rrr.rr)');

    R.Assign (0, 18, 10, 20);
    B := New (PButton, Init (R, 'O~K~', cmOK, bfDefault));
    B^.Options := B^.Options or ofCenterX;
    Insert (B);

    SelectNext (FALSE);
    end;

  TrimDialog (Dialog);
  Control := DeskTop^.ExecView (Dialog);

  Dispose (Dialog, Done);
end;


procedure TMyApp.EditInvoice;
const  DlgLabel = ' Item        Qty   Item Name                          Price        Total';
       DlgInfo  = ' SSSSSSSSSS\WWWW \ ssssssssssssssssssssssssssssssss\rr,rrr.rr \($rrr,rrr.rr)';
       DLabel   : string [length (DlgLabel)] = DlgLabel;
var  R       : TRect;
     P       : PView;
     Dialog  : PCursorDlg;  { TDialog descendant }
     B       : PButton;
     DMX     : PDmxEditDlg;
     Control : word;
     void    : word;
     Date    : DateTime;
     DlgData : array [0..2047] of byte;

    function  InsertRadioButtons : PView;
    var  R   : TRect;
	 P   : PView;
    begin
      R.Assign (52, 3, 75, 7);
      P := New (PRadioButtons, Init (R,
	       NewSItem ('Cash',
	       NewSItem ('Check/MO',
	       NewSItem ('Bank Card',
	       NewSItem ('Store Card',
	       nil))))
	     ));
      P^.HelpCtx := hcDialogs;
      Dialog^.Insert (P);
      InsertRadioButtons := P;
    end;

    function  InsertDmxLabels : PDmxLink;
    var R   : TRect;
	Lbl : PDmxLink;
    begin
      R.Assign (2,10, 78,11);
      Lbl := New (PDmxLabels, Init (@DLabel, R));
      Dialog^.Insert (Lbl);
      InsertDmxLabels := Lbl;
    end;

    function  InsertRecInd : PDmxLink;
    var R   : TRect;
	Ind : PDmxLink;
    begin
      R.Assign (1, pred (Dialog^.Size.Y), 8, Dialog^.Size.Y);
      Ind := New (PDmxRecInd, Init (R, 7));
      Dialog^.Insert (Ind);
      InsertRecInd := Ind;
    end;

begin
  With Date do GetDate (Year, Month, Day, void);
  fillchar (DlgData, sizeof (DlgData), 0);

  DeskTop^.GetExtent (R);
  Dialog := New (PCursorDlg, Init (R, 'Invoice Dialog'));
  With Dialog^ do
    begin
    Options  := Options or ofCentered;
    HelpCtx  := hcDialogs;

    InsertField (Dialog, 5, 2, FALSE, '~N~ame:   ', ' SSSSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 3, FALSE, '~A~ddress:', ' SSSSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 4, FALSE, '~C~ity:   ', ' SSSSSSSSSSSSSSSSSSSSSS');
    InsertField (Dialog, 5, 5, FALSE, '~S~tate:  ', ' SS');
    InsertField (Dialog, 5, 6, FALSE, '~Z~ip:    ', ' #####\####');
    InsertField (Dialog, 5, 7, FALSE, '~P~hone:  ', '~(~###~)~ ###-####');
    InsertField (Dialog, 5, 8, FALSE, '~D~ate:   ', fldDATE)^.SetData (Date);

    R.Assign (50, 2, 75, 3);
    Insert (New (PLabel, Init (R, '~M~ethod of Payment', InsertRadioButtons)));

    InsertField (Dialog, 50, 7, TRUE, 'Card Number', '####-####-####-####');


   { assign the tvDMX view }

    R.Assign (2,11, 78, Size.Y - 4);
    DMX := New (PDmxEditDlg,
		Init (DlgInfo,
		      DlgData,
		      sizeof (DlgData),
		      R,
		      InsertDmxLabels, InsertRecInd,  { local functions }
		      nil, { can be replaced with a horizontal scrollbar }
		      StandardScrollBar (sbVertical + sbHandleKeyboard)
		    ));
    Insert (DMX);
   { move vert. scrollbar to fit beside the tvDMX view }
    R.Assign (78,11, 79, Size.Y - 4);
    DMX^.VScrollBar^.Locate (R);


   { assign buttons near the bottom of the window }

    R.Assign (10, Size.Y - 3, 20, Size.Y - 1);
    Insert (New (PButton, Init (R, 'O~K~', cmOK, bfDefault)));

    R.Assign (21, Size.Y - 3, 31, Size.Y - 1);
    Insert (New (PButton, Init (R, 'Cancel', cmCancel, bfNormal)));


   { select the first view }
    SelectNext (FALSE);
    end;

  Control := DeskTop^.ExecView (Dialog);

  Dispose (Dialog, Done);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


var  MyApp: TMyApp;


Begin
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.
