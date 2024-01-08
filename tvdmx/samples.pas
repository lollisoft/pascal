
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	SAMPLES   --Multi-window sample demo program	}
{	tvDMX     --data editing project (ver 1.5)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Program SAMPLES;

{ This program was written to demonstrate various data structures.  You can
  examine the field templates and copy some portions into WORKSHOP.PAS for
  your own experiments.

  The design of some of these record structures may seem pointless since
  they are intended only to demonstrate the interface mechanism.

  The "Account" window is the simplest example here.  It's somewhat bland,
  but most programmers will only require simple data structures like this.

  The "Payroll" window is a larger data window.  It demonstrates the 'Z'
  template code, which forces the display of leading zeroes in that field.
  Its last three fields are marked as READ-ONLY (with the ^R code).  These
  are entered automatically by the virtual methods in object TDmxPayroll,
  which overrides TDmxEditor.

  The "Busy" window uses a more complex template string.  Note the heavy use
  of control codes, and that the last field in the main window is Read-Only.
  One of the integer fields is marked as a "skip" field (that means that the
  cursor will not land on it).

  The DateTime type is used here, with fldDATETIME, fldDATE, and fldTIME
  constants --as defined in the DMXGIZMA unit.  Its Year, Month and Day are
  swapped by codes in the fldDATETIME and fldDATE string to place it in its
  more familiar Month-Day-Year order.


  Two other views are available from the menu:  "Hex" is a tvDMX-driven
  hex-byte editor using the same data as Busy window;  and "Dialog" is a
  dialog box that uses tvDMX descendants for individual field input, using
  the data in the current window at the current record.  A dialog window
  may also be actuated by double-clicking a record with a mouse.

  The data in most windows can be reported to file SAMPLES.OUT, using the
  objects in unit tvDMXREP.PAS.

  (See file TVDMXHEX.PAS for the code used in the hexadecimal byte editor.)
 }

{$V-,X+ }

uses
    Dos, { required to define DateTime type }
    Objects, Drivers, Views, Menus, Dialogs, App, MsgBox,
    RSet, DmxGizma, tvDMX, StdDMX, tvDmxHex, tvDmxRep, tvGizma;

const
    ReportName    =  'SAMPLES.OUT';

    cmHasDialog   =  103;

    cmAccounts    =  111;
    cmPayroll     =  112;
    cmBusyWin     =  113;
    cmHexWin      =  114;
    cmDialog      =  115;
    cmRecDialog   =  116;
    cmReport	  =  117;

    cmNoCmd       = 1000;

    hcMenus       = 1000;
    hcDeskTop     = 1100;
    hcAccounts    = 1100;
    hcPayroll     = 1200;
    hcBusyWin     = 1300;
    hcHexWin      = 1400;
    hcDialogs     = 4000;


      { Data presentation template for the "Accounts" window.
        The data structure is declared as "TAccount" in the TYPE section.
       }

    AccountLabel : string =
	' Transaction          Debit        Credit      [?] ';

    AccountInfo  : string =
	' SSSSSSSSSSSSSSSS| rrr,rrr.rr  | rrr,rrr.rr  | [x] ';



      { Data presentation template for the "Payroll" window.
	The data structure is declared as "TPayroll" in the TYPE section.
	The last three fields are marked READ-ONLY, and are automatically
	entered by the virtual methods in object TDmxPayroll.
       }

    _PayrollLabel = ' Employee                ID     Earnings       FICA        FITW        SITW   ';
    _PayrollInfo  = ' ssssssssssssssssssssss| ZZW บ $rr,rrr.rr | $r,rrr.rr '^R'| $r,rrr.rr '^R'| $r,rrr.rr '^R;

    PayrollLabel  :  string [length (_PayrollLabel)]  =  _PayrollLabel;
    PayrollInfo   :  string [length (_PayrollInfo)]   =  _PayrollInfo;



      { This next screen will be a bit busy, but I wanted to implement many
	of the special options.  I thought it might be easier to decipher
	all this by separating the field-strings into one field per line.
	Each line (except the first) begins with a field delimiter.
	The data structure is declared as "TBusyData" in the TYPE section.
       }

    _BusyLabel    =
	' Name                    SSN             Balance     Start Date   Time   <A>  [B]   Pointer       Value     RO ';

    _BusyInfo     =   'B' + ^H		{ hidden byte field }
		    + #0' ssssssssssssssssssssss'  { Name field }
		    + '| ###-##-#### '	{ string of numerics only }
		    + '|r,rrr,rrr    '	{ positive or negative }

		   { DateTime type: }
		    + '|' + fldDATE	{ untyped constants }
		    + #0  + fldTIME	{ defined in DMXGIZMA.PAS }

		    + '|iii ' + ^Z^R^S	{ showzeroes/readonly/skip }
		    + '\iii '		{ normal integer }
		    + '| HHHH:HHHH '	{ hex longint value }
		    + '|RRR,RRR.RRR '	{ positive values only }
		    + '| hh ' + ^Z^R;	{ showzeroes/readonly field }


    BusyInfo      :  string [length (_BusyInfo)]  =  _BusyInfo;
    BusyLabel     :  string [length (_BusyLabel)] =  _BusyLabel;


    MaxRecordNum  =   29;



type
    PAccount      = ^TAccount;
    PPayroll      = ^TPayroll;
    PBusyData     = ^TBusyData;


    TAccount      =  RECORD
	Account	:  string [16];
	Debit	:  real;
	Credit	:  real;
	Status	:  boolean;
    end;


    TPayroll      =  RECORD
	Employee :  string [22];
	ID       :  word;
	Earnings :  real;
	FICA     :  real;  { READ-ONLY }
	FITW     :  real;  { READ-ONLY }
	SITW     :  real;  { READ-ONLY }
    end;


    TBusyData     =  RECORD
	Marker		:  byte;	{ HIDDEN field }
	Name		:  string [22];
	SSN		:  string [9];
	realfield1	:  real;
	DT		:  datetime;
	intfield0	:  integer;	{ READ-ONLY }
	intfield1	:  integer;
	ptrfield	:  pointer;
	realfield2	:  real;
	hextwo		:  byte;	{ READ-ONLY }
    end;


    PDmxEditTbl     = ^TDmxEditTbl;
    PDmxEditTblWin  = ^TDmxEditTblWin;


    TDmxEditTbl     =  OBJECT (TDmxEditor)
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
    end;


    TDmxEditTblWin  =  OBJECT (TDmxWindow)
      procedure InitDMX (ATemplate : string;  var AData;
                         ALabels, ARecInd : PDmxLink;
                         BSize  : longint);  VIRTUAL;
    end;


    PDmxPayroll    = ^TDmxPayroll;
    PDmxPayrollWin = ^TDmxPayrollWin;


    TDmxPayroll    =  OBJECT (TDmxEditTbl)
      procedure EvaluateField;  VIRTUAL;
      procedure ZeroizeField (Whole :boolean; Field :pDMXfieldrec);  VIRTUAL;
      procedure RecalcRecord;
    end;


    TDmxPayrollWin =  OBJECT (TDmxWindow)
      procedure InitDMX (ATemplate : string;  var AData;
                         ALabels, ARecInd : PDmxLink;
                         BSize  : longint);  VIRTUAL;
    end;


    PMyStatusLine  = ^TMyStatusLine;
    TMyStatusLine  =  OBJECT (TStatusLine)
      function  Hint (AHelpCtx : word) : string;  VIRTUAL;
    end;


    TAppN          =  OBJECT (TAppA)
    end;


    TMyApp         =  OBJECT (TAppA)
      constructor Init;
      procedure Idle;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure InitMenuBar;     VIRTUAL;
      procedure InitStatusLine;  VIRTUAL;
      procedure AccountWindow;
      procedure PayrollWindow;
      procedure BusyWindow;
      procedure HexWindow;
      procedure AccountDialog (P : PDmxEditTbl);
      procedure PayrollDialog (P : PDmxPayroll);
      procedure BusyDialog (P : PDmxEditTbl);
    end;


var
    Accounts   :  array [0..49] of TAccount;
    Payroll    :  array [0..49] of TPayroll;
    BusyData   :  array [0..MaxRecordNum] of TBusyData;


  procedure InitializeData;  forward;  { for the sample data }


  { ออ TMyStatusLine อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TMyStatusLine.Hint (AHelpCtx : word) : string;
begin
  Case AHelpCtx of
    hcDragging:  Hint := #24#25#26#27' Move  Shift-'#24#25#26#27' Resize  '#17#196#217' Done  Esc Cancel';
   else          Hint := '';
    end;
end;


  { ออ TDmxEditTbl อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxEditTbl.HandleEvent (var Event : TEvent);
begin
  With Event do
    If ((What = evMouseDown) and Double and DoubleValid) or
       ((What = evCommand) and (Command = cmDialog))
     then
      begin
      Message (Application, evCommand, cmRecDialog, @Self);
      ClearEvent (Event);
      end
     else
      If (What = evCommand) and (Command = cmHasDialog) then
	ClearEvent (Event)
       else
	TDmxEditor.HandleEvent (Event);
end;


  { ออ TDmxEditTblWin ออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxEditTblWin.InitDMX (ATemplate : string;  var AData;
				  ALabels, ARecInd : PDmxLink;
				  BSize  : longint);
{ To override TDmxEditor (as does object TDmxEditTbl above), you need
  to override a TDmxWindow object to insert the new object.
 }
var  R  : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);

  Insert (New (PDmxEditTbl, Init (ATemplate, AData, BSize, R,
				ALabels, ARecInd,
				StandardScrollBar (sbHorizontal+ sbHandleKeyboard),
				StandardScrollBar (sbVertical  + sbHandleKeyboard))));

end;


  { ออ TDmxPayroll อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxPayroll.EvaluateField;
{ virtual method called after a field is edited }
begin
  TDmxEditor.EvaluateField;
  If (CurrentField^.fieldnum = 3) and FieldAltered then RecalcRecord;
end;


procedure TDmxPayroll.ZeroizeField (Whole : boolean; Field : pDMXfieldrec);
{ virtual method called to clear a field }
begin
  TDmxEditor.ZeroizeField (Whole, Field);
  If (Field^.fieldnum = 3) then RecalcRecord;
end;


procedure TDmxPayroll.RecalcRecord;
{ new method to follow up on changes }
begin
  With Payroll [CurrentRecord] do
    begin
    FICA  := Earnings * 0.075;
    FITW  := Earnings * 0.28;
    SITW  := Earnings * 0.05;
    end;
  RedrawRecord := TRUE;  { forces entire record to be redrawn }
end;


  { ออ TDmxPayrollWin ออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxPayrollWin.InitDMX (ATemplate : string;  var AData;
                                  ALabels, ARecInd : PDmxLink;
                                  BSize  : longint);
{ To override TDmxEditor (as does object TDmxPayroll above), you need
  to override a TDmxWindow object to insert the new object.
 }
var  R  : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);

  Insert (New (PDmxPayroll, Init (ATemplate, AData, BSize, R,
				  ALabels, ARecInd,
				  StandardScrollBar (sbHorizontal+ sbHandleKeyboard),
				  StandardScrollBar (sbVertical  + sbHandleKeyboard))));

end;


  { ออ TMyApp ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TMyApp.Init;
begin
  TAppA.Init;
  MenuBar^.HelpCtx := hcMenus;
  DeskTop^.HelpCtx := hcDeskTop;
  InitializeData;  { initialize the sample data }

  { Open the first 4 selections }
  AccountWindow;
  PayrollWindow;
  BusyWindow;
  HexWindow;

  DeskTop^.SelectNext (FALSE);  { change back to account window }

  MessageBox (^C'Sample Data Editors'^M^M^C'tvDMX (c) 1992  Randolph Beck',
		nil, mfInformation + mfOKButton);

end;


procedure TMyApp.Idle;
begin
  TAppA.Idle;
  If (Message (DeskTop, evCommand, cmHasDialog, @Self) <> nil) then
    EnableCommands ([cmDialog,cmReport])
   else
    begin
    DisableCommands ([cmDialog]);
    If (Message (DeskTop, evCommand, cmDMX_RollCall, @Self) <> nil) then
      EnableCommands ([cmReport])
     else
      DisableCommands ([cmReport]);
    end;
end;


procedure TMyApp.HandleEvent (var Event : TEvent);

    procedure DoRecDialog;
    var  P : PDmxEditTbl;
    begin
      P := Event.InfoPtr;
      If (P <> nil) then
	begin
	If (P^.WorkingData = @Accounts) then AccountDialog (P)
	else
	If (P^.WorkingData = @Payroll)  then PayrollDialog (PDmxPayroll (P))
	else
	If (P^.WorkingData = @BusyData) then BusyDialog (P);
	end;
    end;

    procedure DoReport;
    var  P      : PDmxScroller;
         Report : PDmxReportFile;
    begin
      P := Message (DeskTop, evCommand, cmDMX_RollCall, @Self);
      If (P <> nil) then
	begin
	{ use different record numbering format for hex listings }
	If (Message (DeskTop, evCommand, cmHasDialog, @Self) <> nil) then
	  Report := New (PDmxReportFile, Init (P, '|', TRUE, 50,78, ReportName))
	 else
	  Report := New (PDmxReportHexFile, Init (P, ' ', TRUE, 48,78, ReportName));
	DmxReportBox ('Working',
		      'Processing report...'^M^M^C'File '+ ReportName, Report);
        end;
    end;

begin
  TAppA.HandleEvent (Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmAccounts:	AccountWindow;
      cmPayroll:	PayrollWindow;
      cmBusyWin:	BusyWindow;
      cmHexWin:		HexWindow;
      cmRecDialog:	DoRecDialog;
      cmReport:		DoReport;
     else
      Exit;
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
    NewSubMenu ('~S~amples', hcMenus, NewMenu (
      NewItem ('~A~ccounts', '',    kbNoKey, cmAccounts,hcMenus,
      NewItem ('~P~ayroll',  '',    kbNoKey, cmPayroll, hcMenus,
      NewItem ('~B~usy',     'F4',  kbF4,    cmBusyWin, hcMenus,
      NewItem ('~H~ex',      '',    kbNoKey, cmHexWin,  hcMenus,
      NewLine (
      NewItem ('~D~ialog',   'F2',  kbF2,    cmDialog,  hcMenus,
      NewItem ('~R~eport',   'F9',  kbF9,    cmReport,  hcMenus,
      NewLine (
      NewItem ('e~X~it',   'Alt-X', kbAltX,  cmQuit,    hcMenus,
      nil)))))))))),
    NewSubMenu ('~W~indow', hcMenus, NewMenu (
      NewItem ('~S~ize/Move', 'Ctrl-F5', kbCtrlF5, cmResize, hcMenus,
      NewItem ('~Z~oom',      'F5',  kbF5,    cmZoom,    hcMenus,
      NewItem ('~T~ile',      '',    kbNoKey, cmTile,    hcMenus,
      NewItem ('C~a~scade',   '',    kbNoKey, cmCascade, hcMenus,
      NewItem ('~N~ext',      'F6',  kbF6,    cmNext,    hcMenus,
      NewItem ('~P~revious',  'Shift-F6', kbShiftF6, cmPrev, hcMenus,
      NewItem ('~C~lose', 'Alt-F3',  kbAltF3, cmClose,   hcMenus,
      NewLine (
      NewItem ('~U~ser screen', 'Alt-F5',  kbAltF5, cmUserScreen, hcMenus,
      nil)))))))))),
    NewSubMenu ('~O~ptions', hcMenus, NewMenu (
      NewSoundItem (hcMenus,
      NewVideoItem (hcMenus,
      nil))),
    nil)
  )))));
end;


procedure TMyApp.InitStatusLine;
var  R: TRect;
begin
  GetExtent (R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New (PMyStatusLine, Init (R,
    NewStatusDef (hcNoContext, hcDeskTop - 1,
      NewStatusKey ('tv~DMX~', kbNoKey, cmNoCmd,
      nil),
    NewStatusDef (hcDeskTop, hcDialogs - 1,
      NewStatusKey ('~F10~ Menu',     kbF10,   cmMenu,
      NewStatusKey ('~F2~ Dialog',    kbF2,    cmDialog,
      NewStatusKey ('~F5~ Zoom',      kbF5,    cmZoom,
      NewStatusKey ('~F6~ Next',      kbF6,    cmNext,
      nil)))),
    NewStatusDef (hcDialogs, $FFFF,
      NewStatusKey ('~Esc~ Cancel',   kbEsc,   cmCancel,
      nil),
    nil)))
  ));
end;


procedure TMyApp.AccountWindow;
var  R  : TRect;
     W  : PDmxWindow;
begin
  R.Assign (0, 0, length (AccountLabel) + 2, 9);
  R.Move (Random (26), Random (14));
  W := New (PDmxEditTblWin, Init (R,	{ window rectangle }
		'Accounts',		{ window title }
		NextWindowNumber,	{ window number }
		AccountInfo,		{ template string }
		Accounts,		{ data records }
		sizeof (Accounts),	{ data size }
		AccountLabel,		{ heading label }
		7));			{ indicator width }
  W^.HelpCtx := hcAccounts;
  DeskTop^.Insert (ValidView (W));
end;


procedure TMyApp.PayrollWindow;
var  R  : TRect;
     W  : PDmxWindow;
begin
  R.Assign (0, 0, 58, 11);
  R.Move (Random (22), Random (12));
  W := New (PDmxPayrollWin, Init (R,	{ window rectangle }
		'Payroll',		{ window title }
		NextWindowNumber,	{ window number }
		PayrollInfo,		{ template string }
		Payroll,		{ data records }
		sizeof (Payroll),	{ data size }
		PayrollLabel,		{ heading label }
		7));			{ indicator width }
  W^.HelpCtx := hcPayroll;
  DeskTop^.Insert (ValidView (W));
end;


procedure TMyApp.BusyWindow;
var  R  : TRect;
     W  : PDmxWindow;
begin
  R.Assign (0, 0, 41, 15);
  R.Move (Random (39), Random (8));
  W := New (PDmxEditTblWin, Init (R,	{ window rectangle }
		'Busy Window',		{ window title }
		NextWindowNumber,	{ window number }
		BusyInfo,		{ template string }
		BusyData,		{ data records }
		sizeof (BusyData),	{ data size }
		BusyLabel,		{ heading label }
		10));			{ indicator width }
  W^.HelpCtx := hcBusyWin;
  DeskTop^.Insert (ValidView (W));
end;


procedure TMyApp.HexWindow;
{ uses objects in file tvDMXHEX.PAS }
var  R  : TRect;
     W  : PDmxWindow;
begin
  R.Assign (0, 0, length (HexLabels) + 2, 10);
  W := New (PDmxHexWin, Init (R, 'Hex Window', NextWindowNumber,
			      BusyData, sizeof (BusyData)));
  W^.HelpCtx := hcHexWin;
  W^.Options := W^.Options or ofCentered;
  DeskTop^.Insert (ValidView (W));
end;


procedure TMyApp.AccountDialog (P : PDmxEditTbl);
var  R       : TRect;
     Dialog  : PCursorDlg;
     B       : PButton;
     A       : string;
     Control : word;
begin
  Str (succ (P^.CurrentRecord), A);
  DeskTop^.GetExtent (R);
  Dialog := New (PCursorDlg, Init (R, 'Account Record #' + A));
  If (Dialog <> nil) then
    begin
    With Dialog^ do
      begin
      HelpCtx  := hcDialogs;
      InsertField (Dialog, 5,2, TRUE,  ' ~T~ransaction', ' SSSSSSSSSSSSSSSS');
      InsertField (Dialog, 2,5, TRUE,  '    ~D~ebit        Credit', ' rrr,rrr.rr  \ rrr,rrr.rr  ');
      InsertField (Dialog, 6,8, FALSE, '~S~tatus: ', '~[Cleared]~'^X);
      R.Assign (0, 10, 10, 12);
      B := New (PButton, Init (R, 'O~K~', cmOK, bfDefault));
      B^.Options := B^.Options or ofCenterX;
      Insert (B);
      SelectNext (FALSE);
      SetData (Accounts [P^.CurrentRecord]);
      end;
    TrimDialog (Dialog);
    Control := DeskTop^.ExecView (Dialog);
    If (Control = cmOK) then
      begin
      { return record to table }
      Dialog^.GetData (Accounts [P^.CurrentRecord]);
      { redraw all windows that use Accounts }
      Message (DeskTop, evBroadcast, cmDMX_DrawData, @Accounts);
      end;
    Dispose (Dialog, Done);
    end;
end;


procedure TMyApp.PayrollDialog (P : PDmxPayroll);
var  R       : TRect;
     Dialog  : PCursorDlg;
     B       : PButton;
     A       : string;
     Control : word;
begin
  Str (succ (P^.CurrentRecord), A);
  DeskTop^.GetExtent (R);
  Dialog := New (PCursorDlg, Init (R, 'Employee Record #' + A));
  If (Dialog <> nil) then
    begin
    With Dialog^ do
      begin
      HelpCtx  := hcDialogs;
      InsertField (Dialog, 2,2, FALSE, '~N~ame: ', ' ssssssssssssssssssssss');
      InsertField (Dialog, 2,4, FALSE, '~I~D Number: ', ' ZZW ');
      InsertField (Dialog, 2,6, FALSE, '~E~arnings: ', ' $rr,rrr.rr ');
      InsertField (Dialog, 0,0, FALSE, '', 'r'^H#0'r'^H#0'r'^H)^.Hide;
      R.Assign (0, 8, 10, 10);
      B := New (PButton, Init (R, 'O~K~', cmOK, bfDefault));
      B^.Options := B^.Options or ofCenterX;
      Insert (B);
      SelectNext (FALSE);
      SetData (Payroll [P^.CurrentRecord]);
      end;
    TrimDialog (Dialog);
    Control := DeskTop^.ExecView (Dialog);
    If (Control = cmOK) then
      begin
      { return record to table }
      Dialog^.GetData (Payroll [P^.CurrentRecord]);
      P^.RecalcRecord;
      { redraw all windows that use Payroll }
      Message (DeskTop, evBroadcast, cmDMX_DrawData, @Payroll);
      end;
    Dispose (Dialog, Done);
    end;
end;


procedure TMyApp.BusyDialog (P : PDmxEditTbl);
var  R       : TRect;
     Dialog  : PCursorDlg;
     B       : PButton;
     A       : string;
     Control : word;
begin
  Str (succ (P^.CurrentRecord), A);
  DeskTop^.GetExtent (R);
  Dialog := New (PCursorDlg, Init (R, 'Busy Record #' + A));
  If (Dialog <> nil) then
    begin
    With Dialog^ do
      begin
      HelpCtx  := hcDialogs;

      { The Read-Only and Hidden fields are also inserted into this view
	so that the entire BusyInfo record structure is transferable.
	They can be hidden using TView^.Hide() because InsertField() is
	a function that returns a PView pointer --as demonstrated in the
	following instance...
       }
      InsertField (Dialog, 0,  0, FALSE, '',  'B')^.Hide;
      InsertField (Dialog, 2,  2, FALSE, '~N~ame:    ',  ' ssssssssssssssssssssss');
      InsertField (Dialog, 2,  4, FALSE, '~S~SN:     ',  ' ###-##-#### ');
      InsertField (Dialog, 2,  6, FALSE, '~B~alance: ',  '($rrr,rrr.rr)');
      InsertField (Dialog,11,  8, TRUE,  '  ~D~ate         Time', fldDATETIME);
      InsertField (Dialog, 0,  0, FALSE, '',  'i')^.Hide;
      InsertField (Dialog, 2, 11, FALSE, '~I~nteger: ',  'iii');
      InsertField (Dialog, 2, 13, FALSE, '~P~ointer: ',  ' HHHH:HHHH ');
      InsertField (Dialog, 2, 14, FALSE, '~V~alue:   ',  'RRR,RRR.RRR ~pts~ ');
      InsertField (Dialog, 0,  0, FALSE, '',  'B')^.Hide;

      R.Assign (0, 16, 10, 18);
      B := New (PButton, Init (R, 'O~K~', cmOK, bfDefault));
      B^.Options := B^.Options or ofCenterX;
      Insert (B);
      SelectNext (FALSE);
      SetData (BusyData [P^.CurrentRecord]);
      end;

    TrimDialog (Dialog);
    Control := DeskTop^.ExecView (Dialog);
    If (Control = cmOK) then
      begin
      { return record to table }
      Dialog^.GetData (BusyData [P^.CurrentRecord]);
      { redraw all windows that use BusyData }
      Message (DeskTop, evBroadcast, cmDMX_DrawData, @BusyData);
      end;
    Dispose (Dialog, Done);
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure InitializeData;
{ creates test data }
var  i,j  : integer;
begin
  fillchar (Accounts,  sizeof (Accounts),  0);
  fillchar (Payroll,   sizeof (Payroll),   0);
  fillchar (BusyData,  sizeof (BusyData),  0);

  BusyData [00].Name := 'Abigail Adams';
  BusyData [01].Name := 'Betty Boop';
  BusyData [02].Name := 'Clark Clifford';
  BusyData [03].Name := 'Dana Delaney';
  BusyData [04].Name := 'Elbert Eagleton';
  BusyData [05].Name := 'Farrah Fawcett';
  BusyData [06].Name := 'Ginger Grant';
  BusyData [07].Name := 'Hugh Hefner';
  BusyData [08].Name := 'Inga Ingersol';
  BusyData [09].Name := 'John Jay';
  BusyData [10].Name := 'Katie Kingfield';
  BusyData [11].Name := 'Lois Lane';
  BusyData [12].Name := 'Melissa Manchester';
  BusyData [13].Name := 'Nancy Nichols';
  BusyData [14].Name := 'Oscar O''Malley';
  BusyData [15].Name := 'Paula Prentiss';
  BusyData [16].Name := 'Quincy Quenton';
  BusyData [17].Name := 'Rita Rudner';
  BusyData [18].Name := 'Samantha Stevens';
  BusyData [19].Name := 'Tina Turner';
  BusyData [20].Name := 'Ute Ueberroth';
  BusyData [21].Name := 'Vicky Vail';
  BusyData [22].Name := 'Wendy Wilson';
  BusyData [23].Name := 'Xavier X. Xylvert';
  BusyData [24].Name := 'Yamaha Yesta';
  BusyData [25].Name := 'Zorro';

  For i := 0 to MaxRecordNum do
    begin
    BusyData [i].intfield0  := i;
    BusyData [i].hextwo     := lo (i);
    If i < 26 then
      begin
      BusyData [i].intfield1	:= Mem [0:i];
      BusyData [i].ptrfield	:= pointer (MemL [0:i shl 2]);
      BusyData [i].realfield1	:= random (200) * random (200) / succ (random (199));
      BusyData [i].realfield2	:= random (200) * random (200) / succ (random (199));

      BusyData [i].DT.Year	:= 1988 + random (4);
      BusyData [i].DT.Month	:= succ (random (12));
      BusyData [i].DT.Day	:= succ (random (28));

      BusyData [i].DT.Hour	:= random (24);
      BusyData [i].DT.Min	:= random (60);
      BusyData [i].DT.Sec	:= random (60);

      BusyData [i].SSN [0]	:= #9;
      For j := 1 to 9 do BusyData [i].SSN [j] := chr (random (10) + 48);
      end;
    end;

  BusyData [0].SSN  := '';

  Accounts [0].Account := 'ACME TOOL CO.';
  Accounts [1].Account := 'READING R. R.';
  Accounts [2].Account := 'EXXON CORP.';
  For i := 0 to 2 do With Accounts [i] do
    begin
    Debit   := Random (50000) * 0.9;
    Credit  := Random (50000) * 0.9;
    Status  := (Credit > Debit);
    end;

  Payroll [0].Employee := 'Alex Trebek';
  Payroll [1].Employee := 'Pat Sajak';
  Payroll [2].Employee := 'Vanna White';
  Payroll [3].Employee := 'Merv Griffin';
  For i := 0 to 3 do With Payroll [i] do
    begin
    ID		:=  Random (400);
    Earnings	:=  Random (3000) + 4000.0;
    FICA	:=  Earnings * 0.075;
    FITW	:=  Earnings * 0.28;
    SITW	:=  Earnings * 0.05;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

var  MyApp  : TMyApp;

Begin
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
End.
