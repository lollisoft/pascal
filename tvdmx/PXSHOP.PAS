
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{                                                       }
{       PxSHOP    --PxDMX Demonstration Program		}
{       tvDMX     --data editing project (ver 2.x)      }
{							}
{       Copyright (c) 1992-96   Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

{

    The TDmxParadox object has been derived into object TDmxPx, which has
    these extensions:

      procedure HandleEvent() sends a kbCtrlY keycode when a cmDeleteRec
	command is received; and calls its CurrentRecordWindow procedure
	when the cmRecordWin command is received.

      function FieldTemplate() modifies the template for the field labeled
	"Identification" to display up to 3 leading zeroes.

      procedure SetFieldName() displays "Identification" as "ID".

      function  Valid() displays the current record and asks for user
      confirmation before deleting the record.


    The TDmxMyApp.OpenTable method creates PXSHOP.DB and PXSHOP.PX files
    using the PxBuild procedure if a table is not found.  Make sure that
    you delete these files to experiment with a different table structure.

    The PxBuild procedure is located in unit PxGIZMA.
}

Program PXSHOP;

{$V-,X+,N+,E+ }

uses  Objects, Drivers, Views, Menus, Dialogs, App, MsgBox, PxEngine, PxMsg,
      RSet, DmxGizma, tvGizma, tvDMX, DmxForms, StdDMX, tvDMXBUF, tvDMXREP,
      PxDMX, PxGizma;

const
    TableName	=  'PXSHOP';

    cmNoCmd	=   100;
    cmOpenPxWin =   111;
    cmDeleteRec =   112;
    cmRecordWin	=   113;
    cmPrint	=   114;


type
    PDmxPx	= ^TDmxPx;
    TDmxPx	=  OBJECT(TDmxParadox)
      procedure HandleEvent(var Event: TEvent);   VIRTUAL;
      function	FieldTemplate(FName,FType: NameString) : string;  VIRTUAL;
      procedure SetFieldName(var FName: string);  VIRTUAL;
      function  Valid(Command: word) : boolean;   VIRTUAL;
    end;


    TAppN	=  OBJECT(TAppA)
    end;

    TMyApp	=  OBJECT(TAppN)
	TblHandle  :  TableHandle;
	RecHandle  :  RecordHandle;
	Status	   :  integer;
      constructor Init;
      destructor  Done;  VIRTUAL;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
      procedure InitStatusLine;  VIRTUAL;
      procedure OpenWindow;
      function	OpenTable(FName : string)  : boolean;
      procedure CloseTable;
    end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxPx.HandleEvent(var Event: TEvent);

    procedure CurrentRecordWindow;
    var  A  : string;
	 FS : boolean;
	 L  : longint;
    begin
      If not RecordSelected then Exit;
      FS := FieldSelected;
      If FS then EvaluateField;
      If Appending then
	A := 'New Record'
       else
	begin
	L := succ(CurrentRecord);
	FormatStr(A, 'Record #%d', L);
	end;
      If (EntryBox(A, RecordData, mfOKCancel,
		PxEntryFields(TableHandle(WorkingData^), @Self)) = cmOK)
       then
	begin
	ChangeMade;
	ModifiedFields := [1..255];
	KeyAltered := TRUE;
	RedrawRecord := TRUE;
	EvaluateRecord;
	SetupRecord;
	If FS then SetupField;
	end
       else
	If FS then SetupField;
    end;

begin
  inherited HandleEvent(Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmDeleteRec:	Message(Desktop, evCommand, cmDMX_ZeroizeRecord, @Self);
      cmRecordWin:	CurrentRecordWindow;
     else
      Exit;
      end;
    ClearEvent(Event);
    end;
end;


function  TDmxPx.FieldTemplate(FName,FType: NameString) : string;
begin
  If (FName = 'Identification') and (FType = 'S') then { 'S'=Integer for PX }
    FieldTemplate := 'IZZI '
   else
    FieldTemplate := inherited FieldTemplate(FName,FType);
end;


procedure TDmxPx.SetFieldName(var FName: string);
begin
  If (FName = 'Identification') then FName := 'ID';
end;


function  TDmxPx.Valid(Command: word) : boolean;
{ verifies Yes or No before deleting a record }
var  V : boolean;
begin
  V := inherited Valid(Command);
  If V and (Command = cmDMX_ZeroizeRecord) and not Appending then
    begin
    If (EntryBox('Delete this Record?', RecordData,
		mfYesButton + mfNoButton + mfViewOnly,
		PxEntryFields(TableHandle(WorkingData^), @Self)) <> cmYes)
     then
      V := FALSE;
    end;
  Valid := V;
end;


  { ออ TMyApp ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TMyApp.Init;
begin
  TAppN.Init;

  If not OpenTable(TableName) then
    begin
    DisableCommands([cmOpenPxWin, cmDeleteRec, cmRecordWin, cmPrint]);
    PxErrorBox(Status);
    end;

end;


destructor TMyApp.Done;
begin
  TAppN.Done;
  CloseTable;
end;


procedure TMyApp.HandleEvent(var Event: TEvent);
begin
  TAppN.HandleEvent(Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmOpenPxWin:	OpenWindow;
      cmPrint:		PrnCurrentDMX;
     else
      Exit;
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
    NewSubMenu('~P~xShop', hcNoContext, NewMenu(
      NewItem('~O~pen',    'F4',      kbF4,	cmOpenPxWin, hcNoContext,
      NewItem('~C~lose',   'Alt-F3',  kbAltF3,	cmClose,   hcNoContext,
      NewLine(
      NewItem('~P~rint',   'Alt-F9',  kbAltF9,	cmPrint,   hcNoContext,
      NewLine(
      NewItem('e~X~it',    'Alt-X',   kbAltX,	cmQuit,    hcNoContext,
      nil))))))),
    NewSubMenu('~R~ecords', hcNoContext, NewMenu(
      NewItem('~E~dit',    'F2',      kbF2,	cmRecordWin, hcNoContext,
      NewItem('~D~elete',    'Ctrl-Y',	kbCtrlY, cmDMX_ZeroizeRecord, hcNoContext,
      nil))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      StdWindowMenuItems(
      nil)),
    nil)
  )))));
end;


procedure TMyApp.InitStatusLine;
var  R	: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F10~ Menu',     kbF10,   cmMenu,
      NewStatusKey('~F5~ Zoom',      kbF5,    cmZoom,
      NewStatusKey('~F6~ Next',      kbF6,    cmNext,
      nil))),
    nil)
  ));
end;


procedure TMyApp.OpenWindow;
var  R	    : TRect;
     W	    : PWindow;
begin
  { Reminder:  The table used for TblHandle must already be open. }

  AssignWinRect(R, 0,0);
  New(W, Init(R, TableName, wnNextAvail));
  With W^ do
    begin
    W^.Options := W^.Options or ofTileable;
    GetExtent(R);		  { create new rectangle for editor object }
    R.Grow(-1,-1);			      { shrink -1 to avoid borders }
    Inc(R.A.Y, 2);			 { make room for TDmxLabels object }
    Insert(New(PDmxPx,
	  Init(TblHandle,				    { working data }
		0,					  { 0 = expandable }
		R,					{ view's rectangle }
		New(PDmxPxLabels, InitInsert(W)),	    { label string }
		New(PDmxExpRecInd,InitInsert(W, 15)),{ indicator width }
		W^.StandardScrollBar(sbHorizontal),
		W^.StandardScrollBar(sbVertical)
		)
      ));
    end;
  DeskTop^.Insert(ValidView(W));
end;


function  TMyApp.OpenTable(FName : string)  : boolean;
var  B	 : boolean;
begin
  OpenTable := FALSE;
  Status := PxTblExist(FName, B);
  If (Status = PxSuccess) then
    begin
    If not B then
      begin
      Status := PxBuild(TableName,
			NewSItem('Last Name, A20*',
			NewSItem('First Name, A20*',
			NewSItem('Identification, S*+',
			NewSItem('Mode, S',
			NewSItem('Date, D+',
			NewSItem('Balance, $',
			NewSItem('Debit, $',
			NewSItem('Credit, $',
			NewSItem('Status, S',
				nil)))))))))
			);
      end;
    If (Status = PxSuccess) then
      begin
      Status := PxTblOpen(FName, TblHandle, 0, TRUE);
      OpenTable := (Status = PxSuccess);
      end;
    end;
  If (Status <> PxSuccess) then TblHandle := 0;
end;


procedure TMyApp.CloseTable;
begin
  If (TblHandle <> 0) then
    begin
    Status := PxRecBufClose(RecHandle);
    Status := PxTblClose(TblHandle);
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

var
    MyApp	:  TMyApp;

Begin
  PX(PxInit);
  fldPxNUMBER := 'rr,rrr,rrr.rrr';

  MyApp.Init;
  MyApp.Run;
  MyApp.Done;

  PX(PxExit);
End.
