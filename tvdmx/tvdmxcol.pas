
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	tvDMXCOL  --Collection Data Editing Unit	}
{	tvDMX     --data editing project		}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit tvDMXCOL;

{$B-,D-,R-,O+,X+,V- }

interface

uses
    Objects, Drivers, Memory, Views, App, MsgBox,
    RSet, DmxGizma, tvDMX, StdDMX;

const
    cmDMX_Reset		=  cmDMX + 49;

type
    PDmxCollectView	= ^TDmxCollectView;
    PDmxCollector	= ^TDmxCollector;
    PDmxCollectViewWin	= ^TDmxCollectViewWin;
    PDmxCollectorWin	= ^TDmxCollectorWin;


    TDmxCollectView	=  OBJECT (TDmxScroller)
      constructor Init (ATemplate : string;  var AData;
			var Bounds : TRect;  ALabels : PView;
			AHScrollBar,AVScrollBar : PScrollBar);
      procedure InitData (var AData );  VIRTUAL;
      procedure SetState (AState : word; Enable : boolean);  VIRTUAL;
      function  DataAt (RecNum : integer) : pointer;  VIRTUAL;
    end;


    TDmxCollector	=  OBJECT (TDmxEditor)
        NewDataRec : pointer;
        MaxCount   : integer;
        MemWarning : boolean;
      procedure LoadStruct (var S : TStream);  VIRTUAL;
      procedure StoreStruct (var S : TStream);  VIRTUAL;
      procedure InitData (var AData );  VIRTUAL;
      procedure InitNewDataRec;
      procedure DoneData;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      function  Valid (Command : word) : boolean;  VIRTUAL;
      procedure SetState (AState : word; Enable : boolean);  VIRTUAL;
      function  DataAt (RecNum : integer) : pointer;  VIRTUAL;
      procedure SetupRecord;  VIRTUAL;
      procedure EvaluateRecord;  VIRTUAL;
      procedure ZeroizeRecord;  VIRTUAL;
    end;


    TDmxCollectViewWin	=  OBJECT (TDmxViewer)
      constructor Init (var Bounds : TRect;  ATitle : TTitleStr;
			ANumber : integer;  ATemplate : string;
			ACollection : PCollection;  var ALabels : string);
      procedure InitDMX (ATemplate  : string;  var AData;
                         ALabels, ARecInd  : PDmxLink;
                         BSize  : longint);  VIRTUAL;
    end;


    TDmxCollectorWin	=  OBJECT (TDmxWindow)
      constructor Init (var Bounds : TRect;
		   ATitle    : TTitleStr;  ANumber  : integer;
		   ATemplate : string;  ACollection : PCollection;
		   BSize     : integer; var ALabels : string; IndLen : integer);
      procedure InitDMX (ATemplate  : string;  var AData;
                         ALabels, ARecInd  : PDmxLink;
                         BSize  : longint);  VIRTUAL;
    end;



  function  fldObjectVMT (Obj : PObject) : string;
    { template prefix to generate a VMT identifier
      for collections of TObject derivatives
     }

  procedure ResetCollection (Collection : PCollection);
    { adjust the size of the database }


implementation

  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  fldObjectVMT (Obj : PObject) : string;
begin
  fldObjectVMT := ^H'c'^V + pchar(Obj)^ + #0^H'c'^V + pstring(Obj)^[1] + #0;
  Dispose (Obj, Done);
end;


procedure ResetCollection (Collection : PCollection);
{ adjust the size of the database }
begin
  Repeat
  Until (Message (DeskTop, evBroadcast, cmDMX_Reset, Collection) = nil)
     or (Collection^.Count > 0);
  Message (DeskTop, evCommand, cmDMX_Reset, Collection);
end;


  { ออ TDmxCollectView อออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxCollectView.Init (ATemplate	: string;  var AData;
				  var Bounds	: TRect;
				  ALabels	: PView;
				  AHScrollBar,AVScrollBar : PScrollBar);
begin
  TDmxScroller.Init (ATemplate, AData, 0, Bounds, ALabels, AHScrollBar, AVScrollBar);
end;


procedure TDmxCollectView.InitData (var AData );
begin
  TDmxScroller.InitData (AData);
  DataBlockSize := (RecordSize * PCollection (WorkingData)^.Count);
end;


procedure TDmxCollectView.SetState (AState : word; Enable : boolean);
begin
  If Enable and (AState = sfFocused) and
    (DataBlockSize <> RecordSize * PCollection (WorkingData)^.Count) then
    DataBlockSize := RecordSize * PCollection (WorkingData)^.Count;
  TDmxScroller.SetState (AState, Enable);
end;


function  TDmxCollectView.DataAt (RecNum : integer) : pointer;
begin
  If (PCollection (WorkingData)^.Count <= RecNum) then
    DataAt := nil
   else
    DataAt := PCollection (WorkingData)^.At (RecNum);
end;


  { ออ TDmxCollector อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxCollector.LoadStruct (var S : TStream);
begin
  TDmxEditor.LoadStruct (S);
  S.Read (MaxCount, sizeof (MaxCount));
  InitNewDataRec;
end;


procedure TDmxCollector.StoreStruct (var S : TStream);
begin
  TDmxEditor.StoreStruct (S);
  S.Write (MaxCount, sizeof (MaxCount));
end;


procedure TDmxCollector.InitData (var AData );
{ this method is called during initialization }
begin
  TDmxEditor.InitData (AData);

  { Note that the given database size is used for max record count. }
  Move (DataBlockSize, MaxCount, 2);

  DataBlockSize := (RecordSize * PCollection (WorkingData)^.Count);
  If (MaxCount = 0) or (PCollection (WorkingData)^.Count < MaxCount) then
    DataBlockSize := DataBlockSize + RecordSize;

  InitNewDataRec;
end;


procedure TDmxCollector.DoneData;
{ this method is called during termination }
begin
  TDmxEditor.DoneData;
  If (NewDataRec <> nil) then FreeMem (NewDataRec, RecordSize);
end;


procedure TDmxCollector.InitNewDataRec;
{ initialize a temporary data object for new records }
begin
  If (RecordSize > 0) then
    begin
    GetMem (NewDataRec, RecordSize);
    RecordData		:= NewDataRec;
    TDmxEditor.ZeroizeRecord;
    RecordAltered	:= FALSE;
    FieldAltered	:= FALSE;
    end
   else
    NewDataRec	:= nil;
end;


procedure TDmxCollector.HandleEvent (var Event : TEvent);
begin
  TDmxEditor.HandleEvent (Event);
  If (Event.What and evMessage <> 0) and (Event.Command = cmDMX_Reset) and
     (Event.InfoPtr = WorkingData) then
    begin
    DataBlockSize := RecordSize;
    DataBlockSize := DataBlockSize * PCollection (WorkingData)^.Count;
    If (MaxCount = 0) or (PCollection (WorkingData)^.Count < MaxCount) then
      DataBlockSize := DataBlockSize + RecordSize;
    If (DataBlockSize <= 0) and (Owner <> nil) and
       (not GetState (sfFocused) or (Event.What = evCommand)) then
      begin
      Event.What := evCommand;
      Event.Command := cmClose;
      Event.InfoPtr := Owner;
      end
     else
      begin
      If RecordSelected then
        begin
        FieldAltered  := FALSE;
        RecordAltered := FALSE;
        EvaluateField;
        EvaluateRecord;
        If (CurrentRecord >= (DataBlockSize div RecordSize)) and
           (DataBlockSize > 0) then
          CurrentRecord := pred (DataBlockSize div RecordSize);
        SetupRecord;
        SetupField;
        end;
      SetLimit (Limit.X, DataBlockSize div RecordSize);
      DrawView;
      If (Event.What = evCommand) then ClearEvent (Event);
      end;
    end;
end;


function  TDmxCollector.Valid (Command : word) : boolean;
var  V : boolean;
begin
  V := TDmxEditor.Valid (Command);
  If V and (Command = cmValid) and
     ((WorkingData = nil) or (DataBlockSize < RecordSize) or (RecordSize <= 0)) then
    begin
    MessageBox ('No data available.', nil, mfError or mfOKButton);
    Valid := FALSE;
    end
   else
    Valid := V;
end;


procedure TDmxCollector.SetState (AState : word; Enable : boolean);
{ resets the DataBlockSize if the collection's limit has changed }
begin
  If Enable and (AState = sfFocused) and
    (DataBlockSize <> RecordSize * succ (PCollection (WorkingData)^.Count)) then
    begin
    DataBlockSize := RecordSize * PCollection (WorkingData)^.Count;
    If (MaxCount = 0) or (PCollection (WorkingData)^.Count < MaxCount) then
      DataBlockSize := DataBlockSize + RecordSize;
    end;
  TDmxEditor.SetState (AState, Enable);
end;


function  TDmxCollector.DataAt (RecNum : integer) : pointer;
{ this method is called whenever it must retrieve a record,
  whether it is for display purposes or for editing }
begin
  If (PCollection (WorkingData)^.Count <= RecNum) then
    DataAt  := NewDataRec
   else
    DataAt  := PCollection (WorkingData)^.At (RecNum);
end;


procedure TDmxCollector.SetupRecord;
{ called before each record is edited }
var  P     : pointer;
begin
  TDmxEditor.SetupRecord;
  If (PCollection (WorkingData)^.Count <= CurrentRecord) then
    begin
    TDmxEditor.ZeroizeRecord;
    RecordAltered := FALSE;
    FieldAltered  := FALSE;
    end;
end;


procedure TDmxCollector.EvaluateRecord;
{ called after each record is edited }
var  P : pointer;
begin
  TDmxEditor.EvaluateRecord;
  If RecordAltered then
    begin
    { If this is an old record, then we can assume that this is the
      one we were editing.  Otherwise, we need to make a new one. }
    If (PCollection (WorkingData)^.Count <= CurrentRecord) then
      begin
      { place the record into the collection }
      P := NewDataRec;
      PCollection (WorkingData)^.Insert (NewDataRec);

      { create a new record for NewDataRec }
      GetMem (NewDataRec, RecordSize);
      RecordData := NewDataRec;
      TDmxEditor.ZeroizeRecord;
      RecordData := P;
      If ((MaxCount = 0) or (PCollection (WorkingData)^.Count < MaxCount))
         and (CurrentRecord < MaxCollectionSize) then
        begin
        If ((MemAvail shr 4) > LowMemSize) then
          begin
          { increase the size of the database }
          DataBlockSize := DataBlockSize + RecordSize;
          SetLimit (Limit.X, DataBlockSize div RecordSize);
          end
         else
          If not MemWarning then
            begin
            MessageBox ('Too little memory to expand collection.', nil, mfError + mfOKCancel);
            MemWarning := TRUE;
            end;
        end;
      end;
    end;
end;


procedure TDmxCollector.ZeroizeRecord;
var  RS : boolean;
     E  : TEvent;
begin
  If Locked then Exit;
  RS := RecordSelected;
  If RS then
    begin
    EvaluateField;
    EvaluateRecord;
    end;
  If (PCollection (WorkingData)^.Count > CurrentRecord) then
    begin
    PCollection (WorkingData)^.AtFree (CurrentRecord);
    { adjust the size of the database }
    Repeat
    Until (Message (DeskTop, evBroadcast, cmDMX_Reset, WorkingData) = nil)
       or (DataBlockSize > 0);
    If (DataBlockSize = 0) then
      begin
      E.What := evCommand;
      E.Command := cmClose;
      E.InfoPtr := Owner;
      PutEvent (E);
      end;
    end;
  If RS then
    begin
    SetupRecord;
    SetupField;
    end;
end;


  { ออ TDmxCollectViewWin ออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxCollectViewWin.Init (var Bounds  : TRect;
		ATitle    : TTitleStr;  ANumber  : integer;
		ATemplate : string;  ACollection : PCollection;
		var ALabels : string);
begin
  TDmxViewer.Init (Bounds, ATitle, ANumber, ATemplate,
		   ACollection^, 0, ALabels);
end;


procedure TDmxCollectViewWin.InitDMX (ATemplate  : string;  var AData;
				ALabels, ARecInd : PDmxLink;
				BSize  : longint);
var  R  : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);
  Insert (New (PDmxCollectView, Init (ATemplate, AData, R, ALabels,
		StandardScrollBar (sbHorizontal + sbHandleKeyboard),
		StandardScrollBar (sbVertical   + sbHandleKeyboard))));
end;


  { ออ TDmxCollectorWin ออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxCollectorWin.Init (var Bounds    : TRect;
		ATitle    : TTitleStr;  ANumber  : integer;
		ATemplate : string;  ACollection : PCollection;
		BSize	  : integer; var ALabels : string; IndLen : integer);
begin
  TDmxWindow.Init (Bounds, ATitle, ANumber, ATemplate,
                   ACollection^, BSize, ALabels, IndLen);
end;


procedure TDmxCollectorWin.InitDMX (ATemplate  : string;  var AData;
			ALabels, ARecInd : PDmxLink;  BSize  : longint);
var  R  : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);
  Insert (New (PDmxCollector, Init (ATemplate, AData, BSize, R,
		ALabels, ARecInd,
		StandardScrollBar (sbHorizontal + sbHandleKeyboard),
		StandardScrollBar (sbVertical   + sbHandleKeyboard))));
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
