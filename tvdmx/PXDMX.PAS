
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{       PxDMX     --Paradox Data Editing Unit           }
{       tvDMX     --data editing project (ver 2.x)      }
{							}
{       Copyright (c) 1992-96   Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{                                                       }
{	Requires:	Paradox Engine 3.x or above	}
{			Borland Pascal 7.x or above	}
{							}
{	Recommended:	TREALNUM=Double (in RSET.PAS)	}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit PxDMX;

{$B-,E+,N+,O+,X+,V- }

interface

uses
    Dos, {Crt,}
    Objects, Drivers, Memory, Views, Dialogs, App, Editors, MsgBox,
    RSet, DmxGizma, tvGizma, tvDMX, StdDMX, tvDMXBUF,
    PxGizma, PxEngine, PxMsg;


const
    cmDMX_EditMemo		=  cmDMX + 35;
    hcPxMemo	  : word	=  hcNoContext;

    StdMemoSize	  : word	=  4094;
    StdMemoRect	  : TRect	=  (A:(X:10;Y:3);B:(X:70;Y:17));
    MemoZoomRect  : TRect	=  (A:(X:10;Y:3);B:(X:70;Y:17));

    BlankFields	  : boolean	=  TRUE;
    Auto1900	  : boolean	=  TRUE;

    PxDelimiter	  : char	=  '|';
    PxCommands	  : TCommandSet	=  [];
    fldPxLen	  : byte	=  36;

    fldPxSTRING	  : char	=  's';
    fldPxMEMO	  : char	=  'c';
    fldPxNUMBER   : string[31]	=  'r,rrr,rrr.rrrrr';
    fldPxCURRENCY : string[31]	=  '($rrr,rrr.zz)';
    fldPxSHORTNUM : string[31]	=  'iii,iii ';
    fldPxDATE	  : string[63]	=  ' WW-'^F + ^U+char(12) +
				  #0'ZW-'  + ^U+char(31) +
				  #0'ZZZW '^F;

type
    PDmxPxEditor	= ^TDmxPxEditor;
    PDmxPxWindow	= ^TDmxPxWindow;

    PDmxParadox		= ^TDmxParadox;
    PDmxPxLabels	= ^TDmxPxLabels;
    PDmxParadoxWin	= ^TDmxParadoxWin;
    PPxExpRecInd	= ^TPxExpRecInd;

    TDmxPxEditor	=  OBJECT(TDmxEditBuf)
	Status		: integer;
	PxInitStat	: integer;
	SeekSlotNum	: longint;
	RecHandle	: RecordHandle;
	FldHandle	: FieldHandle;
	SrchFields	: integer;
	KeyViol		: boolean;
	OverWriteKeyViol: boolean;
	LkHandle	: LockHandle;
	ModifiedFields 	: set of byte;
	FldThere	: array[0..49] of set of byte;
	FieldBlanked	: boolean;
      function  BufValid : boolean;  VIRTUAL;
      procedure DeleteRec;  VIRTUAL;
      procedure DoneData;  VIRTUAL;
      function  EditPxMemo(TblHandle: TableHandle;
                           FldName: NameString;
		           ABufSize: word) : boolean;
      function	ErrorFunc : boolean;  VIRTUAL;
      procedure EvaluateField;	VIRTUAL;
      procedure FieldText(var S: string;  var Color: word;
			  Field: pDMXfieldrec;  var DataRec );  VIRTUAL;
      procedure GetFields(var RecData );
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure InitData(var AData );  VIRTUAL;
      procedure PutFields(var RecData );
      function	ReadRec(var RecData ) : boolean;  VIRTUAL;
      function	RecordLimit : longint;	VIRTUAL;
      procedure ResetRecLock;  VIRTUAL;
      function	SeekEnd : boolean;  VIRTUAL;
      function	SeekRec(RecNum: longint) : boolean;  VIRTUAL;
      procedure SetState(AState: word; Enable: boolean);  VIRTUAL;
      function	SetRecLock : boolean;  VIRTUAL;
      procedure SetupRecord;  VIRTUAL;
      function  UserChoosesOverWrite(RH: RecordHandle) : boolean;  VIRTUAL;
      function	WriteRec(var RecData ) : boolean;  VIRTUAL;
      function	Valid(Command: word) : boolean;  VIRTUAL;
      procedure ZeroizeField(Whole: boolean; Field: pDMXfieldrec);  VIRTUAL;
      private
	ResetRecPtr	: boolean;
    end;


    TDmxPxWindow  =  OBJECT(TDmxExpBufWin)
      procedure InitDMX(ATemplate: string;  var AData;
			ALabels, ARecInd: PDmxLink;
			BSize: longint);  VIRTUAL;
    end;


    TDmxParadox  =  OBJECT(TDmxPxEditor)
      constructor Init(var AData: TableHandle;  BSize: longint;
		       var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
			   AHScrollBar,AVScrollBar: PScrollBar);
      function	FieldTemplate(FName,FType: NameString) : string;  VIRTUAL;
      procedure InitStruct(var Template );  VIRTUAL;
      procedure SetFieldName(var FName: string);  VIRTUAL;
    end;


    TDmxPxLabels  =  OBJECT(TDmxExtLabels)
      constructor Init(var Bounds: TRect);
      constructor InitInsert(AOwner: PGroup);
      destructor  Done;  VIRTUAL;
      procedure AddField(FName: string);
    end;


    TDmxParadoxWin  =  OBJECT(TDmxExpBufWin)
      constructor Init(var Bounds: TRect;  ATitle: TTitleStr;  ANumber: integer;
			var AData: TableHandle;  BSize: longint;  IndLen: integer);
      procedure InitDMX(ATemplate: string;  var AData;
			ALabels, ARecInd: PDmxLink;
			BSize: longint);  VIRTUAL;
      function	NewDmxLabels(var ALabels ) : PDmxLink;	VIRTUAL;
    end;


    TPxExpRecInd =  OBJECT(TDmxRecInd)
      procedure Draw;  VIRTUAL;
    end;


const
    NetInit	  :  boolean  =  FALSE;

  function  PxDmxFieldTemplate(FName,FType: NameString) : string;

  function  PxEntryFields(Px: TableHandle; DMX: PDmxParadox) : PSItem;


implementation

const
    InitializingPxLabels : PDmxPxLabels = nil;

var
   MData	: ^TMemoData;
   Memo		:  PMemo;


  { ออ TDmxPxEditor ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TDmxPxEditor.EditPxMemo(TblHandle: TableHandle;
				FldName: NameString;
				ABufSize: word) : boolean;

var  xFldHandle	: FieldHandle;
     BlbHandle	: BlobHandle;
     FldType	: NameString;
     xSize	: longint;
     xStatus	: integer;

    function  Ok(AStatus: integer) : boolean;
    begin
      xStatus := AStatus;
      Ok := (xStatus = PxSuccess);
    end;

    function  EditMemo : boolean;
    var
       R,DeskRect	: TRect;
       Button		: PButton;
       Win              : PDialog;
    begin
      EditMemo := FALSE;
      If (MData <> nil) then
	begin
	R.Copy(StdMemoRect);
	DeskTop^.GetExtent(DeskRect);
	Inc(DeskRect.B.X);
	Inc(DeskRect.B.Y);
	If not (DeskRect.Contains(R.A) and DeskRect.Contains(R.B)) then
	  R.Assign(10,3,70,17);
	If not (DeskRect.Contains(MemoZoomRect.A) and DeskRect.Contains(MemoZoomRect.B)) then
	  MemoZoomRect.Assign(10,3,70,17);
	While (FldName[length(FldName)] = ' ') do Dec(FldName[0]);
	Win := New(PDialog, Init(R, FldName));
	Win^.Flags := Win^.Flags or wfMove or wfGrow or wfClose or wfZoom;
	Win^.GetExtent(R);
	R.Grow(-1, -1);
	Memo  := New(PMemo, Init(R,
	  Win^.StandardScrollBar(sbHorizontal),
	  Win^.StandardScrollBar(sbVertical),
	  nil,
	  ABufSize));
	Memo^.HelpCtx := hcPxMemo;
	Win^.ZoomRect.Copy(MemoZoomRect);
	Memo^.SetData(MData^);
	Win^.Insert(Memo);
	If (Application^.ValidView(Win) <> nil) then
	  begin
	  DeskTop^.ExecView(Win);
	  If Memo^.Modified then
	    begin
	    Memo^.GetData(MData^);
	    EditMemo := TRUE;
	    end;
	  Win^.GetBounds(StdMemoRect);  { save bounds }
	  MemoZoomRect.Copy(Win^.ZoomRect);
	  Dispose(Win, Done);
	  end;
	end;
    end;

begin
  MData := nil;
  xSize := 0;
  If Ok(PxFldHandle(TblHandle, FldName, xFldHandle)) then
  If Ok(PxBlobOpenRead(RecHandle, xFldHandle, BlbHandle)) then
  If Ok(PxBlobGetSize(BlbHandle, xSize)) then
  If (ABufSize < xSize) then ABufSize := xSize+1000;
  If (MaxAvail < xSize + LowMemSize) then
    begin
    Application^.OutOfMemory;
    end
   else
    begin
    GetMem(MData, ABufSize+2);
    FillChar(MData^, ABufSize+2, 0);
    MData^.Length := xSize;
    If (xSize = 0) or Ok(PXBlobGet(BlbHandle, xSize, 0, MData^.Buffer)) then
      begin
      If Ok(PXBlobClose(BlbHandle, PxBlobAccept)) then
	begin
	If EditMemo then
	If Ok(PxBlobDrop(RecHandle, xFldHandle)) then
	If (MData^.Length > 0) and
	  Ok(PxBlobOpenWrite(RecHandle, xFldHandle, BlbHandle, MData^.Length, PxBlobNew))
	 then
	  begin
	  Ok(PXBlobPut(BlbHandle, MData^.Length, 0, MData^.Buffer));
	  If Ok(PXBlobClose(BlbHandle, PxBlobAccept)) then
	    EditPxMemo := TRUE;
	  end;
	Exit;
	end;
      end;
    end;
  If (MData <> nil) and (xSize > 0) then FreeMem(MData, ABufSize+2);
  EditPxMemo := (PxErrorBox(xStatus) = PxSuccess);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TDmxPxEditor.BufValid : boolean;
var  Err : integer;
     Chg : boolean;
begin
  If NetInit then
    begin
    Err := PxNetTblChanged(TableHandle(WorkingData^), Chg);
    BufValid := not Chg;
    end
   else
    BufValid := TRUE;
end;


procedure TDmxPxEditor.DeleteRec;
begin
  Status := PxErrorBox(PxRecDelete(TableHandle(WorkingData^)));
end;


procedure TDmxPxEditor.DoneData;
begin
  TDmxEditBuf.DoneData;
  If (WorkingData <> nil) and (RecHandle <> 0) then
    Status := PxRecBufClose(RecHandle);
end;


function  TDmxPxEditor.ErrorFunc : boolean;
begin
  Vidis := TRUE;
  PxErrorBox(Status);
  Vidis := FALSE;
  If (Owner^.Owner = nil) then PxInitStat := Status;
  ErrorFunc := TRUE;
end;


procedure TDmxPxEditor.EvaluateField;
var  n : integer;
begin
  If FieldAltered then
    begin
    ModifiedFields := ModifiedFields + [CurrentField^.fieldnum];
    If FieldBlanked then
      Case upcase(CurrentField^.typecode) of
	fldINTEGER	: FieldBlanked := integer(FieldData^) = 0;
	fldLONGINT	: FieldBlanked := longint(FieldData^) = 0;
	fldREALNUM	: FieldBlanked := PREALNUM(FieldData)^ = 0;
       else		  FieldBlanked := FALSE;
	end;
    If (NumSlots > 0) and (not FieldBlanked) then
      begin
      n := CurrentRecord mod NumSlots;
      FldThere[n] := FldThere[n] + [CurrentField^.fieldnum];
      end;
    If Auto1900 and (upcase(CurrentField^.typecode) = 'W') and
       (CurrentField^.upperlimit = 0) and
       (word(FieldData^) > 0) and (word(FieldData^) < 100)
     then
      word(FieldData^) := word(FieldData^) + 1900;
    end;
  TDmxEditBuf.EvaluateField;
end;


procedure TDmxPxEditor.FieldText(var S: string;  var Color: word;
				 Field: pDMXfieldrec;  var DataRec );
var  i: integer;
begin
  If (upcase(Field^.typecode) = 'C') then
    begin
    For i := 1 to length(S) do
      Case S[i] of
	^I:	S[i] :=  ' ';
	^M:	S[i] :=  #17;
	^J:	S[i] := #217;
	end;
    end;
  inherited FieldText(S, Color, Field, DataRec );
end;


procedure TDmxPxEditor.GetFields(var RecData );
{ transfers record from the PX-transfer buffer to RecData }
type DD   =  RECORD M,D,Y : integer; end;
     D	  = ^DD;
var  FH   :  FieldHandle;
     i,n  :  integer;
     P,P1 :  pointer;
     F	  :  pDMXfieldrec;
     R	  :  Double;
     Date :  TDate;
     C	  :  char;

    function  Okay(M: integer) : boolean;
    begin
      If (M = PxErr_InvDate) then M := PxSuccess;
      Status := M;
      If (Status = PxSuccess) then
	Okay  := TRUE
       else
	begin
	Okay  := FALSE;
	If not Locked then
	  begin
	  Locked := TRUE;
	  PxErrorBox(Status);
	  end;
	end;
    end;

    function  Blank : boolean;
    var  B : boolean;
    begin
      If not Okay(PxFldBlank(RecHandle, F^.fieldnum, B)) then
	B := TRUE;
      If not B and (SeekSlotNum < NumSlots) then
	FldThere[SeekSlotNum] := FldThere[SeekSlotNum] + [F^.fieldnum];
      Blank := B;
    end;

begin
  If (SeekSlotNum < NumSlots) then FldThere[SeekSlotNum] := [];
  If (RecordSize = 0) then Exit;
  FillChar(RecData, RecordSize, 0);
  If (Status <> PxSuccess) then Exit;
  P1 := @RecData;
  P  := P1;
  F  := DMXfield1;
  While (F <> nil) and (F^.fieldsize = 0) do F := F^.Next;
  While (F <> nil) do
    begin
    If (F^.fieldsize > 0) then
      begin
      If (not Blank) then
	begin
	PtrRec(P).Ofs := PtrRec(P1).Ofs + F^.datatab;
	C := upcase(F^.typecode);
	If (C = 'H') then
	  Case F^.fieldsize of
	    2:	C := 'I';
	    4:	C := 'L';
	    end;
	Case C of
	  'R': If Okay(PxGetDoub(RecHandle, F^.fieldnum, R)) then
		begin
		Move(ptr(seg(R), ofs(R)+6)^, i, sizeof(i));
		If (i and $7FF0 = $7FF0) then R := -0.0;
		PREALNUM(P)^ := R;
		end;
	  'L': If Okay(PxGetDoub(RecHandle, F^.fieldnum, R)) then PLongInt(P)^ := Round(R);
	  'S','#':  Okay(PxGetAlpha(RecHandle, F^.fieldnum, pstring(P)^));
	  'C','0':  Okay(PXBlobQuickGet(RecHandle, F^.fieldnum, F^.fieldsize, P^, n));
	  'I': Okay(PxGetShort(RecHandle, F^.fieldnum, pinteger(P)^));
	  'W': begin
	       If Okay(PxGetDate(RecHandle, F^.fieldnum, Date)) and
		 Okay(PxDateDecode(Date, D(P)^.M, D(P)^.D, D(P)^.Y))
		then
		 begin
		 n := F^.fieldnum;
		 For i := 1 to 2 do
		   begin
		   If (F^.Next <> nil) and
		     ((F^.Next^.fieldnum = n) or (F^.Next^.fieldsize = 0)) then
		     begin
		     F := F^.Next;
		     While (F^.fieldsize = 0) and (F^.Next <> nil) do F := F^.Next;
		     end;
		   end;
		 end;
	       end;
	  end;
	end;
      end;
    F := F^.Next;
    end;
end;


procedure TDmxPxEditor.HandleEvent(var Event: TEvent);

    function  EditMemo(ABufSize: word) : boolean;
    var  S:  string;
	 FS: boolean;
    begin
      EditMemo := FALSE;
      If FieldSelected and not Locked then
	If (CurrentField^.typecode in ['0','C','c']) then
	  begin
	  If Appending then
	    MessageBox('Record must be appended into the table before memo can be added.',
			nil, mfOKButton or mfError)
	  else
	    begin
	    EditMemo := TRUE;
	    If (PxErrorBox(PxFldName(TableHandle(WorkingData^),
				     CurrentField^.fieldnum, S)) = PxSuccess)
	       and EditPxMemo(TableHandle(WorkingData^), S, StdMemoSize)
	       then
		begin
		ChangeMade;
		FS := FieldSelected;
		If FS then EvaluateField;
		EvaluateRecord;
		SetupRecord;
		If FS then SetupField;
		end;
	    end;
	  end;
    end;

begin
  With Event do
    begin
    If (What = evKeyDown) then
      begin
      If (CharCode in [^H, #32..#255]) or (KeyCode = kbDel) then
	begin
	FieldBlanked := FALSE;
	ChangeMade;
	end;
      If (CurrentField <> nil) and (upcase(CurrentField^.typecode) in ['0','C']) and
         (not (CharCode in [#0,^I,^M,^T,^Y,#27]) or (KeyCode = kbDel)) then
        begin
        If not ((KeyCode = kbDel) and (pstring(FieldData)^ = '')) then
	  PutEvent(Event);
        What := evCommand;
        Command := cmDMX_EditMemo;
        InfoLong := StdMemoSize;
        end;
      end;
    TDmxEditBuf.HandleEvent(Event);
    If (What = evCommand) or (What = evBroadcast) then
      begin
      Case Command of
	cmDMX_DoubleClick:	If not EditMemo(StdMemoSize) then Exit;
	cmDMX_EditMemo:		EditMemo(InfoWord);
       else Exit;
	end;
      If (Event.What = evCommand) then ClearEvent(Event);
      end;
    end;
end;


procedure TDmxPxEditor.InitData(var AData );
var  i,NumOpen	: integer;
     S		: string;
begin
  StepMode := TRUE;
  FillChar(FldThere, sizeof(FldThere), 0);
  If (TableHandle(WorkingData^) = 0) then PxInitStat := PxErr_TableNotFound;
  If (PxInitStat <> PxSuccess) then
    begin
    InitValid := FALSE;
    Exit;
    end;
  NetInit := (PxNetUserName(S) <> PxErr_NoNetInit);
  NumOpen := 0;
  TDmxEditBuf.InitData(AData);
  If (WorkingData = nil) then
    InitValid := FALSE
   else
    begin
    PxInitStat := PxRecBufOpen(TableHandle(WorkingData^), RecHandle);
    If (PxInitStat = PxSuccess) then
      begin
      PxInitStat := PxKeyNFlds(TableHandle(WorkingData^), SrchFields);
      If (PxInitStat = PxSuccess) and (SrchFields > 0) then
	begin
	KeyFields :=[0];
	For i := 1 to SrchFields do KeyFields := KeyFields + [i];
	end;
      If (DataBlockSize = 0) then Expandable := TRUE;
      If (PxInitStat = PxSuccess) then ResetSize;
      end;
    If (PxInitStat <> PxSuccess) then InitValid := FALSE;
    end;
end;


var  RecordHasData  :  boolean;

procedure TDmxPxEditor.PutFields(var RecData );
{ transfers record from RecData to the PX-transfer buffer }
type DD   =  record M,D,Y : integer; end;
     D	  = ^DD;
var  FH   :  FieldHandle;
     i,n  :  integer;
     P,P1 :  pointer;
     F	  :  pDMXfieldrec;
     R	  :  Double;
     Date :  TDate;
     C	  :  char;

    function  Okay(M: integer) : boolean;
    begin
      Status := M;
      If (Status = PxSuccess) then Okay := TRUE else Okay := FALSE;
      If (Status <> PxSuccess) then
	begin
	Vidis := TRUE;
	MessageBox(PxErrMsg(Status) + ^M^M^C'[field #%d]',
		dparam(F^.fieldnum, nil), mfError + mfOkButton);
	Vidis := FALSE;
	end;
    end;

begin
  If (RecordSize = 0) then Exit;
  If (Status <> PxSuccess) then Exit;
  RecordHasData := not Appending;
  P1 := @RecData;
  P  := P1;
  F  := DMXfield1;
  While (F <> nil) do
    begin
    If (F^.fieldsize = 0) then
      begin
      end
    else
{  For future use...
    If BlankFields and not (F^.fieldnum in FldThere[CurrentRecord mod NumSlots]) then
      begin
      Okay(PxPutBlank(RecHandle, F^.fieldnum));
      end
    else
 ... }
    If (F^.fieldsize > 0) then
      begin
      ModifiedFields := ModifiedFields - [F^.fieldnum];
      PtrRec(P).Ofs := PtrRec(P1).Ofs + F^.datatab;
      C := upcase(F^.typecode);
      If (C = 'H') then
	Case F^.fieldsize of
	  2:	C := 'I';
	  4:	C := 'L';
	  end;
      Case C of
	'R': begin
	     R	:= PREALNUM(P)^;
	     Okay(PxPutDoub(RecHandle, F^.fieldnum, R));
	     RecordHasData := RecordHasData or (R <> 0);
	     end;
	'L': begin
	     Okay(PxPutLong(RecHandle, F^.fieldnum, plongint(P)^));
	     RecordHasData := RecordHasData or (plongint(P)^ <> 0);
	     end;
	'S','#':
	     begin
	     Okay(PxPutAlpha(RecHandle, F^.fieldnum, pstring(P)^));
	     If not RecordHasData and (pstring(P)^ <> '') then
		For i := 1 to length(pstring(P)^) do
		  If not (pstring(P)^[i] in [#0,' ']) then RecordHasData := TRUE;
	     end;
	'I': begin
	     Okay(PxPutShort(RecHandle, F^.fieldnum, pinteger(P)^));
	     RecordHasData := RecordHasData or (pinteger(P)^ <> 0);
	     end;
	'W': begin
	     If (D(P)^.M + D(P)^.D + D(P)^.Y = 0) then
		begin
		If BlankFields then
		  Okay(PxPutBlank(RecHandle, F^.fieldnum))
		 else
		  If Okay(PxDateEncode(1, 1, 1982, Date)) then
		    Okay(PxPutDate(RecHandle, F^.fieldnum, Date));
		end
	     else
	     If Okay(PxDateEncode(D(P)^.M, D(P)^.D, D(P)^.Y, Date)) then
		begin
		Okay(PxPutDate(RecHandle, F^.fieldnum, Date));
		RecordHasData := TRUE;
		end;
	     n := F^.fieldnum;
	     For i := 1 to 2 do
		begin
		If (F^.Next <> nil) and
		   ((F^.Next^.fieldnum = n) or (F^.Next^.fieldsize = 0)) then
		  begin
		  F := F^.Next;
		  While (F^.fieldsize = 0) and (F^.Next <> nil) do F := F^.Next;
		  end;
		end;
	     end;
	end;
      end;
    F := F^.Next;
    end;
  If not RecordHasData then RecordAltered := FALSE;
end;


function  TDmxPxEditor.ReadRec(var RecData ) : boolean;
begin
  Status  := PxRecGet(TableHandle(WorkingData^), RecHandle);

  If (Status = PxSuccess) then GetFields(RecData);
  ReadRec := (Status = PxSuccess);
end;


function  TDmxPxEditor.RecordLimit : longint;
{ pseudo-abstract method to return the maximum number of records possible }
var  L : longint;
begin
  Status := PxTblNRecs(TableHandle(WorkingData^), L);
  RecordLimit := L;
end;


procedure TDmxPxEditor.ResetRecLock;
begin
  If NetInit and not Appending then
    Status := PxNetRecUnlock(TableHandle(WorkingData^), LkHandle);
end;


function  TDmxPxEditor.SeekEnd : boolean;
begin
  SeekSlotNum := Limit.Y - 1;
  SeekSlotNum := SeekSlotNum - BaseRecord;
  If (SeekSlotNum < 0) then SeekSlotNum := 0;
  SeekSlotNum := SeekSlotNum mod NumSlots;
  If (SeekSlotNum < NumSlots) then FldThere[SeekSlotNum] := [];
  SeekEnd := TRUE;
end;


function  TDmxPxEditor.SeekRec(RecNum: longint) : boolean;
begin
  SeekSlotNum := RecNum;
  SeekSlotNum := SeekSlotNum - BaseRecord;
  If (SeekSlotNum < 0) then SeekSlotNum := 0;
  SeekSlotNum := SeekSlotNum mod NumSlots;
  Status  := PxRecGoto(TableHandle(WorkingData^), succ(RecNum));
  SeekRec := (Status = PxSuccess);
end;


function  TDmxPxEditor.SetRecLock : boolean;
var  E	: integer;
     S	: string;
begin
  If NetInit and not Appending then
    begin
    E := PxNetRecLock(TableHandle(WorkingData^), LkHandle);
    If (E = PxSuccess) then
      SetRecLock := TRUE
     else
      begin
      If (E = PxErr_RecLocked) and (PxNetErrUser(S) = PxSuccess) and (S <> '') then
	S := 'Record is locked by ' + S
       else
	S := PxErrMsg(E);
      Vidis := TRUE;
      MessageBox(S, nil, mfError + mfOkButton);
      Vidis := FALSE;
      SetRecLock := FALSE;
      end;
    end
   else
    SetRecLock := TRUE;
end;


procedure TDmxPxEditor.SetState(AState: word; Enable: boolean);
begin
  TDmxEditBuf.SetState(AState, Enable);
  If (AState and sfActive <> 0) then
    begin
    If Enable then
      begin
      ResetSize;
      If (PxCommands <> []) then EnableCommands(PxCommands);
      end
     else
      If (PxCommands <> []) then DisableCommands(PxCommands);
    end;
end;


procedure TDmxPxEditor.SetupRecord;
var i : integer;
begin
  ModifiedFields := [];
  TDmxEditBuf.SetupRecord;
  If Appending then Status := PxRecBufEmpty(RecHandle);
end;


function  TDmxPxEditor.Valid(Command: word) : boolean;
begin
  If (Command = cmValid) and (PxInitStat <> 0) then
    begin
    Vidis := TRUE;
    PxErrorBox(PxInitStat);
    Vidis := FALSE;
    Valid := FALSE;
    end
   else
    Valid := TDmxEditBuf.Valid(Command);
end;


function  TDmxPxEditor.UserChoosesOverWrite(RH: RecordHandle) : boolean;
begin
  UserChoosesOverWrite := FALSE;
end;


function  TDmxPxEditor.WriteRec(var RecData ) : boolean;
begin
  KeyViol := FALSE;
  PutFields(RecData);
  ModifiedFields := [];

  If RecordHasData then
    begin
    If not Appending then
      begin
      Status := PxRecUpdate(TableHandle(WorkingData^), RecHandle);
      end
     else
      begin
      Status := PxRecAppend(TableHandle(WorkingData^), RecHandle);
      end;

    If (Status = PxErr_KeyViol) then
      begin
      Status := PxSrchKey(TableHandle(WorkingData^), RecHandle, SrchFields, SearchFirst);
      If Status = PxSuccess then
        begin
        If OverWriteKeyViol then
          begin
          KeyViol := FALSE;
          Status := PxRecUpdate(TableHandle(WorkingData^), RecHandle);
          end
         else
          begin
          If UserChoosesOverWrite(RecHandle) then
            begin
            KeyViol := FALSE;
            Status := PxRecUpdate(TableHandle(WorkingData^), RecHandle);
            end
           else
            begin
            KeyViol := TRUE;
            Status := PxErr_KeyViol;
            end;
          end;
        end
       else
        begin
        { report error }
        end;
      end;
    end;
  WriteRec := (Status = PxSuccess);
end;


procedure TDmxPxEditor.ZeroizeField(Whole: boolean; Field: pDMXfieldrec);
var  n	: integer;
     P	: pointer;
begin
  TDmxEditBuf.ZeroizeField(Whole, Field);
  If (Field <> nil) and (Field^.fieldnum <> 0) then
    begin
    If (upcase(Field^.typecode) in ['0','C']) then
      begin
      PxErrorBox(PxBlobDrop(RecHandle, Field^.fieldnum));
      end
     else
      begin
      If (NumSlots > 0) then
	begin
	n := CurrentRecord mod NumSlots;
	FldThere[n] := FldThere[n] - [CurrentField^.fieldnum];
	end;
      If Appending then ModifiedFields := ModifiedFields - [CurrentField^.fieldnum];
      FieldBlanked := TRUE;
      end;
    end;
end;


  { ออ TDmxPxWindow ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxPxWindow.InitDMX(ATemplate: string;  var AData;
				ALabels, ARecInd: PDmxLink;
				BSize: longint);
var  R : TRect;
begin
  GetExtent(R);
  R.Grow(-1,-1);
  If ALabels <> nil then Inc(R.A.Y, ALabels^.Size.Y);
  Insert(New(PDmxPxEditor, Init(ATemplate, AData, BSize, R,
				ALabels, ARecInd,
				StandardScrollBar(sbHorizontal),
				StandardScrollBar(sbVertical))));
end;


  { ออ TDmxParadox อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxParadox.Init(var AData: TableHandle;  BSize: longint;
			      var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
				  AHScrollBar,AVScrollBar: PScrollBar);
begin
  TDmxPxEditor.Init('', AData, BSize, Bounds, ALabels, ARecInd,
		     AHScrollBar,AVScrollBar);
end;


function  TDmxParadox.FieldTemplate(FName,FType: NameString) : string;
begin
  FieldTemplate := PxDmxFieldTemplate(FName,FType)
end;


procedure TDmxParadox.InitStruct(var Template );
var  i,NumFields : integer;
     P	 : pointer;
     L	 : longint;
     Vis : boolean;

    procedure InitField(FN: FieldHandle);
    var  i,N,Len : integer;
	 S	 : string;
	 FName	 : string;
	 FType	 : string[sizeof(NameString)];
    begin
      If (PxInitStat = PxSuccess) then
	PxInitStat := PxFldName(TableHandle(WorkingData^), FN, FName);
      If (PxInitStat = PxSuccess) then
	PxInitStat := PxFldType(TableHandle(WorkingData^), FN, FType);
      If (PxInitStat = PxSuccess) then
	begin
	While (FName <> '') and (FName[length(FName)] in [#0,' ']) do Dec(FName[0]);
	S := FieldTemplate(FName,FType);
	SetFieldName(FName);
	Len := DmxStrLen(S);
	If (Len > 0) then
	  begin
	  If (FName <> '') then FName := FName + ' ';
	  If (length(FName) > Len) then
	    begin
	    i := succ(length(FName) - Len);
	    FillChar(S[succ(length(S))], i, ' ');
	    S[succ(length(S))] := #0;
	    Inc(S[0], i);
	    Inc(Len, pred(i));
	    end;
	  If ((InitializingPxLabels <> nil) and (InitializingPxLabels^.Len > 0)) or
	     ((InitializingPxLabels = nil) and (FN > 1))
	   then
	    begin
	    S := PxDelimiter + S;
	    Inc(Len);
	    Vis := TRUE;
	    end;
	  end;
	If (S <> '') then TDmxScroller.InitStruct(S) else Inc(TotalFields);
	If (Len > 0) and (InitializingPxLabels <> nil) then
	  begin
	  FillChar(S[1], Len, ' ');
	  S[0] := chr(Len);
	  If Vis then i := 3 else i := 2;
	  If (length(FName) + pred(i) > length(S)) then
	    i := succ(length(FName) - length(S));
	  If (i < 2) then i := 2;
	  Move(FName[1], S[i], length(FName));
	  InitializingPxLabels^.AddField(S);
	  end;
	end;
    end;

begin
  Vis := FALSE;
  If (TableHandle(WorkingData^) = 0) then PxInitStat := PxErr_TableNotFound;
  If (PxInitStat = PxSuccess) then
    begin
    PxInitStat := PxRecNFlds(TableHandle(WorkingData^), NumFields);
    If (PxInitStat = PxSuccess) and (NumFields > 0) then
      For i := 1 to NumFields do InitField(i);
    end;
  P := nil;
  TDmxPxEditor.InitStruct(P);
  InitializingPxLabels := nil;
end;


procedure TDmxParadox.SetFieldName(var FName: string);
begin
end;


  { ออ TDmxPxLabels ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxPxLabels.Init(var Bounds: TRect);
var  n : integer;
     A : PCharArray;
begin
  TDmxExtLabels.Init(0, nil, Bounds);
  Heaped := TRUE;
  InitializingPxLabels := @Self;
end;


constructor TDmxPxLabels.InitInsert(AOwner: PGroup);
var  R : TRect;
begin
  AOwner^.GetExtent(R);
  R.Grow(-1, -1);
  R.B.Y  := R.A.Y + 2;
  TDmxExtLabels.Init(0, nil, R);
  Heaped := TRUE;
  InitializingPxLabels := @Self;
  Insert(AOwner);
end;


destructor TDmxPxLabels.Done;
begin
  InitializingPxLabels := nil;
  TDmxExtLabels.Done;
end;


procedure TDmxPxLabels.AddField(FName: string);
var  n : integer;
     A : PCharArray;
begin
  If (FName <> '') then
    begin
    n := Len;
    A := Data;
    Inc(Len, length(FName));
    GetMem(Data, Len);
    If (n > 0) then Move(A^, Data^, n);
    Move(FName[1], Data^[n], length(FName));
    If (A <> nil) then FreeMem(A, n);
    end;
end;


  { ออ TDmxParadoxWin ออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxParadoxWin.Init(var Bounds: TRect;
				ATitle: TTitleStr;
				ANumber: integer;
				var AData: TableHandle;
				BSize: longint;
				IndLen: integer);
begin
  TWindow.Init(Bounds, ATitle, ANumber);
  InitDMX('',
	AData,
	NewDmxLabels(AData),
	NewRecInd(IndLen),
	BSize);
  Options := Options or ofTileable;
end;


procedure TDmxParadoxWin.InitDMX(ATemplate: string;  var AData;
			ALabels, ARecInd: PDmxLink;
			BSize: longint);
var  R	: TRect;
begin
  GetExtent(R);		  { create new rectangle for editor object }
  R.Grow(-1,-1);			      { shrink -1 to avoid borders }
  If (ALabels <> nil) then Inc(R.A.Y, ALabels^.Size.Y); { make room for labels view }
  Insert(New(PDmxParadox,
	 Init(TableHandle(AData),			    { working data }
	      BSize,					  { 0 = expandable }
	      R,					{ view's rectangle }
	      ALabels, ARecInd,
	      StandardScrollBar(sbHorizontal),
	      StandardScrollBar(sbVertical)
	      )
    ));
end;


function  TDmxParadoxWin.NewDmxLabels(var ALabels ) : PDmxLink;
begin
  NewDmxLabels := New(PDmxPxLabels, InitInsert(@Self));
end;


  { ออ TPxExpRecInd ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TPxExpRecInd.Draw;
var  i	  : integer;
     A,E  : string[80];
     B	  : TDrawBuffer;
     C	  : word;
begin
  If (Link = nil) then
    TView.Draw
   else
    begin
    C := GetColor(6);
    MoveChar(B, 'อ', C, Size.X);
    Str(succ(Link^.ActualRecordNum):1, A);
    i := Link^.Limit.Y;
    If PDmxEditBuf(Link)^.Expandable then
      begin
      Dec(i);
      If Link^.CurrentRecord = pred(Link^.Limit.Y) then A := 'Add';
      end;
    If (Link^.Limit.Y < 32000) then
      begin
      Str(i:1, E);
      A := A + '/' + E;
      If length(A) > Size.X then A[0] := chr(length(A) - succ(length(E)));
      end;
    If length(A) > Size.X then
      MoveChar(B, showOVERFLOW, C, Size.X)
     else
      begin
      If length(A) < Size.X then A := A + ' ';
      If length(A) < Size.X then A := ' ' + A;
      MoveStr(B[succ((Size.X) - length(A)) shr 1], A, C);
      end;
    WriteBuf(0, 0, Size.X, 1, B);
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxDmxFieldTemplate(FName,FType: NameString) : string;
var  i,N : integer;
     S	 : string;
begin
  If (FType = '') then
    PxDmxFieldTemplate := ''
   else
    begin
    Case upcase(FType[1]) of
      'A':
	begin
	Delete(FType,1,1);
	Val(FType, N, i);
	FillChar(S, sizeof(S), fldPxSTRING);
	S[0] := chr(N);
	If (N < 254) then
	  begin
	  Inc(S[0]);
	  Inc(N);
	  S[1] := ' ';
	  end;
	If (N > fldPxLen) then Insert('`', S, succ(fldPxLen));
	end;
      'M':
	begin
	Delete(FType,1,1);
	Val(FType, N, i);
	FillChar(S, sizeof(S), fldPxMEMO);
	S[0] := chr(N);
	If (N < 254) then
	  begin
	  Inc(S[0]);
	  Inc(N);
	  S[1] := ' ';
	  end;
	If (N > fldPxLen) then Insert('`', S, succ(fldPxLen));
	end;
      'N':	S := fldPxNUMBER;
      '$':	S := fldPxCURRENCY;
      'S':	S := fldPxSHORTNUM;
      'D':	S := fldPxDATE;
     else
	begin
	S := '';
	end;
      end;
    PxDmxFieldTemplate := S;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxEntryFields(Px: TableHandle; DMX: PDmxParadox) : PSItem;
var  i,FNum,FNLen,FC,Stat,over	: integer;
     S				: string;
     FName,FType		: string[39];
     FNames,Templates,Master,PS	: PSItem;

  function  FieldLen : integer;
  var  v,e: integer;
  begin
    Delete(FType,1,1);
    Val(FType,v,e);
    If (e = 0) then FieldLen := v else FieldLen := 0;
  end;

  function  RStr(var S: string; Len: integer) : string;
  var  A: string[80];
  begin
    FillChar(A[1], Len, ' ');
    If (S <> '') then
      Move(S[1], A[succ(Len - length(S))], length(S));
    If (Len > length(S)) then A[0] := chr(Len) else A[0] := succ(S[0]);
    RStr := A;
  end;

  function  LStr(var S: string; Len: integer) : string;
  var  A: string;
       L: integer;
  begin
    L := DmxStrLen(S);
    If (L < Len) then
      begin
      FillChar(A[1], Len - L, ' ');
      A[0] := chr(Len - L);
      end
     else
      A := '';
    A := S + A;
    LStr := A;
  end;

begin
  PxEntryFields := nil;
  If (Px = 0) and (DMX <> nil) then Px := TableHandle(DMX^.WorkingData^);
  If (PxErrorBox(PxRecNFlds(Px, FC)) <> PxSuccess) or (FC <= 0) then Exit;
  Templates := nil;
  Master := nil;
  FNames := nil;
  FNLen  := 1;
  over	 := 0;
  Stat	 := PxSuccess;
  For FNum := 1 to FC do If (Stat = PxSuccess) then
    begin
    Stat := PxFldName(Px, FNum, FName);
    If (Stat = PxSuccess) then
      Stat := PxFldType(Px, FNum, FType);
    If (Stat = PxSuccess) then
      begin
      If (DMX = nil) then
	S := PxDmxFieldTemplate(FName,FType)
       else
	begin
	S := DMX^.FieldTemplate(FName,FType);
	DMX^.SetFieldName(FName);
	end;

      If (pos('`', S) > 0) then Delete(S, pos('`', S), 1);

      i := DmxStrLen(S);
      If (i > 0) and (FName <> '') then
	begin
	If (S[1] = ' ') then Delete(S,1,1);
	If (S[length(S)] = ' ') then Dec(S[0]);
	If not (FType[1] in ['A','N','$','S','D']) then S := S + ^R;
	If (length(FName) > FNLen) then FNLen := length(FName);
	If (i > 44) then
	  begin
	  i := pos('`', S);
	  If (i > 0) then Delete(S,i,1);
	  Insert('`', S, 45);
	  i := DmxStrLen(S);
	  end;
{	If (i < 32) and (pos('`', S) > 0) then Delete(S, pos('`', S), 1);
	If (i > 32) then
	  begin
	  i := pos('`', S);
	  If (i > 0) then Delete(S,i,1);
	  Insert('`', S, 33);
	  i := DmxStrLen(S);
	  end;
 }
	If (DMX <> nil) then DMX^.SetFieldName(FName);
	FNames := NewSItem(FName, FNames);
	S := '\' + #27^F+char(FNum) + S+'\';
	While (over > 0) do
	  begin
	  If (over > 127) then
	    begin
	    S := ^P#127 + S;
	    Dec(over, 127);
	    end
	   else
	    begin
	    S := ^P+char(over) + S;
	    over := 0;
	    end;
	  end;
	Templates := NewSItem(S, Templates);
	over := 0;
	end
       else
	begin
	Case upcase(FType[1]) of
	  'A':		Inc(over, FieldLen+1);
	  'N','$':	Inc(over, sizeof(TREALNUM));
	  'S':		Inc(over, sizeof(INTEGER));
	  'D':		Inc(over, sizeof(word)*3);
	  'M':		Inc(over, FieldLen);
	  end;
	end;
      end;
    end;

  If (Stat = PxSuccess) then
    begin
    While (Templates <> nil) do
      begin
      Master := NewSItem('~' + RStr(FNames^.Value^, FNLen+1) + '~\'
			+ LStr(Templates^.Value^, 1),
		Master);
      PS := FNames^.Next;
      DisposeStr(FNames^.Value);
      Dispose(FNames);
      FNames := PS;
      PS := Templates^.Next;
      DisposeStr(Templates^.Value);
      Dispose(Templates);
      Templates := PS;
      end;
    Master := NewSItem(^A, Master);
    end;

  DisposeSItems(FNames);
  DisposeSItems(Templates);

  PxEntryFields := Master;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
