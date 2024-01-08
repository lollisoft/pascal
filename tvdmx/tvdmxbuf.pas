
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	tvDMXBUF  --Buffered Data Editing Unit		}
{	tvDMX     --data editing project (ver 1.4)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit tvDMXBUF;

{$B-,D-,R-,O+,X+,V- }

interface

uses  
    Objects, Drivers, Views, Dialogs, App, MsgBox,
    RSet, DmxGizma, tvDMX, StdDMX;

const
    EmptySlot	=  -1;

type
    PSlot	= ^TSlot;
    TSlot	=  RECORD
        Data   : pointer;
        RowNum : integer;
    end;


    PRowSlots	= ^TRowSlots;
    TRowSlots	=  array [0..99] of TSlot;


    PDmxEditBuf     = ^TDmxEditBuf;
    PDmxStreamBuf   = ^TDmxStreamBuf;
    PDmxExpBuf      = ^TDmxExpBuf;
    PDmxExpRecInd   = ^TDmxExpRecInd;
    PDmxBufWin      = ^TDmxBufWin;
    PDmxExpBufWin   = ^TDmxExpBufWin;
    PDmxEditRecBuf  = ^TDmxEditRecBuf;


    TDmxEditBuf   =  OBJECT (TDmxEditor)
        NumSlots    :  integer;
        RowSlot     :  PRowSlots;
        KeyFields   :  set of byte;
        KeyAltered  :  boolean;
        Expandable  :  boolean;
        Appending   :  boolean;
        StepMode    :  boolean;
      constructor Load (var S : TStream);
      procedure Store (var S : TStream);
      procedure LoadStruct (var S : TStream);  VIRTUAL;
      procedure MakeSlots;
      procedure ResetSlots;
      procedure InitStruct (var ATemplate );  VIRTUAL;
      procedure DoneStruct;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure SetState (AState : word; Enable : boolean);  VIRTUAL;
      function  DataAt (RecNum : integer)  : pointer;  VIRTUAL;
      procedure SetUpRecord;  VIRTUAL;
      procedure EvaluateRecord;  VIRTUAL;
      procedure EvaluateField;  VIRTUAL;
      procedure ZeroizeField (Whole : boolean; Field : pDMXfieldrec);  VIRTUAL;
      procedure ResetSize;
      function  RecordLimit : integer;  VIRTUAL;
      function  ErrorFunc : boolean;  VIRTUAL;
      function  SeekRec (RecNum : integer) : boolean;  VIRTUAL;
      function  SeekEnd : boolean;  VIRTUAL;
      function  ReadRec (var RecData ) : boolean;  VIRTUAL;
      function  WriteRec (var RecData ) : boolean;  VIRTUAL;
      private
        NewRecord   :  boolean;  { indicates this is a new record }
    end;


    TDmxStreamBuf =  OBJECT (TDmxEditBuf)
        Prefix      :  pointer;
        PrefixSize  :  integer;
      procedure LoadStruct (var S : TStream);  VIRTUAL;
      procedure StoreStruct (var S : TStream);  VIRTUAL;
      function  RecordLimit : integer;  VIRTUAL;
      function  ErrorFunc : boolean;  VIRTUAL;
      function  SeekRec (RecNum : integer) : boolean;  VIRTUAL;
      function  SeekEnd : boolean;  VIRTUAL;
      function  ReadRec (var RecData ) : boolean;  VIRTUAL;
      function  WriteRec (var RecData ) : boolean;  VIRTUAL;
    end;


    TDmxExpBuf    =  OBJECT (TDmxStreamBuf)
      procedure InitData (var AData );  VIRTUAL;
    end;


    TDmxExpRecInd =  OBJECT (TDmxRecInd)
      procedure Draw;  VIRTUAL;
    end;


    TDmxBufWin    =  OBJECT (TDmxWindow)
      procedure InitDMX (ATemplate : string;  var AData;
                         ALabels, ARecInd  : PDmxLink;
                         BSize  : longint);  VIRTUAL;
    end;


    TDmxExpBufWin =  OBJECT (TDmxWindow)
      procedure InitDMX (ATemplate : string;  var AData;
                         ALabels, ARecInd  : PDmxLink;
                         BSize  : longint);  VIRTUAL;
      function  NewRecInd (Len : integer)  : PDmxLink;  VIRTUAL;
    end;


    TDmxEditRecBuf  =  OBJECT (TDmxEditBuf)
        RecPosition : longint;
      procedure InitData (var AData );  VIRTUAL;
      procedure ZeroizeRecord;  VIRTUAL;
      function  SeekRec (RecNum : integer) : boolean;  VIRTUAL;
      function  SeekEnd : boolean;  VIRTUAL;
      procedure DeleteRec;  VIRTUAL;
      function  WriteRec (var RecData ) : boolean;  VIRTUAL;
      function  AppendRec (var RecData ) : boolean;  VIRTUAL;
      function  UpdateRec (var RecData ) : boolean;  VIRTUAL;
      function  FirstRec : boolean;  VIRTUAL;
      function  LastRec : boolean;  VIRTUAL;
      function  NextRec : boolean;  VIRTUAL;
      function  PrevRec : boolean;  VIRTUAL;
    end;


const
    RDmxStreamBuf :  TStreamRec = (
        ObjType:   cmDMX + 10;
        VmtLink:   ofs (TypeOf (TDmxStreamBuf)^);
        Load:      @TDmxStreamBuf.Load;
        Store:     @TDmxStreamBuf.Store
      );

    RDmxExpBuf    :  TStreamRec = (
        ObjType:   cmDMX + 11;
        VmtLink:   ofs (TypeOf (TDmxExpBuf)^);
        Load:      @TDmxExpBuf.Load;
        Store:     @TDmxExpBuf.Store
      );

    RDmxExpRecInd :  TStreamRec = (
        ObjType:   cmDMX + 12;
        VmtLink:   ofs (TypeOf (TDmxExpRecInd)^);
        Load:      @TDmxExpRecInd.Load;
        Store:     @TDmxExpRecInd.Store
      );

    RDmxBufWin    :  TStreamRec = (
        ObjType:   cmDMX + 13;
        VmtLink:   ofs (TypeOf (TDmxBufWin)^);
        Load:      @TDmxBufWin.Load;
        Store:     @TDmxBufWin.Store
      );

    RDmxExpBufWin :  TStreamRec = (
        ObjType:   cmDMX + 14;
        VmtLink:   ofs (TypeOf (TDmxExpBufWin)^);
        Load:      @TDmxExpBufWin.Load;
        Store:     @TDmxExpBufWin.Store
      );


implementation


  { ออ TDmxEditBuf อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxEditBuf.Load (var S : TStream);
begin
  TDmxEditor.Load (S);
  S.Read (KeyFields,  sizeof (KeyFields));
  S.Read (Expandable, sizeof (Expandable));
  S.Read (StepMode,   sizeof (StepMode));
end;


procedure TDmxEditBuf.Store (var S : TStream);
begin
  TDmxEditor.Store (S);
  S.Write (KeyFields,  sizeof (KeyFields));
  S.Write (Expandable, sizeof (Expandable));
  S.Write (StepMode,   sizeof (StepMode));
end;


procedure TDmxEditBuf.LoadStruct (var S : TStream);
begin
  TDmxEditor.LoadStruct (S);
  MakeSlots;
end;


procedure TDmxEditBuf.MakeSlots;
var i  : integer;
begin
  If InitValid and (RecordSize > 0) then
    begin
    NumSlots := ScreenHeight;
    If (HiResScreen and (NumSlots < 30)) then NumSlots := 46;
    If (NumSlots < Size.Y) then NumSlots := Size.Y;
    GetMem (RowSlot, NumSlots * sizeof (TSlot));
    fillchar (RowSlot^, NumSlots * sizeof (TSlot), 0);
    For i := 0 to pred (NumSlots) do
      begin
      If ((MaxAvail shr 4) > RecordSize) then
        begin
        RowSlot^ [i].RowNum := EmptySlot;
        GetMem (RowSlot^ [i].Data, RecordSize);
        end
       else
        InitValid := FALSE;
      end;
    end;
end;


procedure TDmxEditBuf.ResetSlots;
var i : integer;
begin
  If (NumSlots > 0) then
    For i := 0 to pred (NumSlots) do RowSlot^ [i].RowNum := EmptySlot;
end;


procedure TDmxEditBuf.InitStruct (var ATemplate );
begin
  TDmxEditor.InitStruct (ATemplate);
  MakeSlots;
end;


procedure TDmxEditBuf.DoneStruct;
var i : integer;
begin
  If (RowSlot <> nil) then
    begin
    For i := 0 to pred (NumSlots) do
      If (RowSlot^ [i].Data <> nil) then FreeMem (RowSlot^ [i].Data, RecordSize);
    FreeMem (RowSlot, NumSlots * sizeof (TSlot));
    RowSlot  := nil;
    NumSlots := 0;
    end;
  TDmxEditor.DoneStruct;
end;


procedure TDmxEditBuf.HandleEvent (var Event : TEvent);
begin
  With Event do
    If (What = evBroadcast) and (NumSlots > 0) and
        (((Command = cmDMX_DrawData) and (WorkingData = InfoPtr))
       or
         ((Command = cmDMX_Draw)
        and (InfoPtr <> @Self)
        and (PDmxScroller (InfoPtr)^.WorkingData = WorkingData)))
     then
      begin
      ResetSlots;
      end;
  TDmxEditor.HandleEvent (Event);
end;


procedure TDmxEditBuf.SetState (AState : word; Enable : boolean);
begin
  If Enable and (AState and sfFocused <> 0) and (not RecordSelected) and
     Expandable and (CurrentField <> nil) then
    ResetSize;
  TDmxEditor.SetState (AState, Enable);
end;


function  TDmxEditBuf.DataAt (RecNum : integer) : pointer;
var  Slot : integer;
    function  SeekOK : boolean;
    var L : longint;
    begin
      L := RecNum;
      If Expandable and (L >= RecordLimit) then
        begin
        NewRecord := TRUE;
        SeekOK    := TRUE;
        end
       else
        SeekOK := SeekRec (RecNum);
    end;
begin
  If (not InitValid) or (NumSlots = 0) or (RecordSize = 0) then
    begin
    Locked := TRUE;
    DataAt := nil;
    Exit;
    end;
  Slot  := RecNum mod NumSlots;
  NewRecord := FALSE;
  If (RowSlot^ [Slot].RowNum <> RecNum) then
    begin
    FillChar (RowSlot^ [Slot].Data^, RecordSize, 0);
    RowSlot^ [Slot].RowNum := RecNum;
    Repeat
    Until (SeekOK and (NewRecord or ReadRec (RowSlot^ [Slot].Data^)))
        or ErrorFunc;
    end;
  DataAt := RowSlot^ [Slot].Data;
end;


procedure TDmxEditBuf.SetUpRecord;
begin
  If (NumSlots > 0) then
    RowSlot^ [CurrentRecord mod NumSlots].RowNum := EmptySlot;
  TDmxEditor.SetUpRecord;
  RedrawRecord := TRUE;
  Appending    := NewRecord;
end;


procedure TDmxEditBuf.EvaluateRecord;
    function  DoWrite : boolean;
    begin
      DoWrite := WriteRec (RowSlot^ [CurrentRecord mod NumSlots].Data^);
    end;
begin
  If RecordAltered then
    begin
    RecordAltered := FALSE;
    If Appending then
      begin
      Repeat until (SeekEnd and DoWrite) or ErrorFunc;
      ResetSize;
      Appending := FALSE;
      end
    else
    If StepMode then
      begin
      Repeat until DoWrite or ErrorFunc;
      end
    else
      begin
      Repeat until (SeekRec (CurrentRecord) and DoWrite) or ErrorFunc;
      end;
    end;
  TDmxEditor.EvaluateRecord;
  If KeyAltered then
    begin
    KeyAltered := FALSE;
    ResetSlots;
    DrawView;
    end;
end;


procedure TDmxEditBuf.EvaluateField;
begin
  If FieldAltered and (CurrentField^.fieldnum in KeyFields) then KeyAltered := TRUE;
  TDmxEditor.EvaluateField;
end;


procedure TDmxEditBuf.ZeroizeField (Whole : boolean; Field : pDMXfieldrec);
begin
  TDmxEditor.ZeroizeField (Whole, Field);
  If (Field <> nil) and (Field^.fieldnum in KeyFields) then KeyAltered := TRUE;
end;


procedure TDmxEditBuf.ResetSize;
var  Recs : longint;
     A    : string;
begin
  Recs := RecordLimit;
  If Expandable and (Recs < 32766) then Inc (Recs);
  If (Recs < 0) then Recs := 0;
  If (Recs * RecordSize <> DataBlockSize) then
    begin
    DataBlockSize := Recs * RecordSize;
    SetLimit (Limit.X, Recs);
    If (succ (CurrentRecord) > Recs) then CurrentRecord := Recs - 1;
    ResetSlots;
    end;
end;


function  TDmxEditBuf.RecordLimit : integer;
{ pseudo-abstract method to return the maximum number of records possible }
var  L : longint;
begin
  If (RecordSize > 0) then
    begin
    L := (DataBlockSize div RecordSize);
    If Expandable and (L > 0) then Dec (L);
    RecordLimit := integer (L);
    end
   else
    RecordLimit := 0;
end;


function  TDmxEditBuf.ErrorFunc : boolean;
{ pseudo-abstract method to handle access errors }
begin
 { This method should take care of the error
   and return TRUE if the error can be ignored
   or FALSE if the operation should be repeated. }

  ErrorFunc := (SystemError (14, 0) = 1);
end;


function  TDmxEditBuf.SeekRec (RecNum : integer) : boolean;
{ abstract virtual method to seek to the record position }
begin
  Abstract;
 { This method should seek to the given record
   number, and return TRUE if there is no error.
  }
end;


function  TDmxEditBuf.SeekEnd : boolean;
{ pseudo-abstract method used for expandable databases }
begin
 { This method should seek to the end of the database, and
   return TRUE if there is no error.  Many database tools
   will just require that you clear its record buffer.
   The default here is to seek to the limit using method SeekRec().
  }
  SeekRec (RecordLimit);
  SeekEnd := TRUE;
end;


function  TDmxEditBuf.ReadRec (var RecData ) : boolean;
{ abstract virtual method to read a record }
begin
  Abstract;
 { This method should read a record and return TRUE if there is no error. }
end;


function  TDmxEditBuf.WriteRec (var RecData ) : boolean;
{ abstract virtual method to write a record }
begin
  Abstract;
 { This method should write a record and return TRUE if there is no error. }
end;


  { ออ TDmxStreamBuf อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxStreamBuf.LoadStruct (var S : TStream);
begin
  TDmxEditBuf.LoadStruct (S);
  S.Read (PrefixSize, sizeof (PrefixSize));
  Prefix := nil;
end;


procedure TDmxStreamBuf.StoreStruct (var S : TStream);
begin
  TDmxEditBuf.StoreStruct (S);
  S.Write (PrefixSize, sizeof (PrefixSize));
end;


function  TDmxStreamBuf.RecordLimit : integer;
var L : longint;
begin
  If (RecordSize = 0) then
    RecordLimit := 0
   else
    begin
    L := (PStream (WorkingData)^.GetSize - PrefixSize) div RecordSize;
    If (L > 32766) then L := 32766;
    RecordLimit := integer (L);
    end;
end;


function  TDmxStreamBuf.ErrorFunc : boolean;
{ virtual method to handle stream errors }
begin
  ErrorFunc := TDmxEditBuf.ErrorFunc;
  PStream (WorkingData)^.Reset;
end;


function  TDmxStreamBuf.SeekRec (RecNum : integer) : boolean;
var  L : longint;
begin
  PStream (WorkingData)^.Reset;
  L := RecNum;
  PStream (WorkingData)^.Seek (PrefixSize + (L * RecordSize));
  SeekRec := (PStream (WorkingData)^.Status = stOk);
end;


function  TDmxStreamBuf.SeekEnd : boolean;
var  L : longint;
begin
  L := RecordLimit;
  PStream (WorkingData)^.Seek (PrefixSize + (L * RecordSize));
  SeekEnd := (PStream (WorkingData)^.Status = stOk);
end;


function  TDmxStreamBuf.ReadRec (var RecData ) : boolean;
begin
  With PStream (WorkingData)^ do
    begin
    If (Status <> stOk) then Reset;
    Read (RecData, RecordSize);
    ReadRec := (Status = stOk);
    end;
end;


function  TDmxStreamBuf.WriteRec (var RecData ) : boolean;
begin
  With PStream (WorkingData)^ do
    begin
    If (Status <> stOk) then Reset;
    Write (RecData, RecordSize);
    WriteRec := (Status = stOk);
    end;
end;


  { ออ TDmxExpRecInd อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxExpRecInd.Draw;
var  A,E  : string [80];
     B    : TDrawBuffer;
     C    : word;
begin
  C := GetColor (6);
  MoveChar (B, 'อ', C, Size.X);
  Str (succ (Link^.CurrentRecord):1, A);
  Str (pred (Link^.Limit.Y):1,       E);
  If Link^.CurrentRecord = pred (Link^.Limit.Y) then A := 'Add';
  A := A + '/' + E;
  If length (A) > Size.X then A [0] := chr (length (A) - succ (length (E)));
  If length (A) > Size.X then
    begin
    MoveChar (B, showOVERFLOW, C, Size.X);
    end
   else
    begin
    If length (A) < Size.X then A := A + ' ';
    If length (A) < Size.X then A := ' ' + A;
    MoveStr (B [succ ((Size.X) - length (A)) shr 1], A, C);
    end;
  WriteBuf (0, 0, Size.X, 1, B);
end;


  { ออ TDmxExpBuf ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxExpBuf.InitData (var AData );
begin
  TDmxStreamBuf.InitData (AData);
  PrefixSize	:= DataBlockSize;
  Expandable	:= TRUE;
  ResetSize;
end;


  { ออ TDmxBufWin ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxBufWin.InitDMX (ATemplate : string;  var AData;
                              ALabels, ARecInd : PDmxLink;  BSize : longint);
var  R     : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  Inc (R.A.Y, 2);

  Insert (New (PDmxStreamBuf, Init (ATemplate, AData, BSize, R,
                               ALabels, ARecInd,
                               StandardScrollBar (sbHorizontal+ sbHandleKeyboard),
                               StandardScrollBar (sbVertical  + sbHandleKeyboard))));

end;


  { ออ TDmxExpBufWin อออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxExpBufWin.InitDMX (ATemplate : string;  var AData;
                                 ALabels, ARecInd : PDmxLink;  BSize : longint);
var  R     : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  Inc (R.A.Y, 2);

  Insert (New (PDmxExpBuf, Init (ATemplate, AData, BSize, R,
                                 ALabels, ARecInd,
                                 StandardScrollBar (sbHorizontal+ sbHandleKeyboard),
                                 StandardScrollBar (sbVertical  + sbHandleKeyboard))));

end;


function  TDmxExpBufWin.NewRecInd (Len : integer)  : PDmxLink;
var  ARecInd   : PDmxExpRecInd;
     R         : TRect;
begin
  If Len <= 0 then
    NewRecInd := nil
   else
    begin
    GetExtent (R);
    Inc (R.A.X);
    R.A.Y  := pred (R.B.Y);
    R.Grow (-1, 0);
    If R.B.X - R.A.X > Len then R.B.X := R.A.X + Len;
    R.B.Y := succ (R.A.Y);
    ARecInd := New (PDmxExpRecInd, Init (R, Len));
    Insert (ARecInd);
    NewRecInd := ARecInd;
    end;
end;


  { ออ TDmxEditRecBuf ออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxEditRecBuf.InitData (var AData );
begin
  TDmxEditBuf.InitData (AData);
  StepMode := TRUE;
end;


procedure TDmxEditRecBuf.ZeroizeRecord;
begin
  If Appending then
    begin
    TDmxEditBuf.ZeroizeRecord;
    RecordAltered := FALSE;
    FieldAltered  := FALSE;
    end
   else
    begin
    Vidis := TRUE;
   { Prevents tvDMX from resetting the current field and record when this
     view loses the focus.  This should only be used for MessageBoxes.
    }
    If (MessageBox ('Do you wish to DELETE this record?', nil,
                    mfConfirmation + mfYesNoCancel) = cmYes)
     then
      begin
      DeleteRec;
      Appending  := TRUE;
      ResetSize;
      DrawView;
      If RecordSelected then
        begin
        RecordAltered := FALSE;
        FieldAltered  := FALSE;
        EvaluateField;
        KeyAltered := TRUE;
        EvaluateRecord;
        SetupRecord;
        SetupField;
        end
       else
        begin
        ResetSize;
        DrawView;
        end;
      end;
    Vidis := FALSE;  { always set this back to FALSE }
    end;
end;


function  TDmxEditRecBuf.SeekRec (RecNum : integer) : boolean;
{ uses FirstRec(), LastRec(), NextRec() and PrevRec() to seek to a record }
var  B      : boolean;
     EndNum : integer;
    function  LastRecord : boolean;
    begin
      If (RecordSize = 0) then
        begin
        EndNum := 0;
        LastRecord := FALSE;
        end
       else
        begin
        EndNum := (DataBlockSize div RecordSize) - 1;
        If Expandable then Dec (EndNum);
        LastRecord := (RecNum = EndNum);
        end;
    end;
begin
  B := TRUE;
  If (RecNum = 0) then
    begin
    B := FirstRec;
    RecPosition := 0;
    end
   else
    If LastRecord then
      begin
      B := LastRec;
      RecPosition := EndNum;
      end
     else
      begin
      While (RecPosition < RecNum) and B do
        begin
        B := NextRec;
        If B then Inc (RecPosition);
        end;
      While (RecPosition > RecNum) and B do
        begin
        B := PrevRec;
        If B then Dec (RecPosition);
        end;
      end;
  SeekRec := B;
end;


function  TDmxEditRecBuf.SeekEnd : boolean;
begin
  SeekEnd := TRUE;
end;


procedure TDmxEditRecBuf.DeleteRec;
{ abstract method to delete the record }
begin
 { This method should be overridden to delete the current record. }
end;


function  TDmxEditRecBuf.WriteRec (var RecData ) : boolean;
{ virtual method to write a record }
begin
  If Appending then
    begin
    KeyAltered := (KeyFields <> []);
    WriteRec := AppendRec (RecData);
    end
   else
    WriteRec := UpdateRec (RecData);
end;


function  TDmxEditRecBuf.AppendRec (var RecData ) : boolean;
{ abstract virtual method to write a record }
begin
  Abstract;
 { This method should append a record and return TRUE if there is no error. }
end;


function  TDmxEditRecBuf.UpdateRec (var RecData ) : boolean;
{ abstract virtual method to update the current record }
begin
  Abstract;
 { This method should write a record and return TRUE if there is no error. }
end;


function  TDmxEditRecBuf.FirstRec : boolean;
{ pseudo-abstract method to seek to the first record position }
begin
 { This method should be overridden to seek directly to the
   first record, and it should return TRUE if there is no error.
   The default method just repeats PrevRec() until it receives
   an error.
  }
  Repeat until not PrevRec;
  FirstRec := TRUE;
end;


function  TDmxEditRecBuf.LastRec : boolean;
{ abstract virtual method to seek to the last record position }
begin
  Abstract;
 { This method should be overridden to seek to the last record
   position, and it should return TRUE if there is no error.
  }
end;


function  TDmxEditRecBuf.NextRec : boolean;
{ abstract virtual method to seek to the next record position }
begin
  Abstract;
 { This method should be overridden to seek to the next record
   position, and it should return TRUE if there is no error.
  }
end;


function  TDmxEditRecBuf.PrevRec : boolean;
{ abstract virtual method to seek to the previous record position }
begin
 { This method should be overridden to seek to the previous record
   position, and it should return TRUE if there is no error.
  }
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure RegisterDMXBUF;
begin
  RegisterType (RDmxStreamBuf);
  RegisterType (RDmxExpBuf);
  RegisterType (RDmxExpRecInd);
  RegisterType (RDmxBufWin);
  RegisterType (RDmxExpBufWin);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }



End.
