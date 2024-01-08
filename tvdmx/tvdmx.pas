
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	tvDMX     --data editing project (ver 1.5)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit tvDMX;

{$B-,D-,O+,R-,X+,V- }

interface

uses  Objects, Drivers, Views, App, RSet, DmxGizma;

type
    PDmxLink	 = ^TDmxLink;
    PDmxLabels	 = ^TDmxLabels;
    PDmxScroller = ^TDmxScroller;
    PDmxRecInd	 = ^TDmxRecInd;
    PDmxEditor	 = ^TDmxEditor;


    TDmxLink	=  OBJECT (TView)
	Link  : PDmxScroller;
      constructor Init (Bounds : TRect);
      constructor Load (var S : TStream);
      function  GetPalette : PPalette;  VIRTUAL;
      procedure Store (var S : TStream);
      procedure Insert (AOwner : PGroup);
    end;


    TDmxLabels	=  OBJECT (TDmxLink)
	Len   : integer;
	Data  : PCharArray;
      constructor Init (DataStr : pstring; var Bounds : TRect);
      constructor InitInsert (AOwner : PGroup;  DataStr : pstring);
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure Draw;  VIRTUAL;
      procedure DrawRuler (Upper, AtLimit : boolean);
      constructor Load (var S : TStream);
      procedure Store (var S : TStream);
    end;


    TDmxScroller =  OBJECT (TScroller)
	Labels		: PDmxLink;
	WorkingData	: pointer;
	DataBlockSize	: longint;
	CurrentRecord	: integer;
	CurrentField	: pDMXfieldrec;
	DMXfield1	: pDMXfieldrec;
	LeftField	: pDMXfieldrec;
	TotalFields	: integer;
	RecordSize	: integer;
	Locked		: boolean;
	InitValid	: boolean;
      constructor Init (ATemplate  : string; var AData; BSize : longint;
		var Bounds : TRect;  ALabels : PView;  AHScrollBar,AVScrollBar : PScrollBar);
      procedure   InitStruct (var ATemplate );  VIRTUAL;
      procedure   InitData (var AData );  VIRTUAL;
      destructor  Done;  VIRTUAL;
      constructor Load (var S : TStream);
      procedure Store (var S : TStream);
      procedure LoadData (var S : TStream);  VIRTUAL;
      procedure LoadStruct (var S : TStream);  VIRTUAL;
      procedure StoreData (var S : TStream);  VIRTUAL;
      procedure StoreStruct (var S : TStream);  VIRTUAL;
      procedure DoneStruct;  VIRTUAL;
      procedure DoneData;  VIRTUAL;
      function  Valid (Command : word)  : boolean;  VIRTUAL;
      procedure ChangeBounds (var Bounds : TRect);  VIRTUAL;
      function  GetPalette  : PPalette;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure WrongKeypressed (var Event : TEvent);  VIRTUAL;
      procedure SetState (AState : word;  Enable : boolean);  VIRTUAL;
      function  DataAt (RecNum : integer)  : pointer;  VIRTUAL;
      procedure DrawRecord (Y : integer;  var DataRecord );
      procedure Draw;  VIRTUAL;
      private
	InBuffer	: boolean;
	DDelta,DSize	: TPoint;
	vwidth		: integer;
    end;


    TDmxRecInd   =  OBJECT (TDmxLink)
      constructor Init (Bounds : TRect;  Len : integer);
      constructor InitInsert (AOwner : PGroup; Len : integer);
      procedure SetState (AState : word; Enable : boolean);  VIRTUAL;
      procedure Draw;  VIRTUAL;
    end;


    TDmxEditor   =  OBJECT (TDmxScroller)
	RecInd		: PDmxLink;
	FieldData	: pointer;
	RecordData	: pointer;
	CurPos		: integer;
	Vidis		: boolean;
	DoubleValid	: boolean;
	FirstKey	: boolean;
	RedrawRecord	: boolean;
	FieldAltered	: boolean;
	RecordAltered	: boolean;
	JustAltered	: boolean;
	DataAltered	: boolean;
	FieldSelected	: boolean;
	RecordSelected	: boolean;
      constructor Init (ATemplate  : string;  var AData; BSize : longint;
			var Bounds : TRect;  ALabels,ARecInd  : PDmxLink;
			AHScrollBar,AVScrollBar : PScrollBar);
      constructor Load (var S : TStream);
      destructor  Done;  VIRTUAL;
      procedure Store (var S : TStream);
      procedure SetState (AState : word;  Enable : boolean);  VIRTUAL;
      procedure ChangeBounds (var Bounds : TRect);  VIRTUAL;
      procedure ChangeMade;
      procedure SetUpField;  VIRTUAL;
      procedure EvaluateField;  VIRTUAL;
      procedure SetUpRecord;  VIRTUAL;
      procedure EvaluateRecord;  VIRTUAL;
      procedure Draw;  VIRTUAL;
      procedure DrawField (var Field : pDMXfieldrec);
      procedure ZeroizeRecord;  VIRTUAL;
      procedure ZeroizeField (Whole : boolean; Field : pDMXfieldrec);  VIRTUAL;
      procedure ProcessMouse   (var Event : TEvent);
      procedure ProcessCommand (var Command : word;  XY  : TPoint);
      procedure ProcessEnter (var Event : TEvent);  VIRTUAL;
      procedure ProcessKey (var Event : TEvent);
      procedure GotoPos (AFieldNum,ARecNum : integer);
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      private
	FirstPos	: integer;
	ShowFmt		: showset;
    end;


const
    RDmxLabels   :  TStreamRec = (
	ObjType:  cmDMX + 1;
	VmtLink:  ofs (TypeOf (TDmxLabels)^);
	Load:     @TDmxLabels.Load;
	Store:    @TDmxLabels.Store
      );

    RDmxScroller :  TStreamRec = (
	ObjType:  cmDMX + 2;
	VmtLink:  ofs (TypeOf (TDmxScroller)^);
	Load:     @TDmxScroller.Load;
	Store:    @TDmxScroller.Store
      );

    RDmxRecInd   :  TStreamRec = (
	ObjType:  cmDMX + 3;
	VmtLink:  ofs (TypeOf (TDmxRecInd)^);
	Load:     @TDmxRecInd.Load;
	Store:    @TDmxRecInd.Store
      );

    RDmxEditor   :  TStreamRec = (
	ObjType:  cmDMX + 4;
	VmtLink:  ofs (TypeOf (TDmxEditor)^);
	Load:     @TDmxEditor.Load;
	Store:    @TDmxEditor.Store
      );


  procedure RegisterDMX;


implementation


  { ออ TDmxLink ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxLink.Init (Bounds : TRect);
begin
  TView.Init (Bounds);
  GrowMode  := gfGrowLoY or gfGrowHiY;
  EventMask := evMessage;
end;


constructor TDmxLink.Load (var S : TStream);
begin
  TView.Load (S);
  GetPeerViewPtr (S, Link);
end;


function  TDmxLink.GetPalette : PPalette;
begin
  GetPalette := @cDMX
end;


procedure TDmxLink.Store (var S : TStream);
begin
  TView.Store (S);
  PutPeerViewPtr (S, Link);
end;


procedure TDmxLink.Insert (AOwner : PGroup);
begin
  If (AOwner <> nil) then AOwner^.Insert (@Self);
end;


  { ออ TDmxLabels ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxLabels.Init (DataStr : pstring;  var Bounds : TRect);
begin
  TDmxLink.Init (Bounds);
  Move (DataStr, Data, sizeof (Data));
  Len := length (DataStr^);
  Inc (PtrRec (Data).Ofs);
  GrowMode := gfGrowHiX;
end;


constructor TDmxLabels.InitInsert (AOwner : PGroup;  DataStr : pstring);
var  R : TRect;
begin
  AOwner^.GetExtent (R);
  Inc (R.A.Y);
  R.B.Y  := R.A.Y + 2;
  R.Grow (-1, 0);
  TDmxLink.Init (R);
  Move (DataStr, Data, sizeof (Data));
  Len := length (DataStr^);
  Inc (PtrRec (Data).Ofs);
  GrowMode := gfGrowHiX;
  Insert (AOwner);
end;


procedure TDmxLabels.HandleEvent (var Event : TEvent);
var  dX,dY  : integer;
begin
  TDmxLink.HandleEvent (Event);
  With Event do
    If (What = evBroadcast) and (Command = cmDMX_FixSize) and (Size.X > Len) then
      begin
      dX := (Owner^.Size.X - Size.X) + Len;
      dY :=  Owner^.Size.Y;
      Owner^.GrowTo (dX, dY);
      end;
end;


procedure TDmxLabels.Draw;
var  A  : string;
begin
  Move (Data^ [Link^.Delta.X], A [1], Size.X);
  If (Link^.Delta.X + Size.X > Len) then
    fillchar (A [succ (Len - Link^.Delta.X)], (Size.X + Link^.Delta.X - Len), ' ');
  A [0] := chr (lo (Size.X));
  WriteStr (0, 0, A, 1);
  If (Size.Y > 1) then DrawRuler (TRUE, FALSE);
end;


procedure TDmxLabels.DrawRuler (Upper, AtLimit : boolean);
const
  LtArr		=  17;
  RtArr		=  16;
  Markers	: string [10] = 'ฤอยัาหมฯะส';
var
  Color		: word;
  i,X,width	: integer;
  Mk		: integer;
  frontcut	: integer;
  fieldrec	: pDMXfieldrec;
  A		: string;
  B		: TDrawBuffer;
begin
  If (longint (Size) = 0) or (Link = nil) or (Link^.DMXfield1 = nil) then Exit;
  fieldrec  := Link^.LeftField;
  If (fieldrec = nil) or (fieldrec^.screentab > Link^.Delta.X) then
    fieldrec := Link^.DMXfield1;
  While (fieldrec^.Next^.screentab <= Link^.Delta.X) and
	(fieldrec^.Next <> nil)
   do
    fieldrec := fieldrec^.Next;
  frontcut  := Link^.Delta.X - fieldrec^.screentab;
  If frontcut < 0 then frontcut := 0;
  X      := 0;
  Color  := GetColor (5);
  If AtLimit then Mk := 2 else Mk := 1;
  MoveChar (B, Markers [Mk], Color, Size.X);
  Inc (Mk, 2);
  If not Upper then Inc (Mk, 4);
  While (X < Size.X) do
    begin
    With fieldrec^ do
      begin
      If (access and accHidden = 0) then
	begin
	If access and accDelimiter <> 0 then
	  begin
	  If fieldrec^.typecode = 'บ' then char (B [X]) := Markers [Mk + 2]
	   else If fieldrec^.typecode = 'ณ' then char (B [X]) := Markers [Mk];
	  Inc (X);
	  end
	 else
	  begin
	  X := X + length (template^) - frontcut;
	  end;
	frontcut := 0;
	end;
      end;
    fieldrec := fieldrec^.Next;
    If (fieldrec = nil) and (Size.X > X) then X := Size.X;
    end;
  If Upper then i := pred (Size.Y) else i := 0;
  WriteLine (0, i, Size.X, succ (i), B);
end;


constructor TDmxLabels.Load (var S : TStream);
begin
  TDmxLink.Load (S);
  S.Read (Len, sizeof (Len));
  If Len > 0 then
    begin
    GetMem (Data, Len);
    S.Read (Data^, Len);
    end
   else
    Data := nil;
end;


procedure TDmxLabels.Store (var S : TStream);
begin
  TDmxLink.Store (S);
  S.Write (Len, sizeof (Len));
  If Len > 0 then S.Write (Data^, Len);
end;


  { ออ TDmxScroller ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxScroller.Init (ATemplate : string; var AData;
			       BSize : longint; var Bounds : TRect;
			       ALabels : PView;
			       AHScrollBar,AVScrollBar : PScrollBar);
begin
  TScroller.Init (Bounds, AHScrollBar, AVScrollBar);
  InitValid     := TRUE;
  DataBlockSize := BSize;
  InitStruct (ATemplate);
  InitData (AData);
  If RecordSize > 0 then SetLimit (vwidth, DataBlockSize div RecordSize);
  LeftField := DMXfield1;
  GrowMode  := gfGrowHiX or gfGrowHiY;
  Labels    := PDmxLink (ALabels);
  If Labels <> nil then Labels^.Link := @Self;
end;


destructor TDmxScroller.Done;
begin
  TScroller.Done;
  DoneData;
  DoneStruct;
end;


constructor TDmxScroller.Load (var S : TStream);
begin
  TScroller.Load (S);
  InitValid := TRUE;
  GetPeerViewPtr (S, Labels);
  S.Read (TotalFields, sizeof (TotalFields));
  S.Read (RecordSize,  sizeof (RecordSize));
  S.Read (CurrentRecord, sizeof (CurrentRecord));
  S.Read (DataBlockSize, sizeof (DataBlockSize));
  InBuffer  := FALSE;
  LoadData (S);
  LoadStruct (S);
end;


procedure TDmxScroller.Store (var S : TStream);
begin
  TScroller.Store (S);
  PutPeerViewPtr (S, Labels);
  S.Write (TotalFields, sizeof (TotalFields));
  S.Write (RecordSize,  sizeof (RecordSize));
  S.Write (CurrentRecord, sizeof (CurrentRecord));
  S.Write (DataBlockSize, sizeof (DataBlockSize));
  StoreData (S);
  StoreStruct (S);
end;


procedure TDmxScroller.LoadData (var S : TStream);
begin
  Abstract;
end;


procedure TDmxScroller.LoadStruct (var S : TStream);
var n	 : integer;
    P,Px : pDMXfieldrec;
begin
  S.Read (vwidth, sizeof (vwidth));
  DMXfield1 := nil;
  S.Read (n, sizeof (n));
  Px := nil;
  While (n > 0) do
    begin
    GetMem (P, sizeof (P^));
    S.Read (P^, sizeof (P^));
    If (P^.template <> nil) then P^.template := S.ReadStr;
    If DMXfield1 = nil then DMXfield1 := P;
    If Px <> nil then Px^.Next := P;
    P^.Prev := Px;
    P^.Next := nil;
    Px      := P;
    Dec (n);
    end;
  LeftField := DMXfield1;
end;


procedure TDmxScroller.StoreData (var S : TStream);
begin
  Abstract;
end;


procedure TDmxScroller.StoreStruct (var S : TStream);
var  n : integer;
     P : pDMXfieldrec;
begin
  S.Write (vwidth, sizeof (vwidth));
  n  := 0;
  P  := DMXfield1;
  While (P <> nil) do
    begin
    Inc (n);
    P := P^.Next;
    end;
  S.Write (n, sizeof (n));
  P := DMXfield1;
  While (P <> nil) do
    begin
    S.Write (P^, sizeof (P^));
    If (P^.template <> nil) then S.WriteStr (P^.template);
    P := P^.Next;
    end;
end;


procedure TDmxScroller.InitStruct (var ATemplate );
var
  i,j		:  integer;
  SameFieldNum	:  boolean;
  WasSameNum	:  boolean;
  AllZeroes	:  boolean;
  C		:  char;
  DoDecimal	:  integer;
  dataformat	:  pstring;
  Rex,X		:  pDMXfieldrec;
  templx	:  string;

  procedure NewRecord;
  var i,j : integer;
      A   : pstring;
  begin
    If not InitValid then Exit;
    With Rex^ do
      begin
      If DoDecimal > 0 then Rex^.decimals := pred (DoDecimal);
      DoDecimal := 0;
      If (fieldsize = 0) then
	access := access or accSkip
       else
	begin
	If SameFieldNum then
	  fieldnum := succ (TotalFields)
	 else
	  If (access and accHidden = 0) or WasSameNum then
	    begin
	    Inc (TotalFields);
	    fieldnum := TotalFields;
	    end;
	datatab    := RecordSize;
	RecordSize := RecordSize + fieldsize;
	end;
      screentab  := vwidth;
      If (typecode = fldBOOLEAN) and (truelen = 0) then showzeroes := FALSE;
      If access and accHidden = 0 then vwidth := vwidth + length (templx);
      If length (templx) > 0 then
	begin
	If (MaxAvail > length (templx)) then
	  template  := NewStr (templx)
	 else
	  InitValid := FALSE;
	templx := '';
	end
       else
	begin
	If (typecode <> #0) and (access and accHidden = 0) then Inc (vwidth);
	end;
      end;
    If (MaxAvail > sizeof (Rex^)) then
      begin
      New (Rex^.Next);
      X   := Rex;
      Rex := Rex^.Next;
      fillchar (Rex^, sizeof (Rex^), 0);
      Rex^.Prev := X;
      Rex^.showzeroes := AllZeroes;
      end
     else
      InitValid := FALSE;
    WasSameNum := FALSE;
  end;

begin
  SameFieldNum := FALSE;
  WasSameNum   := FALSE;
  AllZeroes    := FALSE;
  dataformat   := @ATemplate;
  If dataformat = nil then Exit;
  templx    := '';
  DoDecimal :=  0;
  New (Rex);
  fillchar (Rex^, sizeof (Rex^), 0);
  Rex^.showzeroes := AllZeroes;
  If DMXfield1 = nil then
    DMXfield1 := Rex
   else
    begin
    X := DMXfield1;
    While X^.Next <> nil do X := X^.Next;
    X^.Next := Rex;
    Rex^.Prev := X;
    end;
  i := 1;
  While (i <= length (dataformat^)) do
    begin
    C := upcase (dataformat^ [i]);
    Case C of
      fldSTR, fldSTRNUM:
	With Rex^ do
	  begin
	  templx   := templx + #0;
	  typecode := dataformat^ [i];
	  Inc (truelen);
	  If fieldsize > 0 then
	    Inc (fieldsize)
	   else
	    begin
	    fieldsize :=  2;
	    fillvalue := ' ';
	    end;
	  end;
      fldCHAR, fldCHARVAL, fldCHARNUM:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  Inc (fieldsize);
	  If fieldsize > 0 then fillvalue := ' ';
	  If DoDecimal > 0 then Inc (DoDecimal);
	  end;
      fldBYTE, fldSHORTINT, fldBOOLEAN:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  If upcase (C) <> fldSHORTINT then C := upcase (C);
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  fieldsize := sizeof (BYTE);
	  If fieldsize > 0 then fillvalue := #0;
	  end;
      ^X :
	With Rex^ do
	  begin
	  typecode  := fldBOOLEAN;
	  truelen   := 0;
	  fieldsize := sizeof (BOOLEAN);
	  If fieldsize > 0 then fillvalue := #0;
	  end;
      fldZEROMOD:  { 'Z' }
	With Rex^ do
	  begin
	  If (typecode = #0) or (typecode = fldCHARVAL) then Inc (fieldsize);
	  templx := templx + #1;
	  Inc (truelen);
	  end;
      fldWORD, fldINTEGER:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  fieldsize := sizeof (INTEGER);
	  If fieldsize > 0 then fillvalue := #0;
	  end;
      fldLONGINT:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  fieldsize := sizeof (LONGINT);
	  If fieldsize > 0 then fillvalue := #0;
	  end;
      fldHEXVALUE:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  fieldsize := succ (truelen) shr 1;
	  If fieldsize > 0 then fillvalue := #0;
	  end;
      fldREALNUM:
	With Rex^ do
	  begin
	  templx    := templx + #0;
	  typecode  := dataformat^ [i];
	  Inc (truelen);
	  fieldsize := sizeof (TREALNUM);
	  fillvalue := #0;
	  If DoDecimal > 0 then Inc (DoDecimal);
	  end;
      ')','.':
	With Rex^ do
	  begin
	  templx := templx + C;
	  If (upcase (Rex^.typecode) = fldCHARVAL) then
	    begin
	    If (C = ')') then Inc (truelen);
	    Inc (fieldsize);
	    end;
	  If (C = '.') then
	    begin
	    If (upcase (typecode) = fldREALNUM) or
	       (upcase (typecode) = fldCHARVAL) then
	      DoDecimal := 1;
	    end
	   else
	    parenthesis := TRUE;
	  end;
      '~':
	begin
	Inc (i);
	While (dataformat^[i] <> '~') and (i <= length (dataformat^)) do
	  begin
	  C := dataformat^ [i];
	  If C = #0 then C := ' ';
	  If C = #1 then C := #2;
	  templx := templx + C;
	  Inc (i);
	  end;
	end;
      #0,'\','|','ณ','บ':
	begin
	If (templx <> '') then NewRecord;
	If C <> #0 then
	  begin
	  If C = '|' then C := 'ณ' else If C = '\' then C := ' ';
	  Rex^.access    := Rex^.access or accDelimiter;
	  Rex^.typecode  := C;
	  NewRecord;
	  end;
	end;
      ^A:
	begin
	AllZeroes	:= not AllZeroes;
	Rex^.showzeroes  := AllZeroes;
	end;
      ^D:
	begin
	If (templx <> '') then NewRecord;
	Inc (i);
	C := dataformat^ [i];
	Rex^.access    := Rex^.access or accDelimiter;
	Rex^.typecode  := C;
	NewRecord;
	end;
      ^F:   begin
	    SameFieldNum := not SameFieldNum;
	    WasSameNum   := not SameFieldNum;
	    end;
      ^H:   With Rex^ do access := access or accHidden;
      ^P:   With Rex^ do
	      begin
	      Inc (i);
	      RecordSize := RecordSize + shortint (dataformat^ [i]);
	      end;
      ^R:   With Rex^ do access := access or accReadOnly;
      ^S:   With Rex^ do access := access or accSkip;
      ^U:   With Rex^ do
	      begin
	      Inc (i);
	      upperlimit := byte (dataformat^ [i]);
	      end;
      ^V:   With Rex^ do
	      begin
	      Inc (i);
	      fillvalue := dataformat^ [i];
	      end;
      ^Z:   Rex^.showzeroes := TRUE;
     else
	begin
	templx := templx + dataformat^ [i];
	end;
      end;  { case of C }
    Inc (i);
    end;
  SameFieldNum := FALSE;
  If templx <> '' then NewRecord;
  Dispose (Rex);
  X^.Next := nil;
  If DMXfield1 <> nil then DMXfield1^.Prev := X;
end;


procedure TDmxScroller.DoneStruct;
var  P : pDMXfieldrec;
begin
  While DMXfield1 <> nil do
    begin
    P := DMXfield1^.Next;
    If DMXfield1^.template <> nil then DisposeStr (DMXfield1^.template);
    Dispose (DMXfield1);
    DMXfield1 := P;
    end;
  LeftField    := nil;
  TotalFields  :=  0;
  RecordSize   :=  0;
  vwidth       :=  0;
end;


procedure TDmxScroller.InitData (var AData );
begin
  WorkingData := @AData;
end;


procedure TDmxScroller.DoneData;
begin
end;


function  TDmxScroller.Valid (Command : word)  : boolean;
var  V : boolean;
begin
  V := TScroller.Valid (Command);
  If (Command = cmValid) then V := V and InitValid;
  Valid := V;
end;


procedure TDmxScroller.ChangeBounds (var Bounds : TRect);
begin
  InBuffer := FALSE;
  TScroller.ChangeBounds (Bounds);
end;


function  TDmxScroller.GetPalette : PPalette;
begin
  GetPalette := @cDMX
end;


procedure TDmxScroller.HandleEvent (var Event : TEvent);
var  WasHere : boolean;
begin
  TScroller.HandleEvent (Event);
  With Event do
    If (What and evMessage <> 0) then
      begin
      WasHere := TRUE;
      If (((Command = cmDMX_DrawData) and (WorkingData = InfoPtr)) or
	  ((Command = cmDMX_Draw) and
	  (InfoPtr <> @Self) and
	  ((PDmxScroller (InfoPtr)^.WorkingData = WorkingData) or (What = evCommand))))
      then DrawView
      else
      If not Locked and (((Command = cmDMX_LockData) and (WorkingData = InfoPtr)) or
	((Command = cmDMX_Lock) and
	((PDmxScroller (InfoPtr)^.WorkingData = WorkingData) or (What = evCommand))))
      then Locked := TRUE
      else
      If Locked and (((Command = cmDMX_UnlockData) and (WorkingData = InfoPtr)) or
	((Command = cmDMX_Unlock) and
	((PDmxScroller (InfoPtr)^.WorkingData = WorkingData) or (What = evCommand))))
      then Locked := FALSE
      else
      If (Command = cmDMX_RollCall) then
	begin
	If (InfoPtr <> @Self) and (InfoPtr <> nil) then
	  Message (InfoPtr, evCommand, cmDMX_Ack, @Self);
	end
      else
        WasHere := FALSE;
      If WasHere and (What = evCommand) then ClearEvent (Event);
      end;
end;


procedure TDmxScroller.WrongKeypressed (var Event : TEvent);
begin
  Message (Application, evCommand, cmDMX_WrongKey, @Self);
end;


procedure TDmxScroller.SetState (AState : word; Enable : boolean);
var  L : longint;
begin
  If (AState and sfFocused <> 0) then
    begin
    If Enable then
      begin
      If (RecordSize > 0) then
	begin
	L := DataBlockSize - (DataBlockSize mod RecordSize);
	If (L <> RecordSize * Limit.Y) then
	  SetLimit (vwidth, DataBlockSize div RecordSize);
	end;
      If (Application <> nil) then
	TScroller.SetState (sfCursorIns, Application^.GetState (sfCursorIns));
      end
     else
      begin
      If (Application <> nil) then
	Application^.SetState (sfCursorIns, GetState (sfCursorIns));
      end;
    end;
  TScroller.SetState (AState, Enable);
end;


function  TDmxScroller.DataAt (RecNum : integer) : pointer;
begin
  DataAt := ptr (PtrRec (WorkingData).Seg, PtrRec (WorkingData).Ofs + RecNum * RecordSize);
end;


procedure TDmxScroller.DrawRecord (Y : integer;  var DataRecord );
var Color		: word;
    ColorA, ColorB	: word;
    I,X, width		: integer;
    frontcut		: integer;
    fieldrec		: pDMXfieldrec;
    A			: string;
    B			: TDrawBuffer;
begin
  fieldrec := LeftField;
  frontcut := Delta.X - fieldrec^.screentab;
  X	   := 0;
  ColorA   := GetColor (1);
  ColorB   := GetColor (5);
  While (X < Size.X) do
    begin
    With fieldrec^ do
      begin
      If (access and accHidden = 0) then
	begin
	If access and accDelimiter <> 0 then
	  begin
	  A	:= typecode;
	  Color := ColorB;
	  end
	 else
	  begin
	  If @DataRecord = nil then
	    begin
	    A [0] := fieldrec^.template^ [0];
	    fillchar (A [1], length (fieldrec^.template^), ' ');
	    end
	   else
	    A	:= FieldString (fieldrec, [], DataRecord);
	  If frontcut > 0 then Delete (A, 1, frontcut);
	  Color := ColorA;
	  end;
	frontcut := 0;
	MoveStr (B [X], A, Color);
	X  := X + length (A);
	end;
      end;
    fieldrec := fieldrec^.Next;
    If (fieldrec = nil) and (Size.X > X) then
      begin
      MoveChar (B [X], ' ', ColorA, Size.X - X);
      X  := Size.X;
      end;
    end;
  WriteLine (0, Y, Size.X, 1, B);
end;


procedure TDmxScroller.Draw;
var
  i,rows,Y,owid  :  integer;
  A   :  string;
  B   :  TDrawBuffer;
  Buf : ^TDrawBuffer;
begin
  HideCursor;
  rows := Size.Y;
  Y    := -1;
  LeftField  := DMXfield1;
  While (LeftField^.Next^.screentab <= Delta.X) and
	(LeftField^.Next <> nil)
   do
    LeftField := LeftField^.Next;
  If (Labels <> nil) and (DDelta.X <> Delta.X) then Labels^.DrawView;
  If (Owner^.Buffer <> nil) and InBuffer then
    begin
    If (Delta.X = DDelta.X) and (abs (Delta.Y - DDelta.Y) = 1) and
       (Size.Y > 1) and (longint (Size) = longint (DSize))
     then
      begin
      owid := Owner^.Size.X shl 1;
      longint (Buf) := longint (Owner^.Buffer) + ((Origin.Y * owid) + (Origin.X shl 1));
      If (Delta.Y > DDelta.Y) then  { Down }
	begin
	For i := 0 to (Size.Y - 2) do
	  begin
	  ptrrec (Buf).ofs := ptrrec (Buf).ofs + owid;
	  WriteBuf (0, i, Size.X, 1, Buf^);
	  end;
	Y := Size.Y - 2;
	end
       else  { Up }
	begin
	ptrrec (Buf).ofs := ptrrec (Buf).ofs + ((Size.Y - 2) * owid);
	For i := (Size.Y - 1) downto 1 do
	  begin
	  WriteBuf (0, i, Size.X, 1, Buf^);
	  ptrrec (Buf).ofs := ptrrec (Buf).ofs - owid;
	  end;
	Rows := 1;
	end;
      end;
    end;
  If rows > 0 then
    begin
    While (Y < pred (rows)) do
      begin
      Inc (Y);
      If Y + Delta.Y < Limit.Y then
	DrawRecord (Y, DataAt (Y + Delta.Y)^)
       else
	DrawRecord (Y, Mem [0:0]);
      end;
    end;
  DDelta   := Delta;
  DSize    := Size;
  InBuffer := (Owner^.Buffer <> nil);
end;


  { ออ TDmxRecInd ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxRecInd.Init (Bounds : TRect;  Len : integer);
begin
  TDmxLink.Init (Bounds);
  GrowMode  := gfGrowLoY or gfGrowHiY;
  EventMask := evMessage;
end;


constructor TDmxRecInd.InitInsert (AOwner : PGroup; Len : integer);
var  R : TRect;
begin
  AOwner^.GetExtent (R);
  Inc (R.A.X);
  R.A.Y  := pred (R.B.Y);
  R.Grow (-1, 0);
  If (R.B.X - R.A.X > Len) then R.B.X := R.A.X + Len;
  R.B.Y  := succ (R.A.Y);
  TDmxLink.Init (R);
  GrowMode  := gfGrowLoY or gfGrowHiY;
  EventMask := evMessage;
  Insert (AOwner);
end;


procedure TDmxRecInd.SetState (AState : word;  Enable : boolean);
begin
  If (AState and (sfActive or sfDragging) <> 0) then
    TDmxLink.SetState (sfVisible, Enable xor (AState and sfDragging <> 0));
  TDmxLink.SetState (AState, Enable);
end;


procedure TDmxRecInd.Draw;
var  A  : string;
     B  : TDrawBuffer;
     C  : word;
begin
  C := GetColor (6);
  MoveChar (B, 'อ', C, Size.X);
  Str (succ (Link^.CurrentRecord):1, A);
  If length (A) > Size.X then
    MoveChar (B, showOVERFLOW, C, Size.X)
   else
    begin
    If length (A) < Size.X then A := A + ' ';
    If length (A) < Size.X then A := ' ' + A;
    MoveStr (B [succ ((Size.X) - length (A)) shr 1], A, C);
    end;
  WriteBuf (0, 0, Size.X, 1, B);
end;


  { ออ TDmxEditor อออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxEditor.Init (ATemplate  : string;  var AData;  BSize : longint;
			     var Bounds : TRect;  ALabels,ARecInd  : PDmxLink;
			     AHScrollBar,AVScrollBar : PScrollBar);
var  inbounds  : TRect;
begin
  TDmxScroller.Init (ATemplate, AData, BSize, Bounds, ALabels, AHScrollBar, AVScrollBar);
  CurrentField := DMXfield1;
  While (CurrentField <> nil) and
	(CurrentField^.access and (accHidden or accSkip or accDelimiter) <> 0)
   do
    CurrentField := CurrentField^.Next;
  CurrentRecord  := 0;
  RecInd := ARecInd;
  If RecInd <> nil then
    begin
    RecInd^.Link := @Self;
    If (HScrollBar <> nil) then
      begin
      HScrollBar^.GetBounds (inbounds);
      inbounds.A.X := inbounds.A.X + RecInd^.Size.X + 1;
      HScrollBar^.Locate (inbounds);
      end;
    end;
end;


constructor TDmxEditor.Load (var S : TStream);
var  i,n : integer;
begin
  TDmxScroller.Load (S);
  GetPeerViewPtr (S, RecInd);
  CurrentField := DMXfield1;
  S.Read (n, sizeof (n));
  i := 0;
  While (i <> n) and (CurrentField <> nil) do
    begin
    CurrentField := CurrentField^.Next;
    Inc (i);
    end;
  If CurrentField = nil then CurrentField := DMXfield1;
  S.Read (Locked, sizeof (Locked));
  FieldAltered    :=  FALSE;
  If CurrentField <> nil then
    begin
    SetUpRecord;
    SetUpField;
    end;
end;


destructor TDmxEditor.Done;
begin
  If (CurrentField <> nil) then
    begin
    If FieldSelected  then EvaluateField;
    If RecordSelected then EvaluateRecord;
    end;
  TDmxScroller.Done;
end;


procedure TDmxEditor.Store (var S : TStream);
var n  : integer;
    df : pDMXfieldrec;
begin
  If CurrentField <> nil then
    begin
    EvaluateField;
    EvaluateRecord;
    end;
  TDmxScroller.Store (S);
  PutPeerViewPtr (S, RecInd);
  df := DMXfield1;
  n  := 0;
  While (df <> CurrentField) do
    begin
    df := df^.Next;
    Inc (n);
    end;
  S.Write (n, sizeof (n));
  S.Write (Locked, sizeof (Locked));
end;


procedure TDmxEditor.SetState (AState : word; Enable : boolean);

    procedure HoldState (On : boolean);
    begin
      If On then
	begin
	RedrawRecord := TRUE;
	If (DataBlockSize > 0) and (RecordSize > 0) and
	   (DataBlockSize div RecordSize < CurrentRecord)
	 then CurrentRecord := DataBlockSize div RecordSize;
	SetUpRecord;
	SetUpField;
	TDmxScroller.SetState (AState, Enable);
	end
       else
	begin
	TDmxScroller.SetState (AState, Enable);
	EvaluateField;
	EvaluateRecord;
	If JustAltered then
	  begin
	  If DeskTop <> nil then Message (DeskTop, evBroadcast, cmDMX_Draw, @Self);
	  JustAltered := FALSE;
	  end;
	end;
    end;

begin
  If not Vidis and (CurrentField <> nil) and (AState and sfFocused <> 0) then
    HoldState (Enable)
  else
  If (AState and sfDragging <> 0) then
    HoldState (not Enable)
  else
  TDmxScroller.SetState (AState, Enable);
end;


procedure TDmxEditor.ChangeBounds (var Bounds : TRect);
var  i,j       : integer;
     ReScroll  : boolean;
     xy	: TPoint;
begin
  TDmxScroller.ChangeBounds (Bounds);
  ReScroll := FALSE;
  If CurrentField <> nil then With CurrentField^ do
    If (template <> nil) then
      begin
      xy := Delta;
      If (Size.X - (screentab - Delta.X) < 0) or
	 (Size.X <= length (template^)) then
	begin
	xy.X  := screentab + length (template^) - Size.X;
	If (Size.X <= length (template^)) then xy.X := screentab else If (xy.X > 0) then Inc (xy.X);
	ReScroll := TRUE;
	end
       else
	If (Size.X - (screentab + length (template^) - Delta.X) < 0) then
	  begin
	  xy.X  := screentab + length (template^) - Size.X;
	  ReScroll := TRUE;
	  end;
      end;
    If (Size.Y - (CurrentRecord - Delta.Y) <= 0) then
      begin
      xy.Y := succ (CurrentRecord - Size.Y);
      If xy.Y < 0 then xy.Y := 0;
      ReScroll := TRUE;
      end;
  If ReScroll then ScrollTo (xy.X, xy.Y);
end;


procedure TDmxEditor.ChangeMade;
begin
  FieldAltered  := TRUE;
  RecordAltered := TRUE;
  JustAltered   := TRUE;
  DataAltered   := TRUE;
end;


procedure TDmxEditor.SetUpField;
begin
  FieldSelected := TRUE;
  FieldAltered  := FALSE;
  FieldData := ptr (seg (RecordData^), ofs (RecordData^) + CurrentField^.datatab);
  FirstKey  := TRUE;
  ShowFmt   := [showanyway];
  CurPos    :=  0;
  FirstPos  :=  0;
  With CurrentField^ do
    If upcase (typecode) in [fldCHARVAL, fldBYTE, fldSHORTINT, fldWORD,
			     fldINTEGER, fldLONGINT, fldREALNUM, fldHEXVALUE]
     then
      begin
      CurPos := pred (truelen - decimals);
      If CurPos < 0 then CurPos := 0;
      end;
  If GetState (sfVisible) then DrawField (CurrentField);
  If (RecInd <> nil) then RecInd^.DrawView;
end;


procedure TDmxEditor.EvaluateField;
begin
  ShowFmt   := ShowFmt + [showregular] - [shownegative] - [showanyway];
  DrawField (CurrentField);
  ShowFmt   := ShowFmt - [showregular];
  If FieldAltered then Message (Owner, evBroadcast, cmDMX_FieldAltered, @Self);
  FieldSelected := FALSE;
end;


procedure TDmxEditor.SetUpRecord;
begin
  RecordData	 := DataAt (CurrentRecord);
  RecordAltered  := FALSE;
  RecordSelected := TRUE;
end;


procedure TDmxEditor.EvaluateRecord;
begin
  RecordSelected := FALSE;
end;


procedure TDmxEditor.Draw;
begin
  TDmxScroller.Draw;
  If FieldSelected and (showanyway in ShowFmt) then DrawField (CurrentField);
end;


procedure TDmxEditor.DrawField (var Field : pDMXfieldrec);
const
  rpoint = #16;
  lpoint = #17;
var
  Color  : byte;
  i,j,k  : integer;
  x1,x2  : integer;
  Len    : integer;
  front  : boolean;
  hyde   : boolean;
  S      : string;
  B      : TDrawBuffer;
begin
  If (CurrentField = nil) then Exit;
  If RedrawRecord then
    begin
    DrawRecord (CurrentRecord - Delta.Y, RecordData^);
    RedrawRecord := FALSE;
    end;
  hyde := TRUE;
  With Field^ do If (template <> nil) and (length (template^) > 0) then
    begin
    If (access and (accHidden or accDelimiter) = 0) then
      begin
      S  := FieldString (Field, ShowFmt, RecordData^);
      x1 := screentab - Delta.X;
      x2 := x1 + length (S);
      If x1 < 0 then
	begin
	x1 := 0;
	front := FALSE;
	end
       else
	front := TRUE;
      If x2 > Size.X then x2 := Size.X;
      Len  := x2 - x1;
      If Len > 0 then
	begin
	If not (showregular in ShowFmt) then
	  begin
	  j := 0;
	  k := 0;
	  If fieldsize > 0 then
	    For i := 1 to length (S) do
	      If (ord (template^ [i]) and $FE = 0) then
		begin
		If (CurPos >= j) then k := i;
		Inc (j);
		end;
	  If k > 0 then
	    begin
	    If CurPos = 0 then FirstPos := 0;
	    If (CurPos = truelen) and (length (S) > Len) then
	      FirstPos := length (S) - Len;
	    If length (S) <= Len then
	      begin
	      FirstPos := 0;
	      end
	     else
	      begin
	      If pred (k) <= FirstPos then
		begin
		FirstPos := pred (k);
		If FirstPos > 0 then
		  begin
		  Delete (S, 1,FirstPos);
		  k := k - FirstPos;
		  end;
		end
	       else
		begin
		j := 0;
		If FirstPos > 0 then
		  begin
		  Delete (S, 1,FirstPos);
		  k := k - FirstPos;
		  j := FirstPos;
		  end;
		If length (S) > Len then
		  begin
		  If k > Len then
		    begin
		    i := k - Len;
		    FirstPos := i + j;
		    If i > 0 then Delete (S, 1, i);
		    k := k - i;
		    end;
		  end;
		end;
	      end;
	    If Len > 3 then
	      begin
	      If (k = Len) and (length (S) > Len) then
		begin
		Delete (S, 1,1);
		Inc (FirstPos);
		Dec (k);
		end;
	      If (FirstPos > 0) then
		begin
		If k > 1 then S [1] := lpoint
		 else
		  begin
		  System.Insert (lpoint, S, 1);
		  Inc (k);
		  Inc (FirstPos);
		  end;
		end;
	      If length (S) > Len then S [Len] := rpoint;
	      end;
	    SetCursor (pred (k) + x1, CurrentRecord - Delta.Y);
	    end;
	  If Locked or (access and accReadOnly <> 0) then
	    begin
	    Color := GetColor (3);
	    end
	   else
	    begin
	    If (k > 0) and not GetState (sfDragging) then hyde := FALSE;
	    Color := GetColor (2);
	    end;
	  end
	 else
	  begin
	  If (length (S) > Len) and not front then Delete (S, 1, length (S) - Len);
	  Color := GetColor (1);
	  end;
	MoveStr (B, S, Color);
	i := CurrentRecord - Delta.Y;
	WriteLine (x1, i, Len, 1, B);
	end;
      end;
    end;
  If hyde then HideCursor else ShowCursor;
end;


procedure TDmxEditor.ZeroizeRecord;
var  field : pDMXfieldrec;
begin
  field := DMXfield1;
  If (RecordData <> nil) then
    While (field <> nil) do
      begin
      ZeroizeField (FALSE, field);
      field := field^.Next;
      end;
end;


procedure TDmxEditor.ZeroizeField (Whole : boolean;  Field : pDMXfieldrec);
var  FData : pointer;
     fn    : byte;
begin
  If (RecordData = nil) or (Field = nil) or Locked then Exit;
  fn := Field^.fieldnum;
  If Whole and (fn <> 0) then Field := DMXfield1;
  While Field <> nil do
    begin
    If Field^.fieldnum = fn then
      begin
      With Field^ do
	If (access and accReadOnly = 0) and (fieldsize > 0) then
	  begin
	  FData := ptr (seg (RecordData^), ofs (RecordData^) + datatab);
	  fillchar (FData^, fieldsize, fillvalue);
	  Case upcase (typecode) of
	    fldSTR,
	    fldSTRNUM:   pstring (FData)^ [0] := #0;
	    fldCHARVAL:
	      begin
	      fillchar (FData^, fieldsize, '0');
	      If fieldsize - decimals > 2 then fillchar (FData^, fieldsize - decimals - 2, ' ');
	      If decimals > 0 then pstring (FData)^ [fieldsize - decimals - 1] := '.';
	      end;
	    end;
	  ChangeMade;
	  end;
      end;
    If Whole and (fn <> 0) then Field := Field^.Next else Field := nil;
    end;
  FirstKey	:= TRUE;
  RedrawRecord	:= TRUE;
end;


procedure TDmxEditor.GotoPos (AFieldNum,ARecNum : integer);
var RS  : integer;
    X,Y : integer;
    F   : pDMXfieldrec;
begin
  If RecordSelected then
    begin
    If FieldSelected then
      begin
      RS := 2;
      EvaluateField;
      end
     else
      RS := 1;
    EvaluateRecord;
    end
   else
    RS := 0;
  CurrentRecord := ARecNum;
  Y := CurrentRecord - (Size.Y shr 1);
  If (Y < 0) then Y := 0;
  F := DMXfield1;
  While (F^.fieldnum <> AFieldNum) and (F <> nil) do F := F^.Next;
  If (F = nil) or (AFieldNum = 0) then
    X := Delta.X
   else
    begin
    X := F^.screentab;
    CurrentField := F;
    end;
  If (X > Limit.X) then X := Limit.X;
  If (Y > Limit.Y) then Y := Limit.Y;
  ScrollTo (X, Y);
  If (RS > 0) then
    begin
    SetupRecord;
    If (RS = 2) then SetupField;
    end;
end;


procedure TDmxEditor.HandleEvent (var Event : TEvent);
var  XY : TPoint;
     RS,FS : boolean;
begin
  RS := FALSE;
  FS := FALSE;
  If not GetState (sfDragging) then
    begin
    Case Event.What of
      evNothing:   begin end;
      evMouseDown: ProcessMouse (Event);
      evKeyDown:
	  If (Event.KeyCode <> kbEsc) then
	    begin
	    If (Event.KeyCode = kbEnter) then ProcessEnter (Event);
	    If (Event.What = evKeyDown)  then ProcessKey (Event);
	    end;
      evCommand:
	  If (Event.Command >= cmDMX_ZeroizeRec) and
	     (Event.Command <= cmDMX_Bottom) then
	    begin
	    ProcessCommand (Event.Command, XY);
	    If (Event.Command = 0) then ClearEvent (Event);
	    end;
      end;
    With Event do If (What and evMessage <> 0) then
      If ((Command = cmDMX_DrawData) and (WorkingData = InfoPtr)) or
         ((Command = cmDMX_LockData) and (WorkingData = InfoPtr)) or
         ((Command = cmDMX_UnlockData) and (WorkingData = InfoPtr)) or
         ((((InfoPtr <> @Self) and (PDmxScroller (InfoPtr)^.WorkingData = WorkingData)) or
            (What = evCommand)) and
            ((Command = cmDMX_Draw) or ((Command = cmDMX_Lock) or ((Command = cmDMX_Unlock)))))
       then
        begin
        RS := RecordSelected;
        If RS then
          begin
          FS := FieldSelected;
          If FS then EvaluateField;
          EvaluateRecord;
          end;
        end;
    end;
  If (Event.What <> evNothing) then TDmxScroller.HandleEvent (Event);
  If RS then
    begin
    SetupRecord;
    If FS then SetupField;
    end;
end;


procedure TDmxEditor.ProcessMouse (var Event : TEvent);
var
  i,j		: word;
  MousePlace	: TPoint;
begin
  With Event do
    If (Event.What = evMouseDown) and GetState (sfFocused) and
       (MouseInView (Where)) then
      begin
      MakeLocal (Where, MousePlace);
      MousePlace.X := MousePlace.X + Delta.X;
      MousePlace.Y := MousePlace.Y + Delta.Y;
      i := cmDMX_goto;
      ProcessCommand (i, MousePlace);
      ClearEvent (Event);
      end;
end;


procedure TDmxEditor.ProcessCommand (var Command : word;  XY : TPoint);
var
  i,j   : word;
  xx,yy : integer;
  DoIt  : integer;
  F     : pDMXfieldrec;
  RS,FS : boolean;

    procedure DoHome;
    begin
      F := DMXfield1;
      If F <> nil then
	begin
	While (F^.access and (accHidden or accSkip or accDelimiter) <> 0)
	  and (F^.Next <> nil)
	 do
	  F := F^.Next;
	CurrentField := F;
	end;
      If CurrentField <> nil then With CurrentField^ do
	begin
	xx := 0;
	If (screentab + length (template^) - 1 > Size.X) then xx := screentab;
	end;
    end;

begin
  If (Command = cmDMX_WrongKey) then Exit;
  RS   := RecordSelected;
  FS   := FieldSelected;
  DoIt :=  0;
  xx   := Delta.X;
  yy   := Delta.Y;
  If (Command >= cmDMX_Enter) and (Command <= cmDMX_Bottom) then
    begin
    If FS then EvaluateField;
    DoIt  :=  1;
    If (Command > cmDMX_goto) then
      begin
      If RS then EvaluateRecord;
      DoIt  :=  2;
      end;
    end;
  If ReDrawRecord then
    begin
    DrawRecord (CurrentRecord - Delta.Y, RecordData^);
    ReDrawRecord := FALSE;
    end;

  Case Command of

    cmDMX_ZeroizeRec:  ZeroizeRecord;

    cmDMX_Left:
	If CurrentField <> DMXfield1 then
	  begin
	  F := CurrentField^.Prev;
	  While (F^.access and (accHidden or accSkip or accDelimiter) <> 0)
	    and (F <> nil)
	   do
	    begin
	    If F = DMXfield1 then F := nil else F := F^.Prev;
	    end;
	  If F <> nil then CurrentField := F;
	  If CurrentField <> nil then With CurrentField^ do
	    begin
	    If (screentab < xx) then
	      begin
	      xx := screentab;
	      If (xx > 0) and (Size.X > length (template^)) then Dec (xx);
	      end;
	    end;
	  end;

    cmDMX_Right:
	begin
	F := CurrentField^.Next;
	While (F^.access and (accHidden or accSkip or accDelimiter) <> 0)
	  and (F <> nil)
	 do
	  F := F^.Next;
	If F <> nil then CurrentField := F;
	If CurrentField <> nil then With CurrentField^ do
	  begin
	  If (screentab + length (template^) - 1 > xx + pred (Size.X)) then
	    begin
	    xx := screentab + length (template^) - Size.X;
	    If (xx < Limit.X) and (Size.X > length (template^)) then Inc (xx);
	    end;
	  end;
	end;

    cmDMX_Home:  DoHome;

    cmDMX_End:
	begin
	F := CurrentField;
	If F <> nil then
	  begin
	  While (F^.Next <> nil) do F := F^.Next;
	  While (F^.access and (accHidden or accSkip or accDelimiter) <> 0)
	    and (F^.Prev <> nil)
	   do
	    F := F^.Prev;
	  CurrentField := F;
	  xx := Limit.X;
	  With CurrentField^ do
	    If (screentab < xx) then
	      begin
	      xx := screentab;
	      If (xx > 0) and (Size.X > length (template^)) then Dec (xx);
	      end;
	  end;
	end;

    cmDMX_goto:
	begin
	F := CurrentField;
	DoubleValid := FALSE;
	If F <> nil then
	  begin
	  While ((F^.access and accHidden <> 0) or (F^.screentab < XY.x))
	     and (F^.Next <> nil)
	     and (F <> nil)
	   do
	    F := F^.Next;
	  If (F <> nil) then
	    begin
	    While ((F^.access and accHidden <> 0) or (F^.screentab > XY.x))
	       and (F <> nil)
	     do
	      F := F^.Prev;
	    If (F <> nil) and
	       (F^.access and (accDelimiter or accSkip) = 0)
	     then
	      begin
	      DoubleValid := TRUE;
	      With F^ do
		begin
		If (screentab < xx) then
		  begin
		  xx := screentab;
		  If (xx > 0) and (Size.X > length (template^)) then Dec (xx);
		  end
		 else
		  begin
		  If (screentab + length (template^) - 1 > xx + pred (Size.X)) then
		    begin
		    xx := screentab + length (template^) - Size.X;
		    If (xx < Limit.X) and (Size.X > length (template^)) then Inc (xx);
		    end;
		  end;
		end;
	      If (CurrentRecord = XY.y) then
		CurrentField := F
	       else
		begin
		If RS then EvaluateRecord;
		DoIt  :=  2;
		If ReDrawRecord then
		  begin
		  DrawRecord (CurrentRecord - Delta.Y, RecordData^);
		  ReDrawRecord := FALSE;
		  end;
		CurrentField  :=  F;
		CurrentRecord := XY.y;
		If CurrentRecord >= Limit.Y then CurrentRecord := pred (Limit.Y);
		end;
	      end;
	    end;
	  end;
	end;

    cmDMX_NextRow:
	begin
	If succ (CurrentRecord) < Limit.Y then
	  begin
	  Inc (CurrentRecord);
	  If yy + Size.Y <= CurrentRecord then
	    yy := CurrentRecord - Size.Y + 1;
	  If yy < 0 then yy := 0;
	  end;
	DoHome;
	end;

    cmDMX_Up:
	begin
	If CurrentRecord > 0 then
	  begin
	  Dec (CurrentRecord);
	  If yy > CurrentRecord then yy := CurrentRecord;
	  end;
	end;

    cmDMX_Down:
	begin
	If succ (CurrentRecord) < Limit.Y then
	  begin
	  Inc (CurrentRecord);
	  If yy + Size.Y <= CurrentRecord then
	    yy := CurrentRecord - Size.Y + 1;
	  If yy < 0 then yy := 0;
	  end;
	end;

    cmDMX_PgUp:
	begin
	CurrentRecord := CurrentRecord - Size.Y + 1;
	If CurrentRecord < 0 then CurrentRecord := 0;
	yy := yy - Size.Y + 1;
	If yy < 0 then
	  begin
	  yy := 0;
	  CurrentRecord := 0;
	  end;
	end;

    cmDMX_PgDn:
	begin
	CurrentRecord := CurrentRecord + Size.Y - 1;
	If CurrentRecord >= Limit.Y then
	  CurrentRecord := pred (Limit.Y);
	If CurrentRecord < 0 then CurrentRecord := 0;
	yy := yy + Size.Y - 1;
	If yy < 0 then
	  begin
	  yy := 0;
	  CurrentRecord := 0;
	  end;
	If yy > Limit.Y + Size.Y - 1 then yy := Limit.Y + Size.Y - 1;
	end;

    cmDMX_ScreenTop:  CurrentRecord := Delta.Y;

    cmDMX_ScreenBottom:
	begin
	CurrentRecord := Delta.Y + Size.Y - 1;
	If CurrentRecord > Limit.Y then CurrentRecord := pred (Limit.Y);
	end;

    cmDMX_Top:
	begin
	CurrentRecord := 0;
	yy := 0;
	end;

    cmDMX_Bottom:
	begin
	CurrentRecord := pred (Limit.Y);
	If CurrentRecord < 0 then CurrentRecord := 0;
	yy := pred (Limit.Y);
	end;

   else begin  end;

    end;

  If DoIt <> 0 then
    begin
    If (xx <> Delta.X) or (yy <> Delta.Y) then ScrollTo (xx, yy);
    Command := 0;
    If (DoIt > 1) and RS then SetUpRecord;
    If (DoIt > 0) and FS then SetUpField;
    end;

end;


procedure TDmxEditor.ProcessEnter (var Event : TEvent);
var Cmd : word;
    TP  : TPoint;
begin
  If (CurrentField^.Next <> nil) then
    Event.KeyCode := kbCtrlRight
   else
    begin
    fillchar (TP, sizeof (TP), 0);
    Cmd := cmDMX_NextRow;
    ProcessCommand (Cmd, TP);
    ClearEvent (Event);
    end;
end;


procedure TDmxEditor.ProcessKey (var Event : TEvent);
var i,j,k : integer;
    inx   : integer;
    TC    : char;
    Go    : boolean;
    InsOn : boolean;
    Tabbing : boolean;
    A     : string [80];
    XY    : TPoint;
    DFld  : pDMXfieldrec;

  procedure QuitField (Command : word);
  begin
    ProcessCommand (Command, XY);
    Event.KeyCode := kbNoKey;
    ClearEvent (Event);
  end;

  function  HexByte (Number : byte)  : string;
  const bts  : array [0..15] of char = '0123456789ABCDEF';
  begin
    HexByte := bts [(Number shr 4) and $0F] + bts [Number and $0F]
  end;

  function  EffectField (HEX : boolean;  Min,Max : longint)  : boolean;
  var i,j	: integer;
      FirstChar : integer;
      b		: boolean;
      R		: real;
  begin
    b := FALSE;
    If not ((Event.CharCode in [^G,^H,^T,^Y,'.','-','_','0'..'9']) or
	   (HEX and (upcase (Event.CharCode) in ['A'..'F'])))
       or  (CurrentField^.access and accReadOnly <> 0)
       or  (Locked)
     then
      begin
      WrongKeypressed (Event);
      end
     else
      If A <> '' then With CurrentField^ do
	begin
	If (upperlimit <> 0) and (Max > upperlimit) then Max := upperlimit;
	If (decimals > 0) then i := succ (truelen) else i := truelen;
	If not HEX and (length (A) > i) then
	  begin
	  A [0] := chr (i);
	  fillchar (A [1], length (A), '0');
	  If length (A) - decimals > 2 then
	    fillchar (A [1], length (A) - decimals - 2, ' ');
	  If decimals > 0 then A [length (A) - decimals] := '.';
	  end;
	If typecode in ['A'..'Z'] then Min := 0;
	FirstChar := pos ('.', A);
	If FirstChar > 0 then Dec (FirstChar) else FirstChar := length (A);
	If CurPos < pred (FirstChar) then CurPos := pred (FirstChar);
	Case Event.CharCode of
	  ^G,
	  ^H  :
	      begin
	      If CurPos = pred (FirstChar) then
		begin
		If (FirstChar < length (A)) then
		  fillchar (A [FirstChar + 2], length (A) - succ (FirstChar), '0');
		If FirstChar > 1 then
		  begin
		  Move (A [1], A [2], pred (FirstChar));
		  If HEX then A [1] := '0' else A [1] := ' ';
		  If A [FirstChar] = '-' then
		    begin
		    A [FirstChar] := '0';
		    ShowFmt := ShowFmt - [shownegative];
		    end;
		  end
		 else
		  begin
		  A [1] := '0';
		  end;
		end
	       else
		begin
		A [succ (CurPos)] := '0';
		Dec (CurPos);
		If CurPos = FirstChar then Dec (CurPos);
		end;
	      b := FALSE;
	      For i := 1 to length (A) do If A [i] > '0' then b := TRUE;
	      If not b then ShowFmt := ShowFmt - [shownegative];
	      b := TRUE;
              If (A [FirstChar] = ' ') then A [FirstChar] := '0';
	      end;
	  ^T  :
	      begin
	      b := FALSE;
	      ZeroizeField (TRUE, CurrentField);
	      ShowFmt := ShowFmt - [shownegative];
	      CurPos  := pred (FirstChar);
	      For i := 1 to length (A) do If A [i] >= '0' then A [i] := '0';
	      end;
	  ^Y  :
	      begin
	      b := FALSE;
	      ZeroizeRecord;
	      ShowFmt := ShowFmt - [shownegative];
	      CurPos  := pred (FirstChar);
	      For i := 1 to length (A) do If A [i] >= '0' then A [i] := '0';
	      end;
	  '.' :
	      begin
	      If FirstChar < length (A) then
		begin
		CurPos := FirstChar;
		fillchar (A [FirstChar + 2], length (A) - succ (FirstChar), '0');
		b := TRUE;
		end
	       else WrongKeypressed (Event);
	      end;
	  '-','_' :
	      begin
	      If (Min <> 0) and (A [1] = ' ') and
		 (FirstChar > 1) and (pos ('-', A) = 0) then
		begin
		i := pred (FirstChar);
		ShowFmt := ShowFmt + [shownegative];
		While (A [i] <> ' ') do Dec (i);
		A [i] := '-';
		b := TRUE;
		end
	       else WrongKeypressed (Event);
	      end;
	 else begin
	      If (shownegative in ShowFmt) and (pos ('-',A) = 0) then
		begin
		If A [1] = ' ' then
		  begin
		  i := FirstChar;
		  While (A [i] <> ' ') do Dec (i);
		  If i <> 0 then A [i] := '-';
		  end;
		end;
	      If CurPos = pred (FirstChar) then
		begin
		If A [1] in [' ','0'] then
		  begin
		  If (FirstChar > 1) and not ((A [FirstChar] = '0') and (A [pred (FirstChar)] in ['-',' ']))
		   then Move (A [2], A [1], pred (FirstChar));
		  A [FirstChar] := Event.CharCode;
		  b := TRUE;
		  end;
		end
	       else
		begin
		A [succ (CurPos) + 1] := Event.CharCode;
		If pred (length (A)) > CurPos then Inc (CurPos);
		b := TRUE;
		end;
	      If (Max > 0) then
		begin
		Val (A, R, i);
		If (i <> 0) or (R > Max) or (R < Min) then b := FALSE;
		end
	       else
		begin
		If (TC = fldCHARVAL) and parenthesis and (A [1] > '-') then b := FALSE;
		end;
	      If not b then WrongKeypressed (Event);
	      end;
	  end;
	end;
    If b then
      begin
      ChangeMade;
      end;
    EffectField := b;
  end;

begin
  If (DataBlockSize < RecordSize) or (RecordSize <= 0) then Exit;
  If Locked or (CurrentField^.access and accReadOnly <> 0) then FirstKey := TRUE;
  Tabbing	:= FALSE;
  InsOn		:= not GetState (sfCursorIns);
  Go		:= TRUE;
  If CurrentField = nil then CurrentField := DMXfield1;
  If (Event.What = evKeyDown) then
    begin
    If (Event.KeyCode = kbShiftEnter) then Exit;
    If (Event.KeyCode = kbShiftIns) then Event.CharCode := '0';
    If (Event.KeyCode = kbShiftDel) then Event.CharCode := '.';
    With CurrentField^ do
      begin
      TC := upcase (typecode);
      If (Event.KeyCode = kbEsc) or (Event.KeyCode = kbEnter) then
	begin
	QuitField (cmDMX_Enter);
	end
       else
	begin
	Event.KeyCode := CtrlToArrow (Event.KeyCode);
	If (FirstKey and InsOn) or
	   (Locked or (CurrentField^.access and accReadOnly <> 0)) then
	  begin
	  If Event.KeyCode = kbRight then Event.KeyCode := kbCtrlRight
	  else
	  If Event.KeyCode = kbLeft  then Event.KeyCode := kbCtrlLeft;
	  end
	 else
	  If (TC in [fldSTR,fldSTRNUM,fldCHAR,fldCHARNUM]) then
	    begin
	    If Event.KeyCode = kbRight then Event.CharCode := ^D else
	    If Event.KeyCode = kbLeft  then Event.CharCode := ^S;
	    end;
	If (Event.KeyCode = kbDel)  then Event.CharCode := ^G;
	If (Event.KeyCode = kbTab)  then
	  begin
	  Event.KeyCode  := kbCtrlRight;
	  Tabbing := TRUE;
	  end;
	If (Event.CharCode <> #0) then
	  begin
	  If FirstKey
	    and (upcase (Event.CharCode) in ['-','.','0'..'9','A'..'F'])
	    and (access and accReadOnly = 0)
	   then
	    begin
	    If (TC in [fldBYTE, fldSHORTINT, fldWORD, fldINTEGER,
		       fldLONGINT, fldCHARVAL, fldREALNUM, fldHEXVALUE])
	     then ZeroizeField (FALSE, CurrentField);
	    end;
	  Case TC of
	    fldSTR,
	    fldSTRNUM,
	    fldCHAR,
	    fldCHARNUM :
	      begin
	      If typecode < 'a' then Event.CharCode := upcase (Event.CharCode);
	      If ((TC in [fldSTRNUM, fldCHARNUM]) and
		 not (Event.CharCode in [#0..' ', '0'..'9'])) or Locked
		 or  (access and accReadOnly <> 0) then
		begin
		WrongKeypressed (Event);
		Go  := FALSE;
		end
	       else
		begin
		If TC in [fldSTR, fldSTRNUM] then inx := 1 else inx := 0;
		Case Event.CharCode of
		  ^G,    { kbDel }
		  ^H  :  { kbBack }
		    begin
		    If Event.CharCode = ^H then
		      begin
		      If CurPos = 0 then Go := FALSE else Dec (CurPos);
		      end;
		    If Go then
		      begin
		      If (inx > 0) and (length (pstring (FieldData)^) <= CurPos) then Go := FALSE;
		      If Go then
			begin
			ChangeMade;
			Move (pstring (FieldData)^ [CurPos + inx + 1],
			      pstring (FieldData)^ [CurPos + inx], fieldsize - CurPos - inx);
			pstring (FieldData)^ [pred (fieldsize)] := fillvalue;
			If (inx <> 0) and (pbyte (FieldData)^ > 0) then Dec (pstring (FieldData)^ [0]);
			end;
		      end;
		    end;
		  ^D  :  { kbRight }
		    If CurPos < fieldsize - inx - 1 then Inc (CurPos) else QuitField (cmDMX_Right);
		  ^S  :  { kbLeft }
		    begin
		    If (CurPos > 0) then Dec (CurPos) else QuitField (cmDMX_Left);
		    end;
		  ^T  :  { clear field }
		    begin
		    ZeroizeField (FALSE, CurrentField);
		    CurPos := 0;
		    end;
		  ^Y  :  { clear record }
		    begin
		    ZeroizeRecord;
		    CurPos := 0;
		    end;
		  ^A..^Z  :  { prevent ctrl-characters from being entered }
		    begin
		    end;
	       else begin
		    If inx = 0 then i := fieldsize else i := pbyte (FieldData)^;
		    If InsOn then
		      begin
		      If (fieldsize = succ (inx)) then pstring (FieldData)^ [inx] := fillvalue;
		      If (ord (pstring (FieldData)^ [pred (fieldsize)]) and $DF = 0)
			  or
			 ((inx = 1) and (length (pstring (FieldData)^) < pred (fieldsize)))
		       then
			begin
			ChangeMade;
			If (inx <> 0) then
			  begin
			  If (CurPos > i) then
			    begin
			    fillchar (pstring (FieldData)^ [succ (i)],
				      CurPos - i, fillvalue);
			    pbyte (FieldData)^ := succ (CurPos);
			    end
			   else
			    Inc (pbyte (FieldData)^);
			  end;
			If succ (CurPos) + inx < fieldsize then
			  Move (pstring (FieldData)^ [CurPos + inx],
				pstring (FieldData)^ [CurPos + inx + 1],
				fieldsize - CurPos - inx - 1);
			pstring (FieldData)^ [CurPos + inx] := Event.CharCode;
			end
		       else
			begin
			WrongKeypressed (Event);
			Go := FALSE;
			end;
		      end
		     else
		      begin
		      ChangeMade;
		      If (inx <> 0) and (CurPos >= i) then
			begin
			fillchar (pstring (FieldData)^ [succ (i)],
				  CurPos - i, fillvalue);
			pbyte (FieldData)^ := succ (CurPos);
			end;
		      pstring (FieldData)^ [CurPos + inx] := Event.CharCode;
		      end;
		    If CurPos < fieldsize - inx - 1 then
		      begin
		      If Go then Inc (CurPos);
		      end
		     else QuitField (cmDMX_Right);
		    end;
		  end;  { case of CharCode }
		If (CurPos < FirstPos) then FirstPos := CurPos;
		end;
	      end;

	    fldCHARVAL :
	      begin
	      Move (FieldData^, A [1], fieldsize);
	      A [0] := chr (fieldsize);
	      j := 0;
	      For i := 1 to fieldsize do
		begin
		If (ord (A [i]) and not $20 = 0) then A [i] := ' ' else
		If (A [i] in ['-', '.', '0'..'9']) then j := 1;
		end;
	      If j = 0 then
		begin
		fillchar (A [1], fieldsize, '0');
		If fieldsize - decimals > 2 then fillchar (A [1], fieldsize - decimals - 2, ' ');
		If decimals > 0 then A [fieldsize - decimals] := '.';
		end;
	      If EffectField (FALSE, -1, 0) then
		begin
		i := 1;
		While (i < length (A)) and (A [i] <= '.') do
		  begin
		  If (A [succ (i)] <> '.') then A [i] := CurrentField^.fillvalue;
		  Inc (i);
		  end;
		Move (A [1], FieldData^, fieldsize);
		end;
	      end;

	    fldBYTE :
	      begin
	      Str (pbyte (FieldData)^:truelen, A);
	      If EffectField (FALSE, 0,255) then Val (A,pbyte (FieldData)^,i);
	      end;

	    fldSHORTINT :
	      begin
	      Str (pshortint (FieldData)^:truelen, A);
	      If EffectField (FALSE, -128,127) then Val (A,pshortint (FieldData)^,i);
	      end;

	    fldWORD :
	      begin
	      Str (pword (FieldData)^:truelen, A);
	      If EffectField (FALSE, 0,65535) then Val (A,pword (FieldData)^,i);
	      end;

	    fldINTEGER :
	      begin
	      Str (pinteger (FieldData)^:truelen, A);
	      If EffectField (FALSE, -1 - MaxInt, MaxInt) then Val (A,pinteger (FieldData)^,i);
	      end;

	    fldLONGINT :
	      begin
	      Str (plongint (FieldData)^:truelen, A);
	      If EffectField (FALSE, -1 - MaxLongInt, MaxLongInt) then
		Val (A,plongint (FieldData)^,i);
	      end;

	    fldREALNUM :
	      begin
	      If decimals > 0 then i := 1 else i := 0;
	      Str (prealnum (FieldData)^:truelen + i:decimals, A);
	      If EffectField (FALSE, -1, 0) then Val (A,prealnum (FieldData)^,i);
	      end;

	    fldBOOLEAN :
	      begin
	      If (access and accReadOnly <> 0) or Locked then
		begin
		WrongKeypressed (Event);
		end
	       else
		begin
		Event.CharCode := upcase (Event.CharCode);
		If (Event.CharCode = showTRUE)  then Event.CharCode := '+' else
		If (Event.CharCode = showFALSE) then Event.CharCode := '-';
		Case Event.CharCode of
		  ^T  : ZeroizeField (FALSE, CurrentField);
		  ^Y  : ZeroizeRecord;
		  'A'..'Z', 'a'..'z',
		  '+','*'  :
		    begin
		    fillchar (pboolean (FieldData)^, fieldsize, TRUE);
		    ChangeMade;
		    QuitField (cmDMX_Enter);
		    end;
		  ^G, ^H,
		  '-',' '  :
		    begin
		    fillchar (pboolean (FieldData)^, fieldsize, FALSE);
		    ChangeMade;
		    If not (Event.CharCode in [^G,^H]) then QuitField (cmDMX_Enter);
		    end;
		 else WrongKeypressed (Event);
		  end;
		end;
	      end;

	    fldHEXVALUE :
	      begin
	      Event.CharCode := upcase (Event.CharCode);
	      If Event.CharCode in [^G,^H,^T,^Y, '0'..'9', 'A'..'F'] then
		begin
		A  := '';
		For i := 1 to fieldsize do A := hexbyte (ord (pstring (FieldData)^ [pred (i)])) + A;
		If (length (A) > truelen) then Delete (A, 1,1);
		If EffectField (TRUE, 0, 0) then
		  begin
		  If odd (length (A)) then A [0] := '0' else Move (A [1], A [0], length (A));
		  For i := 0 to pred (fieldsize) do
		    begin
		    j := ord (A [i shl 1]);
		    k := ord (A [succ (i shl 1)]);
		    If j > ord ('9') then Dec (j, 7);
		    If k > ord ('9') then Dec (k, 7);
		    pstring (FieldData)^ [pred (fieldsize) - i] := chr (((j and 15) shl 4) or (k and 15));
		    end;
		  end;
		end
	       else
		begin
		WrongKeypressed (Event);
		end;
	      end;
	    end;
	  end;
	If Event.What <> evNothing then FirstKey := FALSE;
	end;
      end;
    end;
  If (Event.What = evKeyDown) and (Event.CharCode <> #0) then
    begin
    DrawField (CurrentField);
    ClearEvent (Event);
    end
   else
    begin
    Go := TRUE;
    Case Event.ScanCode of
      hi (kbIns):	If InsOn then BlockCursor else NormalCursor;
      hi (kbCtrlEnd):	QuitField (cmDMX_ScreenBottom);
      hi (kbCtrlHome):	QuitField (cmDMX_ScreenTop);
      hi (kbCtrlLeft),
      hi (kbLeft):	QuitField (cmDMX_Left);
      hi (kbShiftTab):
	  begin
	  TScroller.HandleEvent (Event);
	  If GetState (sfFocused) then QuitField (cmDMX_Left) else QuitField (cmDMX_Enter);
	  end;
      hi (kbCtrlPgDn):	QuitField (cmDMX_Bottom);
      hi (kbCtrlPgUp):	QuitField (cmDMX_Top);
      hi (kbCtrlRight),
      hi (kbRight):	QuitField (cmDMX_Right);
      hi (kbEnd):	QuitField (cmDMX_End);
      hi (kbHome):	QuitField (cmDMX_Home);
      hi (kbPgDn):	QuitField (cmDMX_PgDn);
      hi (kbPgUp):	QuitField (cmDMX_PgUp);
      hi (kbUp):	QuitField (cmDMX_Up);
      hi (kbDown):	QuitField (cmDMX_Down);
     else		Go := FALSE;
      end;
    If Go then ClearEvent (Event);
    end;

end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure RegisterDMX;
begin
  RegisterType (RDmxLabels);
  RegisterType (RDmxScroller);
  RegisterType (RDmxRecInd);
  RegisterType (RDmxEditor);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
