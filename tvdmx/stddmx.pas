
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	StdDMX   --Standard Interface Unit		}
{	tvDMX    --data editing project (ver 1.41)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit StdDMX;

{  This unit has been updated with a special frame for tvDMX windows that
   limits the size of data windows according to the width of their headers.
 }

{$B-,D-,O+,R-,X+,V- }

interface

uses  Objects, Drivers, Views, Dialogs, RSet, DmxGizma, tvDMX;

type
    PDmxEditDlg  = ^TDmxEditDlg;  { tvDMX editor for dialog boxes }
    PInputFields = ^TInputFields; { line-editor for dialog boxes }
    PCutFrame	 = ^TCutFrame;	  { special frame for smaller windows }
    PDmxViewer   = ^TDmxViewer;   { tvDMX data scroller window }
    PDmxWindow   = ^TDmxWindow;   { tvDMX data editor window  }


    TDmxEditDlg  =  object (TDmxEditor)
      function  GetPalette  : PPalette;  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
    end;


    TInputFields =  object (TDmxEditDlg)
      constructor Init (InfoStr : string; var Bounds : TRect);
      procedure InitData (var AData );  VIRTUAL;
      procedure DoneData;  VIRTUAL;
      procedure LoadData (var S : TStream);  VIRTUAL;
      procedure StoreData (var S : TStream);  VIRTUAL;
      function  DataSize  : word;  VIRTUAL;
      procedure GetData (var Rec );  VIRTUAL;
      procedure SetData (var Rec );  VIRTUAL;
      procedure HandleEvent (var Event : TEvent);  VIRTUAL;
      procedure SetState (AState : word; Enable : boolean);  VIRTUAL;
      procedure SetUpField;  VIRTUAL;
    end;


    TCutFrame	=  OBJECT (TFrame)
      procedure Draw;  VIRTUAL;
    end;


    TDmxViewer   =  OBJECT (TWindow)
	Limit  : TPoint;
      constructor Init (var Bounds : TRect;  ATitle : TTitleStr;  ANumber : integer;
                            ATemplate : string;  var AData;  BSize : longint;
                        var ALabels  : string);
      procedure InitDMX (ATemplate  : string;  var AData;
                           ALabels, ARecInd  : PDmxLink;
                           BSize  : longint);  VIRTUAL;
      procedure ChangeBounds (var Bounds : TRect);  VIRTUAL;
      procedure InitFrame;  VIRTUAL;
      function  NewDmxLabels (var ALabels ) : PDmxLink;  VIRTUAL;
      function  Valid (Command : word) : boolean;  VIRTUAL;
      procedure Zoom;  VIRTUAL;
    end;


    TDmxWindow   =  OBJECT (TDmxViewer)
      constructor Init (var Bounds : TRect;  ATitle : TTitleStr;  ANumber : integer;
                            ATemplate : string;  var AData;  BSize : longint;
                        var ALabels  : string;  IndLen  : integer);
      procedure InitDMX (ATemplate  : string;  var AData;
                         ALabels, ARecInd  : PDmxLink;
                         BSize  : longint);  VIRTUAL;
      function  NewRecInd (Len : integer) : PDmxLink;  VIRTUAL;
    end;



  function  InsertField (Dialog : PDialog;  Col,Row : integer;
                         Fmt : boolean;  ALabel,ATemplate : string)  : PView;

  procedure RegisterStdDMX;


const
    RDmxEditDlg  :  TStreamRec = (
        ObjType:  cmDMX + 5;
        VmtLink:  ofs (TypeOf (TDmxEditDlg)^);
        Load:     @TDmxEditDlg.Load;
        Store:    @TDmxEditDlg.Store
      );

    RInputFields :  TStreamRec = (
        ObjType:  cmDMX + 6;
        VmtLink:  ofs (TypeOf (TInputFields)^);
        Load:     @TInputFields.Load;
        Store:    @TInputFields.Store
      );

    RDmxWindow   :  TStreamRec = (
        ObjType:  cmDMX + 7;
        VmtLink:  ofs (TypeOf (TDmxWindow)^);
        Load:     @TDmxWindow.Load;
        Store:    @TDmxWindow.Store
      );

    RDmxViewer   :  TStreamRec = (
        ObjType:  cmDMX + 8;
        VmtLink:  ofs (TypeOf (TDmxViewer)^);
        Load:     @TDmxViewer.Load;
        Store:    @TDmxViewer.Store
      );

    RCutFrame	 :  TStreamRec = (
        ObjType:  cmDMX + 9;
        VmtLink:  ofs (TypeOf (TCutFrame)^);
        Load:     @TCutFrame.Load;
        Store:    @TCutFrame.Store
      );


implementation


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  InsertField (Dialog : PDialog;  Col,Row : integer;
                       Fmt : boolean;  ALabel,ATemplate : string)  : PView;
var  i  : integer;
     R  : TRect;
     B  : PView;
begin
  With Dialog^ do
    begin
    i  := succ (CStrLen (ALabel));
    R.Assign (Col, Row, Col + DmxStrLen (ATemplate), succ (Row));
    If (ALabel <> '') then
      begin
      If Fmt then R.Move (1, 1) else R.Move (i, 0);
      end;
    B  := New (PInputFields, Init (ATemplate, R));
    Insert (B);
    If (ALabel <> '') then
      begin
      R.Assign (Col, Row, Col + i, succ (Row));
      Insert (New (PLabel, Init (R, ALabel, B)));
      end;
    end;
  InsertField := B;
end;


  { ออ TDmxEditDlg ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TDmxEditDlg.GetPalette  : PPalette;
{  6 border --------------------------------------+  }
{  5 delimiter --------------------------------+  |  }
{  4 locked field --------------------------+  |  |  }
{  3 read-only selected field -----------+  |  |  |  }
{  2 normal selected field -----------+  |  |  |  |  }
{  1 normal fields ----------------+  |  |  |  |  |  }
{                                  |  |  |  |  |  |  }
const CDmxEditDlg : string [6] = #19#20#06#06#01#02; { similar to CInputLine }
begin
  GetPalette := @CDmxEditDlg
end;


procedure TDmxEditDlg.HandleEvent (var Event : TEvent);
begin
  With Event do
    If (What = evKeyboard) and
      ((KeyCode = kbTab) or (KeyCode = kbShiftTab) or (KeyCode = kbEsc)) then
      TDmxScroller.HandleEvent (Event)
     else
      TDmxEditor.HandleEvent (Event);
end;


  { ออ TInputFields ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TInputFields.Init (InfoStr : string;  var Bounds : TRect);
begin
    { init with no data }
  TDmxEditDlg.Init (^A + InfoStr, Mem [0:0], 0, Bounds, nil,nil, nil,nil);
  GrowMode := gfGrowHiX;
end;


procedure TInputFields.InitData (var AData );
{ allocates memory for the data }
begin
  DataBlockSize := Size.Y * RecordSize;  { correct improper size }
  GetMem (WorkingData, DataBlockSize);
  fillchar (WorkingData^, DataBlockSize, 0);
  TDmxEditDlg.InitData (WorkingData^);
end;


procedure TInputFields.DoneData;
begin
  TDmxEditDlg.DoneData;
  FreeMem (WorkingData, DataBlockSize);
end;


procedure TInputFields.LoadData (var S : TStream);
begin
  S.Read (DataBlockSize, sizeof (DataBlockSize));
  GetMem (WorkingData,  DataBlockSize);
  S.Read (WorkingData^, DataBlockSize);
end;


procedure TInputFields.StoreData (var S : TStream);
begin
  S.Write (DataBlockSize, sizeof (DataBlockSize));
  S.Write (WorkingData^, DataBlockSize);
end;


function  TInputFields.DataSize  : word;
begin
  DataSize  := LongRec (DataBlockSize).Lo
end;


procedure TInputFields.GetData (var Rec );
var  Len : word;
begin
  Len  := DataSize;
  If (Len > 0) and (WorkingData <> nil) then Move (WorkingData^, Rec, Len);
end;


procedure TInputFields.SetData (var Rec );
var  Len : word;
begin
  Len  := DataSize;
  If (Len > 0) and (WorkingData <> nil) then Move (Rec, WorkingData^, Len);
  DrawView;
end;


procedure TInputFields.HandleEvent (var Event : TEvent);
begin
  With Event do
    If (What = evKeyboard) then
      begin
      If ((KeyCode = kbPgUp) or (KeyCode = kbUp)) and (CurrentRecord = 0) then
        KeyCode := kbShiftTab;
      If ((KeyCode = kbPgDn) or (KeyCode = kbDown)
          or ((KeyCode = kbEnter) and (CurrentField^.Next = nil)))
         and (succ (CurrentRecord) = Limit.Y)
       then
        KeyCode := kbTab;
      end;
  TDmxEditDlg.HandleEvent (Event);
end;


procedure TInputFields.SetState (AState : word; Enable : boolean);
var  cmd    : word;
     voidXY : TPoint;
begin
  If (AState and sfFocused <> 0) and not Enable then JustAltered := FALSE;
  TDmxEditDlg.SetState (AState, Enable);
  If Enable and (AState and sfFocused <> 0) then
    begin
    cmd  := cmDMX_Home;
    ProcessCommand (cmd, voidXY);
    end;
end;


procedure TInputFields.SetUpField;
begin
  TDmxEditDlg.SetUpField;
  If (CurrentField <> nil) and
     (upcase (CurrentField^.typecode) in [fldSTR, fldSTRNUM, fldCHAR, fldCHARNUM])
   then
    FirstKey := FALSE;
end;


  { ออ TCutFrame อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TCutFrame.Draw;
var XY : TPoint;
begin
  TFrame.Draw;
  If GetState (sfActive) then
    begin
    If (PDmxViewer (Owner)^.Limit.X > 0) then
      XY.X := PDmxViewer (Owner)^.Limit.X else XY.X := Owner^.Owner^.Size.X;
    If (PDmxViewer (Owner)^.Limit.Y > 0) then
      XY.Y := PDmxViewer (Owner)^.Limit.Y else XY.Y := Owner^.Owner^.Size.Y;
    If (Size.X >= XY.X) and (Size.Y >= XY.Y) then
      WriteStr ((Size.X - 4), 0, #18, 5);
    end;
end;


  { ออ TDmxViewer ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxViewer.Init (var Bounds     : TRect;
                                 ATitle     : TTitleStr;
                                 ANumber    : integer;
                                 ATemplate  : string;
                             var AData;
                                 BSize      : longint;
                             var ALabels    : string);
begin
  TWindow.Init (Bounds, ATitle, ANumber);
  InitDMX (ATemplate, AData, NewDmxLabels (ALabels), nil, BSize);
  Options := Options or ofTileable;
end;


procedure TDmxViewer.InitDMX (    ATemplate  : string;
                              var AData;
                                  ALabels    : PDmxLink;
                                  ARecInd    : PDmxLink;
                                  BSize      : longint);
var  R     : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);
  Insert (New (PDmxScroller, Init (ATemplate, AData, BSize, R,
                                  ALabels,
                                  StandardScrollBar (sbHorizontal+ sbHandleKeyboard),
                                  StandardScrollBar (sbVertical  + sbHandleKeyboard))));
end;


function  TDmxViewer.NewDmxLabels (var ALabels ) : PDmxLink;
begin
  If (string (ALabels) = '') or (@ALabels = nil) then
    NewDmxLabels := nil
   else
    NewDmxLabels := New (PDmxLabels, InitInsert (@Self, @ALabels));
end;


procedure TDmxViewer.ChangeBounds (var Bounds : TRect);
begin
  If (Limit.X > 0) and (Bounds.B.X - Bounds.A.X > Limit.X) then Bounds.B.X := Bounds.A.X + Limit.X;
  If (Limit.Y > 0) and (Bounds.B.Y - Bounds.A.Y > Limit.Y) then Bounds.B.Y := Bounds.A.Y + Limit.Y;
  TWindow.ChangeBounds (Bounds);
end;


procedure TDmxViewer.InitFrame;
var R : TRect;
begin
  GetExtent (R);
  Frame := New (PCutFrame, Init (R));
end;


function  TDmxViewer.Valid (Command : word) : boolean;
var  DMX : PDmxScroller;
     Len : integer;
begin
  Valid := TWindow.Valid (Command);
  DMX := Message (@Self, evCommand, cmDMX_RollCall, nil);
  If (DMX <> nil) and (DMX^.Labels <> nil) then
    begin
    Limit.X := PDmxLabels (DMX^.Labels)^.Len + (Size.X - Current^.Size.X);
    Len     := length (GetTitle (MaxViewWidth)) + 12;
    If (Len > ScreenWidth) then Len := ScreenWidth;
    If (Len > Limit.X) then Limit.X := Len;
    If (Limit.X < MinWinSize.X) then Limit.X := MinWinSize.X;
    end;
end;


procedure TDmxViewer.Zoom;
var R  : TRect;
    XY : TPoint;
begin
  If (Limit.X = 0) or (Limit.X > Owner^.Size.X) then
    XY.X := Owner^.Size.X else XY.X := Limit.X;
  If (Limit.Y = 0) or (Limit.Y > Owner^.Size.Y) then
    XY.Y := Owner^.Size.Y else XY.Y := Limit.Y;
  If ((Size.X <> XY.X) or (Size.Y <> XY.Y)) then
    begin
    GetBounds (ZoomRect);
    Owner^.GetExtent (R);
    Locate (R);
    end
   else
    Locate (ZoomRect);
end;


  { ออ TDmxWindow ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxWindow.Init (var Bounds     : TRect;
                                 ATitle     : TTitleStr;
                                 ANumber    : integer;
                                 ATemplate  : string;
                             var AData;
                                 BSize      : longint;
                             var ALabels    : string;
                                 IndLen     : integer);
begin
  TWindow.Init (Bounds, ATitle, ANumber);
  InitDMX (ATemplate, AData, NewDmxLabels (ALabels), NewRecInd (IndLen), BSize);
  Options := Options or ofTileable;
end;


procedure TDmxWindow.InitDMX (ATemplate  : string;  var AData;
                              ALabels, ARecInd : PDmxLink;
                              BSize  : longint);
var  R  : TRect;
begin
  GetExtent (R);
  R.Grow (-1,-1);
  If ALabels <> nil then Inc (R.A.Y, 2);
  Insert (New (PDmxEditor, Init (ATemplate, AData, BSize, R,
                                 ALabels, ARecInd,
                                 StandardScrollBar (sbHorizontal + sbHandleKeyboard),
                                 StandardScrollBar (sbVertical   + sbHandleKeyboard))));
end;


function  TDmxWindow.NewRecInd (Len : integer) : PDmxLink;
begin
  If (Len <= 0) then
    NewRecInd := nil
   else
    NewRecInd := New (PDmxRecInd, InitInsert (@Self, Len));
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure RegisterStdDMX;
begin
  RegisterType (RDmxEditDlg);
  RegisterType (RInputFields);
  RegisterType (RDmxWindow);
  RegisterType (RDmxViewer);
  RegisterType (RCutFrame);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }



End.
