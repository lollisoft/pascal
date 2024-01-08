
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	tvDMXREP  --tvDMX Data Reporting Objects	}
{	tvDMX     --data editing project		}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit tvDMXREP;

{$V-,X+,B-,R-,I- }

interface

uses
    Dos, Objects, Drivers, Memory, Views, Dialogs, App, MsgBox,
    RSet, DmxGizma, tvDMX, tvGizma;

const
    NewLineStr	:  string [20] =  ^M^J;
    cmPRN_NewPage = cmDMX + 40;

type
    PDmxReport	= ^TDmxReport;
    TDmxReport	=  OBJECT (TObject)
	DMX		: PDmxScroller;
	Delimiter	: char;
	LineNums	: boolean;
	CurPos		: integer;
	LeftMargin	: integer;
	RightMargin	: integer;
	PageWidth	: integer;
	PageSize	: integer;
	CurrentPage	: integer;
	CurrentLine	: integer;
	CurrentRecord	: integer;
	MarginHit	: boolean;
	ErrorInfo	: word;
      constructor Init (aDMX : PDmxScroller;  ADelimiter : char;
			ALineNums : boolean;  APageSize,APageWidth : integer);
      procedure PrintCtrl (St : string);
      procedure DoPrint (var Buf;  Count : word);
      procedure GotoPos (Pos : integer);
      procedure Print (var Buf;  Count : word);  VIRTUAL;
      procedure SetupPage;  VIRTUAL;
      procedure EndPage;  VIRTUAL;
      procedure SetupDMX;  VIRTUAL;
      procedure EndDMX;  VIRTUAL;
      procedure SetupLine;  VIRTUAL;
      procedure EndLine;  VIRTUAL;
      function  RecNumStr (RecNum : integer) : string;  VIRTUAL;
      procedure PrintStr (St : string);
      procedure PrintLabels;  VIRTUAL;
      procedure PrintRec;
      procedure PrintRows;
      procedure Run;  VIRTUAL;
    end;


    PDmxReportFile  = ^TDmxReportFile;
    TDmxReportFile  =  OBJECT (TDmxReport)
	ReportText	: Text;
      constructor Init (aDMX : PDmxScroller;  ADelimiter : char;
			ALineNums : boolean;  APageSize,APageWidth : integer;
			AFilename : FNameStr);
      destructor  Done;  VIRTUAL;
      procedure Print (var Buf;  Count : word);  VIRTUAL;
    end;


    PDmxReportStream  = ^TDmxReportStream;
    TDmxReportStream  =  OBJECT (TDmxReport)
	Stream		: PStream;
      constructor Init (aDMX : PDmxScroller;  ADelimiter : char;
			ALineNums : boolean;  APageSize,APageWidth : integer;
			AStream : PStream);
      procedure Print (var Buf;  Count : word);  VIRTUAL;
    end;


  procedure DmxReportBox (ATitle :TTitleStr; Msg :string; Report :PDmxReport);


implementation

  { ออ TDmxReport ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxReport.Init (aDMX : PDmxScroller;  ADelimiter : char;
		ALineNums : boolean;  APageSize,APageWidth : integer);
begin
  TObject.Init;
  DMX		:= aDMX;
  Delimiter	:= ADelimiter;
  LineNums	:= ALineNums;
  PageSize	:= APageSize;
  PageWidth	:= APageWidth;
end;


procedure TDmxReport.PrintCtrl (St : string);
var  i,j,x : integer;
    procedure IncPos;
    begin
      inc (j);
      If (j <= LeftMargin) or (j >= RightMargin) then
        begin
        Delete (St,i,1);
        Dec (i);
        end;
    end;
    procedure DecPos;
    begin
      dec (j);
      If (j >= LeftMargin) or (j <= RightMargin) then
        begin
        Delete (St,i,1);
        Dec (i);
        end;
    end;
begin
  j := CurPos;
  If (length (St) > 0) then
    begin
    i := 1;
    While (i <= length (St)) do
      begin
      Case St [i] of
	^H :  DecPos;
	^I :
          begin
          x := j;
          Repeat inc (x) until (x mod 8 = 0);
          If (j < LeftMargin) or (x > RightMargin) then
            begin
            Delete (St,i,1);
            Dec (i);
            Repeat
              inc (j);
              If (j > LeftMargin) and (j < RightMargin) then
                begin
                inc (i);
                Insert (' ',St,i);
                end;
            Until (j mod 8 = 0);
            end
           else
            j := x;
          end;
	^J :
          begin
          inc (CurrentLine);
          end;
	^L :
	  begin
	  inc (CurrentPage);
	  CurrentLine := 0;
          j := 0;
	  end;
	^M :
          begin
          j := 0;
          If (NewLineStr = ^M) then inc (CurrentLine);
          end;
       else  IncPos;
	end;
      inc (i);
      end;
    If (length (St) > 0) then Print (St [1], length (St));
    CurPos := j;
    end;
  If (Application <> nil) then Application^.Idle;
end;


procedure TDmxReport.DoPrint (var Buf;  Count : word);
var  i,j : integer;
     x   : integer;
     P   : PCharArray;
     L   : longint;
begin
  If (Count = 0) then Exit;
  P := @Buf;
  L := Count;
  x := CurPos + Count;
  While (CurPos < LeftMargin) and (L > 0) do
    begin
    inc (ptrrec (P).ofs);
    dec (L);
    inc (CurPos);
    end;
  i := x;
  While (i > RightMargin) and (L > 0) do
    begin
    dec (L);
    dec (i);
    MarginHit := TRUE;
    end;
  If (L > 0) then Print (P^, L);
  CurPos := x;
end;


procedure TDmxReport.GotoPos (Pos : integer);
begin
  While (CurPos < Pos) do PrintCtrl (' ');
  While (CurPos > Pos) do PrintCtrl (^H);
end;
          

procedure TDmxReport.Print (var Buf;  Count : word);
begin
  Abstract
end;


procedure TDmxReport.SetupPage;
begin
end;


procedure TDmxReport.EndPage;
begin
  PrintCtrl (^L);
end;


procedure TDmxReport.SetupDMX;
var  i : integer;
     S : string;
begin
  S := RecNumStr (1) + 'ออ';
  If (Delimiter = #0) or (Delimiter >= #127) then
    FillChar (S [1], length (S) - 1, 'อ')
   else
    FillChar (S [1], length (S), '-');
  If LineNums then PrintStr (S);
  If (DMX^.Limit.X > 0) then For i := 1 to DMX^.Limit.X do PrintStr (S [1]);
  PrintCtrl (NewLineStr);
end;


procedure TDmxReport.EndDMX;
begin
  SetupDMX;  { print the same divider line }
end;


procedure TDmxReport.SetupLine;
begin
end;


procedure TDmxReport.EndLine;
begin
  PrintCtrl (NewLineStr);
end;


function  TDmxReport.RecNumStr (RecNum : integer) : string;
var  S : string;
begin
  If (CurrentRecord >= DMX^.DataBlockSize div DMX^.RecordSize) then
    RecNumStr := '      '
   else
    begin
    Str (succ (RecNum):5, S);
    RecNumStr := S + ' ';
    end;
end;


procedure TDmxReport.PrintStr (St : string);
begin
  If (length (St) > 0) then DoPrint (St [1], length (St));
end;


procedure TDmxReport.PrintLabels;
begin
  If (DMX^.Labels <> nil) then With PDmxLabels (DMX^.Labels)^ do
    begin
    DoPrint (Data^, Len);
    end;
end;


procedure TDmxReport.PrintRec;
var  i		: integer;
     A		: string;
     fieldrec	: pDMXfieldrec;
     DataRec	: pointer;
begin
  If (CurrentRecord < 0) or (CurrentRecord >= DMX^.DataBlockSize div DMX^.RecordSize) then
    DataRec := nil
   else
    DataRec := DMX^.DataAt (CurrentRecord);
  fieldrec := DMX^.DMXfield1;
  While (fieldrec <> nil) do
    begin
    With fieldrec^ do
      begin
      If (access and accHidden = 0) then
	begin
	If access and accDelimiter <> 0 then
	  begin
	  If (typecode >= #127) and (Delimiter <> #0) then
	    A := Delimiter else A := typecode;
	  end
	 else
	  begin
	  If (DataRec = nil) then
	    begin
	    A [0] := char (length (fieldrec^.template^));
	    fillchar (A [1], length (A), ' ');
	    end
	   else
	    A	:= FieldString (fieldrec, [], DataRec^);
          For i := 1 to length (A) do
            If (Delimiter <> #0) then
              begin
              If (A [i] = showTRUE) then
                begin
                If (showTRUE >= #127) then A [i] := '*';
                end
              else
              If (A [i] = showFALSE) then
                begin
                If (showFALSE >= #127) then A [i] := ' ';
                end
              else
              If (A [i] = #0) then A [i] := ' '
              else
              If (A [i] < ' ') or (A [i] >= #127) then A [i] := '.';
              end
             else
              If (A [i] in [^H,^I,^J,^L,^M]) then A [i] := '.';
	  end;
        PrintStr (A);
	end;
      end;
    fieldrec := fieldrec^.Next;
    end;
end;


procedure TDmxReport.PrintRows;
var  Recs : integer;
     Line : string;
     F	  : pDMXfieldrec;
begin
  SetupDMX;
  Recs := CurrentRecord + PageSize;
  F := DMX^.DMXfield1;
  While (CurrentRecord < Recs) and (not CtrlBreakHit) do
    begin
    SetupLine;
    If LineNums then
      begin
      Line := RecNumStr (CurrentRecord) + 'ณ ';
      If (Delimiter <> #0) then Line [length (Line) - 1] := Delimiter;
      PrintStr (Line);
      end;
    PrintRec;
    EndLine;
    Inc (CurrentRecord);
    end;
  If not CtrlBreakHit then EndDMX;
end;


procedure TDmxReport.Run;
var  i,n : integer;
     S   : string;
begin
  CtrlBreakHit	:= FALSE;
  While (CurrentRecord < DMX^.DataBlockSize div DMX^.RecordSize) and (not CtrlBreakHit) do
    begin
    LeftMargin  := 0;
    RightMargin := PageWidth;
    If (Application <> nil) then
      Message (Application, evCommand, cmPRN_NewPage, @Self);
    n := CurrentRecord;
    Repeat
      MarginHit := FALSE;
      CurPos	:= 0;
      SetupPage;
      If (DMX^.Labels <> nil) then
        begin
        S := RecNumStr (1) + '  ';
        If LineNums then
          begin
          FillChar (S [1], length (S) - 2, ' ');
          If (Delimiter <> #0) then S [length (S) - 1] := Delimiter;
          PrintStr (S);
          end;
        PrintLabels;
        PrintCtrl (NewLineStr);
        end;
      PrintRows;
      If not CtrlBreakHit then EndPage;
      If MarginHit then
        begin
        Inc (RightMargin, PageWidth);
        Inc (LeftMargin,  PageWidth);
        Dec (CurrentPage);
	CurrentRecord := n;
        end;
    Until CtrlBreakHit or not MarginHit;
    end;
end;

                                
  { ออ TDmxReportFile ออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxReportFile.Init (aDMX : PDmxScroller;  ADelimiter : char;
			 ALineNums : boolean; APageSize,APageWidth : integer;
			 AFilename : FNameStr);
begin
  TDmxReport.Init (aDMX, ADelimiter, ALineNums, APageSize,APageWidth);
  Assign (ReportText, AFilename);
  Append (ReportText);
  ErrorInfo := IOResult;
  If (ErrorInfo <> 0) then
    begin
    ReWrite (ReportText);
    ErrorInfo := IOResult;
    end;
end;


destructor TDmxReportFile.Done;
begin
  Close (ReportText);
  TDmxReport.Done;
end;


procedure TDmxReportFile.Print (var Buf;  Count : word);
var  Reg : registers;
begin
  If (ErrorInfo = 0) and (Count > 0) then
    begin
    With Reg do
      begin
      DS := seg (Buf);
      DX := ofs (Buf);
      CX := Count;
      BX := textrec (ReportText).Handle;
      AX := $4000;
      end;
    MsDos (Reg);
    If (Reg.Flags and FCarry <> 0) then ErrorInfo := Reg.AX;
    end;
end;


  { ออ TDmxReportStream ออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxReportStream.Init (aDMX : PDmxScroller;  ADelimiter : char;
			ALineNums : boolean;  APageSize,APageWidth : integer;
			AStream : PStream);
begin
  TDmxReport.Init (aDMX, ADelimiter, ALineNums, APageSize,APageWidth);
  Stream := AStream;
end;


procedure TDmxReportStream.Print (var Buf;  Count : word);
begin
  Stream^.Write (Buf, Count);
  If (Stream^.ErrorInfo <> stOK) then ErrorInfo := Stream^.ErrorInfo;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

type
    PBlueText	= ^TBlueText;
    TBlueText	=  OBJECT (TStaticText)
      function  GetPalette : PPalette;  VIRTUAL;
    end;


function  TBlueText.GetPalette : PPalette;
const CBlueText : string [1] = #19;
begin
  GetPalette := @CBlueText;
end;


procedure DmxReportBox (ATitle : TTitleStr; Msg : string; Report : PDmxReport);
var  Rect	: TRect;
     View	: PStaticText;
     ECode	: longint;
     Watch	: PDialog;
begin
  If (Report <> nil) and (Report^.DMX <> nil) and
     (Report^.DMX^.DataBlockSize >= Report^.DMX^.RecordSize) then
    begin
    Rect.Assign (0,0, 50,9);
    Watch := New (PDialog, Init (Rect, ATitle));
    Watch^.Options := Watch^.Options or ofCentered;
    Watch^.Flags := 0;

    Rect.Assign (3, 2, Watch^.Size.X - 2, Watch^.Size.Y - 3);
    Watch^.Insert (New (PStaticText, Init (Rect, Msg)));

    Rect.Assign (1, Watch^.Size.Y - 2, Watch^.Size.X - 1, Watch^.Size.Y - 1);
    Watch^.Insert (New (PBlueText, Init (Rect, ^C'Press Ctrl-Break to cancel')));

    DeskTop^.Insert (Watch);
    Report^.Run;
    DeskTop^.Delete (Watch);
    If (Report^.ErrorInfo <> 0) then
      begin
      ECode := Report^.ErrorInfo;
      MessageBox ('Report error: %d.', @ECode, mfError or mfOKButton);
      end;
    CtrlBreakHit  := FALSE;
    end
   else
    begin
    MessageBox ('No data for reporting.', nil, mfError or mfOKButton);
    end;
  If (Report <> nil) then Dispose (Report, Done);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
