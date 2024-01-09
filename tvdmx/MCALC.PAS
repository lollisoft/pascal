
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	MCALC	--Multi-function calculator		}
{		  derived from CALC.PAS			}
{							}
{       Copyright (c) 1994      Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{                                                       }
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit MCALC;

{$D+,F+,O+,X+ }

interface

uses  Drivers, Objects, Views, Dialogs, Menus, Editors, App, Calc, RSet;

type
    PMathCalculator = ^TMathCalculator;
    TMathCalculator =  OBJECT(TCalculator)
	Display		: PCalcDisplay;
	MemVal		: Real;
      constructor Init;
      constructor Load(var S: TStream);
      destructor  Done;  VIRTUAL;
      procedure Store(var S: TStream);
      function	DataSize : word;  VIRTUAL;
      procedure GetData(var Rec );  VIRTUAL;
      procedure SetData(var Rec );  VIRTUAL;
      procedure Error(E: string);
      function	GetX : Real;
      procedure PutX(N: Real);
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure SetState(AState: word;  Enable: boolean);  VIRTUAL;
    end;


    PMathMenu	= ^TMathMenu;
    TMathMenu	=  OBJECT(TMenuBar)
      function	GetPalette : PPalette;	VIRTUAL;
      procedure Draw;  VIRTUAL;
      procedure SetState(AState: word;  Enable: boolean);  VIRTUAL;
    end;


const
    cmMathCalc	=  FirstCmdNum + 400;
    MathCalc	:  PMathCalculator = nil;

    FuncButtonNames	: array[0..16] of string[4] =
       (' ใ',	'log',	'exp',	'ln',
	' xý',	'๛x',	'sin',	'cos',
	'tan', 'atan',	'deg',	'rad',
	'M',	'MR',	'M+',	'M-',	'MC');


  procedure InitMathCalc;
  procedure DoneMathCalc;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

implementation

const cmBase = 4000;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure InitMathCalc;
begin
  MathCalc := New(PMathCalculator, Init);
  Desktop^.Insert(MathCalc);
end;


procedure DoneMathCalc;
begin
  Dispose(MathCalc, Done);
end;


  { ออ TMathCalculator อออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TMathCalculator.Init;
const W  = 2;  { number of extra buttons per line }
      BW = 7;  { width of button fields }
      Y1 = 2;  { first row number }
var  a,i : integer;
     P	 : PView;
     R	 : TRect;
     MnuA,MnuB	: PMenuItem;
    function  IsCalc(P: PView) : boolean;
    begin
      IsCalc := (typeof(P^) = typeof(TCalcDisplay));
    end;
begin
  inherited Init;
  Display := PCalcDisplay(FirstThat(@IsCalc));
  If (Display = nil) then Exit;
  DisposeStr(Title);
  Title := NewStr('Calc');
  a := Size.X;
  GetBounds(R);
  Inc(R.B.X, W * BW + 2);
  Inc(R.B.Y, 3);
  Locate(R);

  MnuB := nil;
  For i := 16 downto 12 do
    begin
    R.A.X := (i - 12) * 6 + 2;
    R.A.Y := Size.Y - 3;
    R.B.X := R.A.X + 6;
    R.B.Y := R.A.Y + 2;
    P := New(PButton, Init(R, FuncButtonNames[i], cmBase+i, bfNormal));
    P^.Options := P^.Options and not ofSelectable;
    Insert(P);
    MnuB := NewItem(FuncButtonNames[i], '', kbNoKey, cmBase+i, hcNoContext, MnuB);
    end;
  MnuB := NewSubMenu('~M~em', hcNoContext, NewMenu(MnuB), nil);

  MnuA := nil;
  For i := 11 downto 0 do
    begin
    R.A.X := (i mod W) * BW + a;
    R.A.Y := (i div W) * 2 + Y1;
    R.B.X := R.A.X + BW;
    R.B.Y := R.A.Y + 2;
    P := New(PButton, Init(R, FuncButtonNames[i], cmBase+i, bfNormal or bfLeftJust));
    P^.Options := P^.Options and not ofSelectable;
    Insert(P);
    MnuA := NewItem(FuncButtonNames[i], '',  kbNoKey, cmBase+i, hcNoContext, MnuA);
    end;
  MnuA := NewSubMenu('~F~(x)', hcNoContext, NewMenu(MnuA), MnuB);

  GetExtent(R);
  Dec(R.B.X);
  R.A.X := R.B.X - 12;
  R.B.Y := succ(R.A.Y);
  InsertBefore(New(PMathMenu, Init(R, NewMenu(MnuA))), Display);
end;


constructor TMathCalculator.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S, Display);
  S.Read(MemVal, sizeof(MemVal));
end;


destructor TMathCalculator.Done;
begin
  If (MathCalc = @Self) then MathCalc := nil;
  inherited Done;
end;


procedure TMathCalculator.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S, Display);
  S.Write(MemVal,  sizeof(MemVal));
end;


function  TMathCalculator.DataSize : word;
begin
  DataSize := sizeof(Real);
end;


procedure TMathCalculator.GetData(var Rec );
begin
  Real(Rec) := GetX;
end;


procedure TMathCalculator.SetData(var Rec );
begin
  PutX(Real(Rec));
end;


procedure TMathCalculator.Error(E: string);
begin
  With Display^ do
    begin
    Status := csError;
    Number := E;
    Sign := ' ';
    Display^.DrawView;
    end;
end;


function  TMathCalculator.GetX : Real;
var  E : integer;
     N : Real;
begin
  With Display^ do Val(Sign + Number, N, E);
  GetX := N;
end;


procedure TMathCalculator.PutX(N: Real);
var  S : string;
begin
  With Display^ do
    If (Status <> csError) then
      begin
      Str(N:0:10, S);
      If (S[1] <> '-') then Sign := ' '
       else
	begin
	System.Delete(S,1,1);
	Sign := '-';
	end;
      If (length(S) > 15 + 1 + 10) then Error('Overflow')
       else
	begin
	While S[length(S)] = '0' do Dec(S[0]);
	If (S[length(S)] = '.') then Dec(S[0]);
	Number := S;
	Status := csFirst;
	Display^.DrawView;
	end;
      end;
end;


procedure TMathCalculator.HandleEvent(var Event: TEvent);
var  SkipIt : boolean;

    procedure ClipCopy;
    var  S : string;
    begin
      If (Clipboard <> nil) then
	begin
	With Display^ do
	  If (Sign = '-') then S := ^M^J+Sign+Number else S := ^M^J+Number;
	Clipboard^.InsertText(@S[1], length(S), TRUE);
	end;
    end;

    procedure ClipPaste;
    var  S	: string;
	 i,Len	: word;
	 R	: Real;
    begin
      If (Clipboard <> nil) then
	begin
        i := 1;
	With Clipboard^ do
	  begin
          Len := SelEnd - SelStart;
          If (Len > 255) then Len := 255;
          If (Len > 0) then Move(Buffer^[BufPtr(SelStart)], S[1], Len);
          S[0] := char(Len);
          While (S <> '') and
            (not (S[1] in ['-','0'..'9']) and
             not ((S[1] = '.') and (S[2] in ['0'..'9']))) do
              System.Delete(S,1,1);
          If (S <> '') then
            begin
            If (length(S) > 1) then For i := length(S) downto 2 do
              begin
              Case S[i] of
                '.','0'..'9':	begin end;
                ',':		System.Delete(S,i,1);
               else		S[0] := chr(pred(i));
                end;
              end;
            Val(S,R,i);
            If (i = 0) then PutX(R);
            end;
	  end;
        If (i <> 0) then Error('Error');
	end;
    end;

    procedure WakeUp;
    begin
      Show;
      SetState(sfVisible, TRUE);
      Select;
    end;

begin
  SkipIt := FALSE;
  With Event do
    begin
    If (What = evKeyDown) then
      begin
      If (KeyCode = kbShiftDel) or (KeyCode = kbCtrlIns) or
	 (KeyCode = kbShiftIns) or (KeyCode = kbCtrlDel) then
	Exit;
      Case upcase(CharCode) of
	 #0:	SkipIt	:= (ScanCode in [hi(kbAltF), hi(kbAltM)]);
	#27:
	  begin
	  If (State and sfModal <> 0) then
	    EndModal(cmCancel)
	   else
	    CharCode := 'C';
	  end;
	'F':	KeyCode := kbAltF;
	'M':	KeyCode := kbAltM;
	end;
      end;
    If (What = evCommand) then
      begin
      If ((Command = cmClose) or (Command = cmCloseAll)) and
	  (MathCalc <> nil) then
	begin
	Hide;
	ClearEvent(Event);
	end;
      end;
    end;
  If not SkipIt then TCalculator.HandleEvent(Event);
  With Event do
    If (What = evCommand) then
      begin
      Case Command of
	cmClear:	PutX(0);
	cmCopy:		ClipCopy;
	cmCut:		begin  ClipCopy;  PutX(0);  end;
	cmPaste:	ClipPaste;
	cmMathCalc:	WakeUp;

	{ These are the extra math functions: }
	cmBase+0:	begin  Display^.Status := csValid;  PutX(Pi);  end;
	cmBase+1:	If (GetX <= 0) then Error('Error') else PutX(ln(GetX) / ln(10));  { log }
	cmBase+2:	If (GetX > 32) then Error('Error') else PutX(exp(GetX));
	cmBase+3:	If (GetX <= 0) then Error('Error') else PutX(ln(GetX));
	cmBase+4:	PutX(sqr(GetX));
	cmBase+5:	If (GetX < 0) then Error('Error') else PutX(sqrt(GetX));
	cmBase+6:	PutX(sin(GetX));
	cmBase+7:	PutX(cos(GetX));
	cmBase+8:	If (cos(GetX) = 0) then Error('Error') else PutX(sin(GetX) / cos(GetX));  { tan }
	cmBase+9:	PutX(arctan(GetX));
	cmBase+10:	PutX(GetX * Pi / 180.0);  { deg }
	cmBase+11:	PutX(GetX * 180.0 / Pi);  { rad }

	{ These are the memory functions: }
	cmBase+12:	MemVal := GetX;		  { M  }
	cmBase+13:	begin Display^.Status := csValid; PutX(MemVal); end; { MR }
	cmBase+14:	MemVal := MemVal + GetX;  { M+ }
	cmBase+15:	MemVal := MemVal - GetX;  { M- }
	cmBase+16:	MemVal := 0;		  { MC }

       else		Exit;
	end;
      ClearEvent(Event);
      end;
end;


procedure TMathCalculator.SetState(AState: word;  Enable: boolean);
begin
  If (MathCalc = @Self) then
    begin
    If (AState and sfExposed <> 0) and Enable and
       (State and sfExposed = 0) then
      State := State and not sfVisible;
    end;
  inherited SetState(AState, Enable);
  If (AState and sfActive <> 0) and (Clipboard <> nil) then
    If Enable then
      begin
      EnableCommands([cmCut, cmCopy, cmPaste, cmClear]);
      DisableCommands([cmUndo, cmFind, cmReplace, cmSearchAgain])
      end
     else
      DisableCommands([cmCut, cmCopy, cmPaste, cmClear]);
end;


  { ออ TMathMenu อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TMathMenu.GetPalette : PPalette;
const  P : string[6] = #2#2#3#3#3#3;
begin
  GetPalette := @P;
end;


procedure TMathMenu.Draw;
type PDrawBuffer = ^TDrawBuffer;
var  i	: integer;
     c	: char;
begin
  Owner^.Lock;
  TMenuBar.Draw;
  If (Owner^.Buffer <> nil) and (Owner^.Size.X > 0) then
    begin
    If (State and (sfDragging or sfActive) = sfActive) then
      c := 'อ' else c := 'ฤ';
    For i := (Origin.X) to pred(Origin.X + Size.X) do
      If (char(PDrawBuffer(Owner^.Buffer)^[i]) = ' ') then
	char(PDrawBuffer(Owner^.Buffer)^[i]) := c;
    end;
  Owner^.Unlock;
end;


procedure TMathMenu.SetState(AState: word;  Enable: boolean);
begin
  TMenuBar.SetState(AState, Enable);
  If (AState and (sfActive or sfDragging) <> 0) then
    begin
    If (State and (sfActive or sfDragging) = sfActive) then Show else Hide;
    DrawView;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.
