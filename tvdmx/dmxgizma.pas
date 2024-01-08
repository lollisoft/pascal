
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	DMXGIZMA  --constants, variables and functions	}
{	tvDMX     --data editing project (ver 1.5)	}
{							}
{	Copyright (c) 1992  Randolph Beck		}
{			    P.O. Box  56-0487		}
{			    Orlando, FL 32856		}
{			    CIS:  72361,753		}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit DMXGIZMA;

{$V-,X+,O+,D-,B-,R- }

interface

uses  Objects, Drivers, Views, App, RSet;

{$DEFINE tvDMX1A }

const
    cmDMX               = 4400;

    cmDMX_RollCall      = cmDMX +  1;
    cmDMX_Ack           = cmDMX +  2;
    cmDMX_Enter         = cmDMX +  3;
    cmDMX_FieldAltered  = cmDMX +  4;
    cmDMX_Draw          = cmDMX +  5;
    cmDMX_DrawData      = cmDMX +  6;
    cmDMX_Lock          = cmDMX +  7;
    cmDMX_LockData      = cmDMX +  8;
    cmDMX_Unlock        = cmDMX +  9;
    cmDMX_UnlockData    = cmDMX + 10;
    cmDMX_FixSize       = cmDMX + 11;
    cmDMX_ZeroizeRec	= cmDMX + 12;
    cmDMX_WrongKey	= cmDMX + 13;

    cmDMX_Left          = cmDMX + 15;
    cmDMX_Right         = cmDMX + 16;

    cmDMX_Home          = cmDMX + 18;
    cmDMX_End           = cmDMX + 19;

    cmDMX_goto          = cmDMX + 20;

    cmDMX_NextRow       = cmDMX + 21;
    cmDMX_Up            = cmDMX + 22;
    cmDMX_Down          = cmDMX + 23;
    cmDMX_PgUp          = cmDMX + 24;
    cmDMX_PgDn          = cmDMX + 25;
    cmDMX_ScreenTop     = cmDMX + 26;
    cmDMX_ScreenBottom  = cmDMX + 27;
    cmDMX_Top           = cmDMX + 28;
    cmDMX_Bottom        = cmDMX + 29;


			{  +------------ 1 normal fields             }
			{  | +---------- 2 normal selected field     }
			{  | | +-------- 3 read-only selected field  }
			{  | | | +------ 4 locked field              }
			{  | | | | +---- 5 delimiter                 }
			{  | | | | | +-- 6 border                    }
			{  | | | | | |  }
    cDMX  : string [6]  = #6#7#5#5#1#2;


    accNormal    =    0;
    accReadOnly  =    1;
    accHidden    =    2;
    accSkip      =    4;
    accDelimiter =    8;


    showTRUE     =   'þ';  { TRUE indicator  }
    showFALSE    =   ' ';  { FALSE indicator }
    showOVERFLOW =   '*';  { overflow indicator for numbers }


    fldSTR       =   'S';  { string field }
    fldSTRNUM    =   '#';  { numeric string field }
    fldCHAR      =   'C';  { character field }
    fldCHARNUM   =   '0';  { numeric character field }
    fldCHARVAL   =   'N';  { dbase formatted numeric field }
    fldBYTE      =   'B';  { byte field }
    fldSHORTINT  =   'J';  { shortint field }
    fldWORD      =   'W';  { word field }
    fldINTEGER   =   'I';  { integer field }
    fldLONGINT   =   'L';  { longint field }
    fldREALNUM   =   'R';  { real number field  (uses TREALNUM) }
    fldBOOLEAN   =   'X';  { boolean value field }
    fldHEXVALUE  =   'H';  { hexadecimal numeric entry }

    fldZEROMOD   =   'Z';  { zero modifier }


  { Complex fields: }

    fldDATE      =  ' WW-'^F^Z + ^U+char(12) + ^P+char(2) +
                     #0'ZW-'^Z + ^U+char(31) +
                     #0'ZZZW '^Z^F + ^P+char(-6) +
                     #0 + ^P+char(4);

    fldTIME      =  ' WW:'^F^Z + ^U+char(23) +
                     #0'ZW '^Z + ^U+char(59) +
                     #0'W'^F^H#0;  { seconds are hidden }

    fldDATETIME  =  ' WW-'^F^Z + ^U+char(12) + ^P+char(2) +
                     #0'ZW-'^Z + ^U+char(31) +
                     #0'ZZZW '^Z^F + ^P+char(-6) +
                     '\' + ^P+char(4) +
                      ' WW:'^F^Z + ^U+char(23) +
                     #0'ZW:'^Z   + ^U+char(59) +
                     #0'ZW '^Z^F + ^U+char(59);  { seconds are not hidden }


type
    pDMXfieldrec = ^tDMXfieldrec;
    tDMXfieldrec =  RECORD    { these records describe each field for tvDMX }
	Next,Prev	:  pDMXfieldrec;
	access		:  byte;	{ read-only, hidden, skip }
	fieldnum	:  byte;	{ 1..totalfields (0=none) }
	screentab	:  integer;	{ virtual column num. }
	typecode	:  char;	{ 's', 'r', etc. }
	fillvalue	:  char;	{ #0 or ' ' }
	upperlimit	:  byte;	{ maximum value limit }
	showzeroes	:  boolean;	{ display zero values }
	truelen		:  byte;	{ unformatted text length }
	parenthesis	:  boolean;	{ '('/')' characters }
	decimals	:  byte;	{ decimal point }
	fieldsize	:  integer;	{ sizeof (datatype) }
	datatab		:  integer;	{ position in record }
	template	:  pstring;	{ field template }
    end;


    showcodes    = (showanyway, shownegative, showregular);
    showset      =  set of showcodes;	{ used when displaying fields }


  function  DmxStrLen (S : string)  : integer;
    { returns the length of the visible portions of a tvDMX template string }

  function  FieldString (fieldrec  : pDMXfieldrec;
			 Show : showset;  var DataRec )  : string;
    { returns a display string from a tvDMX field record }


implementation


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  DmxStrLen (S : string)  : integer;
var  i,Len,Ttl  : integer;
     h          : boolean;

    procedure ResetDelimiter (D : boolean);
    begin
      If not h then Ttl := Ttl + Len;
      If D then Inc (Ttl);
      Len := 0;
      h   := FALSE;
    end;

begin
  h   := FALSE;
  Ttl := 0;
  Len := 0;
  i   := 0;
  While (i < length (S)) do
    begin
    Inc (i);
    Case S [i] of
      '~':
        begin
        Inc (i);
        While (S [i] <> '~') and (i < length (S)) do
          begin
          Inc (Len);
          Inc (i);
          end;
        end;
      ^P, ^U, ^V:  Inc (i);
      ^H:          h := TRUE;
      ^D:
        begin
        ResetDelimiter (TRUE);
        Inc (i);
        end;
      #0,'\','|','ณ','บ':
        begin
        ResetDelimiter (S [i] <> #0);
        end;
      ^A..^Z:  begin  end;
     else      Inc (Len);
      end;
    end;
  ResetDelimiter (FALSE);
  DmxStrLen := Ttl;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  FieldString (fieldrec	: pDMXfieldrec;
		       Show	: showset;  var DataRec )  : string;
var  i,j,Len	:  integer;
     C		:  char;
     Numbers	:  boolean;
     ItsBlank	:  boolean;
     Q		:  boolean;
     A,T	:  string;
     R		:  TREALNUM;

     Data	:  pointer;
     DataBool	:  pboolean  absolute Data;
     DataByte	:  pbyte     absolute Data;
     DataShort	:  pshortint absolute Data;
     DataInt	:  pinteger  absolute Data;
     DataWord	:  pword     absolute Data;
     DataLong	:  plongint  absolute Data;
     DataReal	:  PREALNUM  absolute Data;
     DataStr	:  pstring   absolute Data;

    function  HexByte (Number : byte)  : string;
    const bts  : array [0..15] of char = '0123456789ABCDEF';
    begin
      HexByte := bts [(Number shr 4) and $0F] + bts [Number and $0F]
    end;

    function  BlankField  : boolean;
    var  i : word;
    begin
      BlankField := TRUE;
      If Len > 0 then
        For i := 0 to pred (fieldrec^.fieldsize) do
          If DataStr^ [i] <> #0 then BlankField := FALSE;
    end;

    function  CheckBlank (Zero : boolean) :  boolean;
    begin
      If (Zero) and not ((fieldrec^.showzeroes) or (showanyway in Show)) then
        begin
        fillchar (A [1], Len, ' ');
        A [0]      := chr (Len);
        ItsBlank   := TRUE;
        CheckBlank := TRUE;
        end
       else
        CheckBlank := FALSE;
    end;

    procedure FormNum (sign : boolean);
    { length of A[] must equal Len + 1 }
    var  i,j : integer;
         cc  : char;
    begin
      With fieldrec^ do
        begin
        If sign and (shownegative in Show) then
          begin
          i := 1;
          While (A [i] = ' ') do Inc (i);
          If (i > 1) then A [pred (i)] := '-';
          end;
        If (parenthesis) then
          begin
          If sign then
            begin
            T [pos ('(', T)] := ' ';
            T [pos (')', T)] := ' ';
            end
           else
            begin
            A [pos ('-', A)] := ' ';
            If length (A) > succ (Len) then Delete (A, 1,1);
            end;
          end;
        If (A [1] <> ' ') then
          begin
          fillchar (A [1], Len, showOVERFLOW);
          A [0] := chr (Len);
          end
         else
          begin
          Delete (A, 1,1);
          Numbers := TRUE;
          end;
        end;
    end;


begin
  With fieldrec^ do
    begin
    If (fieldrec = nil) or (access and accHidden <> 0) then
      begin
      FieldString := '';
      Exit;
      end;
    If (template = nil) or (length (template^) = 0) then
      begin
      If typecode <> #0 then FieldString := typecode else FieldString := '';
      Exit;
      end;
    T    := template^;
    If (fieldsize = 0) then
      begin
      FieldString := T;
      Exit;
      end;
    Data := ptr (seg (DataRec), ofs (DataRec) + datatab);
    Len  := truelen;
    Numbers  := FALSE;
    ItsBlank := FALSE;
    Q    := FALSE;
    C    := upcase (typecode);
    Case C of

      fldSTR, fldSTRNUM :              { 'S'/'#' }
        begin
        If DataStr^ <> '' then
          For i := 1 to length (DataStr^) do
            If ord (DataStr^[i]) and $DF <> 0 then Q := TRUE;
        If not CheckBlank (not Q) then
          begin
          fillchar (A [1], Len, ' ');
          Move (DataStr^[1], A [1], length (DataStr^));
          A [0] := chr (Len);
          end;
        end;

      fldCHAR, fldCHARNUM :            { 'C'/'0' }
        begin
        If Len > 0 then
          For i := 0 to pred (Len) do
            If ((ord (DataStr^[i]) and $DF) <> 0) then Q := TRUE;
        If not CheckBlank (not Q) then
          begin
          Move (Data^, A [1], Len);
          A [0] := chr (Len);
          end;
        end;

      fldCHARVAL :                     { 'N' }
        begin
        A [0] := chr (fieldsize);
        Move (Data^, A [1], fieldsize);
        Val (A, R, i);
        If i <> 0 then R := 0.0;
        If not CheckBlank (R = 0.0) then
          begin
          If decimals > 0 then
            begin
            Str (R:(Len + 2):decimals, A);
            Delete (A, (Len + 2) - decimals, 1);
            end
           else
            Str (R:(Len + 1):0, A);
          FormNum (R >= 0);
          end;
        end;

      fldBYTE :                        { 'B' }
        If not CheckBlank (DataByte^ = 0) then
          begin
          Str (DataByte^:(Len + 1), A);
          FormNum (TRUE);
          end;

      fldSHORTINT :                    { 'J' }
        If not CheckBlank (DataShort^ = 0) then
          begin
          Str (DataShort^:(Len + 1), A);
          FormNum (DataShort^ >= 0);
          end;

      fldWORD :                        { 'W' }
        If not CheckBlank (DataWord^ = 0) then
          begin
          Str (DataWord^:(Len + 1), A);
          FormNum (TRUE);
          end;

      fldINTEGER :                     { 'I' }
        If not CheckBlank (DataInt^ = 0) then
          begin
          Str (DataInt^:(Len + 1), A);
          FormNum (DataInt^ >= 0);
          end;

      fldLONGINT :                     { 'L' }
        If not CheckBlank (DataLong^ = 0) then
          begin
          Str (DataLong^:(Len + 1), A);
          FormNum (DataLong^ >= 0);
          end;

      fldREALNUM :                       { 'R' }
        If not CheckBlank (DataReal^ = 0.0) then
          begin
          If decimals > 0 then
            begin
            Str (DataReal^:(Len + 2):decimals, A);
            Delete (A, (Len + 2) - decimals, 1);
            end
           else
            Str (DataReal^:(Len + 1):0, A);
          If (abs (DataReal^) > 1e35) then
            begin
            A := '**********************************';
            If (DataReal^ < 0.0) then A [1] := '-';
            end;
          FormNum (DataReal^ >= 0);
          end;

      fldBOOLEAN :                     { 'X' }
        begin
        If (Len = 0) then
          begin
          If DataBool^ then A := '' else ItsBlank := TRUE;
          end
         else
          begin
          If not CheckBlank (not DataBool^) then
            begin
            If DataBool^ then
              fillchar (A [1], Len, showTRUE)
             else
              fillchar (A [1], Len, showFALSE);
            A [0] := chr (Len);
            end;
          end;
        end;

      fldHEXVALUE :                    { 'H' }
        If not CheckBlank (BlankField) then
          begin
          A  := '';
          For i := 0 to pred (fieldsize) do A := hexbyte (ord (DataStr^ [i])) + A;
          If (length (A) > Len) then Delete (A, 1,1);
          end;

     else
        begin
       { possible virtual method for future expansion }
        end;

      end;  { case of C }

    If ItsBlank then
      begin
      fillchar (T [1], length (T), ' ');
      end
     else
      If A <> '' then
        begin
        j  := length (A);
        For i := length (T) downto 1 do
          begin
          If ord (T [i]) and $FE = 0 then
            begin
            If j > 0 then
              begin
              If (T [i] = #0) or (A [j] > ' ') then
                T [i] := A [j]
               else
                T [i] := '0';
              Dec (j);
              end;
            end
           else
            If Numbers and (T [i] = ',') then
              begin
              If (j <= 0) then T [i] := ' '
               else
                begin
                If (A [j] in [' ','-']) then
                  begin
                  T [i] := A [j];
                  Dec (j);
                  end;
                end;
              end;
          end;
        end;
    end;

  FieldString := T;

end;  { FieldString() }


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }



End.
