
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	PxGIZMA	   --Paradox support unit		}
{							}
{       Copyright (c) 1993-96   Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{                                                       }
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit PxGIZMA;

{$N+ }

interface

uses
    Objects, Drivers, Views, Dialogs, App, MsgBox,
    RSet, tvGizma, DmxGizma, tvDMX, StdDMX, tvDMXCOL, DmxForms,
    PxEngine, PxMsg;

const
    PxAsciiSort		=  0;
    PxIntlSort		=  1;
    PxNorDanSort	=  2;
    PxNorDan4Sort	=  3;
    PxSwedFinSort	=  4;

    PARADOX50		=  PARADOX40 + 1;
    PARADOX70		=  PARADOX50 + 1;

    PxFatalErrors	= [PxErr_OutOfMem,	PxErr_OutOfStack,
			   PxErr_OutOfSwapBuf,	PxErr_InvEngineState,
			   248];


  function  DosErrToPxErr(Err: integer) : integer;
    { converts a DOS error code into an approximate PxERR_XXXX equivalent }

  function  PxBuild(TblName: FNameStr; AFields: PSItem) : integer;
    { creates a Paradox table using field information in AFields }

  function  PxErrorBox(Status: integer) : integer;
    { displays a Paradox error message in a message-box
      (does nothing if Status=Zero) }

  function  PxPswAddBox : boolean;
  function  PxPswDelBox : boolean;
    { takes user input to add or remove a password and returns TRUE if okay }

  function  PxTblInfo(TableName: FNameStr;  var Recs: longint;
			var Fields,KeyFields,SortOrder,VerID: integer;
			var Encrypted: boolean) : integer;
    { returns information about TableName }

  function  PxTblInfoBox(TableName: FNameStr) : boolean;
  function  PxTblDetailBox(TableName: FNameStr) : boolean;
    { displays information about TableName }

  function  PxSetDefaultsBox(HC,hOK,hCanx: word) : boolean;
    { dialog box to set Paradox defaults }

  function  PxTblCreateBox(HC,hOK,hCanx: word; var Name: string) : boolean;
    { dialog box to create a table }


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

implementation

type
    PCreateRec		= ^TCreateRec;
    TCreateRec		=  OBJECT(TObject)
	Name	:  NameString;
	TypeID	:  char;
	Len	:  byte;
	Index	:  boolean;
      function	GetName(var S: NameString) : integer;
      function	GetType(var S: NameString) : integer;
      function	GetIndex(Num: integer; var Last: integer) : integer;
    end;


    PDmxFldCollector	= ^TDmxFldCollector;
    TDmxFldCollector	=  OBJECT(TDmxCollector)
	Name		:  pstring;
      constructor Init(ATemplate: string;  var AData; BSize: longint;
			var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
			AHScrollBar,AVScrollBar: PScrollBar; AName: PView);
      function	GetPalette : PPalette;	VIRTUAL;
    end;


    PDmxFldEntry	= ^TDmxFldEntry;
    TDmxFldEntry	=  OBJECT(TDmxFldCollector)
	NArr,TArr	:  NamesArrayPtr;
      constructor Init(ATemplate: string;  var AData; BSize: longint;
			var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
			AHScrollBar,AVScrollBar: PScrollBar; AName: PView);
      destructor  Done;  VIRTUAL;
      procedure EvaluateField;	VIRTUAL;
      function	GetHelpCtx : word;  VIRTUAL;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      function	Valid(Command: word) : boolean;  VIRTUAL;
    end;



  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  DosErrToPxErr(Err: integer) : integer;
begin
  Case Err of
      0:  DosErrToPxErr := PxSuccess;
      2:  DosErrToPxErr := PxErr_TableNotFound;
      3:  DosErrToPxErr := PxErr_DirNotFound;
      4:  DosErrToPxErr := PxErr_OutOfFileHandles;
      5:  DosErrToPxErr := PxErr_TableWritePro;
      6:  DosErrToPxErr := PxErr_InvTableHandle;
     12:  DosErrToPxErr := PxErr_InvTableHandle;
     15:  DosErrToPxErr := PxErr_DirNotFound;
     18:  DosErrToPxErr := PxErr_FileNotFound;
    102:  DosErrToPxErr := PxErr_InvTableHandle;
    103:  DosErrToPxErr := PxErr_InvTableHandle;
    104:  DosErrToPxErr := PxErr_TableLocked;
    105:  DosErrToPxErr := PxErr_TableLocked;
    150:  DosErrToPxErr := PxErr_DiskWritePro;
    152:  DosErrToPxErr := PxErr_DriveNotReady;
   else   DosErrToPxErr := PxErr_GeneralFailure;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxBuild(TblName: FNameStr; AFields: PSItem) : integer;
var  i,FNum		: integer;
     FldCount,KeyFields	: integer;
     FNBlkSize		: longint;
     Err		: integer;
     F			: PSItem;
     N,T		: string[80];
     Names,Types	: NamesArrayPtr;
     FldHandles		: FieldHandleArray;
     SecInx		: set of byte;

    procedure CheckT;
    begin
      If (T[length(T)] = '*') then KeyFields := FNum;
      If (T[length(T)] = '+') then SecInx := SecInx + [FNum];
      If (T[length(T)] in ['*','+']) then Dec(T[0]);
    end;

begin
  Err  := PxErr_InvParameter;
  FldCount	:= 0;
  F		:= AFields;
  While (F <> nil) do
    begin
    Inc(FldCount);
    F := F^.Next;
    end;
  If (FldCount > 0) then
    begin
    FNBlkSize := sizeof(NameString) * FldCount;
    GetMem(Names, FNBlkSize);
    GetMem(Types, FNBlkSize);
    SecInx := [];
    F	:= AFields;
    KeyFields := 0;
    FNum := 0;
    While (F <> nil) do
      begin
      If (F^.Value <> nil) then
	begin
	Inc(FNum);
	N := F^.Value^;
	T := N;
	While not (N[length(N)] in [#0,' ',',']) do Dec(N[0]);
	Delete(T,1,length(N));
	While (N[length(N)] in [' ',',']) do Dec(N[0]);
	CheckT;
	CheckT;
	Names^[FNum] := N;
	Types^[FNum] := T;
	DisposeStr(F^.Value);
	end;
      AFields := F;
      F := F^.Next;
      Dispose(AFields);
      end;
    Err := PxTblCreate(TblName, FNum, Names, Types);
    FreeMem(Names, FNBlkSize);
    FreeMem(Types, FNBlkSize);
    If (Err = PxSuccess) and (KeyFields > 0) then
      begin
      For i := 1 to KeyFields do FldHandles[i] := i;
      Err := PxKeyAdd(TblName, KeyFields, FldHandles, Primary);
      For i := 1 to FNum do If (Err = PxSuccess) and (i in SecInx) then
	begin
	FldHandles[1] := i;
	Err := PxKeyAdd(TblName, 1, FldHandles, IncSecondary);
	end;
      end;
    end;
  PxBuild := Err;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxErrorBox(Status: integer) : integer;
begin
  If (Status <> PxSuccess) then
    MessageBox(PxErrMsg(Status), nil, mfError + mfOkButton);
  PxErrorBox := Status;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxPswAddBox : boolean;
var  S	: string;
begin
  S := '';
  If (InputBox('Add Password', 'Enter password to add: ', S, 255) = cmOK)
      and (S <> '')
   then
    PxPswAddBox := (PxErrorBox(PxPswAdd(S)) = PxSuccess)
   else
    PxPswAddBox := FALSE;
end;


function  PxPswDelBox : boolean;
var  S	: string;
begin
  S := '';
  While (InputBox('Remove Password', 'Enter password to delete: ', S, 255) = cmOK)
	 and (S <> '')
   do
    If (PxErrorBox(PxPswDel(S)) = PxSuccess) then
      begin
      PxPswDelBox := TRUE;
      Exit;
      end;
  PxPswDelBox := FALSE;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

{ These variables were created to supplement the parameters sent
  from function PxTblInfo to function PxTblDetailBox.
  It was done this way to allow for backward compatibility.
  }
var  DosCodePage	: word;
     AuxPasswords	: byte;
     RefIntegrity	: boolean;
     MaxTblSize		: byte;
     WriteProtect	: boolean;


function  PxTblInfo(TableName: FNameStr;  var Recs: longint;
			var Fields,KeyFields,SortOrder,VerID: integer;
			var Encrypted: boolean) : integer;
var  i,j	: integer;
     w		: word;
     ver        : byte;
     S		: TBufStream;
     L		: longint;
    procedure ClearVars;
    begin
      Recs	:= 0;
      Fields	:= 0;
      KeyFields	:= 0;
      SortOrder	:= 0;
      VerID	:= 0;
    end;
begin
  DosCodePage  := 0;
  RefIntegrity := FALSE;
  AuxPasswords := 0;
  S.Init(TableName, stOpenRead or stDenyNone, 512);
  Encrypted := FALSE;
  S.Seek($05);
  S.Read(MaxTblSize, sizeof(MaxTblSize));
  S.Read(Recs, sizeof(Recs));
  S.Seek($21);
  S.Read(Fields, sizeof(Fields));
  S.Seek($23);
  S.Read(KeyFields, sizeof(KeyFields));
  S.Read(L, sizeof(L));
  S.Read(SortOrder, sizeof(SortOrder));
  S.Seek($38);
  S.Read(WriteProtect, sizeof(WriteProtect));
  S.Read(ver, sizeof(ver));
  Case ver of
    $03:	VerID := PARADOX35;  { It's really 3.0 }
    $04:	VerID := PARADOX35;
    $05..09:	VerID := PARADOX40;
    $0A,$0B:	VerID := PARADOX50;
   else		VerID :=  0;
    end;
  S.Seek($40);
  S.Read(i, sizeof(i));
  If (i = 0) then S.Read(i, sizeof(i));

  Case lo(SortOrder) of
     00:	SortOrder := PxAsciiSort;
    $B7:	SortOrder := PxIntlSort;
    $82:	SortOrder := PxNorDanSort;
    $E6:	SortOrder := PxNorDan4Sort;
    $F0:	SortOrder := PxSwedFinSort;
   else		Recs := -1;
    end;
  If (KeyFields > Fields) or (Fields > 255) or (Recs < 0) or (VerID = 0) then
    begin
    PxTblInfo := PxErr_TableCorrupted;
    ClearVars;
    end
  else
    begin
    S.Seek($3D);
    S.Read(AuxPasswords, sizeof(AuxPasswords));
    S.Seek($55);
    S.Read(RefIntegrity, sizeof(RefIntegrity));
    If (ver >= $05) then
      begin
      S.Seek($5C);
      S.Read(L, sizeof(L));
      S.Seek($6A);
      S.Read(w, sizeof(w));
      If (S.Status = stOK) then DosCodePage := w;
      end;
    Encrypted := (L <> 0);
    PxTblInfo := PxSuccess;
    end;
  If (S.Status <> stOK) then
    begin
    PxTblInfo := DosErrToPxErr(S.ErrorInfo);
    ClearVars;
    end;
  S.Done;
end;


function  PxTblInfoBox(TableName: FNameStr) : boolean;
var  i			: integer;
     R			: TRect;
     Recs		: longint;
     Coded		: boolean;
     Fields,KeyFields,SO,Fmt	: integer;
     TblFmt,SOrder,Encrypted	: string[39];
begin
  If (TableName <> '') and
     (PxErrorBox(PxTblInfo(TableName, Recs, Fields, KeyFields, SO, Fmt, Coded)) = PxSuccess) then
    begin
    If (length(TableName) > 25) then
      While (pos('\', TableName) > 0) do Delete(TableName,1,pos('\', TableName));
    Case Fmt of
      PARADOX35:	TblFmt := 'Paradox 3.5';
      PARADOX40:	TblFmt := 'Paradox 4.0';
      PARADOX50:	TblFmt := 'Paradox 5.0';
     else		TblFmt := 'Unknown';
      end;
    If Coded then Encrypted := 'Yes' else Encrypted := 'No';
    Case SO of
      PxAsciiSort:	SOrder := 'ASCII';
      PxIntlSort:	SOrder := 'International';
      PxNorDanSort:	SOrder := 'Norwegian/Danish';
      PxNorDan4Sort:	SOrder := 'Norwegian/Danish[4]';
      PxSwedFinSort:	SOrder := 'Swedish/Finnish';
     else		SOrder := 'Unknown';
      end;
    R.Assign(0, 0, 40, 15);
    R.Move((Desktop^.Size.X - R.B.X) shr 1, (Desktop^.Size.Y - R.B.Y) shr 1);
    MessageBoxRect(R,
		  ^C'Table: %s'^M
		+ ''^M
		+ ' Fields:%26d'^M
		+ ' Primary Key Fields:%14d'^M
		+ ' Records:      %19d'^M
		+ ' Encrypted:    %19s'^M
		+ ' Table Format: %19s'^M
		+ ' Sort-Order:   %19s'^M,
		sparam(@TableName,
		dparam(Fields,
		dparam(KeyFields,
		dparam(Recs,
		sparam(@Encrypted,
		sparam(@TblFmt,
		sparam(@SOrder,
			nil))))))),
		mfInformation + mfOKButton
		);
    PxTblInfoBox := TRUE;
    end
   else
    PxTblInfoBox := FALSE;
end;


function  PxTblDetailBox(TableName: FNameStr) : boolean;
type TFmtRec	=  RECORD
	Fields,KeyFields: integer;
	Recs		: longint;
	MaxTblSizeW	: word;
	TblFmt		: string[11];
	Encrypted	: string[3];
	AuxPsw		: byte;
	Protected	: string[3];
	RefInt		: string[3];
	SOrder		: string[19];
	DosCodePageStr	: string[22];
	safety		: string[15];
     end;
var  i			: integer;
     R			: TRect;
     Coded		: boolean;
     SO,Fmt		: integer;
     FmtRec		: TFmtRec;
begin
  With FmtRec do If (TableName <> '') and
     (PxErrorBox(PxTblInfo(TableName, Recs, Fields, KeyFields, SO, Fmt, Coded)) = PxSuccess) then
    begin
    If (length(TableName) > 25) then
      While (pos('\', TableName) > 0) do Delete(TableName,1,pos('\', TableName));
    Case Fmt of
      PARADOX35:	TblFmt := 'PARADOX 3.5';
      PARADOX40:	TblFmt := 'PARADOX 4.0';
      PARADOX50:	TblFmt := 'PARADOX 5.0';
     else		TblFmt := 'UNKNOWN';
      end;
    If Coded then Encrypted := 'YES' else Encrypted := ' NO';
    If WriteProtect then Protected := 'YES' else Protected := ' NO';
    If RefIntegrity then RefInt := 'YES' else RefInt := ' NO';
    AuxPsw := AuxPasswords;
    MaxTblSizeW := MaxTblSize;
    MaxTblSizeW := MaxTblSizeW * 64;
    Case SO of
      PxAsciiSort:	SOrder := '              ASCII';
      PxIntlSort:	SOrder := '      International';
      PxNorDanSort:	SOrder := '   Norwegian/Danish';
      PxNorDan4Sort:	SOrder := 'Norwegian/Danish[4]';
      PxSwedFinSort:	SOrder := '    Swedish/Finnish';
     else		SOrder := '            Unknown';
      end;
    Case DosCodePage of
      $0000:		DosCodePageStr := '                  n/a';
      $01B5:		DosCodePageStr := '        United States';
      $0352:		DosCodePageStr := 'Multilingual (Latin I)';
      $0354:		DosCodePageStr := '    Slavic (Latin II)';
      $035C:		DosCodePageStr := '           Portuguese';
      $035F:		DosCodePageStr := '      Canadian French';
      $0361:		DosCodePageStr := '               Nordic';
     else		begin Str(DosCodePage:1, DosCodePageStr); end;
      end;
    EntryBox(TableName, @FmtRec, mfViewOnly + mfOKButton,
	NewSItem(^A,
	NewSItem('~  Fields:                       ~\iii,iii \',
	NewSItem('~  Primary Key Fields:           ~\iii,iii ',
	NewSItem('~  Records:                ~\l,lll,lll,lll ',
	NewSItem('~  Maximum table size:         ~\ww,www~ MB~',
	NewSItem('',
	NewSItem('~  Table format:~            \ sssssssssss',
	NewSItem('~  Encryption:                      ~\ sss',
	NewSItem('~  Auxiliary passwords:             ~\ bbb',
	NewSItem('~  Write protected:                 ~\ sss',
	NewSItem('~  Referential integrity:           ~\ sss',
	NewSItem('~  Sort-Order:       ~\sssssssssssssssssss',
	NewSItem('~  DOS code page: ~\ ssssssssssssssssssssss',
		nil)))))))))))))
	);
    PxTblDetailBox := TRUE;
    end
   else
    PxTblDetailBox := FALSE;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxSetDefaultsBox(HC,hOK,hCanx: word) : boolean;
var  i	: integer;
     R	: TRect;
     D	: PDialog;
     Op : RECORD BufSize,Tables,RecBufs,MaxLocks,MaxFiles,SortOrder: integer; end;
     SOrder	: pointer;

    function  InsertSortButtons : PView;
    var  R : TRect;
	 P : PView;
    begin
      R.Assign(3, 9, 33, 14);
      P := New(PRadioButtons, Init(R,
		NewSItem('~A~SCII',
		NewSItem('~I~nternational',
		NewSItem('~N~orwegian/Danish',
		NewSItem('Norwegian/~D~anish[4]',
		NewSItem('~S~wedish/Finnish',
		nil)))))
	    ));
      P^.HelpCtx := HC+5;
      D^.Insert(P);
      InsertSortButtons := P;
    end;

begin
  With Op do
    If (PxErrorBox(PxGetDefaults(BufSize,Tables,RecBufs,MaxLocks,
				 MaxFiles, SOrder)) = PxSuccess) then
      begin
      If (SOrder = SortOrderAscii) then   SortOrder := 0 else
      If (SOrder = SortOrderIntl)  then   SortOrder := 1 else
      If (SOrder = SortOrderNorDan) then  SortOrder := 2 else
      If (SOrder = SortOrderNorDan4) then SortOrder := 3 else
      If (SOrder = SortOrderSwedFin) then SortOrder := 4 else SortOrder := 0;
      R.Assign(0,0, 36,18);
      D := New(PDialog, Init(R, 'Configuration'));
      With D^ do
	begin
	Options := Options or ofCentered;
	ValidField(D, 6,2, 8,256, FALSE, '~B~uffer size:    ', ' III~k~')^.HelpCtx := HC;
	ValidField(D, 6,3, 1, 64, FALSE, 'Maximum ~T~ables: ', ' III '^U#64)^.HelpCtx := HC+1;
	ValidField(D, 6,4, 1,512, FALSE, '~R~ecord Buffers: ', ' III ')^.HelpCtx := HC+2;
	ValidField(D, 6,5, 1,128, FALSE, 'Record ~L~ocks:   ', ' III '^U#128)^.HelpCtx := HC+3;
	ValidField(D, 6,6, 2,255, FALSE, 'Maximum ~F~iles:  ', ' III ')^.HelpCtx := HC+4;
	R.Assign(4, 8, 16, 9);
	Insert(New(PLabel, Init(R, 'Sort ~o~rder', InsertSortButtons)));
	R.Assign(5, 15, 17, 17);
	Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
	Current^.HelpCtx := hOK;
	R.Assign(19, 15, 31, 17);
	Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
	Current^.HelpCtx := hCanx;
	SelectNext(FALSE);
	end;
      If (Application^.ExecuteDialog(D, @Op) = cmOK) then
	begin
	R.Assign(0, 0, 45, 8);
	R.Move((Desktop^.Size.X - R.B.X) div 2,(Desktop^.Size.Y - R.B.Y) div 2);
	Case Op.SortOrder of
	  0:  SOrder := SortOrderAscii;
	  1:  SOrder := SortOrderIntl;
	  2:  SOrder := SortOrderNorDan;
	  3:  SOrder := SortOrderNorDan4;
	  4:  SOrder := SortOrderSwedFin;
	  end;
	With Op do
	  PxSetDefaultsBox := (PxErrorBox(PxSetDefaults(BufSize,Tables,RecBufs,
					MaxLocks,MaxFiles,SOrder)) = PxSuccess);
	end
       else
	PxSetDefaultsBox := FALSE;
      end
     else
      begin
      PxSetDefaultsBox := FALSE;
      end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  PxTblCreateBox(HC,hOK,hCanx: word; var Name: string) : boolean;
const  FmtLabel  = ' Field name                 Type Inx';
var  R		: TRect;
     Dialog	: PDialog;
     DMX	: PDmxFldEntry;
     FmtInfo	: string[127];
     FmtData	: PCollection;
     InputFld	: PInputFields;
     i		: integer;
     OK		: word;

    procedure FixName(C: char);
    begin
      While (pos(C, Name) > 0) do System.Delete(Name,1,pos(C, Name));
    end;

begin
  New(FmtData, Init(20, 10));
  FmtInfo  := ' sssssssssssssssssssssssss\\C\BBB\~*~'^X;

  DeskTop^.GetExtent(R);
  R.Grow(0,(R.B.Y div -7));
  R.B.X := DmxStrLen(FmtInfo) + 7;
  Dialog := New(PDialog, Init(R, 'Create a Table'));
  With Dialog^ do
    begin
    Options  := Options or ofCentered;
    InputFld := InsertField(Dialog, 5, 2, FALSE, '~T~able name:  ', '"SSSSSSSS"');
    If (Name <> '') then
      begin
      FixName(':');
      FixName(' ');
      FixName('\');
      If (Name <> '') then
	begin
	For i := length(Name) downto 1 do
	  If (Name[i] = '.') then Name[0] := char(pred(i))
			     else Name[i] := upcase(Name[i]);
	If (length(Name) > 8) then Name[0] := #8;
	InputFld^.SetData(Name);
	end;
      end;
    InputFld^.HelpCtx := HC;
    InputFld^.StdEnter := FALSE;

   { assign the tvDMX view }

    Dialog^.GetExtent(R);
    R.Grow(-2, -5);
    R.Move(1, 0);
    R.B.X := DmxStrLen(FmtInfo) + R.A.X;
    Inc(R.B.Y);
    DMX := New(PDmxFldEntry,
	  Init(fldObjectVMT(New(PCreateRec, Init)) + FmtInfo,
		FmtData^,
		255,
		R,
		nil,nil,nil, StandardScrollBar(sbVertical),
		InputFld
	));
    DMX^.HelpCtx := HC + 1;
    Insert(DMX);
    InsertText(Dialog, R.A.X, pred(R.A.Y), FmtLabel);
   { move vert. scrollbar to fit beside the tvDMX view }
    R.A.X := R.B.X;
    Inc(R.B.X);
    DMX^.VScrollBar^.Locate(R);

   { assign buttons near the bottom of the window }
    R.Assign((Size.X shr 1) - 13, Size.Y - 3, (Size.X shr 1) - 1, Size.Y - 1);
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfNormal)));
    Current^.HelpCtx := hOK;
    R.Assign((Size.X shr 1) + 1, Size.Y - 3, (Size.X shr 1) + 13, Size.Y - 1);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
    Current^.HelpCtx := hCanx;
    SelectNext(FALSE);
    end;
  Name := '';
  If (Application^.ValidView(Dialog) <> nil) then
    begin
    OK := DeskTop^.ExecView(Dialog);
    If (OK = cmOK) then InputFld^.GetData(Name);
    Dispose(Dialog, Done);
    FmtData^.FreeAll;
    Dispose(FmtData, Done);
    PxTblCreateBox := (OK = cmOK);
    end
   else
    PxTblCreateBox := FALSE;
end;


  { ออ TCreateRec ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TCreateRec.GetName(var S: NameString) : integer;
begin
  S := Name;
  While (S[length(S)] = ' ') do Dec(S[0]);
  While (S[1] = ' ') and (S <> '') do Delete(S,1,1);
  If (S = '') then GetName := 1 else GetName := 0;
end;


function  TCreateRec.GetType(var S: NameString) : integer;
var  i,j : integer;
begin
  GetType := 0;
  If (TypeID in ['A','M']) then
    begin
    Str(Len:1, S);
    S := TypeID + S;
    If (Len = 0) then
      GetType := 3
     else
      If (TypeID = 'M') and (Len > 240) then GetType := 3;
    end
   else
    begin
    S := TypeID;
    If not (TypeID in['N','$','S','D']) then GetType := 2;
    end;
end;


function  TCreateRec.GetIndex(Num: integer; var Last: integer) : integer;
begin
  GetIndex := 0;
  If (Num = succ(Last)) then
    begin
    If Index then Last := Num;
    end
   else
    If Index then GetIndex := 4;
end;


  { ออ TDmxFldCollector ออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxFldCollector.Init(ATemplate: string;	var AData; BSize: longint;
		var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
		AHScrollBar,AVScrollBar: PScrollBar; AName: PView);
begin
  TDmxCollector.Init(ATemplate, AData, BSize, Bounds, ALabels,ARecInd,
		      AHScrollBar,AVScrollBar);
  Name := PInputFields(AName)^.WorkingData;
end;


function  TDmxFldCollector.GetPalette : PPalette;
const  CDmxEditDlg : string[6] = #19#20#06#06#01#02;
begin
  GetPalette := @CDmxEditDlg
end;


  { ออ TDmxFldEntry ออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


constructor TDmxFldEntry.Init(ATemplate: string;  var AData; BSize: longint;
		var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
		AHScrollBar,AVScrollBar: PScrollBar; AName: PView);
begin
  TDmxFldCollector.Init(ATemplate, AData, BSize, Bounds, ALabels,ARecInd,
			 AHScrollBar,AVScrollBar, AName);
  If (MaxAvail > sizeof(NArr^) + 200) then New(NArr);
  If (MaxAvail > sizeof(TArr^) + 200) then New(TArr);
end;


destructor TDmxFldEntry.Done;
begin
  If (NArr <> nil) then Dispose(NArr);
  If (TArr <> nil) then Dispose(TArr);
  TDmxFldCollector.Done;
end;


procedure TDmxFldEntry.EvaluateField;
var  Fld : pDMXfieldrec;
     T	 : NameString;
begin
  Fld := DMXfield1;
  While (Fld <> nil) and (Fld^.fieldnum <> 3) do Fld := Fld^.Next;
  If (Fld <> nil) then
    begin
    PCreateRec(RecordData)^.GetType(T);
    If (T[1] in[#0, 'A', 'M', ' ']) or (PCreateRec(RecordData)^.Len > 0) then
      begin
      Fld^.access := Fld^.access and not accSkip;
      If (T[1] = 'M') then Fld^.upperlimit := 240 else Fld^.upperlimit := 255;
      end
     else
      begin
      Fld^.access := Fld^.access or accSkip;
      end;
    end;
  TDmxFldCollector.EvaluateField;
end;


function  TDmxFldEntry.GetHelpCtx : word;
begin
  If (HelpCtx = 0) or (CurrentField = nil) or (CurrentField^.fieldnum = 0) then
    GetHelpCtx := hcNoContext
   else
    GetHelpCtx := HelpCtx + pred(CurrentField^.fieldnum);
end;


procedure TDmxFldEntry.HandleEvent(var Event: TEvent);
begin
  With Event do
    If (What = evKeyDown) then
      begin
      If (CurrentField^.fieldnum = 2) and
	not (upcase(CharCode) in[#0..' ', 'A', 'M', 'N', '$', 'S', 'D'])
       then
	begin
	WrongKeypressed(Event);
	ClearEvent(Event);
	end
       else
	If (CurrentRecord = 0) and ((KeyCode = kbUp) or (KeyCode = kbPgUp)) then
	  begin
	  KeyCode := kbShiftTab;
	  end
	 else
	  If (CurrentRecord = pred(Limit.Y)) and (not RecordAltered) and (KeyCode = kbDown) then
	    begin
	    KeyCode := kbTab;
	    end;
      end;
  TDmxFldCollector.HandleEvent(Event);
end;


function  TDmxFldEntry.Valid(Command: word) : boolean;
var  i,m,n : integer;
     LastN : integer;
     Exist : boolean;
     S	   : string[80];
     FA    : FieldHandleArray;

    function  TranslateFields : boolean;
    var  i : integer;
    begin
      TranslateFields := FALSE;
      m := 1;
      With PCollection(WorkingData)^ do
	begin
	If (Count = 0) then Exit;
	If (PCreateRec(At(pred(Count)))^.GetName(NArr^[1]) <> 0) and
	   (PCreateRec(At(pred(Count)))^.GetType(TArr^[1]) <> 0) then
	  begin
	  AtFree(pred(Count));
	  If (Count = 0) then Exit;
	  end;
	LastN := 0;
	For i := 1 to Count do
	  begin
	  n  := pred(i);
	  m  := PCreateRec(At(n))^.GetName(NArr^[i]);
	  If (m <> 0) then Exit;
	  m  := PCreateRec(At(n))^.GetType(TArr^[i]);
	  If (m <> 0) then Exit;
	  m  := PCreateRec(At(n))^.GetIndex(i, LastN);
	  If (m <> 0) then Exit;
	  end;
	end;
      TranslateFields := TRUE;
    end;

begin
  Case Command of
    cmValid:
      begin
      If (NArr = nil) or (TArr = nil) or (Name = nil) then
	begin
	Application^.OutOfMemory;
	Valid := FALSE;
	end
       else
	Valid := TDmxFldCollector.Valid(Command);
      end;
    cmOK:
      begin
      m := 1;
      n := 0;
      While (Name^[length(Name^)] = ' ') do Dec(Name^[0]);
      While (Name^ <> '') and (Name^[1] = ' ') do Delete(Name^, 1,1);
      If (Name^ = '') or (pos(' ', Name^) > 0) then
	begin
	If (Name^ = '') then S := 'Table name required.' else S := 'Invalid table name.';
	MessageBox(S, nil, mfError + mfOKButton);
	Select;
	Owner^.SelectNext(TRUE);
	Valid := FALSE;
	end
      else
      If TranslateFields then
	begin
	i := PxErrorBox(PxTblExist(Name^, Exist));
	If (i = 0) then
	  begin
	  If not Exist then
	    begin
	    i := PxErrorBox(PxTblCreate(Name^, PCollection(WorkingData)^.Count, NArr, TArr));
	    If (i = 0) and (LastN > 0) then
	      begin
	      For m := 1 to LastN do FA[m] := m;
	      PxErrorBox(PxKeyAdd(Name^, LastN, FA, Primary));
	      end;
	    end
	   else
	    begin
	    i := PxErr_TableBusy;
	    MessageBox('A table of that name already exists.', nil, mfError + mfOKButton);
	    end;
	  end;
	Select;
	Owner^.SelectNext(TRUE);
	Valid := (i = PxSuccess);
	end
       else
	begin
	Case m of
	  1 : S := 'Field name required.';
	  2 : S := 'Type code required.';
	  3 : S := 'Field length out of range.';
	 else S := 'Key fields must begin at field 1 and be contiguous.';
	  end;
	MessageBox(S, nil, mfError + mfOKButton);
	Select;
	GotoPos(m, n);
	Valid := FALSE;
	end;
      end;
   else
      begin
      Valid := TDmxFldCollector.Valid(Command);
      end;
    end;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.

