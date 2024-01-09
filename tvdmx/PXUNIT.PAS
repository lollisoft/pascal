
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{	PxUNIT  --an OOP Paradox Engine interface	}
{							}
{	Copyright (c) 1997-99	Randolph Beck		}
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{				rb@randybeck.com	}
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}

Unit PxUNIT;

{$V-,X+,N+,E+,I-,D+ }

interface

uses  Dos, Objects,
{$IFNDEF NOTV }
      Drivers, Views, Dialogs, App,
      DmxGizma, PxGizma, tvDMX, PxDMX,
{$ENDIF }
      PxEngine;

const
    unassignedFieldNum	=  5555;
    unassignedFieldType	: string[15]	=  'A15';

type
    PPxObject		= ^TPxObject;
{$IFNDEF NOTV }
    PPxObjDMX		= ^TPxObjDMX;

{$ELSE }
    PSItem	= ^TSItem;
    TSItem	=  RECORD
	Value		: PString;
	Next		: PSItem;
    end;
{$ENDIF }


    TPxObject	=  OBJECT(TObject)
	Next		: PPxObject;
	Status		: integer;
	Table		: TableHandle;
	RecHandle	: RecordHandle;
	Modified	: boolean;
	eachNext	: boolean;
	FindingFirst	: integer;
      constructor Init(TblName: string; IndexID: integer; SaveEveryChange: boolean);
      destructor  Done;  VIRTUAL;
      function  InitializeFields : boolean;  VIRTUAL;
      function  Open(TblName: string; IndexID: integer; SaveEveryChange: boolean) : boolean;
      function  Close : boolean;
      function  DefaultFileName : string;  VIRTUAL;
      function  DmxTemplate : string;  VIRTUAL;
      function  DmxLabel : string;  VIRTUAL;
      function  BlobQuickGet(FldHandle: FieldHandle; ASize: integer; var A ) : boolean;
      function  Build(TblName: FNameStr) : boolean;  VIRTUAL;
      function  BuildFieldList : PSItem;  VIRTUAL;
      function  CheckStatus(AStatus: integer) : boolean;
      function  RecDelete : boolean;  VIRTUAL;
      procedure Error(AStatus: integer);  VIRTUAL;
      function  ErrorName : string;  VIRTUAL;
      function  FindFirst(FldHandle: FieldHandle) : boolean;
      function  FindFirstByName(AFldName: string) : boolean;
      function  FindFirstLong(FldHandle: FieldHandle; L: longint) : boolean;
      function  FindNearest(FldHandle: FieldHandle) : boolean;
      function  FindNext(FldHandle: FieldHandle) : boolean;
      function  FindKey(keys: integer) : boolean;
      function  FindEach(FldHandle: FieldHandle) : boolean;
      function  RecEach : boolean;
      function  GetText(MaxLen: integer) : string;  VIRTUAL;
      function  RecGoto(ARec: longint) : boolean;
      function  RecFirst : boolean;
      function  RecLast : boolean;
      function  RecNext : boolean;
      function  RecPrev : boolean;
      function  NumFlds : integer;
      function  NumRecs : longint;
      function  GetRecNum(var RecNum: longint) : boolean;
      function  FieldNum(const FldName: string) : integer;
      function  OptField(const FldName: string) : integer;
      function  FieldName(FldHandle: FieldHandle) : string;
      function  FieldType(FldHandle: FieldHandle) : string;
      function  FieldText(FldHandle: FieldHandle) : string;
      function  CheckBlank(FldHandle: FieldHandle; var Blank: boolean) : boolean;
      function  GetAlpha(FldHandle: FieldHandle; var A: string) : boolean;
      function  GetBlobHeader(FldHandle: FieldHandle; var A: string; AMaxLen: integer) : boolean;
      function  GetMDY(FldHandle: FieldHandle; var M,D,Y: integer) : boolean;
      function  GetDoub(FldHandle: FieldHandle; var N: double) : boolean;
      function  GetLong(FldHandle: FieldHandle; var N: longint) : boolean;
      function  GetRecData : boolean;  VIRTUAL;
      function  GetShort(FldHandle: FieldHandle; var N: integer) : boolean;
      function  PutAlpha(FldHandle: FieldHandle; A: string) : boolean;
      function  PutMDY(FldHandle: FieldHandle; M,D,Y: integer) : boolean;
      function  PutDoub(FldHandle: FieldHandle; N: double) : boolean;
      function  PutLong(FldHandle: FieldHandle; N: longint) : boolean;
      function  PutShort(FldHandle: FieldHandle; N: integer) : boolean;
      function  PutRecData : boolean;  VIRTUAL;
      function  RecRawGet(var Buffer;  BufSize: integer) : boolean;
      function  RecRawPut(var Buffer;  BufSize: integer) : boolean;
      function  RecAppend : boolean;
      function  RecBufClear : boolean;
      function  RecClear : boolean;  VIRTUAL;
      function  RecGet: boolean;
      function  RecLoad : boolean;
      function  RecUpdate : boolean;
      function  Reset : boolean;
      function  SearchKey(Fields: integer; Mode: integer) : boolean;
      function  SearchFld(FldHandle: FieldHandle; Mode: integer) : boolean;
      function  UpdateDMX : boolean;
      procedure SetRecDMX(var AData );  VIRTUAL;
      procedure GetRecDMX(var AData );  VIRTUAL;
    end;


{$IFNDEF NOTV }
    TPxObjDMX	=  OBJECT(TDmxPxEditor)
	PXO	: PPxObject;
      constructor Init(PxObject: PPxObject;
			var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
			    AHScrollBar,AVScrollBar: PScrollBar);
      function  ReadRec(var RecData ) : boolean;  VIRTUAL;
      function  WriteRec(var RecData ) : boolean;  VIRTUAL;
    end;


{$ELSE }
function  NewSItem(const Str: string; ANext: PSItem) : PSItem;
function  PxBuild(TblName: FNameStr; AFields: PSItem) : integer;
{$ENDIF }

function  IntToStr(N: longint) : string;
function  FloatToStr(N: double) : string;


implementation


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  IntToStr(N: longint) : string;
var  S: string;
begin
  Str(N:1, S);
  IntToStr := S;
end;


function  FloatToStr(N: double) : string;
var  S: string;
begin
  Str(N:1:4, S);
  If pos('.',S) > 0 then
    begin
    While S[length(S)] = '0' do Dec(S[0]);
    If S[length(S)] = '.' then Dec(S[0]);
    end;
  FloatToStr := S;
end;


{$IFDEF NOTV }
function  NewSItem(const Str: string; ANext: PSItem) : PSItem;
var  Item  : PSItem;
begin
  New(Item);
  Item^.Value := NewStr(Str);
  Item^.Next  := ANext;
  NewSItem := Item;
end;


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
{$ENDIF }


constructor TPxObject.Init(TblName: string; IndexID: integer; SaveEveryChange: boolean);
begin
  inherited Init;
  If TblName <> '' then Open(TblName, IndexID, SaveEveryChange);
end;


destructor TPxObject.Done;
begin
  Close;
  inherited Done;
end;


function  TPxObject.InitializeFields : boolean;
begin
  { virtual method to assign field numbers if desired }
end;


function  TPxObject.Open(TblName: string; IndexID: integer; SaveEveryChange: boolean) : boolean;
begin
  If TblName = '@' then TblName := DefaultFileName;
  Status := PxTblOpen(TblName, Table, IndexID, SaveEveryChange);
  If (Status = PxSuccess) then CheckStatus(PxRecBufOpen(Table, RecHandle));
  If (Status = PxSuccess) then InitializeFields;
  Open := Status = PxSuccess;
end;


function  TPxObject.Close : boolean;
begin
  If (RecHandle <> 0) then PxRecBufClose(RecHandle);
  RecHandle := 0;
  If (Table <> 0) then PxTblClose(Table);
  Table := 0;
  Close := TRUE;
end;


function  TPxObject.DefaultFileName : string;
begin
  DefaultFileName := '';
end;


function  TPxObject.DmxTemplate : string;
begin
  Abstract;
end;


function  TPxObject.DmxLabel : string;
begin
  Abstract;
end;


function  TPxObject.BlobQuickGet(FldHandle: FieldHandle; ASize: integer; var A ) : boolean;
var  BytesRead	: integer;
begin
  BlobQuickGet := (Status = PxSuccess)
	and CheckStatus(PxBlobQuickGet(RecHandle, FldHandle, ASize, A, BytesRead));
end;


function  TPxObject.Build(TblName: FNameStr) : boolean;
begin
  If TblName = '@' then TblName := DefaultFileName;
  Build := (Status = PxSuccess)
	and CheckStatus(PxBuild(TblName, BuildFieldList));
end;


function  TPxObject.BuildFieldList : PSItem;
{ abstract method to generate list of fields to create table }
begin
  BuildFieldList := nil;
  Abstract;
end;


function  TPxObject.CheckStatus(AStatus: integer) : boolean;
begin
  If (AStatus <> PxSuccess) and (Status = PxSuccess) then
    Error(AStatus);
  CheckStatus := (Status = PxSuccess);
end;


function  TPxObject.RecDelete : boolean;
begin
  RecDelete := (Status = PxSuccess) and CheckStatus(PxRecDelete(Table));
end;


procedure TPxObject.Error(AStatus: integer);
begin
{$IFNDEF NOTV }
  If (AStatus in PxFatalErrors) and (Application <> nil) then
    begin
    Message(Application, evCommand, cmBeep, @Self);
    PxErrorBox(AStatus);
    end;
{$ENDIF }
  Status := AStatus;
end;


function  TPxObject.ErrorName : string;
var  S: string[79];
begin
  Case Status of
    PXSUCCESS		: ErrorName := 'No errors';

    PXERR_NOTINITERR	: ErrorName := 'Engine not initialized';
    PXERR_ALREADYINIT	: ErrorName := 'Engine already initialized';
    PXERR_NOTLOGGEDIN	: ErrorName := 'Could not login on network (to PARADOX.NET)';
    PXERR_NONETINIT	: ErrorName := 'Engine not initialized with PXNetInit';
    PXERR_NETMULTIPLE	: ErrorName := 'multiple PARADOX.NET files';

    PXERR_DRIVENOTREADY	: ErrorName := 'Drive not ready';
    PXERR_DISKWRITEPRO	: ErrorName := 'Disk is write protected';
    PXERR_GENERALFAILURE: ErrorName := 'General hardware error';

    PXERR_DIRNOTFOUND	: ErrorName := 'Directory not found';
    PXERR_DIRBUSY	: ErrorName := 'Sharing violation';
    PXERR_DIRLOCKED	: ErrorName := 'Sharing violation';
    PXERR_DIRNOACCESS	: ErrorName := 'No access to directory';
    PXERR_DIRNOTPRIVATE	: ErrorName := 'Single user, but directory is shared';

 { file oriented errors }

    PXERR_FILEBUSY	: ErrorName := 'File is busy';
    PXERR_FILELOCKED	: ErrorName := 'File is locked';
    PXERR_FILENOTFOUND	: ErrorName := 'Could not find file';

 { table oriented errors }

    PXERR_TABLEBUSY	: ErrorName := 'Table is busy';
    PXERR_TABLELOCKED	: ErrorName := 'Table is locked';
    PXERR_TABLENOTFOUND	: ErrorName := 'Table was not found';
    PXERR_TABLEOPEN	: ErrorName := 'Unable to perform operation on open table';
    PXERR_TABLEINDEXED	: ErrorName := 'Table is indexed';
    PXERR_TABLENOTINDEXED: ErrorName := 'Table is not indexed';
    PXERR_TABLEEMPTY	: ErrorName := 'Operation on empty table';
    PXERR_TABLEWRITEPRO	: ErrorName := 'Table is write protected';
    PXERR_TABLEPRE40	: ErrorName := 'Feature not available for pre-Paradox 4.0 tables';
    PXERR_TABLECORRUPTED: ErrorName := 'Table is corrupted';
    PXERR_TABLEFULL	: ErrorName := 'Table is full';
    PXERR_TABLESQL	: ErrorName := 'Table is SQL replica';
    PXERR_INSUFRIGHTS	: ErrorName := 'Insufficient password rights';
    PXERR_CANTUPGRADE	: ErrorName := 'Table too old to upgrade, or existing table header too small.';
    PXERR_LOCKTIMEOUT	: ErrorName := 'Timed out trying to achieve a lock.';

 { index oriented errors }

    PXERR_XCORRUPTED	: ErrorName := 'Primary index is corrupted';
    PXERR_XOUTOFDATE	: ErrorName := 'Primary index is out of date';
    PXERR_XSORTVERSION	: ErrorName := 'Sort for index different from table';

    PXERR_SXCORRUPTED	: ErrorName := 'Secondary index is corrupted';
    PXERR_SXOUTOFDATE	: ErrorName := 'Secondary index is out of date';
    PXERR_SXNOTFOUND	: ErrorName := 'Secondary index was not found';
    PXERR_SXOPEN	: ErrorName := 'Secondary index is already open';
    PXERR_SXCANTUPDATE	: ErrorName := 'Can''t update Heap open on Secondary index';

    PXERR_RECTOOBIG	: ErrorName := 'Record too big for index';

 { record oriented errors }

    PXERR_RECDELETED	: ErrorName := 'Another user deleted record';
    PXERR_RECLOCKED	: ErrorName := 'Record is locked';
    PXERR_RECNOTFOUND	: ErrorName := 'Record was not found';
    PXERR_KEYVIOL	: ErrorName := 'Key violation';

    PXERR_ENDOFTABLE	: ErrorName := 'End of table';
    PXERR_STARTOFTABLE	: ErrorName := 'Start of table';

 { resource errors }

    PXERR_OUTOFMEM	: ErrorName := 'Not enough memory to complete operation';
    PXERR_OUTOFDISK	: ErrorName := 'Not enough disk space to complete operation';
    PXERR_OUTOFSTACK	: ErrorName := 'Not enough stack space to complete operation';
    PXERR_OUTOFSWAPBUF	: ErrorName := 'Not enough swap buffer space to complete operation';

    PXERR_OUTOFFILEHANDLES	: ErrorName := 'No more file handles available';
    PXERR_OUTOFTABLEHANDLES	: ErrorName := 'No more table handles available';
    PXERR_OUTOFRECHANDLES	: ErrorName := 'No more record handles available';
    PXERR_OUTOFLOCKHANDLES	: ErrorName := 'Too many locks on table';

    PXERR_NOMORETMPNAMES: ErrorName := 'No more temporary names available';
    PXERR_TOOMANYPASSW	: ErrorName := 'Too many passwords specified';

 { invalid parameters to functions }

    PXERR_TYPEMISMATCH	: ErrorName := 'Data type mismatch';
    PXERR_OUTOFRANGE	: ErrorName := 'Argument out of range';
    PXERR_INVPARAMETER	: ErrorName := 'Invalid argument';
    PXERR_INVDATE	: ErrorName := 'Invalid date given';

    PXERR_INVFIELDHANDLE: ErrorName := 'Invalid field handle';
    PXERR_INVRECHANDLE	: ErrorName := 'Invalid record handle';
    PXERR_INVTABLEHANDLE: ErrorName := 'Invalid table handle';
    PXERR_INVLOCKHANDLE	: ErrorName := 'Invalid lock handle';

    PXERR_INVDIRNAME	: ErrorName := 'Invalid directory name';
    PXERR_INVFILENAME	: ErrorName := 'Invalid file name';
    PXERR_INVTABLENAME	: ErrorName := 'Invalid table name';
    PXERR_INVFIELDNAME	: ErrorName := 'Invalid field name';

    PXERR_INVLOCKCODE	: ErrorName := 'Invalid lock code';
    PXERR_INVUNLOCK	: ErrorName := 'Invalid unlock';
    PXERR_INVSORTORDER	: ErrorName := 'Invalid sort order table';
    PXERR_INVPASSW	: ErrorName := 'Invalid password';
    PXERR_INVNETTYPE	: ErrorName := 'Invalid net type (PXNetInit)';
    PXERR_BUFTOOSMALL	: ErrorName := 'Buffer too small for result';

    PXERR_STRUCTDIFFER	: ErrorName := 'Table structures are different';

    PXERR_INVENGINESTATE: ErrorName := 'Previous fatal error; cannot proceed';

 { Blob error codes }

    PXERR_BLOBMODE	: ErrorName := 'operation not applicable for Blob''s open mode';
    PXERR_BLOBOPEN	: ErrorName := 'Blob already open';
    PXERR_BLOBINVOFFSET	: ErrorName := 'Invalid offset into Blob';
    PXERR_BLOBINVSIZE	: ErrorName := 'Invalid size for Blob';
    PXERR_BLOBMODIFIED	: ErrorName := 'Another user modified Blob';
    PXERR_BLOBCORRUPT	: ErrorName := 'Blob file corrupted';
    PXERR_BLOBNOINDEX	: ErrorName := 'Blob cannot be part of any key or index';
    PXERR_BLOBINVHANDLE	: ErrorName := 'Invalid Blob handle';
    PXERR_BLOBNOSEARCH	: ErrorName := 'Can''t search on a Blob field';

 { Locking related error codes }

    PXERR_INUSEBYPDOX35	: ErrorName := 'In use by Paradox 3.5';
   else
      begin
      Str(Status:1, S);
      ErrorName := 'Error #' + S;
      end;
  end;
end;



function  TPxObject.FindFirst(FldHandle: FieldHandle) : boolean;
begin
  FindingFirst := FldHandle;
  FindFirst :=
	PutRecData and
	SearchFld(FldHandle, SearchFirst) and
	RecLoad;
  FindingFirst := 0;
end;


function  TPxObject.FindFirstByName(AFldName: string) : boolean;
begin
  FindFirstByName := FindFirst(FieldNum(AFldName));
end;


function  TPxObject.FindNearest(FldHandle: FieldHandle) : boolean;
begin
  FindingFirst := FldHandle;
  If PutRecData then
    begin
    SearchFld(FldHandle, ClosestRecord);
    If (Status = PxErr_RecNotFound) then Reset;
    RecLoad;
    end;
  FindingFirst := 0;
  FindNearest := (Status = PxSuccess);
end;


function  TPxObject.FindFirstLong(FldHandle: FieldHandle; L: longint) : boolean;
begin
  FindFirstLong :=
	PutLong(FldHandle, L) and
	SearchFld(FldHandle, SearchFirst) and
	RecLoad;
end;


function  TPxObject.FindNext(FldHandle: FieldHandle) : boolean;
begin
  FindNext :=
	SearchFld(FldHandle, SearchNext) and
	RecLoad;
end;


function  TPxObject.FindKey(keys: integer) : boolean;
begin
  FindingFirst := -1;
  FindKey := PutRecData and SearchKey(keys, SearchFirst);
  FindingFirst :=  0;
end;


function  TPxObject.FindEach(FldHandle: FieldHandle) : boolean;
begin
  If (Status <> PxSuccess) then
    FindEach := FALSE
   else
    begin
    If eachNext then
      eachNext := FindNext(FldHandle)
     else
      eachNext := FindFirst(FldHandle);
    If (Status = PxErr_RecNotFound) then
      begin
      Reset;
      FindEach := FALSE;
      end
     else
      FindEach := eachNext;
    end;
end;


function  TPxObject.RecEach : boolean;
begin
  If (Status <> PxSuccess) then
    RecEach := FALSE
   else
    begin
    If eachNext then eachNext := RecNext else eachNext := RecFirst;
    If (Status = PxErr_EndOfTable) then
      begin
      Reset;
      RecEach := FALSE;
      end
     else
      begin
      eachNext := RecLoad;
      RecEach  := eachNext;
      end;
    end;
end;


function  TPxObject.GetText(MaxLen: integer) : string;
{ this method gets text for a listbox's GetText method }
var  i,n,fields	: integer;
     A		: string;
begin
  fields := NumFlds;
  A := '';
  If NumFlds > 0 then
    begin
    i := 1;
    Repeat
      If i > 1 then A := A + ', ';
      A := A + FieldText(i);
    Until (length(A) > MaxLen) or (Status <> PxSuccess);
    If length(A) > MaxLen then
      begin
      A[0] := char(MaxLen);
      A[MaxLen] := '>';
      end;
    end;
  GetText := A;
end;


function  TPxObject.RecGoto(ARec: longint) : boolean;
begin
  RecGoto := (Status = PxSuccess) and CheckStatus(PxRecGoto(Table, ARec))
		and RecLoad;
end;


function  TPxObject.RecFirst : boolean;
begin
  RecFirst := (Status = PxSuccess) and CheckStatus(PxRecFirst(Table))
		and RecLoad;
end;


function  TPxObject.RecLast : boolean;
begin
  RecLast := (Status = PxSuccess) and CheckStatus(PxRecLast(Table))
		and RecLoad;
end;


function  TPxObject.RecNext : boolean;
begin
  RecNext := (Status = PxSuccess) and CheckStatus(PxRecNext(Table))
		and RecLoad;
end;


function  TPxObject.RecPrev : boolean;
begin
  RecPrev := (Status = PxSuccess) and CheckStatus(PxRecPrev(Table))
		and RecLoad;
end;


function  TPxObject.NumFlds : integer;
var  Stat,NFlds  : integer;
begin
  NFlds := 0;
  Stat := PxRecNFlds(Table, NFlds);
  CheckStatus(Stat);
  NumFlds := NFlds;
end;


function  TPxObject.NumRecs : longint;
var  Stat	: integer;
     L		: longint;
begin
  L := 0;
  Stat := PxTblNRecs(Table, L);
  CheckStatus(Stat);
  NumRecs := L;
end;


function  TPxObject.GetRecNum(var RecNum: longint) : boolean;
begin
  RecNum := 0;
  GetRecNum := (Status = PxSuccess) and CheckStatus(PxRecNum(Table, RecNum));
end;


function  TPxObject.FieldNum(const FldName: string) : integer;
var  n: FieldHandle;
begin
  If (Status = PxSuccess) and CheckStatus(PxFldHandle(Table, FldName, n)) then
    FieldNum := n
   else
    FieldNum := 0;
end;


function  TPxObject.OptField(const FldName: string) : integer;
var  n: FieldHandle;
     e: integer;
begin
  OptField := 0;
  If (Status = PxSuccess) then
    begin
    e := PxFldHandle(Table, FldName, n);
    Case e of
      PxSuccess:		OptField := n;
      PxErr_InvFieldName:	OptField := unassignedFieldNum;
    else			CheckStatus(e);
      end;
    end;
end;


function  TPxObject.FieldName(FldHandle: FieldHandle) : string;
var  A	: string;
begin
  If (Status = PxSuccess) and
     (FldHandle <> unassignedFieldNum) and
     CheckStatus(PxFldName(Table, FldHandle, A))
  then
    FieldName := A
   else
    FieldName := '';
end;


function  TPxObject.FieldType(FldHandle: FieldHandle) : string;
{ Returns the Paradox type code of a field ("A25", "N", etc.).
  It returns '?' when there is an error to allow immediate use
  of the return value in a case statement. }
var  OK: boolean;
     fldtype: string;
begin
  fldtype := '?';
  If (FldHandle = unassignedFieldNum) then
    fldtype := unassignedFieldType
   else
    begin
    OK := (Status = PxSuccess) and CheckStatus(PxFldType(Table,FldHandle,fldtype));
    If fldtype = '' then fldtype := '?';
    end;
  FieldType := fldtype;
end;


function  TPxObject.FieldText(FldHandle: FieldHandle) : string;
var  i,n	: integer;
     A		: string;
     d		: double;
     mm,dd,yy	: integer;
begin
  A := FieldType(FldHandle);
  Case A[1] of
    'A':	GetAlpha(i, A);
    'N':	begin  GetDoub(i, d);  A := FloatToStr(d);  end;
    'S':	begin  GetShort(i, n);  A := IntToStr(n);  end;
    'D':	begin
		GetMDY(i, mm,dd,yy);
		A := IntToStr(mm) + '/' + IntToStr(dd) + '/' + IntToStr(yy);
		end;
    '?':	A := 'ERROR';
   else		A := '<' + A + '>';
    end;
  If Status <> PxSuccess then A := 'ERROR';
  FieldText := A;
end;


function  TPxObject.CheckBlank(FldHandle: FieldHandle; var Blank: boolean) : boolean;
begin
  Blank := TRUE;
  If FldHandle = unassignedFieldNum then
    CheckBlank := (Status = PxSuccess)
   else
    CheckBlank := (Status = PxSuccess)
		and CheckStatus(PxFldBlank(RecHandle, FldHandle, Blank));
end;


function  TPxObject.GetAlpha(FldHandle: FieldHandle; var A: string) : boolean;
begin
  A := '';
  If FldHandle = unassignedFieldNum then
    GetAlpha := (Status = PxSuccess)
   else
    GetAlpha := (Status = PxSuccess)
             and CheckStatus(PxGetAlpha(RecHandle, FldHandle, A));
end;


function  TPxObject.GetBlobHeader(FldHandle: FieldHandle; var A: string; AMaxLen: integer) : boolean;
begin
  If (FldHandle = unassignedFieldNum) then
    A := ''
   else
    begin
    If BlobQuickGet(FldHandle, AMaxLen, A[1]) then
      begin
      A[0] := char(AMaxLen);
      While (A <> '') and (A[length(A)] in [#0,^J,^M,' ']) do Dec(A[0]);
      end
     else
      A := '';
    end;
  GetBlobHeader := (Status = PxSuccess)
end;


function  TPxObject.GetMDY(FldHandle: FieldHandle; var M,D,Y: integer) : boolean;
var  Date  : TDate;
begin
  M := 0;
  D := 0;
  Y := 0;
  If FldHandle = unassignedFieldNum then
    GetMDY := (Status = PxSuccess)
   else
    begin
    If Status <> PxSuccess then
      GetMDY := FALSE
    else
      begin
      If (Status = PxSuccess) and
	 CheckStatus(PxGetDate(RecHandle, FldHandle, Date)) and
	 (IsBlankDate(Date) or CheckStatus(PxDateDecode(Date, M,D,Y)))
       then
	GetMDY := TRUE
       else
	begin;
        Status := PxSuccess;
	M := 0;
	D := 0;
	Y := 0;
        GetMDY := FALSE;
        end;
      end;
    end;
end;


function  TPxObject.GetDoub(FldHandle: FieldHandle; var N: double) : boolean;
begin
  If FldHandle = unassignedFieldNum then
    begin
    N := 0;
    GetDoub := (Status = PxSuccess);
    end
   else
    GetDoub := (Status = PxSuccess) and CheckStatus(PxGetDoub(RecHandle, FldHandle, N));
  If (Status <> PxSuccess) or IsBlankDouble(N) then N := 0;
end;


function  TPxObject.GetLong(FldHandle: FieldHandle; var N: longint) : boolean;
var  E	: integer;
     R	: double;
begin
  E := 0;
  N := 0;
  If FldHandle = unassignedFieldNum then
    GetLong := (Status = PxSuccess)
   else
    If (Status = PxSuccess) then
    begin
    E := PxGetLong(RecHandle, FldHandle, N);
    If (E = PxErr_TypeMismatch) or (E = PxErr_OutOfRange) then
      begin
      E := PxGetDoub(RecHandle, FldHandle, R);
      If (R >  MaxLongInt) then N :=  MaxLongInt else
      If (R < -MaxLongInt) then N := -MaxLongInt else N := round(R);
      end;
    GetLong := CheckStatus(E);
    end
   else
    GetLong := FALSE;
  If IsBlankLong(N) then N := 0;
end;


function  TPxObject.GetRecData : boolean;
begin
  GetRecData := Status = PxSuccess;
end;


function  TPxObject.GetShort(FldHandle: FieldHandle; var N: integer) : boolean;
begin
  If FldHandle = unassignedFieldNum then
    begin
    N := 0;
    GetShort := (Status = PxSuccess);
    end
   else
    GetShort := (Status = PxSuccess) and CheckStatus(PxGetShort(RecHandle, FldHandle, N));
  If (Status <> PxSuccess) or IsBlankShort(N) then N := 0;
end;


function  TPxObject.PutAlpha(FldHandle: FieldHandle; A: string) : boolean;
begin
  PutAlpha := (Status = PxSuccess) and
	      ((FldHandle = unassignedFieldNum)
		or
	       CheckStatus(PxPutAlpha(RecHandle, FldHandle, A)));
end;


function  TPxObject.PutMDY(FldHandle: FieldHandle; M,D,Y: integer) : boolean;
var  E	   : integer;
     Date  : TDate;
begin
  If (FldHandle = unassignedFieldNum) then
    PutMDY := (Status = PxSuccess)
  else
  If (Status = PxSuccess) then
    begin
    If (M = 0) or (D = 0) or (Y = 0) or
       (M > 12) or (D > 31)
    then
      E := PxPutBlank(RecHandle, FldHandle)
     else
      begin
      E := PxDateEncode(M,D,Y, Date);
      If (E = PxSuccess) then E := PxPutDate(RecHandle, FldHandle, Date);
      end;
    PutMDY := CheckStatus(E);
    end
   else
    PutMDY := FALSE;
end;


function  TPxObject.PutDoub(FldHandle: FieldHandle; N: double) : boolean;
begin
  PutDoub := (Status = PxSuccess) and
	      ((FldHandle = unassignedFieldNum)
	       or CheckStatus(PxPutDoub(RecHandle, FldHandle, N)));
end;


function  TPxObject.PutLong(FldHandle: FieldHandle; N: longint) : boolean;
begin
  PutLong := (Status = PxSuccess) and
	     ((FldHandle = unassignedFieldNum)
	      or CheckStatus(PxPutLong(RecHandle, FldHandle, N)));
end;


function  TPxObject.PutShort(FldHandle: FieldHandle; N: integer) : boolean;
begin
  PutShort := (Status = PxSuccess) and
	      ((FldHandle = unassignedFieldNum)
	       or CheckStatus(PxPutShort(RecHandle, FldHandle, N)));
end;


function  TPxObject.PutRecData : boolean;
begin
  PutRecData := Status = PxSuccess;
end;


function  TPxObject.RecRawGet(var Buffer;  BufSize: integer) : boolean;
{ gets the content of the record buffer without any translation }
begin
  RecRawGet := (Status = PxSuccess) and
		CheckStatus(PxRawGet(RecHandle, Buffer, BufSize));
end;


function  TPxObject.RecRawPut(var Buffer;  BufSize: integer) : boolean;
{ copies the user's buffer into the record buffer without any translation }
begin
  RecRawPut := (Status = PxSuccess) and
		CheckStatus(PxRawPut(RecHandle, Buffer, BufSize));
end;


function  TPxObject.RecAppend : boolean;
begin
  RecAppend := (Status = PxSuccess) and PutRecData and
		CheckStatus(PxRecAppend(Table, RecHandle));
  If (Status = PxSuccess) then Modified := FALSE;
end;


function  TPxObject.RecBufClear : boolean;
begin
  RecBufClear := (Status = PxSuccess) and CheckStatus(PxRecBufEmpty(RecHandle));
end;


function  TPxObject.RecClear : boolean;
begin
  RecClear := RecBufClear and GetRecData;
end;


function  TPxObject.RecGet : boolean;
begin
  RecGet := (Status = PxSuccess) and CheckStatus(PxRecGet(Table, RecHandle));
end;


function  TPxObject.RecLoad : boolean;
begin
  If (Status = PxSuccess) then
    RecLoad := RecGet and GetRecData
   else
    RecLoad := FALSE;
  If (Status = PxSuccess) then Modified := FALSE;
end;


function  TPxObject.RecUpdate : boolean;
begin
  RecUpdate := (Status = PxSuccess) and PutRecData and
		CheckStatus(PxRecUpdate(Table, RecHandle));
  If (Status = PxSuccess) then Modified := FALSE;
end;


function  TPxObject.Reset : boolean;
begin
  Status   := PxSuccess;
  eachNext := FALSE;
  Reset    := TRUE;
end;


function  TPxObject.SearchKey(Fields: integer; Mode: integer) : boolean;
begin
  SearchKey := (Status = PxSuccess) and
		CheckStatus(PxSrchKey(Table,RecHandle,Fields,Mode));
end;


function  TPxObject.SearchFld(FldHandle: FieldHandle; Mode: integer) : boolean;
begin
  SearchFld := (Status = PxSuccess) and
		CheckStatus(PxSrchFld(Table,RecHandle, FldHandle, Mode));
end;


function  TPxObject.UpdateDMX : boolean;
begin
{$IFNDEF NOTV }
  If (Application <> nil) then
    begin
    Reset;
    Message(Application, evBroadcast, cmDMX_DrawData, @Table);
    end;
{$ENDIF }
  UpdateDMX := (Status = PxSuccess);
end;


procedure TPxObject.SetRecDMX(var AData );
{ this is an abstract method to transfer data for a tvDMX's ReadRec method }
begin
  Abstract;
end;


procedure TPxObject.GetRecDMX(var AData );
{ this is an abstract method to transfer data for a tvDMX's WriteRec method }
begin
  Abstract;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

{$IFNDEF NOTV }
constructor TPxObjDMX.Init(PxObject: PPxObject;
			   var Bounds: TRect;  ALabels,ARecInd: PDmxLink;
			       AHScrollBar,AVScrollBar: PScrollBar);
begin
  TDmxPxEditor.Init(PxObject^.DmxTemplate, PxObject^.Table, 1, Bounds, ALabels, ARecInd,
		     AHScrollBar,AVScrollBar);
  PXO := PxObject;
  Locked := TRUE;
end;


function  TPxObjDMX.ReadRec(var RecData ) : boolean;
begin
  If PXO^.RecLoad then
    begin
    Status := PxSuccess;
    PXO^.SetRecDMX(RecData);
    end
   else
    Status := PXO^.Status;
  PXO^.Reset;
  ReadRec := (Status = PxSuccess);
end;


function  TPxObjDMX.WriteRec(var RecData ) : boolean;
{ for future use: this version of TPxObjDMX does not edit data }
begin
  PXO^.Reset;
  PXO^.GetRecDMX(RecData);
  PXO^.RecUpdate;
  WriteRec := (Status = PxSuccess);
  PXO^.Reset;
end;
{$ENDIF }

  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


End.


3-21-98
  Default methods of GetRecData and PutRecData were originally abstract
  methods that fail with a PxErr_TypeMismatch error.  It now returns TRUE
  when there was no previous error.  This change allows the raw base object
  to be used for quick programming tasks.

  Added RecClear virtual method.  The default calls RecBufClear and then
  GetRecData to retrieve blanks from the buffer.


3-22-98
  Added FieldNum method to make it easier to use the raw base object.  This
  also makes it easier to create descendants that can operate after tables
  have been restructured.

  UpdateDMX now checks to see if Application has been initialized so that
  this function may be called by non-TV applications without potential for
  serious error.  Also modified this method to return FALSE if Status is
  not zero.


3-25-98
  Added InitializeFields method to allow for assignment of field numbers.
  This method is called by the Open method.

  Added the constant unassignedFieldNum and modified the Get-field and Set-
  field methods to ignore unassigned fields.

  Added OptField method which operates like function FieldNum but returns
  unassignedFieldNum if there is no field of that name.


4-03-98
  Added GetBlobHeader method to retrieve blob headers as a string.

  If "NOTV" is defined then TSItem, NewSItem and PxBuild are compiled in
  this unit.  RSET.PAS has also been updated for NOTV.


4-04-98
  Added ErrorName function to return an error message string from Status.

  Close method zeroes the Table and RecHandle fields.


4-14-98
  Added CheckBlank method.


6-13-98
  Added FieldName method.


6-25-98
  RecLoad now resets Modified if Status = PxSuccess.


10-10-98
  Functions RecGoto, RecFirst, RecLast, RecNext and RecPrev now call the
  RecLoad method.


10-19-98
  Added RecRawGet and RecRawPut methods.
  Added DefaultFileName method.


9-18-99
  Added FieldType method.


9-22-99
  Added FieldText method.
  Created default return value for GetText method.
  Added FindFirstByName method.

