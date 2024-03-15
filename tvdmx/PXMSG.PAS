{ pxmsg.pas -- returns error strings from error codes }
Unit PXMsg;

Interface

{$ifdef WINDOWS}
function PXErrMsg( ErrCode:Integer ): PChar;
{$else}
function PXErrMsg( ErrCode:Integer ): String;
procedure Error( Msg: String );  { Not defined for Windows }
procedure PX( errCode:Integer ); { Not defined for Windows }
{$endif}

Implementation

{$ifdef WINDOWS}                 { To get error codes for Windows }
  Uses PXEngWin,Strings,WinProcs;
{$else}                          { To get error codes for DOS }
  Uses PXEngine;
{$endif}

type
{$ifdef WINDOWS}
  Message = record
    code : Integer;
    msg  : PChar;
  end;
{$else}
  Message = record
    code : Integer;
    msg  : String[80];
  end;
{$endif}


const
{$ifdef WINDOWS}     { Count Windows error messages too. }
  KnownMsgSize = 91;
{$else}              { Just DOS error messages. }
  KnownMsgSize = 86;
{$endif}
  KnownMessages: array [1..KnownMsgSize] of Message =
    ( ( code: PXSUCCESS;                 msg: 'No error' ),
      ( code: PXERR_DRIVENOTREADy;       msg: 'Drive not ready' ),
      ( code: PXERR_DIRNOTFOUND;         msg: 'Directory not found' ),
      ( code: PXERR_FILEBUSY;            msg: 'File is busy' ),
      ( code: PXERR_FILELOCKED;          msg: 'File is locked' ),
      ( code: PXERR_FILENOTFOUND;        msg: 'Could not find file' ),
      ( code: PXERR_TABLECORRUPTED;      msg: 'Table is corrupted' ),
      ( code: PXERR_XCORRUPTED;          msg: 'Primary index is corrupted' ),
      ( code: PXERR_XOUTOFDATE;          msg: 'Primary index is out of date' ),
      ( code: PXERR_RECLOCKED;           msg: 'Record is locked' ),
      ( code: PXERR_DIRBUSY;             msg: 'Sharing violation' ),
      ( code: PXERR_DIRLOCKED;           msg: 'Sharing violation' ),
      ( code: PXERR_DIRNOACCESS;         msg: 'No access to directory' ),
      ( code: PXERR_XSORTVERSION;        msg: 'Sort for index different from table' ),
      ( code: PXERR_DIRNOTPRIVATE;       msg: 'Single user but directory is shared' ),
      ( code: PXERR_NETMULTIPLE;         msg: 'Multiple Paradox net files found' ),
      ( code: PXERR_TOOMANYPASSW;        msg: 'Too many passwords specified' ),
      ( code: PXERR_INSUFRIGHTS;         msg: 'Insufficient password rights' ),
      ( code: PXERR_TABLEWRITEPRO;       msg: 'Table is write-protected' ),
      ( code: PXERR_INVPASSW;            msg: 'Invalid password' ),
      ( code: PXERR_TYPEMISMATCH;        msg: 'Data type mismatch' ),
      ( code: PXERR_OUTOFRANGE;          msg: 'Argument is out of range' ),
      ( code: PXERR_BUFTOOSMALL;         msg: 'Buffer is too small for result' ),
      ( code: PXERR_INVPARAMETER;        msg: 'Invalid argument' ),
      ( code: PXERR_TABLEBUSY;           msg: 'Table is busy' ),
      ( code: PXERR_TABLELOCKED;         msg: 'Table is locked' ),
      ( code: PXERR_OUTOFMEM;            msg: 'Not enough memory to complete operation' ),
      ( code: PXERR_OUTOFDISK;           msg: 'Not enough disk space to complete operation' ),
      ( code: PXERR_TABLENOTFOUND;       msg: 'Table was not found' ),
      ( code: PXERR_SXNOTFOUND;          msg: 'Secondary index was not found' ),
      ( code: PXERR_RECDELETED;          msg: 'Another user deleted record' ),
      ( code: PXERR_SXCORRUPTED;         msg: 'Secondary index is corrupted' ),
      ( code: PXERR_DISKWRITEPRO;        msg: 'Disk is write protected' ),
      ( code: PXERR_RECTOOBIG;           msg: 'Record too big' ),
      ( code: PXERR_GENERALFAILURE;      msg: 'General System error' ),
      ( code: PXERR_OUTOFSTACK;          msg: 'Not enough stack space to complete operation' ),
      ( code: PXERR_OUTOFFILEHANDLES;    msg: 'No more file handles available' ),
      ( code: PXERR_TABLEFULL;           msg: 'Table is full' ),
      ( code: PXERR_OUTOFTABLEHANDLES;   msg: 'No more table handles available' ),
      ( code: PXERR_INVDATE;             msg: 'Invalid date given' ),
      ( code: PXERR_INVFIELDNAME;        msg: 'Invalid field name' ),
      ( code: PXERR_INVFIELDHANDLE;      msg: 'Invalid field handle' ),
      ( code: PXERR_INVTABLEHANDLE;      msg: 'Invalid table handle' ),
      ( code: PXERR_OUTOFSWAPBUF;        msg: 'Not enough swap buffer space to complete operation' ),
      ( code: PXERR_NOTINITERR;          msg: 'Engine not initialized' ),
      ( code: PXERR_INVENGINESTATE;      msg: 'Previous fatal error; cannot proceed' ),
      ( code: PXERR_TABLESQL;            msg: 'Table is SQL replica' ),
      ( code: PXERR_STRUCTDIFFER;        msg: 'Table structures are different' ),
      ( code: PXERR_ALREADYINIT;         msg: 'Engine already initialized' ),
      ( code: PXERR_TABLEOPEN;           msg: 'Unable to perform operation on open table' ),
      ( code: PXERR_NOMORETMPNAMES;      msg: 'No more temporary names available' ),
      ( code: PXERR_RECNOTFOUND;         msg: 'Record was not found' ),
      ( code: PXERR_SXCANTUPDATE;        msg: 'Can''t modify table opened on non-maintained secondary index' ),
      ( code: PXERR_TABLEPRE40;          msg: 'Feature not available for tables older than Paradox 4.0' ),
      ( code: PXERR_TABLEINDEXED;        msg: 'Table is indexed' ),
      ( code: PXERR_TABLENOTINDEXED;     msg: 'Table is not indexed' ),
      ( code: PXERR_SXOUTOFDATE;         msg: 'Secondary index is out of date' ),
      ( code: PXERR_KEYVIOL;             msg: 'Key violation' ),
      ( code: PXERR_NOTLOGGEDIN;         msg: 'Could not login on network' ),
      ( code: PXERR_INVTABLENAME;        msg: 'Table name is invalid' ),
      ( code: PXERR_INUSEBYPDOX35;       msg: 'Directory is in use by Paradox 3.5' ),
      ( code: PXERR_ENDOFTABLE;          msg: 'End of table' ),
      ( code: PXERR_STARTOFTABLE;        msg: 'Start of table' ),
      ( code: PXERR_OUTOFRECHANDLES;     msg: 'No more record handles available' ),
      ( code: PXERR_INVRECHANDLE;        msg: 'Invalid record handle' ),
      ( code: PXERR_TABLEEMPTY;          msg: 'Operation on empty table' ),
      ( code: PXERR_INVLOCKCODE;         msg: 'Invalid lock code' ),
      ( code: PXERR_NONETINIT;           msg: 'Engine not initialized with PXNetInit' ),
      ( code: PXERR_INVFILENAME;         msg: 'Invalid file name' ),
      ( code: PXERR_INVUNLOCK;           msg: 'Invalid unlock' ),
      ( code: PXERR_INVLOCKHANDLE;       msg: 'Invalid lock handle' ),
      ( code: PXERR_OUTOFLOCKHANDLES;    msg: 'Too many locks on table' ),
      ( code: PXERR_INVSORTORDER;        msg: 'Invalid sort order table' ),
      ( code: PXERR_INVNETTYPE;          msg: 'Invalid net type' ),
      ( code: PXERR_INVDIRNAME;          msg: 'Invalid directory name' ),
      ( code: PXERR_BLOBMODE;            msg: 'Operation N/A for Blob open mode' ),
      ( code: PXERR_BLOBOPEN;            msg: 'Blob already open' ),
      ( code: PXERR_BLOBINVOFFSET;       msg: 'Invalid offset into Blob' ),
      ( code: PXERR_BLOBINVSIZE;         msg: 'Invalid size for Blob' ),
      ( code: PXERR_BLOBMODIFIED;        msg: 'Another user modified Blob' ),
      ( code: PXERR_BLOBCORRUPT;         msg: 'Blob file corrupted' ),
      ( code: PXERR_BLOBNOINDEX;         msg: 'Cannot index on a Blob' ),
      ( code: PXERR_BLOBINVHANDLE;       msg: 'Invalid Blob handle' ),
      ( code: PXERR_BLOBNOSEARCH;        msg: 'Cannot search on a Blob field' ),
      ( code: PXERR_CANTUPGRADE;         msg: 'Table cannot be upgraded' ),
      ( code: PXERR_LOCKTIMEOUT;         msg: 'Timed out trying to get a lock' )
{$ifdef WINDOWS}
      ,
      ( code: PXERR_TOOMANYCLIENTS;      msg: 'Too many clients for the Engine DLL' ),
      ( code: PXERR_EXCEEDSCONFIGLIMITS; msg: 'Exceeds limits specified in WIN.INI' ),
      ( code: PXERR_CANTREMAPFILEHANDLE; msg: 'No more slots for file handle remapping' ),
      ( code: PXERR_CANTSHAREPDOXNET;    msg: 'Can''t share Paradox net file -- is SHARE.EXE loaded?' ),
      ( code: PXERR_WINDOWSREALMODE;     msg: 'Can''t run Engine in Windows real mode' )
{$endif}
         );



(* internal error >200 *)
{$ifdef WINDOWS}
const
  IntErrorStr : PChar = 'Internal error no: ';
  Undef : PChar    = 'Undefined errorcode';

var
  IntErrorMsg : array [0..80] of Char;
{$else}
const
  IntError : String = 'Internal error no: ';
  Undef : String    = 'Undefined errorcode';
{$endif}




(* returns the error message string for the given error code *)

{$ifdef WINDOWS}
function PXErrMsg(ErrCode:Integer): PChar;
{$else}
function PXErrMsg(ErrCode:Integer): String;
{$endif}
var
  I: Integer;
  Fail: Boolean;
{$ifdef WINDOWS}
  Msg: PChar;
  PMsg : String;
{$else}
  Msg: String;
{$endif}

begin
  I := 1;
  Fail := False;

  while ( I <= KnownMsgSize ) and ( not Fail ) do
  begin
    if( KnownMessages[I].code = ErrCode ) then
    begin
      Msg := KnownMessages[I].msg;  { Set error string to Msg }
      Fail := True;                 { Fail out of while..do loop }
    end;
    Inc( I );
  end;

  if ( not Fail ) then              { No error message was found }
  begin
    if ( ErrCode>=200 ) and ( ErrCode<=250 ) then
    begin                           { It is an internal error }
{$ifdef WINDOWS}
      { Initialize IntErrorMsg to IntErrorStr and Msg to end of the string. }
      Msg := StrECopy( IntErrorMsg, IntErrorStr );
      { Convert error code to a Pascal string. }
      Str( ErrCode, PMsg );
      { Convert Pascal string to PChar and concatenate to end of IntErrorMsg. }
      StrPCopy( Msg,  PMsg );
      { Assign final string to Msg. }
      Msg := IntErrorMsg;
{$else}
      Str( ErrCode, Msg );
      Msg := IntError + Msg;
{$endif}
    end
    else                            { It is undefined }
      Msg := Undef;
  end;

{$ifdef WINDOWS}
   OemToAnsi( Msg, Msg );
{$endif}

   PXErrMsg := Msg;
end;


{$ifndef WINDOWS}                { DOS-only error functions }
procedure Error( Msg: String);
begin
  Writeln('Engine error: ', Msg);
  Halt;
end;


procedure PX(errCode:Integer);
begin
  if errCode <> 0 then
    Error(PXErrMsg(errCode));
end;
{$endif}


end.