{*******************************************************}
{                                                       }
{                                                       }
{*******************************************************}

UNIT VtType;

INTERFACE

USES Objects;

CONST

  cDNamLen     = 28;
  cDNumLen     = 28;
  cDListLen    = 62;
  cDialDirName = 'VTERM.DIR';
  DialPrefix   = 'ATDP';

  cMyBase           = 3000;
  cmOpenLogFile     = cMyBase+1;
  cmTile            = cMyBase+2;
  cmLineSet         = cMyBase+3;
  cmTerminalSet     = cMyBase+4;
  cmSetLocalEcho    = cMyBase+5;
  cmDialDlg         = cMyBase+7;
  cmDoDial          = cMyBase+8;
  cmViewFile        = cMyBase+9;
  cmDoHangUp        = cMyBase+10;
  cmCloseLogFile    = cMyBase+11;
  cmLogThisLine     = cMyBase+12;
  cmFlushLog        = cMyBase+13;

TYPE
  TLineSettings = RECORD
    Port     : Word;
    Baud     : Word;
    DataBits : Word;
    Parity   : Word;
    StopBits : Word;
  END;

  PDialEntry = ^TDialEntry;
  TDialEntry = OBJECT(TObject)
    Name: STRING[cDNamLen];
    Number: STRING[cDNumLen];
    LineSettings: TLineSettings;
    LocalEcho: Word;
    CONSTRUCTOR Load(VAR S: TStream);
    PROCEDURE Store(VAR S: TStream);
  END;

IMPLEMENTATION

CONST
  RDialEntry : TStreamRec = (
    ObjType : 1000;
    VmtLink : Ofs(TypeOf(TDialEntry)^);
    Load    : @TDialEntry.Load;
    Store   : @TDialEntry.Store);


{ TDialEntry }

CONSTRUCTOR TDialEntry.Load(VAR S: TStream);
VAR
  P: PString;
BEGIN
  TObject.Init;
  P := S.ReadStr;
  IF P <> NIL THEN Name := P^
  ELSE Name := '';
  P := S.ReadStr;
  IF P <> NIL THEN Number := P^
  ELSE Number := '';
  S.Read(LineSettings, SizeOf(LineSettings));
  S.Read(LocalEcho, SizeOf(LocalEcho));
END;

PROCEDURE TDialEntry.Store(VAR S : TStream);
BEGIN
  S.WriteStr(@Name);
  S.WriteStr(@Number);
  S.Write(LineSettings, SizeOf(LineSettings));
  S.Write(LocalEcho, SizeOf(LocalEcho));
END;

BEGIN
  RegisterType(RDialEntry);
END.