PROGRAM EMSTest;

USES Crt, EMS;

CONST
  PagesToAllocate  = 10;

VAR
  AvailablePages, TotalPages : WORD;
  MyEMMHandle, LogicalPage   : WORD;
  PhysicalPage               : BYTE;
  VersionString, PagesString : String;
  PageFrameBaseAddr          : WORD;
  Offset                     : WORD;
  Verify                     : BOOLEAN;

  FUNCTION GetHexChar(Number: Word): Char;
  BEGIN
    IF (Number < 10)
      THEN GetHexChar := CHAR(Number+48)
      ELSE GetHexChar := CHAR(Number+55);
  END;

  FUNCTION WordToHexString (Number: Word): String;
  VAR
    OutString: String;
  BEGIN
    OutString :='';
    OutString := GetHexChar( (Number SHR 1) DIV 2048);
    Number :=( ((Number SHR 1) MOD 2048) SHL 1) + (Number AND 1) ;
    OutString := OutString + GetHexChar(Number DIV 256);
    Number := Number MOD 256;
    OutString := OutString + GetHexChar(Number DIV 16);
    Number := Number MOD 16;
    OutString := OutString + GetHexChar(Number);
    WordToHexString:= OutString + 'h';
  END;

BEGIN
  ClrScr;
  WriteLn ('**************************** EMSTest ****************************');
  WriteLn ('*                                                               *');
  IF NOT TestEMMDriver THEN
  BEGIN
    WriteLn ('*     The LIM Expanded Memory Manager is not installed !        *');
    ReadLn;
    Halt(1);
  END;

  VersionString := GetVersion;
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('*       Error trying to get the EMS version number:               *');
    WriteLn (EMMErrorMsg(EMMError));
  END
  ELSE
    WriteLn ('*             LIM Expanded Memory Manager, version ', VersionString,'          *');
  WriteLn ('*                                                               *');
  WriteLn ('*****************************************************************');

  EMMPageCount (AvailablePages, TotalPages);
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('Error trying to determine the number of EMS pages available.');
    WriteLn (EMMErrorMsg(EMMError));
  END;
  WriteLn ('  ',TotalPages:4,' expanded memory pages are present.');
  WriteLn ('  ',AvailablePages:4,' of those pages are available for your usage.');
  WriteLn;

  IF (PagesToAllocate > AvailablePages) THEN
  BEGIN
    Str(PagesToAllocate, PagesString);
    WriteLn ('  We need '+PagesString+' EMS pages. There are not that many available.');
  END;

  MyEMMHandle := AllocatePages (PagesToAllocate);
  Str(PagesToAllocate, PagesString);
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('  EMS test program failed trying to allocate '+PagesString+
            ' pages for usage. ');
    WriteLn (EMMErrorMsg(EMMError));
  END;
  WriteLn ('  ',PagesToAllocate:4,' EMS page(s) allocated for the EMS test program.');
  WriteLn;

  LogicalPage :=0;
  PhysicalPage:=0;
  EMMPageMap (PhysicalPage, LogicalPage, MyEMMHandle);
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('  Mapping logical pages onto physical pages failed.');
    WriteLn (EMMErrorMsg(EMMError));
  END;
  WriteLn ('  Logical Page ',LogicalPage,
           ' successfully mapped onto Physical Page ', PhysicalPage);
  WriteLn;

  GetPageFrameAddr (PageFrameBaseAddr);
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('  EMS test program unable to get the base Page Frame Address.');
    WriteLn ('  ',EMMErrorMsg(EMMError));
  END;
  WriteLn ('  The base address of the EMS page frame is - ',
          WordToHexString(PageFrameBaseAddr));
  WriteLn;

  { Write a test pattern to expanded memory }
  For Offset:=0 to 16380 DO
    Mem[PageFrameBaseAddr:Offset]:= Offset MOD 256;

  Write ('  Testing EMS memory');

  Offset:=1;
  Verify:=True;
  WHILE (Offset<=16380) and (Verify=True) DO
  BEGIN
    IF (Mem[PageFrameBaseAddr:Offset] <> Offset MOD 256) THEN
      Verify:=False;
    Offset:=Succ(Offset);
  END;

  IF NOT Verify THEN
  BEGIN
    WriteLn ('.');
    WriteLn ('  What was written to EMS memory was not found during '+
             'memory verification test.');
  END
  ELSE BEGIN
    WriteLn ('=> EMS memory test verification successful.');
    WriteLn;
  END;

  ReleasePage (MyEMMHandle);
  IF (EMMError <> 0) THEN
  BEGIN
    WriteLn ('EMS test program was unable to deallocate the EMS pages in use.');
    WriteLn (EMMErrorMsg(EMMError));
  END;
  WriteLn ('  ',PagesToAllocate:4,' page(s) deallocated.');

  WriteLn;
  WriteLn ('**************************** EMSTest ****************************');
  WriteLn ('*                                                               *');
  WriteLn ('*            EMS test program successfully completed.           *');
  WriteLn ('*                                                               *');
  WriteLn ('*****************************************************************');
  ReadLn;
END.