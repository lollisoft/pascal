PROGRAM FileFind;

USES
  Dos, DosTools;

VAR
  I: INTEGER;
  FileList: TFileList;

BEGIN
  WITH FileList DO BEGIN
    IF ParamCount = 0
     THEN Init('*.*', AnyFile, True)
     ELSE IF Pos('.',ParamStr(1)) = 0
      THEN Init(ParamStr(1)+'.*', AnyFile, True)
      ELSE Init(ParamStr(1), AnyFile, True);
    FOR I := 0 TO Count-1 DO Writeln(Next);
  END;
END.