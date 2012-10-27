PROGRAM Commands;

USES
  Objects,
  DosTools, Strings,
  Dos;

VAR
  Files: PStrCollection;

PROCEDURE FindFiles(Path: STRING);
  VAR
    FileList: TFileList;
    I: INTEGER;
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
  BEGIN
    WITH FileList DO BEGIN
      Init(Path, AnyFile, False);
      FOR I := 0 TO Count-1 DO BEGIN
        FSplit(Next, Dir, Name, Ext);
        Files^.Insert(New(PStr, Init(Name)));
      END;
      Done;
    END;
  END;

PROCEDURE ExecutableFiles(Path: STRING);
  BEGIN
    IF (Path[Length(Path)] <> '\') AND (Length(Path) > 0)
     THEN Path := Path +'\';
    FindFiles(Path+'*.COM');
    FindFiles(Path+'*.EXE');
    FindFiles(Path+'*.BAT');
  END;

PROCEDURE MainProgram;
  VAR
    SearchDirs: STRING;
    I: INTEGER;
  BEGIN
    Files := New(PStrCollection, Init(20,1));
    SearchDirs := GetEnv('Path');
    IF SearchDirs = ''
     THEN SearchDirs := ';'
     ELSE IF SearchDirs[Length(SearchDirs)] <> ';'
      THEN SearchDirs := SearchDirs+';';
    REPEAT
      IF Pos(';',SearchDirs) = 0
       THEN ExecutableFiles('')
       ELSE ExecutableFiles(Copy(SearchDirs,1,Pos(';', SearchDirs)-1));
      Delete(SearchDirs, 1, Pos(';',SearchDirs));
    UNTIL SearchDirs = '';
    FOR I := 0 TO Files^.Count-1 DO BEGIN
     Write(PStr(Files^.At(I))^.GetStr);
     Write('':10-Length(PStr(Files^.At(I))^.GetStr));
    END;
    Writeln;
    Writeln;
  END;

BEGIN
  MainProgram;
END.