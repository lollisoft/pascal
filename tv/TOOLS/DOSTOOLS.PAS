UNIT DosTools;

INTERFACE

USES
  Objects,
  Dos;

TYPE
  NameExtStr = STRING[8+1+3];
  PFileList = ^TFileList;
  TFileList = OBJECT (TStringCollection)
    CONSTRUCTOR Init (Path: PathStr; Attr: BYTE; SubDirs: BOOLEAN);
    FUNCTION First: STRING;
    FUNCTION Next: STRING;
  PRIVATE
    Counter: INTEGER;
  END;

FUNCTION LoadPath (Path: PathStr): DirStr;

FUNCTION FileName (Path: PathStr): NameExtStr;

IMPLEMENTATION

CONSTRUCTOR TFileList.Init (Path: PathStr; Attr: BYTE; SubDirs: BOOLEAN);
  PROCEDURE BuildList(Path: PathStr);
    VAR
      Search: SearchRec;
      Dir: DirStr;
      Name: NameStr;
      Ext: ExtStr;
    FUNCTION ValidDir: BOOLEAN;
      BEGIN
        ValidDir := (Search.Name <> '.') AND
                    (Search.Name <> '..') AND
                    (Search.Attr = Directory);
      END;
    BEGIN
      Attr := Attr AND NOT Directory AND NOT VolumeId;
      FSplit(FExpand(Path), Dir, Name, Ext);
      FindFirst(Path, Attr, Search);
      WHILE DosError = 0 DO BEGIN
        Insert(NewStr(Dir+Search.Name));
        FindNext(Search);
      END;
      IF SubDirs THEN BEGIN
        FindFirst(Dir+'*.*', Directory, Search);
        WHILE DosError = 0 DO BEGIN
          IF ValidDir THEN BuildList(Dir+Search.Name+'\'+Name+Ext);
          FindNext(Search);
        END;
      END;
    END;
  BEGIN
    TStringCollection.Init(10,10);
    BuildList(Path);
  END;

FUNCTION TFileList.First: STRING;
  BEGIN
    Counter := 0;
    First := Next;
  END;

FUNCTION TFileList.Next: STRING;
  BEGIN
    IF Counter >= Count
     THEN Next := ''
     ELSE BEGIN
      IF At(Counter) = Nil THEN Next := ''
                           ELSE Next := PString(At(Counter))^;
      Inc(Counter);
    END;
  END;

FUNCTION LoadPath (Path: PathStr): DirStr;
  VAR
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
  BEGIN
    FSplit(FExpand(Path), Dir, Name, Ext);
    LoadPath := Dir;
  END;

FUNCTION FileName (Path: PathStr): NameExtStr;
  VAR
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
  BEGIN
    FSplit(Path, Dir, Name, Ext);
    FileName := Name+Ext;
  END;

END.