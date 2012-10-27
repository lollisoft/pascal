{$A+,B-,D+,E+,F-,G-,I+,L+,N-,O-,R-,S+,V-,X-}
{$M 16384,0,655360}
USES Dos;

{**** Date, Time and Version stamp. (c) 1990 by EMS. For Pascal! ****}
CONST Version =       4;    { 18.11.1990 16:27 }

PROCEDURE MainProgram;
  CONST LegalChars = '_ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  VAR F, F1: TEXT;
      Counter: WORD;
      Dir, Name, Ext,
      Line,
      EndLine: STRING;
  BEGIN
    Assign(F, ParamStr(1));
    FSplit(ParamStr(1), Dir,Name,Ext);
    Assign(F1, Dir+Name+'.INC');
    {$I-}Reset(F);{$I+}
    IF IOResult <> 0 THEN BEGIN
      Assign(F, Dir+Name+'.LOG');
      Reset(F);
    END;
    Rewrite(F1);
    EndLine := '    HNames';
    Writeln(F1,'FUNCTION Names: PNameRec;');
    Writeln(F1,'  VAR HNames: PNameRec;');
    Writeln(F1,'  BEGIN');
    Writeln(F1,'    HNames := Nil;');
    Writeln(F1,'    HNames := ');
    Counter := 1;
    WHILE NOT EOF(F) DO BEGIN
      ReadLn(F, Line);
      IF Length(Line) > 0 THEN BEGIN
        IF Counter MOD 30 = 0 THEN BEGIN
          Writeln(F1, Endline,';');
          EndLine := '    HNames';
          Writeln(F1,'    HNames :=');
        END;
        WHILE Pos(Line[1],LegalChars) = 0 DO Delete(Line,1,1);
        WHILE Line[Length(Line)] = ' ' DO Delete(Line,Length(Line),1);
        Write(F1, '      NewNameRec(Ofs(TypeOf(', Line, ')^), SizeOf(');
        Write(F1, Line, '),');
        Writeln(F1, ' ''', Line, ''',');
        EndLine := EndLine + ')';
        Inc(Counter);
      END;
    END;
    Writeln(F1, EndLine,';');
    Writeln(F1, '   Names := HNames;');
    Writeln(F1, ' END;');
    Close(F);
    Close(F1);
  END;

BEGIN
  Writeln('Source-Generator by EMS   Version ', Version);
  IF ParamCount <> 1
   THEN Writeln('SYNTAX: NameReg <logfile>')
   ELSE MainProgram;
END.