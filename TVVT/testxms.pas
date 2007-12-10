PROGRAM ScreenEMS;

USES Crt, XMS;

VAR
  XAvail: Word;
  RMem0,
  RMem  : Word;

BEGIN
  If Result = 0 then Begin
    Writeln('Es ist XMS - Speicher vorhanden.');
    XAvail := XMS.MaxAvail;
    Writeln('Vorhandener Speicher: ', XAvail, ' kB.');
    RMem := $A;
    RMem0 := 0;
    RMem0 := XMS.ReserveMem(RMem);
    XAvail := XMS.MemAvail;
    Writeln('Adressevon RMem0    : ', RMem0);
    Writeln('VerfÅgbarer Speicher: ', XAvail, ' kB.');
  End
  Else
  Writeln('Es ist kein XMS - Speicher da!');
END.