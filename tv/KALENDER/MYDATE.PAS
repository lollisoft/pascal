UNIT MyDate;

INTERFACE

CONST
  CEMonthNames : ARRAY [1..12] OF String =
                ('January', 'February', 'March', 'April', 'May', 'June',
                 'July', 'August', 'September', 'October', 'November',
                 'December');
  CEDayNames   : ARRAY [0..6] OF String =
                ('Monday', 'Tuesday', 'Wednesday','Thursday','Friday',
                 'Saturday','Sunday');
  CEDayNamesSh : ARRAY [0..6] OF String[4] =
                ('Mon ','Tue ','Wed ','Thu ','Fri ','Sat ','Sun ');
  CDMonthNames : ARRAY [1..12] OF String =
                ('Januar', 'Februar', 'M„rz', 'April', 'Mai', 'Juni',
                 'Juli', 'August', 'September', 'Oktober', 'November',
                 'Dezember');
  CDDayNames   : ARRAY [0..6] OF String =
                ('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag',
                 'Samstag', 'Sonntag');
  CDDayNamesSh : ARRAY [0..6] OF String[4] =
                ('Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa','So');

TYPE
  TDate    = OBJECT
                CONSTRUCTOR MakeMyDay;
                (* initialize object with today's date                   *)
                CONSTRUCTOR Init (d, m, y : WORD);
                (* initialize object with value of starting date         *)
                FUNCTION GetDay   : WORD;
                FUNCTION GetMonth : WORD;
                FUNCTION GetYear  : WORD;
                FUNCTION GetWeekDay : BYTE;
		(* returns byte value for weekday offset                 *)
                (* 0 = monday, 1 = tuesday, ... , 6 = sunday             *)
                PROCEDURE IncDay;
                (* increment day                                         *)
             PRIVATE
                Day, Month, Year : WORD;
             END;

IMPLEMENTATION

USES DOS;

CONST
  MonthLength  : ARRAY [1..12] OF BYTE =
               (* Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec        *)
                 (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

CONSTRUCTOR TDate.MakeMyDay;
VAR
  Dummy : WORD;
BEGIN
  DOS.GetDate (Year, Month, Day, Dummy);
END;

CONSTRUCTOR TDate.Init (d, m, y : WORD);
BEGIN
  Day   := d;
  Month := m;
  Year  := y;
END;

FUNCTION TDate.GetDay   : WORD;
BEGIN
  GetDay := Day;
END;

FUNCTION TDate.GetMonth : WORD;
BEGIN
  GetMonth := Month;
END;

FUNCTION TDate.GetYear  : WORD;
BEGIN
  GetYear := Year;
END;

FUNCTION TDate.GetWeekDay : BYTE;
VAR
  j, d, m, y : WORD;
BEGIN
  d := Day;
  m := Month;
  y := Year;
  IF ((Y = 1900) AND (M < 3))
  THEN IF (M = 1) THEN J := PRED(D)        (* January 1900          *)
                      ELSE J := D + 30     (* February 1900         *)
  ELSE BEGIN
    IF (M > 2) THEN DEC(M,3)
                   ELSE BEGIN
                          INC(M,9);
                          DEC(Y);
                        END;
    DEC(Y,1900);
    J := (1461 * LONGINT(Y) DIV 4) + ((153 * M + 2) DIV 5) + D + 58;
  END;
  GetWeekDay := J MOD 7;
END;

FUNCTION IsLeapYear (y : WORD): BOOLEAN;
BEGIN
  IsLeapYear := (Y MOD 4 = 0) AND (Y MOD 100 <> 0) OR (Y MOD 400 = 0);
END;

PROCEDURE TDate.IncDay;
BEGIN
  IF (MonthLength[Month] > Day)
    THEN INC(Day)
    ELSE IF ((Month = 2) AND (Day = 28) AND IsLeapYear(Year))
	   THEN (* February 29th is okay *)
                INC(Day)
           ELSE BEGIN
	          Day := 1;
	          IF (Month <> 12)
	            THEN INC(Month)
                    ELSE BEGIN
                           INC(Year);
                           Month := 1;
                         END;
                END;
END;

END.