UNIT XDate;

INTERFACE

USES MyDate;

TYPE
  PXDate    = ^TXDate;
  TXDate    = OBJECT (TDate)
    PROCEDURE ChangeMonth (Value: INTEGER);
  END;

IMPLEMENTATION

PROCEDURE TXDate.ChangeMonth (Value : INTEGER);
BEGIN
  IF (Value = 1) THEN
    IF (Month = 12) THEN TXDate.Init (Day, 1, Year+1)
                    ELSE TXDate.Init (Day, Month+1, Year);
  IF (Value = -1) THEN
    IF (Month = 1) THEN TXDate.Init (Day, 12, Year-1)
                   ELSE TXDate.Init (Day, Month-1, Year);
END;


END.