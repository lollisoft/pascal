UNIT XMScreen;

INTERFACE                 
  PROCEDURE SaveScreenXMS;
                 
  PROCEDURE RestoreScreenXMS;
                 
IMPLEMENTATION

USES Xms;
                 
VAR Screen:  WORD ABSOLUTE $B800:0;
    Handle:  WORD;
    isSaved: BOOLEAN;

PROCEDURE SaveScreenXMS;
  BEGIN
   IF NOT isSaved THEN BEGIN
     Handle := ReserveMem(4);
     MoveToXMS(4000, Handle, @Screen, 0);
     isSaved := True;
   END;
  END;

PROCEDURE RestoreScreenXMS;
  BEGIN
    IF isSaved THEN BEGIN
      MoveFromXMS(4000, Handle, 0, @Screen);
      isSaved := False;
      FreeMem(Handle);
    END;
  END;

BEGIN
  IF Result <> 0 THEN RunError(0);
  isSaved := False;
END.

