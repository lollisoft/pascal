UNIT HintStat;

{$F+,O+,S-}

INTERFACE

USES Objects, Drivers, Menus;

TYPE
  PHintStatusLine = ^THintStatusLine;
  THintStatusLine = OBJECT (TStatusLine)
                     MessageDisplayed : BOOLEAN;
                     FUNCTION Hint (Ctx : WORD): String; VIRTUAL;
                     PROCEDURE Update; VIRTUAL;
                  END;

IMPLEMENTATION

FUNCTION GetHintStr (HintNo : WORD): String;
BEGIN
  CASE HintNo OF
    00 : GetHintStr := 'No hint available';
    09 : GetHintStr := 'Hier, im rechten Teil der Statuszeile stehen die Hinweise';
    10 : GetHintStr := 'MenÅ fÅr Systemdienste';
    11 : GetHintStr := 'Verlassen des Programmes';
    20 : GetHintStr := 'MenÅ fÅr Dateidienste';
    21 : GetHintStr := 'Laden einer Datei';
    22 : GetHintStr := 'Erstellen einer neuen Datei';
    30 : GetHintStr := 'MenÅ mit Editierfunktionen';
    31 : GetHintStr := 'Zeile wiederherstellen';
    32 : GetHintStr := 'Lîschen des markierten Blocks';
    40 : GetHintStr := 'MenÅ zum Aufrufen des Compilers';
    41 : GetHintStr := 'öbersetzen';
    42 : GetHintStr := 'Nur öbersetzen der geÑnderten Dateien';
    43 : GetHintStr := 'Neu-öbersetzen aller Dateien';
    ELSE    GetHintStr := 'Unknown hint number';
  END;
END;

FUNCTION THintStatusLine.Hint (Ctx : WORD): String;
BEGIN
  Hint := GetHintStr (Ctx);
END;

PROCEDURE THintStatusLine.Update;
BEGIN
  IF MessageDisplayed THEN
  BEGIN
    DrawView;
    MessageDisplayed := FALSE;
  END ELSE
    TStatusLine.Update;
END;

END.