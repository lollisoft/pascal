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
    10 : GetHintStr := 'Men� f�r Systemdienste';
    11 : GetHintStr := 'Verlassen des Programmes';
    20 : GetHintStr := 'Men� f�r Dateidienste';
    21 : GetHintStr := 'Laden einer Datei';
    22 : GetHintStr := 'Erstellen einer neuen Datei';
    30 : GetHintStr := 'Men� mit Editierfunktionen';
    31 : GetHintStr := 'Zeile wiederherstellen';
    32 : GetHintStr := 'L�schen des markierten Blocks';
    40 : GetHintStr := 'Men� zum Aufrufen des Compilers';
    41 : GetHintStr := '�bersetzen';
    42 : GetHintStr := 'Nur �bersetzen der ge�nderten Dateien';
    43 : GetHintStr := 'Neu-�bersetzen aller Dateien';
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