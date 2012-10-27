{**********************************************}
{                                              }
{    "šberlauf"-Collection, die nur            }
{     eine gewisse Zahl von Eintr„gen          }
{     speichert. šberschreitet man die         }
{     Maximalzahl, werden die „ltesten         }
{     Eintr„ge aus der Collection entfernt.    }
{                                              }
{**********************************************}

UNIT RollCol;

INTERFACE

USES
  Objects;

TYPE
  PRollOverCollection = ^TRollOverCollection;
  TRollOverCollection = OBJECT(TCollection)
    Max: Integer;
    CONSTRUCTOR Init(theMaxValue: Integer);
    PROCEDURE Insert(Item: Pointer); VIRTUAL;
  END;

IMPLEMENTATION

CONSTRUCTOR TRollOverCollection.Init(theMaxValue: Integer);
BEGIN
  Max := theMaxValue;
  TCollection.Init(theMaxValue,0);
END;

PROCEDURE TRollOverCollection.Insert(Item: Pointer);
BEGIN
  IF Count = Limit THEN Free(At(0));
  TCollection.Insert(Item);
END;

END.
