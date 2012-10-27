{********************************************}
{                                            }
{      Event-Collection zum Abspeichern      }
{      von Ereignissen in Turbo Vision       }
{                                            }
{********************************************}

UNIT EvCol;

INTERFACE

USES Objects, RollCol, Drivers;

TYPE
  PEventCollection = ^TEventCollection;
  TEventCollection = OBJECT(TRollOverCollection)
    PROCEDURE InsertEvent(Event: TEvent);
    FUNCTION GetItem(VAR S: TStream): Pointer; VIRTUAL;
    PROCEDURE PutItem(VAR S: TStream; Item: Pointer); VIRTUAL;
    PROCEDURE FreeItem(Item: Pointer); VIRTUAL;
    PROCEDURE FetchEvent(VAR Event: TEvent);
  END;

IMPLEMENTATION

PROCEDURE TEventCollection.FetchEvent(VAR Event: TEvent);
BEGIN
  IF Count > 0 THEN BEGIN
    Event := PEvent(At(0))^;
    AtFree(0);
  END;
END;

PROCEDURE TEventCollection.InsertEvent(Event: TEvent);
VAR E: PEvent;
BEGIN
  E := New(PEvent);
  E^ := Event;
  Insert(E);
END;

PROCEDURE TEventCollection.FreeItem(Item: Pointer);
BEGIN
  Dispose(PEvent(Item));
END;

FUNCTION TEventCollection.GetItem(VAR S: TStream): Pointer;
VAR P: PEvent;
BEGIN
  P := New(PEvent);
  S.Read(P^,SizeOf(TEvent));
  GetItem := P;
END;

PROCEDURE TEventCollection.PutItem(VAR S: TStream; Item: Pointer);
BEGIN
  S.Write(PEvent(Item)^, SizeOf(TEvent));
END;

END.
