UNIT MskBoxes;

INTERFACE

USES Views, Drivers, Dialogs, Objects;


TYPE
  PMaskField = ^TMaskField;
  TMaskField = ARRAY[1..16] OF PString;
  PMaskBox   = ^TMaskBox;
  TMaskBox   = OBJECT (TListBox)
    Mask: WORD;
    MaskField: PMaskField;
    CONSTRUCTOR Init (VAR R: TRect; ScrollBar: PScrollBar;
                      AMaskField: PMaskField);
    FUNCTION GetText(Item: Integer; MaxLen: Integer): String; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

CONST
  MUndefined:STRING[20]                 = 'undefined';
  MsfVisible:STRING[20]                 = 'sfVisible';
  MsfCursorVis:STRING[20]               = 'sfCursorVis';
  MsfCursorIns:STRING[20]               = 'sfCursorIns';
  MsfShadow:STRING[20]                  = 'sfShadow';
  MsfActive:STRING[20]                  = 'sfActive';
  MsfSelected:STRING[20]                = 'sfSelected';
  MsfFocused:STRING[20]                 = 'sfFocused';
  MsfDragging:STRING[20]                = 'sfDragging';
  MsfDisabled:STRING[20]                = 'sfDisabled';
  MsfModal:STRING[20]                   = 'sfModal';
  MsfDefault:STRING[20]                 = 'sfDefault';
  MsfExposed:STRING[20]                 = 'sfExposed';
  sfMaskField: TMaskField =
    ( @MsfVisible, @MsfActive, @MsfDisabled, @Mundefined,
      @MsfCursorVis, @MsfSelected, @MsfModal, @Mundefined,
      @MsfCursorIns, @MsfFocused, @MsfDefault, @Mundefined,
      @MsfShadow, @MsfDragging, @MsfExposed, @Mundefined);
  MofSelectable:STRING[20]              = 'ofSelectable';
  MofTopSelect:STRING[20]               = 'ofTopSelect';
  MofFirstClick:STRING[20]              = 'ofFirstClick';
  MofFramed:STRING[20]                  = 'ofFramed';
  MofPreProcess:STRING[20]              = 'ofPreProcess';
  MofPostProcess:STRING[20]             = 'ofPostProcess';
  MofBuffered:STRING[20]                = 'ofBuffered';
  MofTileable:STRING[20]                = 'ofTileable';
  MofCenterX:STRING[20]                 = 'ofCenterX';
  MofCenterY:STRING[20]                 = 'ofCenterY';
  ofMaskField: TMaskField =
    ( @MofSelectable, @MofPreProcess, @MofCenterX, @Mundefined,
      @MofTopSelect, @MofPostProcess, @MofCenterY, @Mundefined,
      @MofFirstClick, @MofBuffered, @Mundefined, @Mundefined,
      @MofFramed, @MofTileable, @Mundefined, @Mundefined);
  MevMouseDown:STRING[20]               = 'evMouseDown';
  MevMouseUp:STRING[20]                 = 'evMouseUp';
  MevMouseMove:STRING[20]               = 'evMouseMove';
  MevMouseAuto:STRING[20]               = 'evMouseAuto';
  MevKeyDown:STRING[20]                 = 'evKeyDown';
  MevCommand:STRING[20]                 = 'evCommand';
  MevBroadcast:STRING[20]               = 'evBroadcast';
  evMaskField: TMaskField =
    ( @MevMouseDown, @MevKeyDown, @Mundefined, @Mundefined,
      @MevMouseUp, @MevCommand, @Mundefined, @Mundefined,
      @MevMouseMove, @MevBroadcast, @Mundefined, @Mundefined,
      @MevMouseAuto, @Mundefined, @Mundefined, @Mundefined);
  cmNewMask = $AFFE;

IMPLEMENTATION

TYPE
  PStrAddrCollection = ^TStrAddrCollection;
  TStrAddrCollection = OBJECT (TStringCollection)
    PROCEDURE FreeItem (Item: POINTER); VIRTUAL;
  END;

PROCEDURE TStrAddrCollection.FreeItem (Item: POINTER);
  BEGIN
  END;

CONSTRUCTOR TMaskBox.Init (VAR R: TRect; ScrollBar: PScrollBar;
                           AMaskField: PMaskField);
  BEGIN
    TListBox.Init(R, 1, ScrollBar);
    MaskField := AMaskField;
    NewList(New(PStrAddrCollection, Init(20,1)));
  END;

FUNCTION TMaskBox.GetText(Item: Integer; MaxLen: Integer): String;
BEGIN
  GetText := PString(List^.At(Item))^;
END;

PROCEDURE TMaskBox.HandleEvent(VAR Event: TEvent);
  VAR I: INTEGER;
      P: PStrAddrCollection;
  BEGIN
    TListBox.HandleEvent(Event);
    IF (Event.What = evBroadcast) AND (Event.Command = cmNewMask)
     THEN BEGIN
      New(P, Init(20,1));
      FOR I := 1 TO 16 DO BEGIN
       IF Odd(Mask) THEN P^.Insert(MaskField^[I]);
       Mask := Mask SHR 1;
      END;
      NewList(P);
    END;
  END;

END.