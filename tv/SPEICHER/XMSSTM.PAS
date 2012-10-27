UNIT XMSSTM;

INTERFACE

USES Objects, Xms;

TYPE
  PXmsStream = ^TXmsStream;
  TXmsStream = OBJECT (TStream)
    Handle: Word;
    Size: Longint;
    Position: Longint;
    MaxSize: WORD;
    CONSTRUCTOR Init;
    DESTRUCTOR Done; virtual;
    FUNCTION TestError(ErrCode: INTEGER; Info: WORD): BOOLEAN;
    FUNCTION GetPos: Longint; virtual;
    FUNCTION GetSize: Longint; virtual;
    PROCEDURE Read(var Buf; Count: Word); virtual;
    PROCEDURE Seek(Pos: Longint); virtual;
    PROCEDURE Truncate; virtual;
    PROCEDURE Write(var Buf; Count: Word); virtual;
  END;

IMPLEMENTATION

FUNCTION NextEven(Value: LONGINT): LONGINT;
  BEGIN
    NextEven := (Value+1) AND $FFFFFFFE;
  END;

CONSTRUCTOR TXmsStream.Init;
  VAR ToAllocate : WORD;
  BEGIN
    IF Result <> 0
     THEN Status := stInitError
     ELSE BEGIN
      Handle := ReserveMem(1);
      IF Result <> 0 THEN Status := stInitError
                     ELSE Status := stOk;
      MaxSize := 1;
      Position := 0;
      Size := 0;
    END;
    ErrorInfo := INTEGER(Result);
  END;

DESTRUCTOR TXmsStream.Done;
  BEGIN
    FreeMem (Handle);
    TStream.Done;
  END;

FUNCTION TXmsStream.GetPos: Longint;
  BEGIN
    GetPos := Position;
  END;

FUNCTION TXmsStream.GetSize: Longint;
  BEGIN
    GetSize := Size;
  END;

FUNCTION TXMSStream.TestError(ErrCode: INTEGER; Info: WORD): BOOLEAN;
  BEGIN
    IF Result <> 0
     THEN BEGIN
      Status := ErrCode;
      ErrorInfo := Info;
     END
     ELSE BEGIN
      Status := stOk;
      ErrorInfo := Info;
    END;
    TestError := Result <> 0;
  END;

PROCEDURE TXmsStream.Read(var Buf; Count: Word);
  BEGIN
    IF TestError(stReadError, Result) THEN Exit;
    IF Count = 0 THEN Exit;
    MoveFromXMS(Count, Handle, Position, @Buf);
    IF TestError(stReadError, Result) THEN Exit;
    INC(Position, Count);
    IF Size < Position THEN Size := Position;
  END;

PROCEDURE TXmsStream.Seek(Pos: Longint);
  BEGIN
    IF (Pos <= Size) THEN Position := Pos;
  END;

PROCEDURE TXmsStream.Truncate;
  BEGIN
    Size := Position;
    (* Hier bleibt zu berlegen, ob und unter welchen Bedingungen ein        *)
    (* ReAllocate durchgefhrt werden soll.                                  *)
  END;

PROCEDURE TXmsStream.Write(var Buf; Count: Word);
  BEGIN
    IF TestError(stWriteError, Result) THEN Exit;
    IF Count = 0 THEN Exit;
    IF Position + ((Count+1) AND $FFFE) > LONGINT(MaxSize) * 1024 THEN BEGIN
      ResizeMem(((Position + Count - 1) DIV 1024) + 1, Handle);
      IF TestError(stWriteError, Result) THEN Exit;
      MaxSize := ((Position + Count - 1) DIV 1024) + 1;
    END;
    MoveToXMS(Count, Handle, @Buf, Position);
    IF TestError(stWriteError, Result) THEN Exit;
    INC(Position, Count);
    IF Size < Position THEN Size := Position;
  END;

END.

