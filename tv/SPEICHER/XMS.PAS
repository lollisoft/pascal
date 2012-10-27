UNIT XMS;

INTERFACE

VAR Result: BYTE;

FUNCTION MaxAvail: WORD;

FUNCTION MemAvail: WORD;

FUNCTION ReserveMem( KB: WORD): WORD;  { Returns XMS-Handle }

PROCEDURE FreeMem ( Handle: WORD);

PROCEDURE MoveFromXMS(Size, Handle: WORD; FromAddress: LONGINT;
                      ToAddress: POINTER);

PROCEDURE MoveToXMS(Size, Handle: WORD; FromAddress: POINTER;
                    ToAddress: LONGINT);

PROCEDURE ResizeMem (NewSize, Handle: WORD);

IMPLEMENTATION

VAR XMMAddr: POINTER;

PROCEDURE MoveMem (ToAddress: POINTER; ToHandle: WORD;
                   FromAddress: POINTER; FromHandle: WORD; Size: LONGINT);
  BEGIN
    ASM
      MOV  AH,$0B
      LEA  SI,Size
      PUSH DS
      POP  ES
      PUSH SS
      POP  DS
      CALL ES:[XMMAddr]
      PUSH ES
      POP  DS
      OR   AX,AX
      JNZ  @@1
      MOV  BYTE PTR Result,BL
    @@1:
    END;
  END;

FUNCTION GetByte(Handle: WORD; FromAddress: LONGINT): BYTE;
  VAR TempBuf: ARRAY[0..1] OF BYTE;
  BEGIN
    MoveMem(@TempBuf, 0, POINTER(FromAddress AND $FFFFFFFE), Handle, 2);
    GetByte := TempBuf[FromAddress AND $00000001];
  END;

PROCEDURE SetByte(Handle: WORD; ToAddress: LONGINT; Value: BYTE);
  VAR TempBuf: ARRAY[0..1] OF BYTE;
  BEGIN
    MoveMem(@TempBuf, 0, POINTER(ToAddress AND $FFFFFFFE), Handle, 2);
    TempBuf[ToAddress AND $00000001] := Value;
    MoveMem(POINTER(ToAddress AND $FFFFFFFE), Handle, @TempBuf, 0, 2);
  END;

FUNCTION MaxAvail: WORD;
  BEGIN
    ASM
      MOV  AH,$08
      CALL [XMMAddr]
      OR   AX,AX
      JNZ  @@1
      MOV  BYTE PTR Result,BL
    @@1:
    END;
  END;

FUNCTION MemAvail: WORD;
  BEGIN
    ASM
      MOV  AH,$08
      CALL [XMMAddr]
      OR   AX,AX
      JZ   @@1
      MOV  AX,DX
      JMP  @@2
    @@1:
      MOV  BYTE PTR Result,BL
    @@2:
    END;
  END;

FUNCTION ReserveMem( KB: WORD): WORD; ASSEMBLER;
  ASM
    MOV  AH,$09
    MOV  DX,WORD PTR KB
    CALL [XMMAddr]
    OR   AX,AX
    JZ   @@1
    MOV  AX,DX
    JMP  @@2
  @@1:
    MOV  BYTE PTR Result,BL
  @@2:
  END;

PROCEDURE FreeMem ( Handle: WORD);
  BEGIN
    ASM
      MOV  AH,$0A
      MOV  DX,WORD PTR Handle
      CALL [XMMAddr]
      OR   AX,AX
      JNZ  @@1
      MOV  BYTE PTR Result,BL
    @@1:
    END;
  END;

PROCEDURE MoveFromXMS(Size, Handle: WORD; FromAddress: LONGINT;
                      ToAddress: POINTER);
  TYPE ByteArr = ARRAY[0..MaxInt] OF BYTE;
       BytePtr = ^ByteArr;
  BEGIN
    IF Size = 0 THEN Exit;
    IF Odd(FromAddress) THEN BEGIN
      BytePtr(ToAddress)^[0] := GetByte(Handle,FromAddress);
      IF Result <> 0 THEN Exit;
      Dec(Size);
      Inc(FromAddress);
      Inc(LONGINT(ToAddress));
    END;
    MoveMem(ToAddress, 0, POINTER(FromAddress), Handle, Size AND $FFFE);
    IF Result <> 0 THEN Exit;
    IF Odd(Size)
     THEN BytePtr(ToAddress)^[Size-1] := GetByte(Handle, FromAddress+Size-1);
    IF Result <> 0 THEN Exit;
  END;

PROCEDURE MoveToXMS(Size,Handle: WORD; FromAddress:POINTER;
                    ToAddress: LONGINT);
  TYPE ByteArr = ARRAY[0..MaxInt] OF BYTE;
       BytePtr = ^ByteArr;
  BEGIN
    IF Size = 0 THEN Exit;
    IF Odd(ToAddress) THEN BEGIN
      SetByte(Handle, ToAddress, BytePtr(FromAddress)^[0]);
      IF Result <> 0 THEN Exit;
      Dec(Size);
      Inc(LONGINT(FromAddress));
      Inc(ToAddress);
    END;
    MoveMem(POINTER(ToAddress), Handle, FromAddress, 0, Size AND $FFFE);
    IF Result <> 0 THEN Exit;
    IF Odd(Size)
     THEN SetByte(Handle, ToAddress+Size-1, BytePtr(FromAddress)^[Size-1]);
    IF Result <> 0 THEN Exit;
  END;

PROCEDURE ResizeMem (NewSize, Handle: WORD);
  BEGIN
    ASM
      MOV  AH,$0F
      MOV  BX,WORD PTR NewSize
      MOV  DX,WORD PTR Handle
      CALL [XMMAddr]
      OR   AX,AX
      JNZ  @@1
      MOV  BYTE PTR Result,BL
    @@1:
    END;
  END;

PROCEDURE InitUnit; ASSEMBLER;
  ASM
    MOV  AX,$4300
    INT  $2F
    CMP  AL,$80
    JNE  @@1
    MOV  AX,$4310
    INT  $2F
    MOV  WORD PTR XMMAddr,BX
    MOV  WORD PTR XMMAddr+2,ES
    JMP  @@2
  @@1:
    MOV  BYTE PTR Result,$80
  @@2:
  END;

BEGIN
  XMMAddr := Nil;
  Result := 0;
  InitUnit;
END.