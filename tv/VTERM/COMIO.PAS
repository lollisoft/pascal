{*******************************************************}
{                                                       }
{   Unit zur Ansteuerung der seriellen Schnittstelle    }
{                                                       }
{*******************************************************}

UNIT ComIO;

{$R-,S-}

INTERFACE

USES
  Dos, Crt;

CONST
  Baud110 = $00;
  Baud150 = $20;
  Baud300 = $40;
  Baud600 = $60;
  Baud1200 = $80;
  Baud2400 = $A0;
  Baud4800 = $C0;
  Baud9600 = $E0;
  EvenParity = $18;
  OddParity = $08;
  NoParity = $00;
  WordSize7 = $02;
  WordSize8 = $03;
  StopBits1 = $04;
  StopBits2 = $00;
  COM1Port = $00;
  COM2Port = $01;

TYPE
  PCom = ^TCom;
  TCom = OBJECT
    CONSTRUCTOR Init(BufSize: Word);
    DESTRUCTOR Done;
    FUNCTION Avail: Boolean;
    FUNCTION Get: Byte;
    FUNCTION Overflow: Boolean;
    PROCEDURE Put(B: Byte);
    PROCEDURE PutString(S: STRING);
    FUNCTION SetParam(PortNum, Params: Word): Boolean;
  END;

IMPLEMENTATION

CONST
  UART_THR = $00;
  UART_RBR = $00;
  UART_IER = $01;
  UART_IIR = $02;
  UART_LCR = $03;
  UART_MCR = $04;
  UART_LSR = $05;
  UART_MSR = $06;
  I8088_IMR = $21;

TYPE
  ComBufferType = ARRAY[1..1] OF Byte;
  ComBufferPtrType = ^ComBufferType;

VAR
  COM:
    RECORD
      Overflow,
      Instantiated,
      IRQInitialized: Boolean;
      PortNum,
      Base,
      Max,
      Head,
      Tail: Word;
      BufferPtr: ComBufferPtrType;
      SaveCom1Int: Pointer;
      SaveCom2Int: Pointer;
      IntNumber: Byte;
   END;

BIOSPorts: ARRAY[1..2] OF Word ABSOLUTE $40:0;

PROCEDURE STI; INLINE($FB);

PROCEDURE CLI; INLINE($FA);

PROCEDURE ComIntHandler; FAR; ASSEMBLER;
ASM
  STI
  PUSH  DS
  PUSH  DX
  PUSH  BX
  PUSH  AX
  PUSH  ES
  PUSH  DI
  MOV   DX,Seg @Data
  MOV   DS,DX            { restore Turbo's data segment }
  MOV   DX,COM.Base      { Receive buffer register is at offset 0 }
  IN    AL,DX
  LES   DI,COM.BufferPtr { get pointer into the buffer }
  MOV   BX,COM.Head
  MOV   ES:[DI+BX-1],AL  { put character into buffer; array is 1 based ! }
  MOV   AX,COM.Max
  SUB   AX,BX            { do we need a wrap around ? }
  JE    @@1
  Inc   BX               { no, just increment }
  JMP   @@2
@@1:
  MOV   BX,1             { after wrap around }
@@2:
  MOV   AX,COM.Tail
  CMP   AX,BX            { buffer overflow ? }
  JNE   @@3
  Inc   COM.Overflow
  JMP   @@4
@@3:
  MOV   COM.Head,BX      { put head pointer back }
@@4:
  MOV   AL,$20
  OUT   $20,AL           { send non-specific EOI to interrupt controller }
  POP   DI
  POP   ES
  POP   AX
  POP   BX
  POP   DX
  POP   DS
  IRET
END;

PROCEDURE BiosInitCom(PortNum,Params: Word); ASSEMBLER;
ASM
  MOV   AX,Params
  MOV   DX,PortNum
  XOR   AH,AH
  Int   14H
END;

CONSTRUCTOR TCom.Init(BufSize: Word);
BEGIN
  WITH COM DO
  BEGIN
    IF Instantiated THEN Fail; { only one instance is allowed }
    Instantiated := True;
    GetIntVec($C, SaveCom1Int);
    GetIntVec($B, SaveCom2Int);
    GetMem(BufferPtr, BufSize);
    Max:=BufSize;
    Head := 1;
    Tail := 1;
    OverFlow := False;
  END;
END;

DESTRUCTOR TCom.Done;
VAR IRQ:  Byte;
BEGIN
  SetIntVec($C, COM.SaveCom1Int);
  SetIntVec($B, COM.SaveCom2Int);
  FreeMem(COM.BufferPtr, COM.Max);
  IF COM.IRQInitialized THEN BEGIN { mask out used IRQ in PIC }
    IRQ := Hi(COM.Base) + 1;
    CLI;
      Port[I8088_IMR] := Port[I8088_IMR] OR (1 SHL IRQ);
      Port[UART_IER + COM.Base] := 0;
      Port[UART_MCR + COM.Base] := 0;
    STI;
  END;
  COM.Instantiated := False;
END;

FUNCTION TCom.Avail: Boolean;
BEGIN
  Avail := COM.Head <> COM.Tail;
END;

FUNCTION TCom.Get: Byte;
BEGIN
  WITH COM DO
  BEGIN
    REPEAT UNTIL Head <> Tail;
    Get := BufferPtr^[Tail];
    CLI;
      Inc(Tail);
      IF Tail > Max THEN Tail := 1;
    STI;
  END;
END;

FUNCTION TCom.Overflow: Boolean;
BEGIN
  Overflow := COM.Overflow;
END;

PROCEDURE TCom.Put(B: Byte);
BEGIN
  WHILE (Port[UART_LSR + COM.Base] AND $20) = 0 DO {nichts};
  CLI;
  Port[UART_THR + COM.Base] := B;
  STI;
END;

PROCEDURE TCom.PutString(S: STRING);
VAR L: Integer;
BEGIN
  FOR L := 1 TO Length(S) DO Put(Ord(S[L]));
END;

FUNCTION TCom.SetParam(PortNum, Params: Word): Boolean;
VAR
  Junk: Word;
  IRQ: Byte;
BEGIN
  IRQ := Hi(BIOSPorts[PortNum + 1]) + 1; { compute IRQ first }
  IF (Port[I8088_IMR] AND (1 SHL IRQ) = 0)
     AND (IRQ <> COM.IntNumber - $8) THEN BEGIN {is IRQ already in use?}
    SetParam := False; { yes, then ignore request }
    Exit;
  END;
  COM.Base := BIOSPorts[PortNum + 1];
  IRQ := Hi(COM.Base) + 1;
  COM.IntNumber := IRQ + $8;
  WITH COM DO BEGIN
    Overflow := False;
    Head := 1;
    Tail := 1;
  END;
  SetIntVec($C, COM.SaveCom1Int);
  SetIntVec($B, COM.SaveCom2Int);
  IF (Port[UART_IIR + COM.Base] AND $F8) <> 0 THEN BEGIN
    SetParam := False;
    Exit;
  END;
  SetIntVec(COM.IntNumber, @ComIntHandler); { install interrupt handler }
  COM.PortNum := PortNum;
  BiosInitCom(PortNum,Params); { use BIOS call for easy param setting }
  CLI;
    Port[UART_LCR + COM.Base] := Port[UART_LCR + COM.Base] AND $7F;
    Port[I8088_IMR] := Port[I8088_IMR] AND ((1 SHL IRQ) XOR $FF);
                                      { enable PIC to recognize IRQ's }
    Port[UART_IER + COM.Base] := $01; { allow UART to generate IRQ's }
    Port[UART_MCR + COM.Base] := Port[UART_MCR + COM.Base] OR $0B;
    Junk := Port[UART_LSR + COM.Base];  { clear these registers }
    Junk := Port[UART_RBR + COM.Base];
  STI;
  COM.IRQInitialized := True;
    { don't forget to mask out IRQ in PIC when done }
  SetParam:=True
END;

BEGIN
  COM.Instantiated := False;
  COM.IRQInitialized := False;
END.
