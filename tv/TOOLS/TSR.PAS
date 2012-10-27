UNIT TSR;

INTERFACE

USES
  Dos;

FUNCTION CallerIP
  : WORD;
  INLINE($8B/$46/$02);            (*  MOV AX,[BP+02]                *)

FUNCTION CallerCS
  : WORD;
  INLINE($8B/$46/$04);            (*  MOV AX,[BP+04]                *)

PROCEDURE CallRoutine
  ( Routine: POINTER);
  INLINE ( $89/$E3/               (*  MOV     BX,SP                 *)
           $9C/                   (*  PUSHF                         *)
           $36/ $FF/ $1F/         (*  SS:CALL FAR [BX]              *)
           $81/ $C4/ $04/ $00);   (*  ADD     SP,0004               *)

PROCEDURE JmpRoutine
  ( Address : POINTER);
  INLINE( $5B/                    (*  POP  BX                       *)
          $58/                    (*  POP  AX                       *)
          $87/$5E/$0E/            (*  XCHG BX,[BP+14]               *)
          $87/$46/$10/            (*  XCHG AX,[BP+16]               *)
          $89/$EC/                (*  MOV  SP,BP                    *)
          $5D/                    (*  POP  BP                       *)
          $07/                    (*  POP  ES                       *)
          $1F/                    (*  POP  DS                       *)
          $5F/                    (*  POP  DI                       *)
          $5E/                    (*  POP  SI                       *)
          $5A/                    (*  POP  DX                       *)
          $59/                    (*  POP  CX                       *)
          $CB);                   (*  RETF                          *)

FUNCTION GetStackPtr: POINTER;
  INLINE( $8C/$D2/                (* MOV   DX,SS                    *)
          $89/$E0);               (* MOV   AX,SP                    *)

PROCEDURE SetStackPtr
  ( NewValue: POINTER );
  INLINE( $58/                    (* POP   AX                       *)
          $5A/                    (* POP   DX                       *)
          $8E/$D2/                (* MOV   SS,DX                    *)
          $89/$C4);               (* MOV   SP,AX                    *)

FUNCTION ScanCode : BYTE;
  INLINE( $E4 / $60 );           (*  IN   AL, Kbd_Data              *)

PROCEDURE ResetKbd;
  INLINE( $E4 / $61 /            (*  IN   AL, Kbd_Ctrl              *)
          $8A / $E0 /            (*  MOV  AH, AL                    *)
          $0C / $80 /            (*  OR   AL, $80                   *)
          $E6 / $61 /            (*  OUT  Kbd_Ctrl, AL              *)
          $86 / $E0 /            (*  XCHG AH, AL                    *)
          $E6 / $61 );           (*  OUT  Kbd_Ctrl, AL              *)

PROCEDURE EndOfIntCmd;
  INLINE( $FA /                  (*  CLI                            *)
          $B0 / $20 /            (*  MOV  AL, $20                   *)
          $E6 / $20 );           (*  OUT  $20, AL                   *)

PROCEDURE Install
  ( VNo: INTEGER;
    PNew: POINTER;
    VAR POld: POINTER);

PROCEDURE DeInstall
  ( VNo: INTEGER;
    P: POINTER);

PROCEDURE ExecOnHeap
( Prog, Arg : STRING);

PROCEDURE SetNewHeap
( NoOfBytes: LONGINT);

IMPLEMENTATION

PROCEDURE DeInstall ( VNo: INTEGER; P: POINTER);
  BEGIN
    INLINE($FA);
    SetIntVec(VNo, P);
    INLINE($FB);
  END;

PROCEDURE Install ( VNo: INTEGER; PNew: POINTER; VAR POld: POINTER);
  BEGIN
    INLINE($FA);
    GetIntVec(VNo, POld);
    SetIntVec(VNo, PNew);
    INLINE($FB);
  END;

PROCEDURE ExecOnHeap ( Prog, Arg : STRING);
  VAR Regs: Registers;
  BEGIN
    WITH Regs DO BEGIN
      AH := $4A;
      ES := PrefixSeg;
      BX := Seg(HeapEnd^) + 1 - PrefixSeg;
    END;
    MsDos(Regs);
    SwapVectors;
    Exec(Prog, Arg);
    SwapVectors;
    WITH Regs DO BEGIN
      AH := $4A;
      ES := PrefixSeg;
      BX := $FFFF;
    END;
    MsDos(Regs);
    WITH Regs DO BEGIN
      AH := $4A;
      ES := PrefixSeg;
    END;
    MsDos(Regs);
  END;

PROCEDURE SetNewHeap ( NoOfBytes: LONGINT);
  VAR
    Regs: Registers;
  BEGIN
    WITH Regs DO BEGIN
      AH := $4A;
      ES := PrefixSeg;
      BX := Seg(HeapPtr^) - PrefixSeg + (NoOfBytes SHR 4);
    END;
    MsDos(Regs);
  END;

END.

