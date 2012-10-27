UNIT EMS;
(***************************************************************************)
(* Expanded Memory Unit fÅr Turbo Pascal 6.0                               *)
(* (c) vorerst keines                                                      *)
(* 1990, Reebear in : E.,M.,S. - Turbo 6.0 & Vision                        *)
(***************************************************************************)

INTERFACE

VAR
  EMMError : BYTE;

FUNCTION GetVersion : String;

FUNCTION EMMErrorMsg (ErrorCode : INTEGER): String;

FUNCTION TestEMMDriver : BOOLEAN;

PROCEDURE GetPageFrameAddr (VAR PageFrameSeg: WORD);

PROCEDURE EMMPageCount (VAR PagesAvail, PagesTotal: WORD);

FUNCTION AllocatePages (Pages: WORD): WORD;

PROCEDURE EMMPageMap (PhysPage: BYTE; LogPage, EMMHandle: WORD);

PROCEDURE ReleasePage (EMMHandle: WORD);


IMPLEMENTATION

USES DOS;

CONST
  EMMIntr : WORD = $67;

VAR
  Regs : Registers;

PROCEDURE CallEMM (VAR Regs : Registers);
BEGIN
  Intr (EMMIntr, Regs);
  EMMError := Regs.AH;
END;

FUNCTION EMMErrorMsg (ErrorCode : INTEGER): String;
BEGIN
  CASE ErrorCode OF
    $00 : EMMErrorMsg := 'No error';
    $80 : EMMErrorMsg := 'Internal error in EMM software';
    $81 : EMMErrorMsg := 'Malfunction in EMS hardware';
    $82 : EMMErrorMsg := 'Memory manager busy';
    $83 : EMMErrorMsg := 'Invalid handle';
    $84 : EMMErrorMsg := 'Function not defined';
    $85 : EMMErrorMsg := 'Handles exhausted';
    $86 : EMMErrorMsg := 'Error in save or restore of mapping context';
    $87 : EMMErrorMsg := 'Not enough pages physically available';
    $88 : EMMErrorMsg := 'Not enough pages currently available';
    $89 : EMMErrorMsg := 'Zero pages cannot be allocated';
    $8A : EMMErrorMsg := 'Requested logical page is outside of pages owned by handle';
    $8B : EMMErrorMsg := 'Illegal physical page number in mapping request';
    $8C : EMMErrorMsg := 'Page mapping hardware-state save area is full';
    $8D : EMMErrorMsg := 'Mapping context save failed';
    $8E : EMMErrorMsg := 'Mapping context restore failed';
    $8F : EMMErrorMsg := 'Subfunction parameter not defined';
    $90 : EMMErrorMsg := 'Attribute type not defined';
    $91 : EMMErrorMsg := 'Feature not supported';
    $92 : EMMErrorMsg := 'Memory regions overlap - move was performed, part of source region overwritten';
    $93 : EMMErrorMsg := 'Specified length is longer than actual length';
    $94 : EMMErrorMsg := 'Conventional and expanded memory regions overlap';
    $95 : EMMErrorMsg := 'Specified offset is outside logical page';
    $96 : EMMErrorMsg := 'Region length exceeds 1 MByte';
    $97 : EMMErrorMsg := 'Memory regions overlap - exchange not performed';
    $98 : EMMErrorMsg := 'Memory source and destination types are undefined';
    $99 : EMMErrorMsg := 'Error code currently unused';
    $9A : EMMErrorMsg := 'Specified alternate register set is not supported';
    $9B : EMMErrorMsg := 'All alternate register sets are currently allocated';
    $9C : EMMErrorMsg := 'Alternate map or DMA register not supported  but register set is not zero';
    $9D : EMMErrorMsg := 'Alternate register set not defined or not allocated';
    $9E : EMMErrorMsg := 'Dedicated DMA channels are not supported';
    $9F : EMMErrorMsg := 'Specified DMA channel is not supported';
    $A0 : EMMErrorMsg := 'No handle found for specified name';
    $A1 : EMMErrorMsg := 'Handle with same name already exists';
    $A3 : EMMErrorMsg := 'Invalid pointer passed to function, or contents of source array corrupted';
    $A4 : EMMErrorMsg := 'Access to function denied by DOS';
    ELSE  EMMErrorMsg := 'Unknown error code';
  END;
END;

FUNCTION TestEMMDriver : BOOLEAN;
BEGIN
  Regs.AH := $40;
  CallEMM (Regs);
  TestEMMDriver := (Regs.AH = 0);
END;


PROCEDURE GetPageFrameAddr (VAR PageFrameSeg: WORD);
BEGIN
  Regs.AH := $41;
  CallEMM (Regs);
  PageFrameSeg := Regs.BX;
END;

PROCEDURE EMMPageCount (VAR PagesAvail, PagesTotal: WORD);
BEGIN
  Regs.AH := $42;
  CallEMM (Regs);
  EMMError := Regs.AH;
  PagesAvail := Regs.BX;
  PagesTotal := Regs.DX;
END;

FUNCTION AllocatePages (Pages: WORD): WORD;
BEGIN
  Regs.AH := $43;
  Regs.BX := Pages;
  CallEMM (Regs);
  AllocatePages := Regs.DX;
END;

PROCEDURE EMMPageMap (PhysPage: BYTE; LogPage, EMMHandle: WORD);
BEGIN
  Regs.AH := $44;
  Regs.AL := PhysPage;
  Regs.BX := LogPage;
  Regs.DX := EMMHandle;
  CallEMM (Regs);
  EMMError := Regs.AH;
END;

PROCEDURE ReleasePage (EMMHandle: WORD);
BEGIN
  Regs.AH := $45;
  Regs.DX := EMMHandle;
  CallEMM (Regs);
END;

FUNCTION GetVersion : String;
VAR
  VersionStr : String;
  VersionBCD : BYTE;
BEGIN
  Regs.AH := $46;
  CallEMM (Regs);
  VersionBCD := Regs.AL;
  VersionStr := CHR((VersionBCD DIV 16)+48) + '.' + CHR((VersionBCD MOD 16)+48);
  GetVersion := VersionStr;
END;

END.