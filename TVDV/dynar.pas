Unit DynAr;
(**********************************)
(* Diese Unit entscheidet, welche *)
(* Art von Array verwendet werden *)
(* soll. Dabei wird ÅberprÅft, ob *)
(* Normaler Arbeitsspeicher oder  *)
(* EMS verwendet werden kann.     *)
(*                                *)
(* Ist EMS nîtig, aber nicht      *)
(* vorhanden, so wird ein Fehler  *)
(* zurÅckgegeben.                 *)
(**********************************)

Interface


Function GetArray(Var Adress: Pointer;
                      ESize: Integer;
                      Anzahl: Integer;
                  Var IfEMS: Boolean): Byte;
Function ReleaseArray(Adress: Pointer;
                      ESize: Integer;
                      Anzahl: Integer;
                      IfEMS: Boolean): Byte;

Implementation

Procedure NewArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);
var AnyPointer : ^Integer Absolute Pointer;

begin
  getmem(AnyPointer,ElementSize * Anzahl);
end;

Procedure DisposeArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);
var AnyPointer : ^Integer Absolute Pointer;

begin
  freemem(AnyPointer,ElementSize * Anzahl);
end;



Function GetArray;
Begin
  GetArray := 1;
  If (ESize * Anzahl) < System.MemAvail then
  Begin
    NewArray(Adress, ESize, Anzahl);
    GetArray := 0;
    IfEMS := False
  End
  Else
  Begin

  End
End;

Function ReleaseArray;
Begin
  If IfEMS then
  Begin
    (*Wait('EMS - Reservierung freigeben!')*)
  End
  Else
  Begin
    DisposeArray(Adress, ESize, Anzahl)
  End
End;

End.