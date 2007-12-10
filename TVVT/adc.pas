Unit ADC;

Interface

uses crt, Video;

const bit: array[1..8] of byte = (1,2,4,8,16,32,64,128);
      adr= $280;

Type Funktionswerte_Array    = Array [1..1] of Real;
     Funktionswerte_Ptr_typ  = ^Funktionswerte_Array;

Type ADC_Array    = Array [1..1] of Byte;
     ADCPtrtyp    = ^ADC_Array;


Var
     Funktionswerte,
     Funktionswerte1,
     ADCwerte          : Funktionswerte_Ptr_typ;
     ADCFileData       : ADCPtrtyp;
     ADCKanal,
     ADCAnzahl,
     ADCRate           : Integer;


Procedure GetADC(Kanal : Integer;
                 Anzahl: Integer;
                 Rate  : Integer);


Implementation

Function BitString(Data: byte): String;
var a: integer;
    S: String;
begin
  S := '';
  for a:= 8 downto 1 do if (Data and Bit[a]) > 0 then S := S + '1 ' else S := S + '0 ';
end;

Procedure GetADC;
Var A: Integer;
    Wert: Real;
begin
  NewArray(ADCFileData, 1, Anzahl);
  If (Kanal >= 0) and
     (Kanal <= 15) then
  For A := 1 to Anzahl do
  Begin
    port[adr + 1]:= Kanal + 8;
    port[adr]:= 0;

    delay(Rate);

    Wert := Port[Adr];
    ADCFileData^[A] := Port[Adr];

    ADCWerte^[A] := Wert
  End
  Else Halt
end;

Begin
  ADCWerte := Nil;
  ADCFileData := Nil
End.

