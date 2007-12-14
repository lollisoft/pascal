Program Plot;
Uses Crt, Graph, Pasmouse, Vid, Curscont, ADC;



Var
    a, b : Longint;

    Werte_Da        : Boolean;
    Ch              : Char;
    GraphDriver,
    GraphMode       : Integer;
    Anfang,
    Ende            : Integer;


    Datafile        : File of Byte;
    Dateiname       : String;



Procedure New_Array(Var Pointer; Element_Size: Integer; Anzahl: Longint);
var Any_Pointer : ^Integer Absolute Pointer;
begin
  getmem(Any_Pointer,Element_Size * Anzahl);
end;

Procedure Dispose_Array(Var Pointer; Element_Size: Integer; Anzahl: Longint);
var Any_Pointer : ^Integer Absolute Pointer;
begin
  freemem(Any_Pointer,Element_Size * Anzahl);
end;

Type Arttyp     = (Vorzeichen, Operator, Funktion, Zahl, VarX);
     String128  = String[128];
     OpTyp      = Set Of Char;
     Ptr        = ^Knotentyp;
     Knotentyp  = Record Case Art: Arttyp of
                    Vorzeichen : (Von: Ptr);
                    Operator   : (Op: Char; Links, Rechts: Ptr);
                    Funktion   : (Fn: String[6]; Next: Ptr);
                    Zahl       : (Za: Real);
                    VarX       : ();
                  end;

Var  Term : String128;
     Fkt  : Ptr;

Function SucheOperator(Ops: OpTyp; Var K: Integer; Term: String128):Boolean;
Var Klammer : Integer; Gefunden : Boolean;
Begin
  Gefunden := False;
  Klammer := 0;
  k := Length(Term) + 1;
  Repeat
    k := k - 1;
    Case Term[k] of
      '(' : Klammer := Pred(Klammer);
      ')' : Klammer := Succ(Klammer);
    else If (Klammer = 0) and (Term[k] in Ops) and (k > 1) and
            (Not(Term[k - 1] in ['E', '^']))
         then Gefunden := True
    end
  Until Gefunden or (k = 1);
  SucheOperator := Gefunden
end;

Function SuchePlusMinus(Var k: Integer; Term: String128):Boolean;
Begin
  SuchePlusMinus := SucheOperator(['+', '-'], k, Term)
end;

Function SucheMalDurch(Var k: Integer; Term: String128):Boolean;
Begin
  SucheMalDurch := SucheOperator(['*', '/'], k, Term)
end;

Function SucheVorzeichen(Term: String128):Boolean;
Begin
  SucheVorzeichen := (Term[1] In ['+', '-'])
end;

Function SuchePotenz(Var k: Integer; Term: String128):Boolean;
Begin
  SuchePotenz := SucheOperator(['^'], k, Term)
end;

Function SucheFunktion(Var k: Integer; Term: String128):Boolean;
Var F: String128;
Begin
  SucheFunktion := False;
  k := Pos('(', Term);
  If k > 0 then
  Begin
    F := Copy(Term, 1, k - 1);
    If (F = 'ABS') or
       (F = 'ARCTAN') or
       (F = 'COS') or
       (F = 'EXP') or
       (F = 'FRAC') or
       (F = 'INT') or
       (F = 'LN') or
       (F = 'SIN') or
       (F = 'SQR') or
       (F = 'SQRT') or
       (F = 'TAN') or
       (F = '') and
       (Term[Length(Term)] = ')')
    then SucheFunktion := True
  end
end;

Function SucheZahl(Var Wert: Real; Term: String128):Boolean;
Var Code: Integer;
Begin
  Val(Term, Wert, Code);
  SucheZahl := (Code = 0)
end;

Function SucheX(Term: String128):Boolean;
Begin
  SucheX := (Term = 'X')
end;




Function FunktionsAnalyse(Term: String128):Ptr;
Var TermOk: Boolean;
    Fkt   : Ptr;
Function UpDate(Term:String128):String128;
Var i      : Integer;
    ch     : Char;
    UpTerm : String128;
Begin
  UpTerm := '';
  For i := 1 to Length(Term) do
  Begin
    ch := UpCase(Term[i]);
    If Ch <> ' ' then UpTerm := UpTerm + ch
  end;
  UpDate := UpTerm
end;


Procedure BaueBaum(Var Knoten: Ptr; Term: String128);
Var Wert: Real;
    k   : Integer;
    Vz  : Char;
Begin
  If TermOk and (Length(Term) > 0) then
    If SuchePlusMinus(k, Term) then
    begin
      New(Knoten);
      Knoten^.Art := Operator;
      Knoten^.Op := Term[k];
      BaueBaum(Knoten^.Links,  Copy(Term, 1, k - 1));
      BaueBaum(Knoten^.Rechts, Copy(Term, k + 1, Length(Term) - k));
    end
    else
    If SucheMalDurch(k, Term) then
    begin
      New(Knoten);
      Knoten^.Art := Operator;
      Knoten^.Op := Term[k];
      BaueBaum(Knoten^.Links,  Copy(Term, 1, k - 1));
      BaueBaum(Knoten^.Rechts, Copy(Term, k + 1, Length(Term) - k));
    end
    else
    If SucheVorzeichen(Term) then
    Begin
      Vz := Term[1];
      Delete(Term, 1, 1);
      Case Vz of
        '+' : BaueBaum(Knoten, Term);
        '-' : begin
                New(Knoten);
                Knoten^.Art := Vorzeichen;
                BaueBaum(Knoten^.von, Term)
              end
      end
    end
    else
    If SuchePotenz(k, Term) then
    begin
      New(Knoten);
      Knoten^.Art := Operator;
      Knoten^.Op := Term[k];
      BaueBaum(Knoten^.Links,  Copy(Term, 1, k - 1));
      BaueBaum(Knoten^.Rechts, Copy(Term, k + 1, Length(Term) - k))
    end
    else
    If SucheFunktion(k, Term) then
    begin
      New(Knoten);
      Knoten^.Art := Funktion;
      Knoten^.Fn := Copy(Term, 1, k - 1);
      BaueBaum(Knoten^.Next, Copy(Term, k + 1, Length(Term) - 1 - k));
    end
    else
    If SucheZahl(Wert, Term) then
    begin
      New(Knoten);
      Knoten^.Art := Zahl;
      Knoten^.Za := Wert
    end
    else
    If SucheX(Term) then
    begin
      New(Knoten);
      Knoten^.Art := VarX;
    end
    else TermOk := False
  else TermOk := False
end;


Begin { FunktionsAnalyse }
  TermOk := True;
  Term := UpDate(Term);
  BaueBaum(Fkt, Term);
  If Not TermOk
    then FunktionsAnalyse := Nil
    else FunktionsAnalyse := Fkt
end;

Function FunktionsBerechnung(Fkt: Ptr; X: Real):Real;
Const MaxReal = 1.0E+30;
      MinReal = 1.0E-30;
Var   WertOk  : Boolean;


Function Wert(Zeiger: Ptr):Real;
Var Test: Real;
Begin
  If WertOk then
    With Zeiger^ do
      Case Art of
        Vorzeichen : Wert := - Wert(von);
        Operator   : Case Op of
                       '+' : Wert := Wert(Links) + Wert(Rechts);
                       '-' : Wert := Wert(Links) - Wert(Rechts);
                       '*' : Wert := Wert(Links) * Wert(Rechts);
                       '/' : Begin
                               Test := Wert(Rechts);
                               If Abs(Test) > MinReal
                                 then Wert := Wert(Links) / Test
                                 else WertOk := False
                             end;
                       '^' : Begin
                               Test := Wert(Links);
                               If Test > MinReal
                                 then Wert := Exp(Wert(Rechts) * Ln(Test))
                                 else WertOk := False
                             end
                     end;
        Funktion   : Begin
                       If Fn = ''       then Wert := Wert(Next); { Klammern }
                       If Fn = 'ABS'    then Wert := ABS(Wert(Next));
                       If Fn = 'ARCTAN' then Wert := ARCTAN(Wert(Next));
                       If Fn = 'COS'    then Wert := COS(Wert(Next));
                       If Fn = 'EXP'    then Wert := EXP(Wert(Next));
                       If Fn = 'FRAC'   then Wert := FRAC(Wert(Next));
                       If Fn = 'INT'    then Wert := INT(Wert(Next));
                       If Fn = 'LN'     then Begin
                                               Test := Wert(Next);
                                               If Test > 0
                                                 then Wert := LN(Test)
                                                 else WertOk := False
                                             end;
                       If Fn = 'SIN'    then Wert := SIN(Wert(Next));
                       If Fn = 'SQR'    then Wert := SQR(Wert(Next));
                       If Fn = 'SQRT'   then Begin
                                               Test := Wert(Next);
                                               If Test > 0
                                                 then Wert := SQRT(Test)
                                                 else WertOk := False
                                             end;
                       If Fn = 'TAN'    then Wert := SIN(Wert(Next)) /
                                                     COS(Wert(Next))
                     end;
        Zahl       : Wert := Za;
        VarX       : Wert := X
      end
end; { Wert }


begin { FunktionsBerechnung }
  If Fkt <> Nil then
  begin
    WertOk := True;
    FunktionsBerechnung := Wert(Fkt);
    If Not WertOk then FunktionsBerechnung := MaxReal
  end
  else FunktionsBerechnung := MaxReal
end;



Procedure FunktionsEingabe(Var Term: String128; Var Fkt: Ptr);
Var Hilf : Integer;
begin
  Repeat
    Term := '';
    Clrscr;
    If Fkt = Nil then Writeln('Fehler in der Funktion!');
    Write('Bitte geben Sie die Funktion ein: ');
    Readln(Term);
    If Term = '' then
    begin
      Clrscr;
      Exit
    end;
    Fkt := FunktionsAnalyse(Term)
  Until Fkt <> Nil;
  Writeln('Bitte geben Sie den wertebereich ein ( nur ganze Zahlen ): ');
  Write('Anfang : ');
  Readln(Anfang);
  Write('Ende   : ');
  Readln(Ende);
  If Anfang > Ende then
  begin
    Hilf := Anfang;
    Anfang := Ende;
    Ende := Hilf
  end;
  New_Array(Funktionswerte, 6, Ende - Anfang);
  New_Array(Funktionswerte1, 6, Ende - Anfang);
end;





Procedure Zeichne(R : Funktionswerte_Ptr_typ);

var Max,
    Min : Real;
    X   : Integer;
    MX  : Integer;
begin
  MX := GetMaxX;

  Max := 0;
  For A := 1 to Ende - Anfang do if R^[A] > Max then
    Max := R^[A];

  Min := Max;

  For A := 1 to Ende - Anfang do If R^[A] < Min then
  Min := R^[A];

  For A := 1 to Ende - Anfang do
    R^[A] := R^[A] - Min;

  If Max - Min = 0 then Min := Min - 1;

  For A := 1 to Ende - Anfang do
    R^[A] := Round((R^[A] / (Max - Min)) * GetMaxY);

  MoveTo(0, Round(R^[1]));

  For A := 1 to Ende - Anfang do
  begin
    X := Round((A * MX) / (Ende - Anfang));
    LineTo(X, Round(R^[A]))
  end

end;


Procedure Termeingabe;
begin
  Werte_Da := True;
  Repeat
    FunktionsEingabe(Term, Fkt);
    If Term = '' then Exit;
    DetectGraph(GraphDriver, GraphMode);
    InitGraph(GraphDriver, GraphMode, '');

    For A := 1 to Ende - Anfang do
        Funktionswerte^[A] := FunktionsBerechnung(Fkt, A + Anfang);

    Zeichne(Funktionswerte);

    Ch := Readkey;
    CloseGraph;
    Writeln('Ende = ESC');
    Ch := Readkey
  Until Ch = Chr(27);
  Clrscr
end;


Procedure Daten_Lesen;
Var Data : Byte;
Begin
  Anfang := 0;
  Ende := Filesize(Datafile);
  New_Array(Funktionswerte1, 6, Ende);
  For A := 1 to Ende do
  begin
    Read(Datafile, Data);
    Funktionswerte1^[A] := Data
  end;
end;

Procedure Daten_Schreiben;
Begin
{  If Funktionswerte <> Nil then
  Begin
    For A := Anfang to Ende do
    Begin
      Write(Datafile, Funktionswerte1^[Ende - Anfang - A]
    end
  end
  else Wait('Keine Daten vorhanden!', 3)  }
end;

Procedure Datei_Oeffnen(Var Error:Boolean);
Begin
  Clrscr;
  Error := False;
  Writeln('Bitte geben Sie einen Dateinamen an: ');
  Readln(Dateiname);
  Assign(Datafile, Dateiname);
  (*$I-*)
  reset(Datafile);
  if IOResult <> 0 then rewrite(Datafile);
  if IOResult <> 0 then
  begin
    writeln;
    Error := True;
    write('Datei kann nicht geîffnet werden !')
  end
  (*$I+*)
end;

Procedure Datei_Schliessen;
Begin
  Close(Datafile)
end;

Procedure Edit_Funktionswerte;
Var Menu : Integer;

Procedure Daten_Laden;
Var Error : Boolean;
begin
  Datei_Oeffnen(Error);
  If Not(Error) then
  begin
    Werte_Da := True;
    Daten_Lesen;
    Datei_Schliessen
  end
End;

Procedure Daten_Speichern;
Var Error : Boolean;
begin
  Datei_Oeffnen(Error);
  Daten_Schreiben;
  Datei_Schliessen
end;

Procedure Daten_Behandeln;

Procedure Graf_Edit;
begin
end;
Procedure Daten_Edit;
begin
end;
Procedure Fenster_Edit;
begin
end;
begin
  Repeat
    Clrscr;
    Texte[1] := 'Daten als Graf behandeln .....';
    Texte[2] := 'Daten als Werte behandeln ....';
    Texte[3] := 'Fenster fÅr Graf einstellen ..';
    Texte[4] := 'Ende ( ESC ) .................';
    Texte[5] := '*';

    Balken_Menue(Menu);

    Case Menu of
      1 : Graf_Edit;
      2 : Daten_Edit;
      3 : Fenster_Edit;
    end
  until Menu = 4

end;

begin
  Repeat
    Clrscr;
    Texte[1] := 'BinÑr - Datei lesen .......';
    Texte[2] := 'BinÑr - Datei schreiben ...';
    Texte[3] := 'Daten behandeln ...........';
    Texte[4] := 'Ende ( ESC ) ..............';
    Texte[5] := '*';

    Balken_Menue(Menu);

    Case Menu of
      1 : Daten_Laden;
      2 : Daten_Speichern;
      3 : If Werte_Da then Daten_Behandeln;
    end
  until Menu = 4
end;

Procedure Tabelle_Plotten;
Begin
  DetectGraph(GraphDriver, GraphMode);
  InitGraph(GraphDriver, GraphMode, '');

  Zeichne(ADCWerte);

  Ch := Readkey;
  CloseGraph;
End;

Procedure ADCTabDel;
Begin
  If ADCWerte <> Nil then Dispose_Array(ADCwerte, 6, ADCAnzahl);
  If ADCFileData <> Nil then Dispose_Array(ADCFileData, 1, ADCAnzahl);
End;

Procedure ADCEinlesen;
Begin
  ClrScr;
  Write('Bitte geben Sie den Kanal an: ');
  Readln(ADCKanal);
  Write('Bitte geben Sie die Anzahl an: ');
  Readln(ADCAnzahl);
  Write('Bitte geben Sie die Rate an: ');
  Readln(ADCRate);

  If ADCRate <= 0 then ADCRate := 1;
  If ADCKanal < 0 then ADCKanal := 0;
  If ADCAnzahl < 1 then ADCAnzahl := 1;

  Anfang := 1;
  Ende := ADCAnzahl;

  If ADCWerte <> Nil then DisposeArray(ADCWerte, 6, ADCAnzahl);
  If ADCFileData <> Nil then DisposeArray(ADCFileData, 1, ADCAnzahl);

  New_Array(ADCwerte, 6, ADCAnzahl);



  GetADC(ADCKanal, ADCAnzahl, ADCRate);
  Clrscr
End;


Procedure Oszi;
Begin
  ADCRate := 1;
  ADCKanal := 1;

  If ADCWerte <> Nil then DisposeArray(ADCWerte, 6, ADCAnzahl);
  If ADCFileData <> Nil then DisposeArray(ADCFileData, 1, ADCAnzahl);

  Write('Bitte geben Sie die Anzahl an: ');
  Readln(ADCAnzahl);

  Anfang := 1;
  Ende := ADCAnzahl;


  New_Array(ADCwerte, 6, ADCAnzahl);

  DetectGraph(GraphDriver, GraphMode);
  
  InitGraph(GraphDriver, GraphMode, '');

  GetADC(ADCKanal, ADCAnzahl, ADCRate);
  Zeichne(ADCWerte);
  If ADCFileData <> Nil then DisposeArray(ADCFileData, 1, ADCAnzahl);


  Repeat
    SetColor(Black);
    Zeichne(ADCWerte);
    SetColor(White);

    GetADC(ADCKanal, ADCAnzahl, ADCRate);

    Zeichne(ADCWerte);
    If ADCFileData <> Nil then DisposeArray(ADCFileData, 1, ADCAnzahl);

  Until Keypressed;
  CloseGraph;

  DisposeArray(ADCwerte, 6, ADCAnzahl);
End;

Procedure ADCLoad;
Var F: File of Byte;
    FName: String;
    Wert: Real;
Begin

  {$I-}
  ClrScr;
  Write('Dateiname ? ');
  Readln(FName);

  TestMaskenSyntax(FName);

  Assign(F, FName);
  Reset(F);
  If IOResult <> 0 then
  Begin
    Clrscr;
    Writeln('Datei nicht vorhanden!');
    Exit
  End;

  If ADCWerte <> Nil then DisposeArray(ADCWerte, 6,ADCAnzahl);
  If ADCFileData <> Nil then DisposeArray(ADCFileData, 1, ADCAnzahl);

  ADCAnzahl := FileSize(F);
  New_Array(ADCFileData, 1, ADCAnzahl);
  New_Array(ADCWerte, 6, ADCAnzahl);

  For A := 1 to ADCAnzahl do
  Begin
    Read(F, ADCFileData^[A]);
    Wert := ADCFileData^[A];
    ADCWerte^[A] := Wert
  End;

  Anfang := 1;
  Ende := ADCAnzahl;

  Close(F)
  {$I+}
End;

Procedure ADCSave;
Var F: File of Byte;
    FName: String;
Begin
  {$I-}
  ClrScr;
  Write('Dateiname ? ');
  Readln(FName);

  TestMaskenSyntax(FName);

  Assign(F, FName);

  Rewrite(F);
  If IOResult <> 0 then Halt;
  For A := 1 to ADCAnzahl do
    Write(F, ADCFileData^[A]);
  Close(F)
  {$I+}
End;

Procedure Edit_Funktion;
begin
end;

Var Menu : Integer;
Begin
  Werte_Da := False;
  SaveCursor;

  Cursoraus;
  Clrscr;
  Repeat
    Texte[1] := 'Direkte Eingabe von Funktionen ....';
    Texte[2] := 'Funktionswerte - Editor ...........';
    Texte[3] := 'Funktion - Editor .................';
    Texte[4] := 'Werte von ADC einlesen ............';
    Texte[5] := 'ADCTabelle Plotten ................';
    Texte[6] := 'ADCTabelle Lîschen ................';
    Texte[7] := 'ADCTabelle Laden ..................';
    Texte[8] := 'ADCTabelle Speichern ..............';
    Texte[9] := 'Oszi ..............................';
    Texte[10] := 'Ende ( ESC ) ......................';
    Texte[11] := '*';

    Balken_Menue(Menu);

    Case Menu of
      1 : Begin
            Termeingabe;
            Werte_Da := True
          end;
      2 : Edit_Funktionswerte;
      3 : Edit_Funktion;
      4 : ADCEinlesen;
      5 : Tabelle_Plotten;
      6 : ADCTabDel;
      7 : ADCLoad;
      8 : ADCSave;
      9 : Oszi;
    end
  until Menu = 10;
  Cursorein;
  OrgCursor;
  ClrScr
end.