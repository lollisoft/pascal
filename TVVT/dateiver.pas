Program Dateiverwaltung;

Uses Crt, Dos, Video;


Procedure Freigabe;
begin
end;

Procedure aufreum;
begin
end;

Procedure Daten_Vernichten;
var  Hilf : Pointer;
begin

  while Zusatz_List <> Nil do
  begin
    Hilf := Zusatz_List;
    Zusatz_List := Zusatz_List^.Next;
    Dispose(Hilf)
  end;
end;

Procedure Zeiger_Reset;
begin
  Struckt_List_end := Struckt_List;
  Hilf_List_end := Hilf_List
end;

Procedure Zeiger_Trennen;
begin
  Alt_Hilf := Hilf_List_end^.Next;
  Hilf_List_end^.Next := Nil;

  Alt_Struckt := Struckt_List_end^.Next;
  Struckt_List_end^.Next := Nil
end;

Function Opened: Boolean;
begin
  Opened := True;
  if Hilf_List = Nil then Opened := False
end;

Procedure Scann;
var ch: char;
begin
  if Nachgetragen = False then
  begin
    Fehler('Wollen Sie wirklich die alte Liste zerstîren ( J/N ) ? ',ch);
    if ch = 'J' then
    begin
      Daten_Vernichten;
      Zeiger_Reset;
      Zeiger_Trennen;
      Dir;
      if Dir_Status then
      Nachgetragen := False;
    end
    else
    begin
      Fehler('Mîchten Sie die Liste anhÑngen ( J/N ) ? ',ch);
      if ch = 'J' then
      begin
        Dir;
        If Dir_Status then
        Nachgetragen := False
      end
    end
  end
  else
  begin
    if Opened then
    begin
      Daten_Vernichten;
      Zeiger_Reset;
      Zeiger_Trennen
    end;
    Dir;
    if Dir_Status then
    Nachgetragen := False
  end
end;

Procedure Disk_Auto_Scann;
var ch : char;
begin
  repeat
    Mask := 'A:\*.*';
    N := Mask;
    (*$I-*)
    if memavail > 1024 then
    begin
      dir;
      Fehler('Weiter ( J/N ) ? ',ch)
    end
    else
    begin
      wait('Speicher ist voll !',2);
      ch := 'N'
    end
    (*$I+*)
  until ch = 'N';
  nachgetragen := False
end;

Procedure D_A_S;
var ch : char;
begin
  if Nachgetragen = False then
  begin
    Fehler('Wollen Sie wirklich die alte Liste zerstîren ( J/N ) ? ',ch);
    if ch = 'J' then
    begin
      Daten_Vernichten;
      Zeiger_Reset;
      Zeiger_Trennen;
      Disk_Auto_Scann;
      Nachgetragen := False;
    end
    else
    begin
      Fehler('Mîchten Sie die Liste anhÑngen ( J/N ) ? ',ch);
      if ch = 'J' then
      begin
        Disk_Auto_Scann;
        Nachgetragen := False
      end
    end
  end
  else
  begin
    if Opened then
    begin
      Daten_Vernichten;
      Zeiger_Reset;
      Zeiger_Trennen
    end;
    Disk_Auto_Scann;
    Nachgetragen := False
  end
end;

Function Dateien_Vorhanden: boolean;
begin
  Dateien_Vorhanden := True;
  (*$I-*)
  reset(StrucktDatafile);
  if IOResult <> 0 then Dateien_Vorhanden := False;
  reset(HilfDatafile);
  if IOResult <> 0 then Dateien_Vorhanden := False;
  reset(LabelDatafile);
  if IOResult <> 0 then Dateien_Vorhanden := False;
  reset(PfadDatafile);
  if IOResult <> 0 then Dateien_Vorhanden := False;
  reset(DateiDatafile);
  if IOResult <> 0 then Dateien_Vorhanden := False
end;

Procedure Close_all_Files;
begin
  close(StrucktDatafile);
  close(HilfDatafile);
  close(LabelDatafile);
  close(PfadDatafile);
  close(DateiDatafile);
  close(ZusatzDatafile)
end;

Procedure Open_all_Files;
begin
  ZusatzData_Datei_Oeffnen(ZusatzDatafile);
  DateiData_Datei_Oeffnen(DateiDatafile);
  PfadData_Datei_Oeffnen(PfadDatafile);
  LabelData_Datei_Oeffnen(LabelDatafile);
  HilfData_Datei_Oeffnen(HilfDatafile);
  StrucktData_Datei_Oeffnen(StrucktDatafile)
end;

{
Function M2_in_M1(var sl: sl_typ): boolean;
var Help : Hilf_List_Ptr_typ;
    sh   : Struckt_List_Ptr_typ;
    sh1,
    sh2  : String;

begin
  sl.vp1 := Nil;
  sl.vp2 := Nil;
  sl.vp3 := Nil;
  Help := M[1].h;
  M2_in_M1 := True;
  while (M[1].h <> Nil) and
        (M[1].h^.HilfData.Label_List^.StrucktData.Label_List^.LabelData.LabelData <> sl.s1) do
        begin
          M[1].h := M[1].h^.Next;
        end;
  if M[1].h <> Nil then
  begin
    sl.vp1 := M[1].h;  (* Zeiger von gefundenem Label *)
    M[1].h := M[1].h^.Next;
    while (M[1].h <> Nil) and
          (M[1].h^.HilfData.Pfad_List^.StrucktData.Pfad_List^.PfadData.PfadData <> sl.s2) do
          begin
            M[1].h := M[1].h^.Next
          end;
    if M[1].h <> Nil then
    begin
      sl.vp2 := M[1].h;
      sh := M[1].s;
      M[1].s := M[1].h^.HilfData.Pfad_List^.Next;
      while (M[1].s <> Nil) and
            (M[1].s^.StrucktData.Datei_List^.DateiData.DateiData <> sl.s3) do
            begin
              M[1].s := M[1].s^.Next
            end;
      if M[1].s = Nil then M2_in_M1 := False else sl.vp3 := M[1].s
    end else M2_in_M1 := False
  end
  else M2_in_M1 := False;
  M[1].s := sh;
  M[1].h := help
end;

Procedure Loesche_M2(sl: sl_typ);
var l1 : Label_List_Ptr_typ;
    l2 : Pfad_List_Ptr_typ;
    l3 : Datei_List_Ptr_typ;
    l4 : Struckt_List_Ptr_typ;
    l5 : Hilf_List_Ptr_typ;
begin
  (* Sichern der drei Zeiger die auf Label, Pfad und Datei zeigen *)
  l1 := sl.vp1^.HilfData.Label_List^.StrucktData.Label_List;
  l2 := sl.vp2^.HilfData.Pfad_List^.StrucktData.Pfad_List;
  l3 := sl.vp3^.StrucktData.Datei_List;
  (* Sichern des Struckt und Hilf Eintrages *)
  l4 := sl.vp3;
  l5 := sl.vp2;
  if l1^.Anzahl = 1 then begin
                           l1^.Back := l1^.Next;
                           Dispose(l1)
                         end
                         else dec(l1^.Anzahl);
  if l2^.Anzahl = 1 then begin
                           l2^.Back := l2^.Next;
                           Dispose(l2)
                         end
                         else dec(l2^.Anzahl);
  if l3^.Anzahl = 1 then begin
                           l3^.Back := l3^.Next;
                           Dispose(l3)
                         end
                         else dec(l3^.Anzahl);
  l4^.Back := l4^.Next;
  Dispose(l4);
  l5^.Back := l5^.Next;
  Dispose(l5)
end;
}

Procedure Mach_Bin_Baum(Var p: Global_List_Ptr_typ;
                        l, r : Longint);

var mitte        : Longint;

begin
  Mitte := (l + r) Div 2;

  P := Global_Feld_Ptr^[Mitte];

  Global_Feld_Ptr^[Mitte]^.Links := Nil;
  Global_Feld_Ptr^[Mitte]^.Rechts := Nil;

  if Mitte > l then Mach_Bin_Baum(Global_Feld_Ptr^[Mitte]^.Links,l,Mitte-1);
  if Mitte < r then Mach_Bin_Baum(Global_Feld_Ptr^[Mitte]^.Rechts,Mitte+1,r)
end;

Procedure Lade_Dateien;
var
    Struckt_Feld : Struckt_Feld_Ptr_typ;
    Label_Feld   : Label_Feld_Ptr_typ;
    Pfad_Feld    : Pfad_Feld_Ptr_typ;
    Datei_Feld   : Datei_Feld_Ptr_typ;
    Zusatz_Feld  : Zusatz_Feld_Ptr_typ;

    Lauf         : Hilf_List_Ptr_typ;





Procedure Lade_Hilf;
begin
  Anzahl := Filesize(HilfDatafile);
  if Anzahl <> 0 then
  begin
    new(Hilf_List);
    Hilf_List^.Back := Nil;
    Hilf_List_end := Hilf_List;
    Alt_Hilf := Nil;
    For A := 1 to Anzahl do
    begin
      Read(HilfDatafile,Hilf_List_end^.HilfData);
      New(Hilf_List_end^.Next);
      Hilf_List_end^.Next^.Back := Hilf_List_end;
      Hilf_List_end := Hilf_List_end^.Next
    end;
    Hilf_List_end := Hilf_List_end^.Back;
    Dispose(Hilf_List_end^.Next);
    Hilf_List_end^.Next := Nil
  end
end;

Procedure Lade_Struckt;
begin
  S_Anzahl := Filesize(StrucktDatafile);
  if S_Anzahl <> 0 then
  begin
    new_array(Struckt_Feld,4,S_Anzahl);
    For A := 1 to S_Anzahl do
    begin
      new(Struckt_Feld^[A]);
      read(StrucktDatafile,Struckt_Feld^[A]^.StrucktData);
    end
  end
end;

Procedure Wandle_Hilf;
begin
  Lauf := Hilf_List;
  while Lauf <> Nil do
  begin
    case Lauf^.HilfData.art of
     3  : begin
            Stelle := Lauf^.HilfData.D_Label_List;
            Lauf^.HilfData.art := 1;
            Lauf^.HilfData.Label_List := Struckt_Feld^[Stelle]
          end;
     4  : begin
            Stelle := Lauf^.HilfData.D_Pfad_List;
            Lauf^.HilfData.art := 2;
            Lauf^.HilfData.Pfad_List := Struckt_Feld^[Stelle]
          end
    end;
    Lauf := Lauf^.Next
  end
end;

Procedure Lade_Zusatz;
begin
  Z_Anzahl := Filesize(ZusatzDatafile);
  if Z_Anzahl <> 0 then
  begin
    new_array(Zusatz_Feld,4,Z_Anzahl);
    For A := 1 to Z_Anzahl do
    begin
      new(Zusatz_Feld^[A]);
      read(ZusatzDatafile,Zusatz_Feld^[A]^.ZusatzData)
    end
  end
end;

Procedure Wandle_Zusatz;
begin
  For A := 1 to Z_Anzahl do
  begin
    Zusatz_Feld^[A]^.ZusatzData.sl :=
    Struckt_Feld^[Longint(Zusatz_Feld^[A]^.ZusatzData.sl)];
    Zusatz_Feld^[A]^.ZusatzData.sp :=
    Struckt_Feld^[Longint(Zusatz_Feld^[A]^.ZusatzData.sp)]
  end
end;

Procedure Lade_Label;
begin
  L_Anzahl := Filesize(LabelDatafile);
  if L_Anzahl <> 0 then
  begin
    new_array(Label_Feld,4,L_Anzahl);
    For A := 1 to L_Anzahl do
    begin
      new(Label_Feld^[A]);
      read(LabelDatafile,Label_Feld^[A]^.LabelData)
    end
  end
end;

Procedure Lade_Pfad;
begin
  P_Anzahl := Filesize(PfadDatafile);
  if P_Anzahl <> 0 then
  begin
    new_array(Pfad_Feld,4,P_Anzahl);
    For A := 1 to P_Anzahl do
    begin
      new(Pfad_Feld^[A]);
      read(PfadDatafile,Pfad_Feld^[A]^.PfadData)
    end
  end
end;

Procedure Lade_Datei;
begin
  D_Anzahl := Filesize(DateiDatafile);
  if D_Anzahl <> 0 then
  begin
    new_array(Datei_Feld,4,D_Anzahl);
    For A := 1 to D_Anzahl do
    begin
      new(Datei_Feld^[A]);
      read(DateiDatafile,Datei_Feld^[A]^.DateiData);
    end
  end
end;

Procedure Wandle_Struckt;
Begin
  For A :=1 to S_Anzahl do
  begin
    case Struckt_Feld^[A]^.StrucktData.art of
     4  : begin
            Stelle := Struckt_Feld^[A]^.StrucktData.D_Label_List;
            Struckt_Feld^[A]^.StrucktData.art := 1;
            Struckt_Feld^[A]^.StrucktData.Label_List := Label_Feld^[Stelle]
          end;
     5  : begin
            Stelle := Struckt_Feld^[A]^.StrucktData.D_Pfad_List;
            Struckt_Feld^[A]^.StrucktData.art := 2;
            Struckt_Feld^[A]^.StrucktData.Pfad_List := Pfad_Feld^[Stelle]
          end;
     6  : begin
            Stelle := Struckt_Feld^[A]^.StrucktData.D_Datei_List;
            Struckt_Feld^[A]^.StrucktData.art := 3;
            Struckt_Feld^[A]^.StrucktData.Datei_List := Datei_Feld^[Stelle]
          end
    end
  end
end;

Procedure Wandle_Datei;
begin
  For A := 1 to D_Anzahl do
  begin
    Stelle := Longint(Datei_Feld^[A]^.DateiData.Loc);
    Datei_Feld^[A]^.DateiData.Loc := Zusatz_Feld^[Stelle]
  end
end;

begin
  Open_all_Files;
  Lade_Hilf;     (* Als Liste *)
  Lade_Struckt;  (* Als Feld  *)
  Wandle_Hilf;
  Lade_Zusatz;   (* Als Feld  *)
  Wandle_Zusatz;


  Lade_Label;  (* Als Feld  *)
  Lade_Pfad;   (* Als Feld  *)
  Lade_Datei;  (* Als Feld  *)
  Wandle_Struckt;
  Wandle_Datei;


(* --- Verketten der Strucktliste --- *)

  For A := 1 to S_Anzahl-1 do
  begin
    Struckt_Feld^[A]^.Next := Struckt_Feld^[A+1];
    Struckt_Feld^[A+1]^.Back := Struckt_Feld^[A]
  end;
  Struckt_Feld^[1]^.Back := Nil;
  Struckt_Feld^[S_Anzahl]^.Next := Nil;


  Struckt_List := Struckt_Feld^[1];
  Struckt_List_end := Struckt_Feld^[S_Anzahl];

  Dispose_Array(Struckt_Feld,4,S_Anzahl);
  Struckt_Feld := Nil;



(* ---    Umwandeln des Restes    --- *)

  For A := 1 to Z_Anzahl-1 do
  begin
    Zusatz_Feld^[A]^.Next := Zusatz_Feld^[A+1];
    Zusatz_Feld^[A+1]^.Back := Zusatz_Feld^[A]
  end;
  Zusatz_Feld^[1]^.Back := Nil;
  Zusatz_Feld^[Z_Anzahl]^.Next := Nil;

  Zusatz_List := Zusatz_Feld^[1];
  Zusatz_List_end := Zusatz_Feld^[Z_Anzahl];

  Dispose_Array(Zusatz_Feld,4,Z_Anzahl);
  Zusatz_Feld := Nil;

  (* Mache die BÑume Label, Pfad und Datei: *)

  Global_Feld_Ptr := Pointer(Datei_Feld);
  Mach_Bin_Baum(Global_List_Ptr_typ(Datei_List),1,D_Anzahl);
  Dispose_Array(Datei_Feld,4,D_Anzahl);
   Datei_Feld := Nil;

  Global_Feld_Ptr := Pointer(Pfad_Feld);
  Mach_Bin_Baum(Global_List_Ptr_typ(Pfad_List),1,P_Anzahl);
  Dispose_Array(Pfad_Feld,4,P_Anzahl);
  Pfad_Feld := Nil;

  Global_Feld_Ptr := Pointer(Label_Feld);
  Mach_Bin_Baum(Global_List_Ptr_typ(Label_List),1,L_Anzahl);
  Dispose_Array(Label_Feld,4,L_Anzahl);
  Label_Feld := Nil;

  Close_all_Files

end;



Procedure Test_Masken_Syntax;
var Ext : String[4];

Procedure Bearbeite_String(var Mask: String);

begin
  (* Lîsche '?' direckt vor dem '*' *)

    repeat
      If Mask[Pos('*', Mask)-1] = '?' then
        delete(Mask, Pos('*', Mask)-1, 1)
    until Mask[Pos('*', Mask)-1] <> '?';

    (* Nach erstem '*' abschneiden, wenn noch Zeichen da sind: *)

    If Pos('*', Mask) <> 0 then
      If Length(Mask) > Pos('*', Mask) then
        delete(Mask, Pos('*', Mask)+1, Length(Mask)-Pos('*', Mask))
end;

begin
  Pfad := '';
  Befehl := '';
  Extender := '';

  Fsplit(Maske, Pfad, Befehl, Extender);

  Bearbeite_String(Befehl);
  Bearbeite_String(Extender);
  Maske := Befehl+Extender
end;



Procedure Put_Mx_on_My(x,y: byte);

begin

(* Diese Prozedur hÑngt an die jeweiligen Listen x *)
(* die Listen aus y hinten an: *)

  with M[y] do
  begin
    eh^.Next := M[x].h;
    es^.Next := M[x].s;

    M[x].h^.Back := eh;
    M[x].s^.Back := es;
  end;
end;

var
    Struckt_Feld : Struckt_Feld_Ptr_typ;
    Label_Feld   : Label_Feld_Ptr_typ;
    Pfad_Feld    : Pfad_Feld_Ptr_typ;
    Datei_Feld   : Datei_Feld_Ptr_typ;
    Zusatz_Feld  : Zusatz_Feld_Ptr_typ;


{$F+}
Procedure L_Speichern(Root: Pointer; Stelle: Longint);
begin
  Label_Feld^[Stelle] := Label_List_Ptr_typ(Root)
end;

Procedure P_Speichern(Root: Pointer; Stelle: Longint);
begin
  Pfad_Feld^[Stelle] := Pfad_List_Ptr_typ(Root)
end;

Procedure D_Speichern(Root: Pointer; Stelle: Longint);
begin
  Datei_Feld^[Stelle] := Datei_List_Ptr_typ(Root)
end;

{$F-}

Procedure Speichere_Dateien;

type Speichern = Procedure(Root: Pointer; Stelle: Longint);

var Speichere    : Speichern;
    Lauf         : Hilf_List_Ptr_typ;



Procedure Zaehle_Elemente(Root: Global_List_Ptr_typ);
begin
  if Root <> Nil then
    if Root^.Anzahl > 0 then
    begin
      Zaehle_Elemente(Root^.Rechts);
      Inc(Anzahl);
      Zaehle_Elemente(Root^.Links)
    end
end;


Procedure Strucktfeld_Bauen;
var Hilf : Pointer;
begin
  (* Wichtig: *)

  Anzahl := 0;
  Hilf := Struckt_List;

  (* ZÑhlen der Elemente: *)

  while Struckt_List <> Nil do
  begin
    Inc(Anzahl);
    Struckt_List := Struckt_List^.Next
  end;

  (* GezÑhlte Elemente: *)

  S_Anzahl := Anzahl;

  (* Strucktfeld reservieren: *)

  New_Array(Struckt_Feld,4,S_Anzahl);

  (* Strucktfeld mit werten belegen: *)

  (* Wichtig: *)
  Struckt_List := Hilf;

  for a := 1 to S_Anzahl do
  begin
    Struckt_Feld^[A] := Struckt_List;
    Struckt_Feld^[A]^.Back := Pointer(A);
    Struckt_List := Struckt_List^.Next;
  end;
  Struckt_List := Hilf
end;



Procedure Zusatz_Umlenken;
var hilf : Pointer;
begin
  Hilf := Zusatz_List;
  While Zusatz_List <> Nil do
  begin
    Zusatz_List^.ZusatzData.sl := Zusatz_List^.ZusatzData.sl^.Back;
    Zusatz_List^.ZusatzData.sp := Zusatz_List^.ZusatzData.sp^.Back;
    Zusatz_List := Zusatz_List^.Next
  end;
  Zusatz_List := Hilf
end;

Procedure Baue_Feld(Root: Global_List_Ptr_typ);
var Hilf : Global_List_Ptr_typ;
begin
  if Root <> Nil then
  begin
    Baue_Feld(Root^.Links);

    if Root^.Anzahl <> 0 then

    begin
      Inc(Anzahl);
      Root^.Links := Pointer(Anzahl);
      Speichere(Root, Anzahl)
    end;

    Baue_Feld(Root^.Rechts)
  end
end;


Procedure L_P_D_Felder_Bauen;

begin
  Anzahl := 0;
  Zaehle_Elemente(Global_List_Ptr_typ(Label_List));
  L_Anzahl := Anzahl;
  New_Array(Label_Feld,4,Anzahl);
  Speichere := L_Speichern;
  Anzahl := 0;
  Baue_Feld(Global_List_Ptr_typ(Label_List));

  Anzahl := 0;
  Zaehle_Elemente(Global_List_Ptr_typ(Pfad_List));
  P_Anzahl := Anzahl;
  New_Array(Pfad_Feld,4,Anzahl);
  Speichere := P_Speichern;
  Anzahl := 0;
  Baue_Feld(Global_List_Ptr_typ(Pfad_List));

  Anzahl := 0;
  Zaehle_Elemente(Global_List_Ptr_typ(Datei_List));
  D_Anzahl := Anzahl;
  New_Array(Datei_Feld,4,Anzahl);
  Speichere := D_Speichern;
  Anzahl := 0;
  Baue_Feld(Global_List_Ptr_typ(Datei_List));

end;

Procedure Hilf_Umlenken;
var Hilf : Pointer;
begin
  Hilf := Hilf_List;
  For A := 1 to S_Anzahl do
    case Struckt_Feld^[a]^.StrucktData.Art of
      4  : begin
             Hilf_List^.HilfData.D_Label_List := A;
             Hilf_List^.HilfData.art := 3;
             Hilf_List := Hilf_List^.Next
           end;
      5  : begin
             Hilf_List^.HilfData.D_Pfad_List := A;
             Hilf_List^.HilfData.art := 4;
             Hilf_List := Hilf_List^.Next
           end
    end;
  Hilf_List := Hilf
end;

Procedure Zusatz_Feld_Bauen;
var
    Hilf   : Pointer;
begin
  Anzahl := 0;
  Hilf := Zusatz_List;
  while Zusatz_List <> Nil do
  begin
    Inc(Anzahl);
    Zusatz_List^.Back := Pointer(Anzahl);
    Zusatz_List := Zusatz_List^.Next
  end;
  Z_Anzahl := Anzahl;
  Zusatz_List := Hilf;
  New_Array(Zusatz_Feld,4,Anzahl);

  (* Feld mit aktuellem Zeiger der Zusatzliste belegen: *)

  For a := 1 to Anzahl do
  begin
    Zusatz_Feld^[a] := Zusatz_List;
    Zusatz_List := Zusatz_List^.Next
  end;
  Zusatz_List := Hilf
end;

Procedure Struckt_Umlenken;
begin
  for A := 1 to S_Anzahl do
  begin
    case Struckt_Feld^[A]^.StrucktData.art of

      1 : begin
            Struckt_Feld^[A]^.StrucktData.D_Label_List :=
            Longint(Struckt_Feld^[A]^.StrucktData.Label_List^.Links);
            Struckt_Feld^[A]^.StrucktData.art := 4
          end;

      2 : begin
            Struckt_Feld^[A]^.StrucktData.D_Pfad_List :=
            Longint(Struckt_Feld^[A]^.StrucktData.Pfad_List^.Links);
            Struckt_Feld^[A]^.StrucktData.art := 5
          end;

      3 : begin
            Struckt_Feld^[A]^.StrucktData.D_Datei_List :=
            Longint(Struckt_Feld^[A]^.StrucktData.Datei_List^.Links);
            Struckt_Feld^[A]^.StrucktData.art := 6
          end
    end
  end
end;

Procedure Datei_Umlenken;
begin
  For A := 1 to D_Anzahl do
    Datei_Feld^[A]^.DateiData.Loc := Datei_Feld^[A]^.DateiData.Loc^.Back
end;



begin

  (* Listen und BÑume in Felder umwandeln: *)

  L_P_D_Felder_Bauen; (* Erzeugen der Felder fÅr Label, Pfad und Datei *)
  Strucktfeld_Bauen;  (* s. Befehl *)
  Struckt_Umlenken;   (* Zeiger, die auf Labels, Pfade und Dateien zeigen *)
                      (* werden in Feldzeiger ( Longint ) umgewandelt     *)
  Zusatz_Umlenken;    (* Wandelt die Zeiger sl und sp in Longint um.      *)
  Hilf_Umlenken;      (* Wandelt Zeiger aus der Hilfliste um in Longint.  *)
  Zusatz_Feld_Bauen;
  Datei_Umlenken;

  (* Felder abspeichern: *)

  Open_all_Files;


  for A := 1 to S_Anzahl do
    write(StrucktDatafile,Struckt_Feld^[A]^.StrucktData);

  While Hilf_List <> Nil do
  begin
    write(HilfDatafile,Hilf_List^.HilfData);
    Hilf_List := Hilf_list^.Next
  end;



  for A := 1 to Z_Anzahl do
    write(ZusatzDatafile,Zusatz_Feld^[A]^.ZusatzData);

  for A := 1 to L_Anzahl do
    write(LabelDatafile,Label_Feld^[A]^.LabelData);

  for A := 1 to P_Anzahl do
    write(PfadDatafile,Pfad_Feld^[A]^.PfadData);

  for A := 1 to D_Anzahl do
    write(DateiDatafile,Datei_Feld^[A]^.DateiData);

  Close_all_Files;

end;


Procedure Set_All_Nil;
begin
  Struckt_List := Nil;
  Struckt_List_end := Nil;
  Alt_Struckt := Nil;

  Hilf_List := Nil;
  Hilf_List_end := Nil;
  Alt_Hilf := Nil;

  Zusatz_List := Nil;
  Zusatz_List_end := Nil;

  Label_List := Nil;
  Pfad_List := Nil;
  Datei_List := Nil;

  Datei_Dif := False;
  Pfad_Dif := False;
  Label_Dif := False;
  Nachgetragen := True
end;

Procedure Copy_Baum_Data_Mx_to_Baum_Data_My(x, y: Byte);
var Hilf1,
    Hilf2,
    Hilf3,
    Hilf4  : Struckt_List_Ptr_typ;
begin
  Hilf4 := M[x].s;
  Copy_M_in_Work(y);

  While M[x].s <> Nil do
  begin
    case M[x].s^.StrucktData.art of
      1 : begin
            If M[x].s^.StrucktData.Label_List^.LabelData.Anzahl > 0 then
            begin
              Speichere_Label(M[x].s^.StrucktData.Label_List^.LabelData.LabelData,
                              Label_List,
                              Label_Dif);
              M[x].s^.StrucktData.Label_List := labeladress;
              Hilf1 := M[x].s
            end
          end;
      2 : begin
            If M[x].s^.StrucktData.Pfad_List^.PfadData.Anzahl > 0 then
            begin
              Speichere_Pfad(M[x].s^.StrucktData.Pfad_List^.PfadData.PfadData,
                             Pfad_List,
                             Pfad_Dif);
              M[x].s^.StrucktData.Pfad_List := pfadadress;
              Hilf2 := M[x].s
            end
          end;
      3 : begin
            If M[x].s^.StrucktData.Datei_List^.DateiData.Anzahl > 0 then
            begin
              Speichere_Datei(M[x].s^.StrucktData.Datei_List^.DateiData.DateiData,
                              Datei_List,
                              Datei_Dif);
              M[x].s^.StrucktData.Datei_List := dateiadress;

              Speichere_Such(Hilf1,Hilf2,dateiadress);
              if Dateiadress^.dateidata.loc = Nil then halt
            end
          end
    end;
    If M[x].s = Nil then halt;
    M[x].s := M[x].s^.Next
  end;
  Copy_Work_in_M(y);
  M[x].s := Hilf4
end;




Procedure Nachtrag;
var Help      : Hilf_List_Ptr_typ;
    Hilf_Lauf : Hilf_List_Ptr_typ;

Procedure Loesche_Disk_Bereich;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    Lauf : Struckt_List_Ptr_typ;

Procedure Loesche_Element(Var Anfang,
                              Ende,
                              Element          : Listen_Ptr_typ);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure Loesche_Struckt_Element(var Anfang,
                                      Ende,
                                      Element          : Struckt_List_Ptr_typ);
var Hilf : Struckt_List_Ptr_typ;
begin
  Hilf := Element;
  Element := Element^.Next;
  Loesche_Element(Listen_Ptr_typ(Anfang),
                  Listen_Ptr_typ(Ende),
                  Listen_Ptr_typ(Hilf));
  Dispose(Hilf)
end;


Procedure Loesche_Hilf_Element(var Anfang,
                                   Ende,
                                   Element          : Hilf_List_Ptr_typ);
var Hilf : Hilf_List_Ptr_typ;
begin
  Hilf := Element;
  Element := Element^.Next;
  Loesche_Element(Listen_Ptr_typ(Anfang),
                  Listen_Ptr_typ(Ende),
                  Listen_Ptr_typ(Hilf));
  Dispose(Hilf)
end;


Procedure Loesche_Zusatz_Element(var Anfang,
                                   Ende,
                                   Element    : Zusatz_List_Ptr_typ);
var Hilf: Zusatz_List_Ptr_typ;
begin
  Hilf := Element;
  Element := Element^.Next;
  Loesche_Element(Listen_Ptr_typ(Anfang),
                  Listen_Ptr_typ(Ende),
                  Listen_Ptr_typ(Hilf));
  Dispose(Hilf)
end;



Procedure Loesche_Label(var l: String);
begin
  l := Lauf^.StrucktData.Label_List^.LabelData.LabelData;
  Dec(Lauf^.StrucktData.Label_List^.LabelData.Anzahl);
  Loesche_Struckt_Element(M[2].s, M[2].es, Lauf);
  Loesche_Hilf_Element(M[2].h, M[2].eh, Hilf_Lauf)
end;

Procedure Loesche_Pfad(var p: String);
begin
  p := Lauf^.StrucktData.Pfad_List^.PfadData.PfadData;
  Dec(Lauf^.StrucktData.Pfad_List^.PfadData.Anzahl);
  Loesche_Struckt_Element(M[2].s, M[2].es, Lauf);
  Loesche_Hilf_Element(M[2].h, M[2].eh, Hilf_Lauf)
end;

Procedure Loesche_Datei(zl, zp : String);
var Zaehler : Byte;
    Help,
    Loc_Help : Zusatz_List_Ptr_typ;
begin
  Zaehler := 1;

  Help := Lauf^.StrucktData.Datei_List^.DateiData.Loc;
  Loc_Help := Help;

  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          Label_List^.LabelData.LabelData) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          Pfad_List^.PfadData.PfadData)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler)
  end;

  If Lauf^.StrucktData.Datei_List^.DateiData.Anzahl = 1 then
  begin
    Lauf^.StrucktData.Datei_List^.DateiData.Loc := Nil;
    Writeln('DateiData.Loc wird auf Nil gesetzt.')
  end
  else
    If Help = Loc_Help then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)

      Lauf^.StrucktData.Datei_List^.DateiData.Loc := Loc_Help^.Next;
      Writeln('DateiData.Loc wird weitergerÅckt.')
    end;


  Loesche_Zusatz_Element(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.Datei_List^.DateiData.Anzahl);

  Loesche_Struckt_Element(M[2].s, M[2].es, Lauf)
end;


begin

(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)

  Lauf := Hilf_Lauf^.HilfData.Label_List;

  (* Laufe die Strucktliste durch und lîsche die Daten: *)

  repeat
    case Lauf^.StrucktData.art of
      1 : Loesche_Label(l);
      2 : Loesche_Pfad(p);
      3 : Loesche_Datei(l, p);
    end
  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
end;


begin
  clrscr;

(* Speichern der gescannten Daten in M1: *)

  Copy_Work_in_M(1);

(* ZurÅcksetzen des Mediums Work: *)

  Set_All_Nil;

  Lade_Dateien;
  Copy_Work_in_M(2);

  Set_all_Nil;

  Help := M[1].h;

(* Entfernen von alten Daten: *)

  While Help <> Nil do
  begin
    Hilf_Lauf := M[2].h;

    (* Suche Label in Diskdaten: *)

    While (Hilf_Lauf <> Nil) and
          (Hilf_Lauf^.HilfData.Label_List^.StrucktData.
           Label_List^.LabelData.LabelData
           <>
           Help^.HilfData.Label_List^.StrucktData.
           Label_List^.LabelData.LabelData)

          do Hilf_Lauf := Hilf_Lauf^.Next;

    (* Wenn nicht Nil, dann ist ein alter
       Label unter diesem Namen vorhanden *)

    If Hilf_Lauf <> Nil then
      Loesche_Disk_Bereich;

    (* Hole neuen Label aus gescannten Daten: *)

    repeat
      Help := Help^.Next
    until (Help^.HilfData.art = 1) or
          (Help = Nil)
  end; (* Von While M1... *)

  If M[2].s <> Nil then
  begin
    Copy_Baum_Data_Mx_to_Baum_Data_My(1, 2);
    Put_Mx_on_My(1, 2);
    Copy_M_in_Work(2)
  end
  else Copy_M_in_Work(1);

  Speichere_Dateien
end;



Procedure Daten_Aktuallisieren;
var ch: char;
     b : boolean;
begin
  clrscr;
  repeat
    Auswahl1(ch);
    case ch of
      '1' : begin
              Mask := 'A:\*.*';
              N := Mask;
              Scann;
              write(chr(7))
            end;
      '2' : begin
              Mask := 'C:\*.*';
              N := Mask;
              Scann;
              write(chr(7))
            end;
      '3' : begin
              D_A_S;
              write(chr(7))
            end;
      '4' : begin
              Nachtrag;
              write(chr(7))
            end;
      '5' : begin
              Aufreum;
              write(chr(7))
            end;
      '6' : begin
              clrscr;
              write( 'Bitte geben Sie eine neue Maske ein: ');
              readln(Maske);
              Test_Masken_Syntax;
              Writeln(Maske);
              readln;
              clrscr
            end;
    end
  until ch = '0'
end;



Procedure Finde_Dateien_im_Speicher;
var Name : String12;
    gef  : Datei_List_Ptr_typ;

Function find(P: Datei_List_Ptr_typ): Datei_List_Ptr_typ;

begin
  if P <> Nil then
  begin
    if Name = P^.DateiData.DateiData then Find := P
    else
    begin
      if Name < P^.DateiData.DateiData then Find := find(P^.Links);
      if name > P^.DateiData.DateiData then Find := find(P^.Rechts)
    end
  end
  else Find := P
end;

begin
  clrscr;
  write('Bitte geben Sie einen Dateinamen an: ');
  readln(name);
  Name := Update(name);
  gef := Find(Datei_List);
  if Gef <> Nil then
  begin
    Writeln('Dateiname gefunden: ');
    writeln('Label: ',
    Gef^.DateiData.Loc^.ZusatzData.Sl^.StrucktData.Label_List^.LabelData.LabelData);
    writeln('Pfad:  ',
    Gef^.DateiData.Loc^.ZusatzData.Sp^.StrucktData.Pfad_List^.PfadData.PfadData);
    readln
  end
end;

Procedure Get_Datei;
begin
end;


Procedure Finde_Dateien_im_Dateisystem;
var Name        : String12;
    Gefunden,
    Min,
    Max         : Longint;
    DateiData   : DateiDatatyp;
    ZusatzData  : ZusatzDatatyp;
    StrucktData : StrucktDatatyp;
    LabelData   : Label_Data_typ;
    PfadData    : Pfad_Data_typ;

    Datei_Array : Array [1..24] of DateiDatatyp;

    Zeile,
    Anzahl      : Integer;

Procedure Finde(l, r : Longint);

var mitte        : Longint;


begin
  if l <= r then
  begin
    Mitte := (l + r) Div 2;
    Seek(DateiDataFile, Mitte);
    Read(DateiDataFile, DateiData);
    if DateiData.DateiData > Maske then Finde(l,Mitte-1);
    if DateiData.DateiData < Maske then Finde(Mitte+1,r);
    If DateiData.DateiData = Maske then Gefunden := Mitte
  end
end;


Procedure Datenausgabe;
var ch : char;
begin
  if Gefunden <> -1 then
  begin
    Writeln;
    Writeln('Es gibt ', DateiData.Anzahl, ' Datei(en).');
    if DateiData.Anzahl = 1 then
      Writeln('Zu finden ist sie auf:')
    else
      Writeln('Zu finden sind sie auf:');
    Writeln;
    For A := 1 to DateiData.Anzahl do
    begin
      if Filesize(ZusatzDataFile) < Longint(DateiData.Loc)-1 then halt;
      seek(ZusatzDataFile, Longint(DateiData.Loc)-2+A);
      Read(ZusatzDataFile, ZusatzData);

      Seek(StrucktDataFile, Longint(ZusatzData.Sl)-1);
      Read(StrucktDataFile, StrucktData);
      Seek(LabelDataFile, Longint(StrucktData.Label_List)-1);
      Read(LabelDataFile, LabelData);


      Seek(StrucktDataFile, Longint(ZusatzData.Sp)-1);
      Read(StrucktDataFile, StrucktData);
      Seek(PfadDataFile, Longint(StrucktData.Pfad_List)-1);
      Read(PfadDataFile, PfadData);

      Writeln('Label: ', LabelData.LabelData);
      Writeln('Pfad : ', PfadData.PfadData);
      Writeln;
      If A = 6 then
      begin
        ch := readkey;

        for b := 5 to 24 do
        begin
          gotoxy(1, Byte(b));
          clreol
        end;
        gotoxy(1, 5)
      end
    end;
  end
  else Writeln('Datei nicht vorhanden.');
end;

Function Passt_in_Maske(Name: String12):Boolean;
var Extender,
    Extender1,
    Befehl,
    Befehl1,
    Pfad,
    Pfad1    : String;
    Befehl_ist_gleich,
    Extender_ist_gleich : Boolean;

Procedure Behandle_Befehl;
begin
  (* Ist nur ein Stern vorhanden? *)

  If Befehl1[1] = '*' then Befehl_ist_gleich := True

  else

  (* Ist ein Stern vorhanden? *)

  If (Pos('*', Befehl1) <> 0) and

     (* Wenn der gegebene Dateiname nicht kleiner ist, als  *)
     (* die Maske ohne Stern, dann kann die Maske passen:   *)

     (Length(Befehl) >= Length(Befehl1) - 1) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen *)
       (* und nach Stern abtrennen:                      *)

     begin
       For A := 1 to Length(Befehl1) - 1 do
         If Befehl1[A] = '?' then Befehl[A] := '?';

       Delete(Befehl, Pos('*', Befehl1), Length(Befehl));
       Delete(Befehl1, Pos('*', Befehl1), 1)
     end
  else
    If (Not(Pos('*', Befehl1) <> 0)) and
       (Length(Befehl) = Length(Befehl1)) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen: *)

       For A := 1 to Length(Befehl1) - 1 do

         If Befehl1[A] = '?' then Befehl[A] := '?';

  If Befehl = Befehl1 then Befehl_ist_gleich := True

end;

Procedure Behandle_Extender;
begin
  (* Ist nur ein Stern vorhanden? *)

  If Extender1[2] = '*' then Extender_ist_gleich := True

  else

  (* Ist ein Stern vorhanden? *)

  If (Pos('*', Extender1) <> 0) and

     (* Wenn der gegebene Dateiname nicht kleiner ist, als  *)
     (* die Maske ohne Stern, dann kann die Maske passen:   *)

     (Length(Extender) >= Length(Extender1) - 1) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen *)
       (* und nach Stern abtrennen:                      *)

       begin
         For A := 1 to Length(Extender1) - 1 do
           If Extender1[A] = '?' then Extender[A] := '?';
         Delete(Extender, Pos('*', Extender1), Length(Extender));
         Delete(Extender1, Pos('*', Extender1), 1)
       end
  else
    If (Not(Pos('*', Extender1) <> 0)) and
       (Length(Extender) = Length(Extender1)) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen: *)

       For A := 1 to Length(Extender1) - 1 do

         If Extender1[A] = '?' then Extender[A] := '?';

  If Extender = Extender1 then Extender_ist_gleich := True

end;


begin
  Passt_in_Maske := False;
  Befehl_ist_gleich := False;
  Extender_ist_gleich := False;

  FSplit(Name, Pfad, Befehl, Extender);
  FSplit(Maske, Pfad1, Befehl1, Extender1);

  Behandle_Befehl;
  Behandle_Extender;

  If Befehl_ist_gleich and
     Extender_ist_gleich   then Passt_in_Maske := True

end;

Procedure Feld_Aufbauen;
begin
  Window(1, 1, 13, 24);
  TextBackground(0);
  clrscr;
  For B := 1 to Anzahl do Writexy(1, B, Datei_Array[B].DateiData)
end;

Procedure Feld_Auswahl(Var Ch: Char);
var Zeile: Integer;
begin
  Window(1, 1, 13, 24);
  TextBackground(5);
  Zeile := Integer(Ch);
  Writexy(1, Byte(Zeile), Datei_Array[Zeile].DateiData);
  Repeat
    Ch := Readkey;
    if Ch = #0 then Ch := Readkey;
    Case Ch of
      'H' : begin
              TextBackground(0);

              Writexy(1, Zeile, Datei_Array[Zeile].DateiData);

              TextBackground(5);

              If Zeile = 1 then

              begin
                Zeile := Anzahl;
                Writexy(1, 24, Datei_Array[Anzahl].DateiData)
              end

              else

              begin
                Dec(Zeile);
                Writexy(1, Zeile, Datei_Array[Zeile].DateiData)
              end
            end;
      'P' : begin
              TextBackground(0);

              Writexy(1, Zeile, Datei_Array[Zeile].DateiData);

              TextBackground(5);

              If Zeile = Anzahl then

              begin
                Zeile := 1;
                Writexy(1, Zeile,  Datei_Array[Zeile].DateiData)
              end

              else

              begin
                Inc(Zeile);
                Writexy(1, Zeile, Datei_Array[Zeile].DateiData)
              end
            end;
    end;  (* Case *)
  until Ch = ' ';
  Ch := Char(Zeile)
end;

Procedure Feld_Ausgabe(Nummer: Integer);
begin
  DateiData := Datei_Array[Nummer];
  Window(13, 1, 80, 24);
  clrscr;
  Datenausgabe
end;


begin
  clrscr;
  Write('Bitte geben Sie eine Suchmaske an: ');
  Readln(Maske);
  clrscr;
  Maske := UpDate(Maske);

  Test_Masken_Syntax;

  Open_all_Files;
  Max := Filesize(DateiDataFile);

  if (Pos('*', Maske) = 0) and
     (Pos('?', Maske) = 0) then
  begin
    Gefunden := -1;
    Finde(0,max-1);
    Datenausgabe;
    readln;
    clrscr
  end
  else
  While (Not(Eof(DateiDataFile))) and
        (Not(ch = 'N')) do
  begin

    Anzahl := 0;
    repeat
      Read(DateiDataFile,DateiData);
      If Passt_in_Maske(DateiData.DateiData) then
      begin
        Gefunden := 0;
        Inc(Anzahl);
        Datei_Array[Anzahl] := DateiData
      end;
    until (Anzahl = 24) or Eof(DateiDataFile);

    If Anzahl <> 0 then
    begin
      Feld_Aufbauen;
      Zeile := 1;
      Repeat
        Feld_Auswahl(Char(Zeile));
        Feld_Ausgabe(Zeile);
        ch := Readkey;
        If ch = #0 then ch := Readkey;
        clrscr
      until ch = 'R';
      Fehler('Weiter (J / N) ? ',ch)
    end
    else Wait('Dateien mit dieser Maske gibt es nicht.', 2)
  end;
  Window(1, 1, 80, 24);
  TextBackground(0);
  Close_all_Files
end;

Procedure Datei_Such_Dienst;
var ch : char;
begin
  clrscr;
  Auswahl2(ch);
  case ch of
    '1' : Get_Datei;
    '2' : Finde_Dateien_im_Dateisystem;
    '3' : Finde_Dateien_im_Speicher;
  end
end;

Procedure Delete_Dateien;
begin
end;





Procedure Initfiles;
begin
  assign(StrucktDatafile,Strucktfilename);
  assign(HilfDatafile,Hilffilename);
  assign(DateiDatafile,Dateifilename);
  assign(LabelDatafile,Labelfilename);
  assign(PfadDatafile,Pfadfilename);
  assign(ZusatzDatafile,Zusatzfilename);
end;

Procedure Status;
begin
  Window(1,1,80,25);
  For ch := 'a' to 'j' do
  begin
    With Medium_Feld[upcase(ch)] do
    begin
      if l <> Nil then writexy(Byte(ch)-96,25,upcase(ch))
                  else writexy(Byte(ch)-96,25,ch)
    end;
    if Label_List <> Nil then writexy(11,25,'    WORK with '+cha)
                         else writexy(11,25,'    work with '+cha)

  end;
  writexy(30,25,'leer oder VOLL');
  Window(1,1,80,24)
end;
var ja : char;

begin
(*  Cursor_Aus;     *)

  cha := ' ';
  Nachgetragen := True;
  Raus := 'A';
  for ch := 'A' to 'J' do
  begin
    Medium_Feld[ch].Datei_Dif := False;
    Medium_Feld[ch].Pfad_Dif := False;
    Medium_Feld[ch].Label_Dif := False;
    With Medium_Feld[ch] do
    begin
      n  := '';
      L  := Nil;
      P  := Nil;
      D  := Nil;
      S  := Nil;
      es := Nil;
      as := Nil;
      H  := Nil;
      eh := Nil;
      ah := Nil;
      Z  := Nil;
      ez := Nil;
      na := Nachgetragen;
    end
  end;
  M[1] := Medium_Feld['A'];
  M[2] := Medium_Feld['A'];
  N := '';

  Set_all_Nil;

  Initfiles;

  repeat
    Z_Hilf := 0;
    clrscr;
    Status;
    auswahl(ziffer);
    case ziffer of
      '1' : Daten_Aktuallisieren;
      '2' : Zeige_Dateien;
      '3' : Datei_Such_Dienst;
      '4' : Delete_Dateien;
      '5' : begin
              repeat
                Fehler('Bitte geben Sie eine Medien - Nummer ( A - J ) an :',ch);
              until ch in ['A'..'J'];
              cha := ch;
              Get_Medium(ch)
            end;
      '6' : begin
              if Struckt_List = nil then
                if Test_Files then
                  Lade_Dateien
                else
                  wait('Keine Dateien vorhanden!',2)

              else wait('Work ist nicht leer!',2);
              write(chr(7))
            end;
      '7' : begin
              clrscr;
              write('StrucktDatafile : ',Strucktfilename:12,': ');
              readln(Strucktfilename);

              write('HilfDatafile    : ',Hilffilename:12,': ');
              readln(Hilffilename);

              write('LabelDatafile   : ',Labelfilename:12,': ');
              readln(Labelfilename);

              write('PfadDatafile    : ',Pfadfilename:12,': ');
              readln(Pfadfilename);

              write('DateiDatafile   : ',Dateifilename:12,': ');
              readln(Dateifilename);

              write('ZusatzDatafile  : ',Zusatzfilename:12,': ');
              readln(Zusatzfilename);

              Initfiles
            end;
      '8' : Begin
              Speichere_Dateien;
              write(chr(7))
            end;
      '9' : begin
              H_Hilf := Hilf_List;
              clrscr;
              While Hilf_List <> Nil do
              begin
                if Hilf_List^.HilfData.art = 1 then
                writeln(Hilf_List^.HilfData.Label_List^.
                        StrucktData.Label_List^.LabelData.LabelData);
                Hilf_List := Hilf_List^.Next
              end;
              Hilf_List := H_Hilf;
              Readln
            end
    end
  until ziffer = '0';
  Window(1,1,80,25);
(*  Cursor_Ein; *)
  clrscr
end.


