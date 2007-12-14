Program Dateiver;


{$M 65520,8192,655360}
{$X+,S+}


Uses Crt, Dos, Objects, Drivers, Views, Menus, Dialogs,
     StdDlg, MsgBox, LabHist, Editors, App, Vid, CursCont;

Const
    cmScann                     = 101;
    cmDiskAutoScann             = 102;
    cmNachtrag                  = 103;
    cmAufreum                   = 104;

    cmLaden                     = 106;
    cmSpeichern                 = 107;
    cmDatenAufgeben             = 108;
    cmNewFileName               = 109;
    cmChangeDir                 = 110;

    cmZeigeDaten                = 112;
    cmSucheDaten                = 113;
    cmSucheMehrfache            = 114;
    cmMemory                    = 115;

    cmNextDatei                 = 116;
    cmNextPfad                  = 117;
    cmNextLabel                 = 118;
    cmDateiMaske                = 119;
    cmEnde                      = 120;

    FileName           : String = 'Dateiver';

Type
  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Constructor Init;
    Destructor  Done; Virtual;
    Procedure   HandleEvent(Var Event: TEvent); Virtual;
    Procedure   InitMenuBar;                    Virtual;
    Procedure   InitStatusLine;                 Virtual;
    Procedure   OutOfMemory;                    Virtual;
  end;



var
    Struckt_Feld : Struckt_Feld_Ptr_typ;
    Label_Feld   : Label_Feld_Ptr_typ;
    Pfad_Feld    : Pfad_Feld_Ptr_typ;
    Datei_Feld   : Datei_Feld_Ptr_typ;
    Zusatz_Feld  : Zusatz_Feld_Ptr_typ;

    Laufwerk     : String[1];


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



Procedure Initfiles;
begin
  assign(StrucktDatafile,Strucktfilename);
  assign(HilfDatafile,Hilffilename);
  assign(DateiDatafile,Dateifilename);
  assign(LabelDatafile,Labelfilename);
  assign(PfadDatafile,Pfadfilename);
  assign(ZusatzDatafile,Zusatzfilename);
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


Constructor TDateiverApp.Init;
var
  H: Word;
  R: TRect;
begin
(*  H := PtrRec(HeapEnd).Seg - PtrRec(HeapPtr).Seg;
  if H > HeapSize then BufHeapSize := H - HeapSize else BufHeapSize := 0;
  InitBuffers; *)

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

  Laufwerk := '';


  TApplication.Init;

  DisableCommands([cmSpeichern,     cmZeigeDaten,
                   cmNachtrag,      cmAufreum,
                   cmDatenAufgeben               ]);
end;

Destructor TDateiverApp.Done;
Begin
  TApplication.Done
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
  Begin
    write(StrucktDatafile,Struckt_Feld^[A]^.StrucktData);
    Dispose(Struckt_Feld^[A])
  end;

  While Hilf_List^.Next <> Nil do
  begin
    write(HilfDatafile,Hilf_List^.HilfData);
    Hilf_List := Hilf_list^.Next;

    Dispose(Hilf_List^.Back)
  end;
  Write(HilfDatafile, Hilf_List^.HilfData);
  Dispose(Hilf_List);


  for A := 1 to Z_Anzahl do
  Begin
    write(ZusatzDatafile,Zusatz_Feld^[A]^.ZusatzData);
    Dispose(Zusatz_Feld^[A])
  end;

  for A := 1 to L_Anzahl do
  Begin
    write(LabelDatafile,Label_Feld^[A]^.LabelData);
    Dispose(Label_Feld^[A])
  end;

  for A := 1 to P_Anzahl do
  Begin
    write(PfadDatafile,Pfad_Feld^[A]^.PfadData);
    Dispose(Pfad_Feld^[A])
  end;

  for A := 1 to D_Anzahl do
  Begin
    write(DateiDatafile,Datei_Feld^[A]^.DateiData);
    Dispose(Datei_Feld^[A])
  end;

  Dispose_Array(Label_Feld  , 4, L_Anzahl);
  Dispose_Array(Pfad_Feld   , 4, P_Anzahl);
  Dispose_Array(Datei_Feld  , 4, D_Anzahl);
  Dispose_Array(Struckt_Feld, 4, S_Anzahl);
  Dispose_Array(Zusatz_Feld , 4, Z_Anzahl);

  Close_all_Files;

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



Procedure TDateiverApp.HandleEvent(Var Event: TEvent);

function ExecDialog(P: PDialog; Data: Pointer): Word;
var
  Result: Word;
begin
  Result := cmCancel;
  P := PDialog(Application^.ValidView(P));
  if P <> nil then
  begin
    if Data <> nil then P^.SetData(Data^);
    Result := DeskTop^.ExecView(P);
    if (Result <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    Dispose(P, Done);
  end;
  ExecDialog := Result;
end;





function ReadString(DialogTyp: String): PDialog;
var
  D: PDialog;
  Control: PView;
  R: TRect;
begin
  R.Assign(0, 0, 38, 12);
  D := New(PDialog, Init(R, DialogTyp));
  with D^ do
  begin
    Options := Options or ofCentered;

    R.Assign(3, 3, 32, 4);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);

    R.Assign(32, 3, 35, 4);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(14, 9, 24, 11);
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

    SelectNext(False);
  end;
  ReadString := D;
end;




Procedure Scanner;
var ch    : char;
    LName : String;
begin
  LabelHistList := Nil;
  LName := '';
  if Nachgetragen = True then
  begin
    Dir(LName);
    if Dir_Status then
    Nachgetragen := False
  end
  else
    MessageBox('Es sind noch Daten im Arbeitsbereich!',
         nil, mfError + mfOkButton)
end;


Procedure Scann;
var Info     : ^String;
    Zahl     : String[5];

begin
  LabelHistList := Nil;
  New(Info);
  Repeat
    info^ := Laufwerk;
    ExecDialog(ReadString('Welches Laufwerk'), Info);
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  Scanner
end;

Procedure Disk_Auto_Scann;

var  Ch            : char;
     LName,
     LSName        : String;

begin
 LabelHistList := Nil;

 LName := '';

  repeat
    Mask := Laufwerk + ':\*.*';
    N := Mask;
    (*$I-*)
    if memavail > 1024 then
    begin
      Dir(LName);
      If LName <> '' then
        If LabelInLabelHistList(LName) then
          MessageBox('Sie haben entweder eine Festplatte gewÑhlt, oder eine Disk ' +
                     'nochmal eingelegt!', nil, mfOkButton)
        else
          PutLabelInHistList(LName);

      If MessageBox('Noch eine Disk einlesen?',
              nil, mfInformation + mfYesButton + mfNoButton) =
         cmYes then
           Ch := 'J'
         else
           Ch := 'N'
    end
    else
    begin
      MessageBox('Speicher ist voll !', Nil, mfOkButton);
      ch := 'N'
    end
    (*$I+*)
  until ch = 'N';

  Nachgetragen := False;

  DelLabelHistList
end;


Procedure DiskAutoScann;
Var Info : ^String;
Begin
  New(Info);
  Repeat
    info^ := Laufwerk;
    ExecDialog(ReadString('Welches Laufwerk'), Info);
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  Disk_Auto_Scann
end;

Procedure Nachtrag;
Begin
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
end;

Procedure Aufreum;
Begin
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
end;


Procedure LadeDateien;
Begin
  if Struckt_List = nil then
    if Test_Files then
      Lade_Dateien
    else
      MessageBox('Es sind Dateien unter diesem Namen vorhanden!',
             nil, mfError + mfOkButton)

  else MessageBox('Es sind noch Daten im Arbeitsbereich!',
             nil, mfError + mfOkButton);
end;

Procedure SpeichereDateien;
Begin
  Speichere_Dateien;
  Set_all_Nil
end;




procedure ChangeDir;
begin
  ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
end;

Procedure NewFileName;
var Info: ^String;
begin
  New(Info);
  Repeat
    info^ := FileName;
    ExecDialog(ReadString('Dateiname?'), Info);
    If Info^ = '' then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Info^ <> '';
  FileName := Info^;
  Dispose(Info)
end;



Procedure Test_Masken_Syntax(Var Maske: String);
var Ext : String[4];

Procedure Bearbeite_String(var Mask: String);

begin
  (* Lîsche '?' direckt vor dem '*' *)

    repeat
      If Mask[Pos('*', Mask)-1] = '?' then
        System.delete(Mask, Pos('*', Mask)-1, 1)
    until Mask[Pos('*', Mask)-1] <> '?';

    (* Nach erstem '*' abschneiden, wenn noch Zeichen da sind: *)

    If Pos('*', Mask) <> 0 then
      If Length(Mask) > Pos('*', Mask) then
       System.delete(Mask, Pos('*', Mask)+1, Length(Mask)-Pos('*', Mask))
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


Function Passt_in_Maske(Maske, Name: String12):Boolean;
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

       System.Delete(Befehl, Pos('*', Befehl1), Length(Befehl));
       System.Delete(Befehl1, Pos('*', Befehl1), 1)
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
         System.Delete(Extender, Pos('*', Extender1), Length(Extender));
         System.Delete(Extender1, Pos('*', Extender1), 1)
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





Procedure Zeige_Dateien;

Type LaengenTyp = Record
                    Lo,
                    Hi : Byte
                  end;

    TYPE
      ZeigeDateienData = RECORD
        Cluster0: WORD;
        TextLen0: WORD;
        TextRec0: ARRAY [0..12] OF CHAR;
      END;


var help      : Struckt_List_Ptr_typ;

    ch        : char;

    s2,
    s3        : string;
    Pfad      : String;

    Datei     : String12;

    Labell    : string;

    Hilf      : Datei_List_Ptr_typ;

    i         : integer;

    LocHilf   : Pointer;

    Data      : ZeigeDateienData;

    Laenge    : LaengenTyp;

    Label      Exit1;







    VAR
      Zaehler : Longint;
      R       : tRect;
      View    : pView;
      Code    : INTEGER;
      Dialog  : pDialog;


PROCEDURE ZeigeDateienDialog(Var Befehl : Char;
                             Var ZData  : ZeigeDateienData;
                             DName, PName, LName: String);
  
BEGIN
  R.Assign (5, 5, 69, 17);
  Dialog := New (pDialog, Init (R, 'Zeige Dateien'));
  Dialog^.State := 2171;
 
  R.Assign (6, 2, 61, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'Dateiname  : ' + DName)));

  R.Assign (6, 3, 61, 4);
  Dialog^.Insert (New (pStaticText, Init (R, 'Pfad       : ' + PName)));

  R.Assign (6, 4, 61, 5);
  Dialog^.Insert (New (pStaticText, Init (R, 'Disk - Nr  : ' + LName)));
 
  R.Assign (6, 7, 25, 10);
  View := New (pRadioButtons,
               Init (R, 
                     NewSItem ('Label',
                     NewSItem ('Pfad',
                     NewSItem ('Datei',
                     NIL)))));


  R.Assign (6, 7, 25, 10);
  View := New (pRadioButtons,
               Init (R, 
                     NewSItem ('Label',
                     NewSItem ('Pfad',
                     NewSItem ('Datei',
                     NIL)))));
  Dialog^.Insert (View);
  R.Assign (6, 6, 25, 7);
  Dialog^.Insert (New (pLabel, Init (R, '~N~Ñchstes Element', View)));
 
  R.Assign (35, 6, 47, 8);
  Dialog^.Insert (New (pButton, Init (R, '~W~eiter', 12, 0)));

  R.Assign (50, 6, 62, 8);
  Dialog^.Insert (New (pButton, Init (R, '~Z~urÅck', 10, 1)));
 
  R.Assign (26, 9, 39, 10);
  View := New (pMemo, Init (R, NIL, NIL, NIL, 12));
  Dialog^.Insert (View);
  R.Assign (25, 8, 32, 9);
  Dialog^.Insert (New (pLabel, Init (R, 'Maske:', View)));
 
  R.Assign (44, 8, 54, 10);
  Dialog^.Insert (New (pButton, Init (R, '~E~nde', 11, 0)));
 
  Dialog^.SelectNext (FALSE);


  Dialog^.SetData (ZData);

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));

  IF Code <> cmCancel THEN
  BEGIN
    Dialog^.GetData (ZData);

    Case ZData.Cluster0 of
      0 : Befehl := 'L';
      1 : Befehl := 'P';
      2 : Befehl := 'D';
    end;

    If Code = 10 then Befehl := 'Z'
    { Data muss ausgewertet werden ! }

  END
  else Befehl := Chr(13);

  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;


Procedure MoveList(ch: Char);
Begin

  If Ch <> 'Z' then
  Begin
    Struckt_List := Struckt_List^.Next;
    Inc(Zaehler)
  end
  else
  Begin
    Struckt_List := Struckt_List^.Back;
    Dec(Zaehler)
  end
end;


    
begin
  { Datenrecord Initialisieren ! }

  FillChar (Data, SizeOf (ZeigeDateienData), 0);

  Data.Cluster0 := 2; (* Nach Dateien suchen *)


  Ch := ' ';
  Datei := '*.*';
  help := Struckt_List;
  if Struckt_List <> Nil then
  begin


    repeat

      case Struckt_List^.StrucktData.art of
        3 : begin
              if (ch <> 'p') and
                 (ch <> 'P') and
                 (ch <> 'l') and
                 (ch <> 'L') then
              begin
                if Passt_In_Maske(Datei, Struckt_List^.
                                         StrucktData.
                                         Datei_List^.
                                         DateiData.DateiData) then
                begin
                  For A := 1 to Length(Datei) do
                    Data.TextRec0[A - 1] := Datei[A];

                  Data.TextLen0 := Length(Datei);
                  ZeigeDateienDialog(Ch, Data,
                                              Struckt_List^.
                                              StrucktData.
                                              Datei_List^.
                                              DateiData.DateiData,
                                              s2,
                                              s3);
                  For A := 1 to Data.TextLen0 do
                  Datei[A] := Data.TextRec0[A - 1];
                  Word(Laenge) := Data.TextLen0;
                  Datei[0] := Char(Laenge.Lo);
                  Datei := UpDate(Datei);
                end
              end;

              MoveList(Ch)

            end;
        2 : begin
              s2 := Struckt_List^.StrucktData.Pfad_List^.PfadData.PfadData;

              MoveList(Ch)

            end;
        1 : begin
              s3 := Struckt_List^.StrucktData.Label_List^.LabelData.LabelData;

              MoveList(Ch)

        end
        else Begin
               Struckt_List := Struckt_List^.Next;
               Wait('Fehlerhaftes Element!',2)
             end
      end
    until ( ch = chr(13) ) or
          ( Struckt_List = Nil );
    if Struckt_List = Nil then
      wait('Listenende erreicht.',2)
    else if ch <> chr(13) then
      wait('Keine Liste vorhanden !',2);
    Struckt_List := help
  end
end;


Procedure ZeigeDaten;
Begin
  Zeige_Dateien
end;

Procedure SucheDaten;
Begin
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
end;

Procedure SucheMehrfache;
Begin
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
end;

Procedure DatenAufgeben;
Begin
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
end;



Procedure Memory;
Var LMem : Longint;
    SMem : String;
Begin
  LMem := MemAvail;
  Str(LMem, SMem);
  MessageBox('Speicherplatz: ' + SMem, Nil, mfInformation + mfOkButton)
end;




(* Begin von TDareiverApp.HandleEvent: *)

Var Regs: Registers;
begin
  TApplication.HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmMemory              : Memory;
        cmLaden               : LadeDateien;
        cmDatenAufgeben       : DatenAufgeben;
        cmChangeDir           : ChangeDir;
        cmScann               : Scann;
        cmDiskAutoScann       : DiskAutoScann;
        cmNachtrag            : Nachtrag;
        cmAufreum             : Aufreum;
        cmLaden               : LadeDateien;
        cmSpeichern           : SpeichereDateien;
        cmNewFileName         : NewFileName;
        cmChangeDir           : ChangeDir;
        cmZeigeDaten          : ZeigeDaten;
        cmSucheDaten          : SucheDaten;
        cmSucheMehrfache      : SucheMehrfache;


      else
        Exit;
      end;
  else
    Exit;
  end;
  If Struckt_List <> Nil then
  Begin
    EnableCommands( [cmSpeichern,
                     cmDatenAufgeben,
                     cmZeigeDaten,
                     cmNachtrag,
                     cmAufreum ]);

    DisableCommands([cmScann,
                     cmDiskAutoScann])
  end

  else
  Begin
    DisableCommands([cmSpeichern,
                     cmDatenAufgeben,
                     cmZeigeDaten,
                     cmNachtrag,
                     cmAufreum]);

    EnableCommands( [cmScann,
                     cmDiskAutoScann])
  end;

  ClearEvent(Event);
end;

Procedure TDateiverApp.InitMenuBar;
Var R: TRect;
Begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('Da~t~ei', hcNoContext, NewMenu(
      NewItem('~D~aten aufgeben  ', 'Alt-F3', kbAltF3, cmDatenAufgeben, hcNoContext,
      NewItem('~L~aden', 'F3', kbF3, cmLaden, hcNoContext,
      NewItem('~S~peichern', 'F2', kbF2, cmSpeichern, hcNoContext,
      NewLine(
      NewItem('~C~hange dir...', '', kbNoKey, cmChangeDir, hcNoContext,
      NewItem('~N~eue Datendateien', '', kbNoKey, cmNewFileName, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      nil)))))))),

    NewSubMenu('Daten ~A~ktualisieren', hcNoContext, NewMenu(
      NewItem('~S~cann', '', kbNoKey, cmScann, hcNoContext,
      NewItem('~D~isk Autoscann', '', kbNoKey, cmDiskAutoScann, hcNoContext,
      NewLine(
      NewItem('~N~achtrag', '', kbNoKey, cmNachtrag, hcNoContext,
      NewItem('~A~ufreum', '', kbNoKey, cmAufreum, hcNoContext,
      nil)))))),

    NewSubMenu('~D~aten', hcNoContext, NewMenu(
      NewItem('~Z~eigen',          '', kbNoKey, cmZeigeDaten,     hcNoContext,
      NewItem('~S~uchen',          '', kbNoKey, cmSucheDaten,     hcNoContext,
      NewItem('Suche ~M~ehrfache', '', kbNoKey, cmSucheMehrfache, hcNoContext,
      NewLine(
      NewItem('~F~reier Speicher', '', kbNoKey, cmMemory,         hcNoContext,
      nil)))))),
    nil))))));
end;

Procedure TDateiverApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~F2~ Speichern', kbF2, cmSpeichern,
      NewStatusKey('~F3~ Laden', kbF3, cmLaden,
      NewStatusKey('~Alt-F3~ Daten aufgeben', kbAltF3, cmDatenAufgeben,
      NewStatusKey('~F10~ Menu', kbF10, cmMenu,
      nil))))),
    nil)));

end;

Procedure TDateiverApp.OutOfMemory;
Begin
  MessageBox('Nicht genug Speicher fÅr diese Operation.',
    nil, mfError + mfOkButton);
end;


(* Hauptprogramm: *)

Var DateiverApp : TDateiverApp;

Begin
  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done
end.