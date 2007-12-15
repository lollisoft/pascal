Unit FileShow;
{$O+,F+}
Interface

Uses (*Video,*)

     TVDVDATA,

     App,
     Views,
     Objects,
     Drivers,
     Strings,
     MsgBox,
     Dialogs,
     Editors;

Type LaengenTyp = Record
                    Lo,
                    Hi : Byte
                  END;

    TYPE
      ZeigeDateienData = RECORD
        Cluster0: WORD;
        TextLen0: WORD;
        TextRec0: ARRAY [0..12] OF CHAR;
      END;



Type PFileShowWindow      = ^TFileShowWindow;
     TFileShowWindow      = Object(TDialog)


       SHelp     : StrucktListPtrtyp;
       HHilf     : HilfListPtrtyp;

       Ch, Z     : char;

       s2,
       s3        : string;
       Pfad      : String;

       Datei     : String12;

       Hilf      : DateiListPtrtyp;

       i         : integer;

       LocHilf   : Pointer;

       Data      : ZeigeDateienData;

       Laenge    : LaengenTyp;

       Zaehler            : Longint;
       R                  : tRect;
       View               : pView;
       Code               : INTEGER;
       Dialog             : pDialog;

       Hs2                : HilfListPtrtyp;

      



       Constructor Init;
       Destructor  Done; Virtual;
       Procedure   HandleEvent(Var Event: TEvent); Virtual;
     End;




Procedure ZeigeDateien;





Implementation

Constructor TFileShowWindow.Init;
Begin
End;

Destructor TFileShowWindow.Done;
Begin
End;

Procedure TFileShowWindow.HandleEvent(Var Event: TEvent);
Begin
End;


Procedure ZeigeDateien;

Type LaengenTyp = Record
                    Lo,
                    Hi : Byte
                  END;

    TYPE
      ZeigeDateienData = RECORD
        Cluster0: WORD;
        TextLen0: WORD;
        TextRec0: ARRAY [0..12] OF CHAR;
      END;


var SHelp     : StrucktListXPtrtyp;
    HHilf     : HilfListXPtrtyp;

    Ch, Z     : char;

    s2,
    s3        : string;
    Pfad      : String;

    Datei     : String12;

    Hilf      : DateiListPtrtyp;

    i         : integer;

    LocHilf   : Pointer;

    Data      : ZeigeDateienData;

    Laenge    : LaengenTyp;

    Label      Exit1;







Var
    Zaehler            : Longint;
    R                  : tRect;
    View               : pView;
    Code               : INTEGER;
    Dialog             : pDialog;
    Hs2                : HilfListXPtrtyp;



      




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
  Dialog^.Insert (New (pStaticText, Init (R, 'Disk - Nr. : ' + LName)));
 
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
  Dialog^.Insert (New (pButton, Init (R, '~W~eiter', cmYes, 0)));

  R.Assign (50, 6, 62, 8);
  Dialog^.Insert (New (pButton, Init (R, '~Z~urÅck', cmNo, 1)));

  R.Assign (26, 9, 39, 10);
  View := New (pMemo, Init (R, NIL, NIL, NIL, 12));
  Dialog^.Insert (View);
  R.Assign (25, 8, 32, 9);
  Dialog^.Insert (New (pLabel, Init (R, 'Maske:', View)));
 
  R.Assign (44, 8, 54, 10);
  Dialog^.Insert (New (pButton, Init (R, '~E~nde', cmCancel, 0)));

  Dialog^.SelectNext (FALSE);


  Dialog^.SetData (ZData);

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));


  IF (Code <> cmCancel) THEN
  BEGIN
    Dialog^.GetData (ZData);

    Case Code of
      cmYes : Befehl := ' ';
      cmNo  : Befehl := 'Z';
    end

  END
  else Befehl := Chr(13);

  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;



BEGIN
  { Datenrecord Initialisieren ! }

  FillChar (Data, SizeOf (ZeigeDateienData), 0);

  Data.Cluster0 := 2; (* Nach Dateien suchen *)

  Ch := ' ';

  Datei := '*.*';
  SHelp := StrucktList;
  HHilf := HilfList.Ptr^.Next;
  if StrucktList.Ptr <> Nil then
  BEGIN
    repeat
      case StrucktList.Ptr^.StrucktData.art of
        3 : BEGIN
              If StrucktList.Ptr^.StrucktData.DateiList.Ptr^.
                 DateiData.Anzahl < 1 then
                Wait('Anzahl eines Dateieintrags ist kleiner 1!');
              if PData^.PasstInMaske(Datei, StrucktList.Ptr^.
                                       StrucktData.
                                       DateiList.Ptr^.
                                       DateiData.DateiName) then
              BEGIN
                For A := 1 to Length(Datei) do
                  Data.TextRec0[A - 1] := Datei[A];

                Data.TextLen0 := Length(Datei);
                ZeigeDateienDialog(Ch, Data,
                                            StrucktList.Ptr^.
                                            StrucktData.
                                            DateiList.Ptr^.
                                            DateiData.DateiName,
                                            s2,
                                            s3);
                For A := 1 to Data.TextLen0 do
                Datei[A] := Data.TextRec0[A - 1];
                Word(Laenge) := Data.TextLen0;
                Datei[0] := Char(Laenge.Lo);
                Datei := UpDate(Datei);
                Z := Ch
              END;

              If Ch <> Chr(13) then
                If Datei <> '*.*' then
                BEGIN
                  Ch := 'D';
                  Data.Cluster0 := 2
                END
                else
                  Case Data.Cluster0 of
                    0 :
                        Ch := 'L';
                    1 :
                        Ch := 'P';
                    2 :
                        Ch := 'D';
                  end;

              


              If Z = 'Z' then
              BEGIN
                CASE Ch of
(* Pfad zurÅck *) 'P' : BEGIN
                          HHilf := HHilf.Ptr^.Back;

                          If HHilf.Ptr^.HilfData.art = 1 then
                          BEGIN
                            If HHilf.Ptr^.Back.Ptr <> Nil then
                            BEGIN

                              (* HHilf^.HilfData.art = 1: *)
                              (* HHilf^.Back ist dann ein *)
                              (* Pfadelement.             *)
                              (* Hole Pfad vor Label:     *)

(* HHilf nach Aktion --> *)   Hs2 := HHilf.Ptr^.Back;

                              (* Pfad zuvor ist auf einer anderen  *)
                              (* Diskette, hole Label dieser Disk: *)

                              Repeat
                                HHilf := HHilf.Ptr^.Back
                              Until (HHilf.Ptr = Nil) or
                                    (HHilf.Ptr^.HilfData.art = 1);

                              (* Vor - Label in s3 speichern: *)

                              s3 := HHilf.Ptr^.
                                    HilfData.
                                    LabelList.Ptr^.
                                    StrucktData.
                                    LabelList.Ptr^.
                                    LabelData.
                                    LabelName;

                              (* StrucktList auf gesicherten Pfad setzen: *)

                              StrucktList := Hs2.Ptr^.HilfData.PfadList;

(* HHilf := Hs2; --> *)       HHilf := Hs2;

                              While HHilf.Ptr^.HilfData.PfadList.Ptr^.Next.Ptr^.
                                  StrucktData.Art <> 3 do
                              HHilf := HHilf.Ptr^.Back;


                            END

                            (* Durch zurÅckgehen zum Anfang der *)
                            (* Liste gekommen:                  *)

                            ELSE StrucktList.Ptr := Nil

                          END
                          ELSE

                            (* HHilf^.HilfData.Art ist 2. *)

                            (* Auf Disk ist noch ein      *)
                            (* Vorpfad                    *)
                          Begin
                            While HHilf.Ptr^.HilfData.PfadList.Ptr^.Next.Ptr^.
                                  StrucktData.Art <> 3 do
                              HHilf := HHilf.Ptr^.Back;

(* HHilf Ok --> *)          StrucktList := HHilf.Ptr^.HilfData.PfadList
                          End



                        END;

(* Label zurÅck *)'L' : BEGIN

                          (* Auf Label zurÅckgehen: *)

                          Repeat
                            HHilf := HHilf.Ptr^.Back;
                          Until HHilf.Ptr^.HilfData.art = 1;

                          If HHilf.Ptr^.Back.Ptr = Nil then

                            (* Kein Vor - Label mehr da: *)

                            StrucktList.Ptr := Nil
                          ELSE
                          BEGIN

                            (* Vor - Label da, suchen: *)

                            Repeat
                              HHilf := HHilf.Ptr^.Back
                            Until HHilf.Ptr^.HilfData.art = 1;


                            StrucktList := HHilf.Ptr^.HilfData.LabelList;

                            (* Sicherstellen, da· leere Pfade nicht *)
                            (* Åber HHilf lokalisiert werden:       *)

(* HHilf dann Ok --> *)     Repeat
                              HHilf := HHilf.Ptr^.Next
                            Until (HHilf.Ptr = Nil) or
                                  (HHilf.Ptr^.HilfData.
                                   PfadList.Ptr^.Next.Ptr^.
                                   StrucktData.Art = 3)


                          END
                        END;
(* Datei zurÅck *)'D' : BEGIN
                          StrucktList := StrucktList.Ptr^.Back;
                          If StrucktList.Ptr^.StrucktData.art = 2 then
                          BEGIN
                            HHilf := HHilf.Ptr^.Back;

                            StrucktList := StrucktList.Ptr^.Back;

                            If StrucktList.Ptr^.StrucktData.art = 1 then

                              If StrucktList.Ptr^.Back.Ptr <> Nil then

                              BEGIN
                                HHilf := HHilf.Ptr^.Back;
                                s2 := HHilf.Ptr^.
                                      HilfData.
                                      PfadList.Ptr^.
                                      StrucktData.
                                      PfadList.Ptr^.
                                      PfadData.
                                      PfadName;
                                Hs2 := HHilf;
                                Repeat
                                  HHilf := HHilf.Ptr^.Back
                                Until (HHilf.Ptr = Nil) or
                                      (HHilf.Ptr^.HilfData.art = 1);
                                s3 := HHilf.Ptr^.
                                      HilfData.
                                      LabelList.Ptr^.
                                      StrucktData.
                                      LabelList.Ptr^.
                                      LabelData.
                                      LabelName;
                                HHilf := Hs2;
                                StrucktList := StrucktList.Ptr^.Back
                              END

                              ELSE StrucktList.Ptr := Nil
                            ELSE
                              s2 := HHilf.Ptr^.
                                    HilfData.
                                    PfadList.Ptr^.
                                    StrucktData.
                                    PfadList.Ptr^.
                                    PfadData.
                                    PfadName
                          END
                        END   (* Datei zurÅck *)
                END (* Ende von Case D, P, L zurÅck *)
              END
              ELSE      (* Z <> 'Z' *)
              BEGIN
                CASE Ch of
(* Pfad vor *)    'P' : BEGIN
                          Repeat
                            HHilf := HHilf.Ptr^.Next;
                            If HHilf.Ptr <> Nil
                            then
                            If HHilf.Ptr^.HilfData.art = 1 then
                              s3 := HHilf.Ptr^.
                                    HilfData.
                                    LabelList.Ptr^.
                                    StrucktData.
                                    LabelList.Ptr^.
                                    LabelData.LabelName
                          Until (HHilf.Ptr = Nil)  or
                                (HHilf.Ptr^.HilfData.PfadList.Ptr^.
                                 Next.Ptr^.StrucktData.art = 3);
                          If HHilf.Ptr = Nil then
                            StrucktList.Ptr := Nil
                          else
                          Begin
                            If (StrucktList.Ptr = HHilf.Ptr^.HilfData.PfadList.Ptr) and
                               (StrucktList.BlockNr = HHilf.Ptr^.HilfData.PfadList.BlockNr) then
                              Wait('Fehler in der Hilfliste!');
                            StrucktList := HHilf.Ptr^.HilfData.PfadList
                          End
                        END;

(* Label vor *)   'L' : BEGIN
                          Repeat
                            HHilf := HHilf.Ptr^.Next;
                          Until (HHilf.Ptr = Nil) or
                                (HHilf.Ptr^.HilfData.art = 1);
                          If HHilf.Ptr = Nil then
                            StrucktList.Ptr := Nil
                          else
                          BEGIN
                            StrucktList := HHilf.Ptr^.HilfData.LabelList;
                            HHilf := HHilf.Ptr^.Next
                          END
                        END;
(* Datei vor *)   'D' : Begin
                          StrucktList := StrucktList.Ptr^.Next;
                          If StrucktList.Ptr^.StrucktData.art = 1 then
                            HHilf := HHilf.Ptr^.Next.Ptr^.Next;
                          If StrucktList.Ptr^.StrucktData.art = 2 then
                            HHilf := HHilf.Ptr^.Next
                        END
                END (* Case *)

              END (* Else Z <> 'Z' *)

            END; (* Case Art = 3  *)
        2 : BEGIN
              s2 := StrucktList.Ptr^.StrucktData.PfadList.Ptr^.PfadData.PfadName;
              StrucktList := StrucktList.Ptr^.Next
            END;
        1 : BEGIN
              s3 := StrucktList.Ptr^.StrucktData.LabelList.Ptr^.LabelData.LabelName;
              StrucktList := StrucktList.Ptr^.Next
            END

        else BEGIN
               StrucktList := StrucktList.Ptr^.Next;
               Wait('Fehlerhaftes Element!')
             end
      end
    until ( ch = chr(13) ) or
          ( StrucktList.Ptr = Nil );

    if StrucktList.Ptr = Nil then
      wait('Listenende erreicht.')
    else if ch <> chr(13) then
      wait('Keine Liste vorhanden !');

    StrucktList := SHelp
  end
  Else Wait('Strucktliste ist Nil')
END;

End.
