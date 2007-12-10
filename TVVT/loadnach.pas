Unit LoadNach;

Interface

Uses Video;

Procedure LadeAlteDaten;

Implementation

Procedure LadeAlteDaten;

Type FehltListPtrTyp = ^FehltListTyp;
     FehltListTyp    = Record
                         Zahl: Longint;
                         Next: FehltListPtrTyp
                       End;

     SListPtrTyp    = ^SListTyp;
     SListTyp       = Record
                        StrucktData: StrucktDataTyp;
                        Next       : SListPtrTyp
                      End;

     HListPtrTyp    = ^HListTyp;
     HListTyp       = Record
                        HilfData   : HilfDataTyp;
                        Next       : HListPtrTyp
                      End;


     ZListPtrTyp    = ^ZListTyp;
     ZListTyp       = Record
                        ZusatzData: ZusatzDataTyp;
                        Next      : ZListPtrTyp
                      End;

     LListPtrTyp    = ^LListTyp;
     LListTyp       = Record
                        LabelData: LabelDataTyp;
                        Next     : LListPtrTyp
                      End;

     PListPtrTyp    = ^PListTyp;
     PListTyp       = Record
                        PfadData: PfadDataTyp;
                        Next    : PListPtrTyp
                      End;

     DListPtrTyp    = ^DListTyp;
     DListTyp       = Record
                        DateiData: DateiDataTyp;
                        Next     : DListPtrTyp
                      End;

Var StrucktFehltList: FehltListPtrTyp;
     ZusatzFehltList: FehltListPtrTyp;
      LabelFehltList: FehltListPtrTyp;
      DateiFehltList: FehltListPtrTyp;
       PfadFehltList: FehltListPtrTyp;
       HilfFehltList: FehltListPtrTyp;

    SList           : SListPtrTyp;
    HList           : HListPtrTyp;
    ZList           : ZListPtrTyp;
    LList           : LListPtrTyp;
    PList           : PListPtrTyp;
    DList           : DListPtrTyp;

    ScannSList      : StrucktListPtrTyp;
    ScannHList      : HilfListPtrTyp;
    ScannZList      : ZusatzListPtrTyp;
    ScannLList      : LabelListPtrTyp;
    ScannPList      : PfadListPtrTyp;
    ScannDList      : DateiListPtrTyp;



    StrucktData     : StrucktDataTyp;
    ZusatzData      : ZusatzDataTyp;
    LabelData       : LabelDataTyp;
    PfadData        : PfadDataTyp;
    DateiData       : DateiDataTyp;


    HAnzahl         : Longint;


(*********************************************************)
(*                                                       *)
(*      Entscheidung, ob ein Element gelanen wird.       *)
(*                                                       *)
(*********************************************************)



Function WirdStrucktErsetzt(StrucktData: StrucktDataTyp): Boolean;
Begin
End;

Function WirdHilfErsetzt(HilfData: HilfDataTyp): Boolean;
Begin
End;

Function WirdZusatzErsetzt(ZusatzData: ZusatzDataTyp): Boolean;
Begin
End;

Function WirdLabelErsetzt(LabelData: LabelDataTyp): Boolean;
Begin
End;

Function WirdPfadErsetzt(PfadData: PfadDataTyp): Boolean;
Begin
End;

Function WirdDateiErsetzt(DateiData: DateiDataTyp): Boolean;
Begin
End;

(*********************************************************)
(*                                                       *)
(*      Neue Elemente in die Listen aufnehmen:           *)
(*                                                       *)
(*      Listen mit Elementen, die nicht geladen werden.  *)
(*                                                       *)
(*********************************************************)




Procedure NewStrucktFehltList(Var StrucktFehltList: FehltListPtrTyp;
                                  SAnzahl: Longint);
Begin
End;

Procedure NewHilfFehltList(Var HilfFehltList: FehltListPtrTyp;
                               HAnzahl: Longint);
Begin
End;

Procedure NewZusatzFehltList(Var ZusatzFehltList: FehltListPtrTyp;
                               ZAnzahl: Longint);
Begin
End;

Procedure NewLabelFehltList(Var LabelFehltList: FehltListPtrTyp;
                               LAnzahl: Longint);
Begin
End;

Procedure NewPfadFehltList(Var PfadFehltList: FehltListPtrTyp;
                               PAnzahl: Longint);
Begin
End;

Procedure NewDateiFehltList(Var DateiFehltList: FehltListPtrTyp;
                               DAnzahl: Longint);
Begin
End;


(*********************************************************)
(*                                                       *)
(*      Neue Elemente in die Listen aufnehmen:           *)
(*                                                       *)
(*********************************************************)

Procedure GetStrucktElement(Var SList: SListPtrTyp);
Begin
End;

Procedure GetZusatzElement(Var ZList: ZListPtrTyp);
Begin
End;

Procedure GetHilfElement(Var HList: HListPtrTyp);
Begin
End;

Procedure GetLabelElement(Var LList: LListPtrTyp);
Begin
End;

Procedure GetPfadElement(Var PList: PListPtrTyp);
Begin
End;

Procedure GetDateiElement(Var DList: DListPtrTyp);
Begin
End;

  Var Liste : Pointer;
Begin
  SList := Nil;
  HList := Nil;
  ZList := Nil;
  LList := Nil;
  PList := Nil;
  DList := Nil;

  StrucktFehltList := Nil;
  ZusatzFehltList := Nil;
  LabelFehltList := Nil;
  DateiFehltList := Nil;
  PfadFehltList := Nil;

  SAnzahl := 0;
  ZAnzahl := 0;
  LAnzahl := 0;
  PAnzahl := 0;
  DAnzahl := 0;


  ScannSList := StrucktList;
  ScannHList := HilfList;
  ScannZList := ZusatzList;
  ScannLList := LabelList;
  ScannPList := PfadList;
  ScannDList := DateiList;

  New(LList);
  Liste := LList;

  For A := 1 to Filesize(LabelDataFile) do
  Begin
    Read(LabelDataFile, LabelData);
    LList^.LabelData := LabelData;
    New(LList^.Next);
    LList := LList^.Next
  End;

  LList := Liste;

  New(HList);
  Liste := HList;

  For A := 1 to FileSize(HilfDataFile) do
  Begin
    Read(HilfDataFile, HilfData);
    HList^.HilfData := HilfData;
    New(HList^.Next);
    HList := HList^.Next
  End;

  HList := Liste;

  New(SList);
  Liste := SList;

  For A := 1 to FileSize(StrucktDataFile) do
  Begin
    Read(StrucktDataFile, StrucktData);
    SList^.StrucktData := StrucktData;
    New(SList^.Next);
    SList := SList^.Next
  End;

  SList := Liste;

  Liste := ScannHList;

  While ScannHList <> Nil do
  Begin
    If ScannHList^.HilfData.art = 1 then
    Begin

    End;
    ScannHList := ScannHList^.Next
  End;
End;
End.

