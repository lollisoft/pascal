Unit KomBuild; (* Kommando - Verwaltung *)

(********************************************)
(*                                          *)
(* KommandoBuild - Objekt:                  *)
(* -----------------------                  *)
(*                                          *)
(* Dieses Objekt soll Daten und Editoren    *)
(* fÅr Kommandos eines Projektes enthalten. *)
(*                                          *)
(*                                          *)
(*                                          *)
(*                                          *)
(*                                          *)
(*                                          *)
(********************************************)

Interface

Uses Objects, Menus;



Type

     PKommando = ^TKommando;
     TKommando = Object(TObject)

       ACM     : String;
       ACMNum  : Integer;
       AHC     : String;
       AHCNum  : Integer;

       Constructor Init(S: String; I: Integer);
       Destructor  Done; Virtual;

       Procedure SetKommando(S: String; I: Integer);
       Procedure GetKommando(Var S: String; Var I: Integer);
     End;

     PMenuKommando = ^TMenuKommando;
     TMenuKommando = Object(TKommando)
     End;

     PDialogKommando = ^TDialogKommando;
     TDialogKommando = Object(TKommando)
     End;


     PKommandoList  = ^TKommandoList;
     TKommandoList  = Object(TCollection)
     End;

     PKommandoBuild = ^TKommandoBuild;
     TKommandoBuild = Object (TObject)
       AMenu        : PMenuBar;



       Procedure WriteKommandos;
       Procedure ReadKommandos;

       Procedure EditMenu;

Private
       AKommandoList: PKommandoList;


     End;


Implementation

(*********************)
(* Kommando - Object *)
(*********************)

Constructor TKommando.Init(S: String; I: Integer);
Begin
End;

Destructor TKommando.Done;
Begin
End;


Procedure TKommando.SetKommando(S: String; I: Integer);
Begin
End;

Procedure TKommando.GetKommando(Var S: String; Var I: Integer);
Begin
End;

Procedure TKommandoBuild.WriteKommandos;
Begin
End;

Procedure TKommandoBuild.ReadKommandos;
Begin
End;

Procedure TKommandoBuild.EditMenu;
Begin
End;

End.
