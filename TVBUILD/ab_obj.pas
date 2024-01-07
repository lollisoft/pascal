Unit AB_OBJ; (* Gundlegende Klassen fÅr Elemente eines Programms *)







Interface

Uses App, Objects, Views, MyStr, MyEditor, Dialogs, MsgBox, Var_Obj,
     Dirs,
     TVBUCMDS;





(* Grund - Objekte die den Programmcode Representieren *)














Type
























      (********************************)

      (* Die Quelldatei ( zum Lesen ) *)

      (********************************)



























































      PBeginBlock = ^TBeginBlock;

      PEndBlock = ^TEndBlock;


















      TBeginBlock = Object(TObject)

        Constructor Init;

        Procedure WriteCode;

        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;

      End;



      TEndBlock = Object(TObject)

        Constructor Init;

        Procedure WriteCode;

        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;

      End;









Const



















     RProcedureList : TStreamRec = (

       ObjType: StRegProcedureList;

       VmtLink: Ofs(TypeOf(TProcedureList)^);

       Load   : @TProcedureList.Load;

       Store  : @TProcedureList.Store

     );










Implementation

Uses UInPro, VarInPro, ConInPro, SubBuild;













(*********************************************)







(**********************************************)






(************************************************)





(************************************************)




















Constructor TBeginBlock.Init;

Begin

End;



Procedure TBeginBlock.WriteCode;

Begin

  Writeln(F, 'Begin');

End;



Function TBeginBlock.ReadCode(Var ErrorData: PErrorDataList): Boolean;

Begin

End;





Constructor TEndBlock.Init;

Begin

End;



Procedure TEndBlock.WriteCode;

Begin

  Writeln(F, 'End.');

End;



Function TEndBlock.ReadCode(Var ErrorData: PErrorDataList): Boolean;

Begin

End;












































































Procedure InitUnit;

Begin

  ObjectStack := New(PObjectStack, Init(1, 1));








  RegisterType(RUnitList);

  RegisterType(RUnitObj);










  RegisterType(RVarList);


  RegisterType(RProcedureList);

End;



Begin

  InitUnit;

End.