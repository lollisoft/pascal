Unit LabHist;
{$O+,F+}
Interface


Type LabelHistListPtrTyp = ^LabelHistListTyp;
     LabelHistListTyp    = Record
                             Name: String[12];
                             Back,
                             Next : LabelHistListPtrTyp
                           end;


Var LabelHistList : LabelHistListPtrTyp;

Function LabelInLabelHistList(Name:String):Boolean;
Procedure PutLabelInHistList(Name: String);
Procedure DelLabelHistList;


Implementation

Function LabelInLabelHistList;
Var LHilf : LabelHistListPtrTyp;
Begin
  LHilf := LabelHistList;
  LabelInLabelHistList := True;

  While (LHilf       <> Nil ) and
        (LHilf^.Name <> Name) do

    LHilf := LHilf^.Next;

  If LHilf = Nil then LabelInLabelHistList := False
end;

Procedure PutLabelInHistList;
Var LHilf: LabelHistListPtrTyp;
Begin
  If LabelHistList <> Nil then
  Begin
    New(LHilf);

    LHilf^.Next := LabelHistList;
    LHilf^.Back := Nil;

    LabelHistList^.Back := LHilf;
    LabelHistList := LHilf;
  end
  else
  Begin
    New(LabelHistList);
    LabelHistList^.Next := Nil;
    LabelHistList^.Back := Nil
  end;

  LabelHistList^.Name := Name
end;

Procedure DelLabelHistList;
Begin
  If LabelHistList = Nil then Exit;
  While LabelHistList^.Next <> Nil do
  Begin
    LabelHistList := LabelHistList^.Next;
    Dispose(LabelHistList^.Back)
  end;
  Dispose(LabelHistList);
  LabelHistList := Nil
end;


end.