Program RMDEL;
Uses Dos;

Procedure List(Pfad: String);
var sr          : searchrec;
    h           : Integer;
begin
  FindFirst(pfad+'*.*',attr,sr);
  while DosError = 0 do
  begin
    if (sr.attr and Directory) <> 0 then
      if sr.name <> '.' then
        if sr.name <> '..' then


    Begin
      (* Verzeichni· anwÑhlen... *)

      (* Alle Dateien lîschen... *)

      (* Verzeichni· zurÅck  ... *)
    End;

    FindNext(sr)
  end
end;



Procedure Dir;
Label Exit1;
Var Info : ^String;
    F    : File;
begin

  Pfad := '';
  Befehl := '';
  Extender := '';
  DirStatus := True;
  Attr := $3F;

  Fsplit(Mask,Pfad,Befehl,Extender);

  if Length(Pfad) <> 0 then Delete(Mask,1,Length(Pfad));
  W := False;
  if (Pos(':\',Pfad) = 2) and
     (Length(Pfad) = 3) then W := True;

  if pfad[Length(pfad)] = '\' then delete(pfad,length(pfad),1);
  s := 0;

  FindFirst(Pfad + Befehl + Extender, $08, srec);
  lab := srec.name;

  if pos('DISK',Lab) <> 0 then
  begin

    FindFirst(Pfad + Befehl + Extender, AnyFile, srec);
    if Doserror = 0 then
    begin
      SpeichereLabel(lab,LabelList,LabelDif, NoNew);
      Name := Lab;

      GetStruckt(StrucktListend,AltStruckt);

      StrucktListend^.StrucktData.art := 1;
      StrucktListend^.StrucktData.LabelList := Labeladress;


      GetHilf(HilfListend,AltHilf);

      HilfListend^.HilfData.art := 1;
      HilfListend^.HilfData.LabelList := Pointer(StrucktListend);

      Labelzeiger := StrucktListend;

      list(UpDate(pfad))
    end

    else
    begin
      MessageBox('Diskette leer!', nil, mfOkButton);
      DirStatus := False
    end
  end

  else

  begin
    MessageBox('Diskette hat einen ungÅltigen Label!',
        Nil, mfError + mfOkButton);
        Name := '';
    DirStatus := False
  end;

  Exit1:

end;

Begin
End.