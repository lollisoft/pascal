Unit Video1;
Interface
Uses Objects;

type

     PfadDatatyp      = Record
                            Anzahl   : Byte;
                            PfadName : PString
                          end;

     PfadListPtrtyp  = ^PfadListtyp;

     PfadListtyp      = record
                            Links,
                            Rechts    : PfadListPtrtyp;
                            Bal       : Shortint;
                            PfadData  : PfadDatatyp
                          end;



Var  PfadName       : string;



Procedure SpeicherePfad(var  Lauf : PfadListPtrtyp;
                        var  dif  : Boolean);

Implementation


Procedure SpeicherePfad;
var p1, p2 : PfadListPtrtyp;
begin
  if Lauf = Nil then
  begin
    new(lauf);
    dif := true;
    with lauf^ do
    begin
      PfadData.PfadName := NewStr(PfadName);
      If PfadData.PfadName <> Nil then
        PfadData.PfadName^ := PfadName;

      Links := Nil;
      Rechts := Nil;
      Bal := 0;
      PfadData.Anzahl := 1
    end
  end

  else

    if PfadName < Lauf^.PfadData.PfadName^ then
    begin
      SpeicherePfad(lauf^.links,dif);

      if dif then
      case lauf^.bal of

      1 : begin lauf^.bal := 0; dif := false end;

      0 : lauf^.bal := -1;

      -1: begin { Ausgleichen }
            p1 := Lauf^.links;
            if p1^.bal = -1 then
            begin
              lauf^.links := p1^.rechts;
              p1^.rechts := lauf;
              lauf^.bal := 0;
              lauf := p1
            end

            else

            begin
              p2 := p1^.rechts;
              p1^.rechts := p2^.links;
              p2^.links := p1;
              lauf^.links := p2^.rechts;
              p2^.rechts := lauf;

              if p2^.bal = -1 then lauf^.bal := 1
                              else lauf^.bal := 0;

              if p2^.bal = 1  then p1^.bal := -1
                              else p1^.bal := 0;
              lauf := p2;
            end;
            lauf^.bal := 0;
            dif := false
          end
        end
    end

    else

      if PfadName > Lauf^.PfadData.PfadName^ then
      begin
        SpeicherePfad(lauf^.rechts,dif);

        if dif then
        case lauf^.bal of
        -1 : begin lauf^.bal := 0; dif := false end;

        0 : lauf^.bal := +1;

        1: begin { Ausgleichen }
             p1 := Lauf^.rechts;
             if p1^.bal = + 1 then
             begin
               lauf^.rechts := p1^.links;
               p1^.links := lauf;
               lauf^.bal := 0;
               lauf := p1
             end

             else

             begin
               p2 := p1^.links;
               p1^.links := p2^.rechts;
               p2^.rechts := p1;
               lauf^.rechts := p2^.links;
               p2^.links := lauf;

               if p2^.bal = +1 then lauf^.bal := -1
                               else lauf^.bal := 0;

               if p2^.bal = -1 then p1^.bal := 1
                               else p1^.bal := 0;
               lauf := p2;
             end;

             lauf^.bal := 0;
             dif := false
           end
         end
      end
      else
        if PfadName = Lauf^.PfadData.PfadName^ then
        begin
          Inc(Lauf^.PfadData.Anzahl)
        end


end;


end.
