
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}
{							}
{       MAINSHOE  --SIDESHOE complement                 }
{       tvDMX     --data editing project (ver 2.x)      }
{							}
{       Copyright (c) 1994-96   Randolph Beck           }
{				P.O. Box  540433	}
{				Orlando, FL 32854	}
{							}
{	http://www.byronref.com/rmb/			}
{	email:  rb@randybeck.com			}
{                                                       }
{       This unit displays a tvDMX view that will be    }
{       Attached to a SIDESHOE view.                    }
{       See SIDESHOE.PAS.                               }
{                                                       }
{       Warning:  Using SIDESHOE can be tricky.         }
{							}
{þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ}


Unit MAINSHOE;

{$B-,D+,E+,N+,O+,X+,V- }

interface

uses
    Objects, Views, Drivers, Menus, Dialogs, App,
    RSet, DmxGizma, StdDlg, tvDMX, SideShoe;

type
    PDmxMain	= ^TDmxMain;
    TDmxMain	=  OBJECT (TDmxEditor)
	Side	:  PDmxSide;
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure SetState(AState: word; Enable: boolean);  VIRTUAL;
    end;


implementation


  { ออ TDmxMain ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TDmxMain.HandleEvent(var Event: TEvent);
var  RS,FS,RSs,FSs	: boolean;

    procedure DoCommand;
    var  AWhat,ACommand	: word;
	 AInfoPtr	: pointer;
    begin
      With Event do
	begin
	Case Command of
	  cmDMX_ZeroizeRecord:
	    begin
	    inherited HandleEvent(Event);
	    end;
	  cmDMX_Enter:
	    begin
	    Exit;
	    end;
	  cmDMX_Left:
	    If (CurrentField^.screentab <> 0) then Exit else
	      If Valid(Command) and Side^.Valid(Command) then
		begin
		Message(Side, evCommand, cmDMX_End, @Self);
		Side^.Select;
		end;
	  cmDMX_Right:
	    begin
	    Exit;
	    end;
	  cmDMX_Home:
	    If Valid(Command) and Side^.Valid(Command) then
	      begin
	      Side^.Select;
	      Message(Side, evCommand, cmDMX_Home, @Self);
	      end;
	  cmDMX_End:
	    begin
	    Exit;
	    end;
	  cmDMX_NextRow:
	    If Valid(Command) and Side^.Valid(Command) then
	      begin
	      RS := Side^.RecordSelected;  FS := Side^.FieldSelected;
	      If FS then Side^.EvaluateField;
	      If RS then Side^.EvaluateRecord;
	      inherited HandleEvent(Event);
	      Message(Side, evCommand, cmDMX_Down, @Self);
	      If not Side^.RecordSelected then Side^.SetupRecord;
	      Message(Side, evCommand, cmDMX_Home, @Self);
	      Side^.Select;
	      end;
	  cmDMX_goto:
	    If Valid(Command) and Side^.Valid(Command) then
	      begin
	      RSs := Side^.RecordSelected;  FSs := Side^.FieldSelected;
	      If FSs then Side^.EvaluateField;
	      If RSs then Side^.EvaluateRecord;
	      inherited HandleEvent(Event);
	      If (CurrentRecord <> Side^.CurrentRecord) then
		Side^.CurrentRecord := CurrentRecord;
	      If RSs then
		begin
		If not RecordSelected then SetupRecord;
		Side^.SetupRecord;
		If FSs then Side^.SetupField;
		end;
	      If (RecInd <> nil) then RecInd^.DrawView;
	      end;
	  cmDMX_NextRow,
	  cmDMX_Up,
	  cmDMX_Down,
	  cmDMX_PgUp,
	  cmDMX_PgDn,
	  cmDMX_ScreenTop,
	  cmDMX_ScreenBottom,
	  cmDMX_Top,
	  cmDMX_Bottom:
	    If Valid(Command) and Side^.Valid(Command) then
	      begin
	      RS := Side^.RecordSelected;  FS := Side^.FieldSelected;
	      If FS then Side^.EvaluateField;
	      If RS then Side^.EvaluateRecord;
	      AWhat := What;
	      ACommand := Command;
	      AInfoPtr := InfoPtr;
	      inherited HandleEvent(Event);
	      Message(Side, AWhat, ACommand, @Self);
	      If RS then Side^.SetupRecord;
	      If FS then Side^.SetupField;
	      If (RecInd <> nil) then RecInd^.DrawView;
	      end;
	 else
	    Exit;
	  end;
	If (Event.What and FocusedEvents <> 0) then ClearEvent(Event);
	end;
    end;

begin
  If (Side <> nil) and
     (Event.What and evMessage <> 0) and
     (Event.InfoPtr <> Side)
   then
    DoCommand;
  If (Event.What <> evNothing) then inherited HandleEvent(Event);
end;


procedure TDmxMain.SetState(AState: word; Enable: boolean);
begin
  If (AState and sfSelected <> 0) and Enable then
    begin
    ShowFmt := ShowFmt + [showCurrentField];
    RedrawRecord := TRUE;
    If (Side <> nil) then
      begin
      Side^.ShowFmt := Side^.ShowFmt - [showCurrentField];
      If (Side^.CurrentField = nil) or (Side^.RecordData = nil) then
	Side^.RedrawRecord := TRUE
       else
	Side^.DrawRecord(CurrentRecord - Delta.Y, RecordData^);
      end;
    end;
  inherited SetState(AState, Enable);
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }



End.
