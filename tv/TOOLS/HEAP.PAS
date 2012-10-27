{$F+,O+,S-,V-,X+}
UNIT Heap;

INTERFACE

USES Crt,
     Objects, Drivers, Views;

TYPE PHeapView = ^THeapView;
     THeapView = object(TView)
       FreeMem: Longint;
       constructor Init (VAR Bounds: TRect);
       procedure Draw; virtual;
       function GetPalette: PPalette; virtual;
       procedure Update;
     END;

implementation

constructor THeapView.Init (VAR Bounds: TRect);
begin
  TView.Init(Bounds);
  GrowMode := gfGrowAll;
end;

procedure THeapView.Draw;
var
  S: string[7];
begin
  FreeMem := MemAvail;
  FormatStr(S, '%7d', FreeMem);
  IF FreeMem > 100000
   THEN WriteStr(0, 0, S, 1)
   ELSE IF FreeMem > 50000
     THEN WriteStr(0, 0, S, 2)
     ELSE BEGIN
      WriteStr(0, 0, S, 3);
      Sound(3000);
      Delay((50000-MemAvail) DIV 100);
      NoSound;
  END;
end;

function THeapView.GetPalette: PPalette;
const
  P: String[2] = #2#1;
begin
  GetPalette := @P;
end;

procedure THeapView.Update;
begin
  if FreeMem <> MemAvail then DrawView;
end;

end.
