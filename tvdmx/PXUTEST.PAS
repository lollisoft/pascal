
Program PxUTEST;  { Demonstration of PxUNIT }

{$V-,X+,N+,E+,I-,D+ }

uses  Dos, Objects, Drivers, Views, Dialogs, Menus, App, PxEngine,
      RSet, DmxGizma, tvGizma, PxGizma, tvDMX, PxDMX, PxUnit;

const
    cmFindFirst		=  100;
    cmFindEach		=  101;
    cmRecEach		=  102;

    { field number constants for table SERTEST.DB }
    seriesBook		=  1;
    seriesRelease	=  2;
    seriesSeries	=  3;
    seriesTitle		=  4;
    seriesAuthor	=  5;
    seriesPubDate	=  6;
    seriesDueDate	=  7;

    { this field type (just Month and Year) was created for this example }
    fldPxShortDATE	=  ' WW-'^F   + ^U+char(12) + #0'ZZZW '^F;

type
    TShortDate	=  RECORD Month,Year: integer; end;
    TRegDate	=  RECORD Month,Day,Year: integer; end;


    PSeriesObj	= ^TSeriesObj;
    TSeriesObj	=  OBJECT(TPxObject)
	Series		: string[15];
	Book		: longint;
	Release		: integer;
	Author		: string[20];
	Title		: string;
	PubDate		: TShortDate;
	DueDate		: TRegDate;
      function  BuildFieldList : PSItem;  VIRTUAL;
      function  DmxTemplate : string;  VIRTUAL;
      function  DmxLabel : string;  VIRTUAL;
      function  GetText(MaxLen: integer) : string;  VIRTUAL;
      function  GetRecData : boolean;  VIRTUAL;
      function  PutRecData : boolean;  VIRTUAL;
      procedure GetRecDMX(var AData );  VIRTUAL;
      procedure SetRecDMX(var AData );  VIRTUAL;
    end;


    TSeriesRec  =  RECORD
      { required only when the PxObject is attached to a tvDMX object }
	Series			: string[13];
	Title			: string[30];
	Author			: string[16];
	PubDate			: TShortDate;
	DueDate			: TRegDate;
    end;


    TMyApp      =  OBJECT(TAppA)
      procedure HandleEvent(var Event: TEvent);  VIRTUAL;
      procedure InitMenuBar;  VIRTUAL;
    end;


var
    SeriesTbl	: PSeriesObj;



  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


function  TSeriesObj.BuildFieldList : PSItem;
{ required only when a PxObject needs to create a table }
begin
  BuildFieldList :=
	NewSItem('Book, N*',		{ seriesBook }
	NewSItem('Release, S*',		{ seriesRelease }
	NewSItem('Series, A10+',	{ seriesSeries }
	NewSItem('Title, A30',		{ seriesTitle }
	NewSItem('Author, A16',		{ seriesAuthor }
	NewSItem('Date, S',		{ seriesPubDate }
	NewSItem('Due Date, D',		{ seriesDueDate }
		nil)))))));
end;


function  TSeriesObj.DmxLabel : string;
{ required only when the PxObject is attached to a tvDMX object }
begin
  DmxLabel := ' Series         Title                           Author             Date       Due';
end;


function  TSeriesObj.DmxTemplate : string;
{ required only when the PxObject is attached to a tvDMX object }
begin
  DmxTemplate :=
		' sssssssssssssบ ssssssssssssssssssssssssssssss| ssssssssssssssss'
		+'บ'+fldPxShortDATE  { really a 16 bit integer field }
		+'บ'+fldPxDATE;
end;


function  TSeriesObj.GetText(MaxLen: integer) : string;
{ required only when the PxObject needs to generate a string }
var  i		: integer;
     S		: string;
     Fmts	: array[0..7] of pointer;
begin
  Fmts[0] := @Title;
  Fmts[1] := @Author;
  Fmts[2] := ptr(0, PubDate.Month);
  Fmts[3] := ptr(0, PubDate.Year);
  Fmts[4] := @Series;
  Fmts[5] := ptr(0, DueDate.Month);
  Fmts[6] := ptr(0, DueDate.Day);
  Fmts[7] := ptr(0, DueDate.Year);

  FormatStr(S, '%s, %s %2d/%04d  %s -- due on %2d/%02d/%04d', Fmts);

  If (length(S) > MaxLen) then S[0] := char(lo(MaxLen));
  GetText := S;
end;


function  TSeriesObj.GetRecData : boolean;
{ must be overridden to get records }
var  i,date	: integer;
     OK		: boolean;
begin
  OK := GetAlpha(seriesSeries,	Series) and
	GetLong(seriesBook,	Book) and
	GetShort(seriesRelease,	Release) and
	GetAlpha(seriesTitle,	Title) and
	GetAlpha(seriesAuthor,	Author) and
	GetShort(seriesPubDate,	date) and
	GetMDY(seriesDueDate,	DueDate.Month, DueDate.Day, DueDate.Year);
  PubDate.Year  := Date shr  4;
  PubDate.Month := Date and 15;
  GetRecData := OK;
end;


function  TSeriesObj.PutRecData : boolean;
{ must be overridden to store records, or to use some of the "Find" methods }
begin
  PutRecData :=
	PutAlpha(seriesSeries,	Series) and
	PutLong(seriesBook,	Book) and
	PutShort(seriesRelease,	Release) and
	PutAlpha(seriesTitle,	Title) and
	PutAlpha(seriesAuthor,	Author) and
	PutShort(seriesPubDate,	(PubDate.Year * 16) or (PubDate.Month and 15)) and
	PutMDY(seriesDueDate,	DueDate.Month, DueDate.Day, DueDate.Year);
end;


procedure TSeriesObj.GetRecDMX(var AData );
{ This abstract method transfers data for a tvDMX's ReadRec method. }
{ It is required only when the PxObject is attached to a tvDMX object. }
begin
  Title		:= TSeriesRec(AData).Title;
  Author	:= TSeriesRec(AData).Author;
  PubDate.Month	:= TSeriesRec(AData).PubDate.Month;
  PubDate.Year	:= TSeriesRec(AData).PubDate.Year;
  Series	:= TSeriesRec(AData).Series;
  DueDate.Day	:= TSeriesRec(AData).DueDate.Day;
  DueDate.Month	:= TSeriesRec(AData).DueDate.Month;
  DueDate.Year	:= TSeriesRec(AData).DueDate.Year;
end;


procedure TSeriesObj.SetRecDMX(var AData );
{ This abstract method transfers data for a tvDMX's ReadRec method. }
{ It is required only when the PxObject is attached to a tvDMX object. }
begin
  TSeriesRec(AData).Title		:= Title;
  TSeriesRec(AData).Author		:= Author;
  TSeriesRec(AData).PubDate.Month	:= PubDate.Month;
  TSeriesRec(AData).PubDate.Year	:= PubDate.Year;
  TSeriesRec(AData).Series		:= Series;
  TSeriesRec(AData).DueDate.Day		:= DueDate.Day;
  TSeriesRec(AData).DueDate.Month	:= DueDate.Month;
  TSeriesRec(AData).DueDate.Year	:= DueDate.Year;
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }


procedure TMyApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  If Event.What = evCommand then
    begin
    Case Event.Command of
      cmFindFirst:
          begin
         { This example searches for the record with Book = 5... }
          SeriesTbl^.Book := 5;
          If SeriesTbl^.FindFirst(seriesBook) then
            begin
         { ...and changes the book's title. }
            SeriesTbl^.Title := 'FindFirst demo worked!';
            SeriesTbl^.RecUpdate;  { update the table }
            SeriesTbl^.UpdateDMX;  { redraw the table }
            end;
          UserMessage('FindFirst test done');
          PxErrorBox(SeriesTbl^.Status);  { show any errors }
          SeriesTbl^.Reset;  { always reset errors }
          end;
      cmFindEach:
          begin
          BlinkMessage('Working...');
         { This example works on every book by "Mr. Hyde"... }
          SeriesTbl^.Author := 'Mr. Hyde';
          While SeriesTbl^.FindEach(seriesAuthor) do
            begin
         { ...and changes the book's title. }
            SeriesTbl^.Title := 'FindEach works too!';
            SeriesTbl^.RecUpdate;  { update the table }
            end;
          UserMessage('FindEach test done');
          PxErrorBox(SeriesTbl^.Status);  { show any errors }
          SeriesTbl^.UpdateDMX;  { redraw the table }
          SeriesTbl^.Reset;  { always reset errors }
          end;
      cmRecEach:
          begin
          BlinkMessage('Working...');
         { This example works on every book in the table... }
          While SeriesTbl^.RecEach do
            begin
         { ...and changes the book's title. }
            SeriesTbl^.Title := 'RecEach works too';
            SeriesTbl^.RecUpdate;  { update the table }
            end;
          UserMessage('RecEach test done');
          SeriesTbl^.UpdateDMX;  { redraw the table }
          SeriesTbl^.Reset;  { always reset errors }
          end;
     else Exit;
      end;
    ClearEvent(Event);
    end;
end;


procedure TMyApp.InitMenuBar;
var  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~Px~UNIT', hcNoContext, NewMenu(
      NewSoundItem(hcNoContext,
      NewVideoItem(hcNoContext,
      NewLine(
      NewItem('e~X~it',  'Alt-X',  kbAltX, cmQuit, hcNoContext,
      nil))))),
    NewSubMenu('~T~est', hcNoContext, NewMenu(
      NewItem('~F~indFirst',  '',  kbNoKey, cmFindFirst, hcNoContext,
      NewItem('Find~E~ach',  '',   kbNoKey, cmFindEach,  hcNoContext,
      NewItem('~R~ecEach',  '',    kbNoKey, cmRecEach,   hcNoContext,
      nil)))),
    NewSubMenu('~W~indow', hcNoContext, NewMenu(
      NewItem('~S~ize/Move', 'Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom',      'F5', kbF5,    cmZoom,	hcNoContext,
      NewItem('~T~ile',      '',   kbNoKey, cmTile,	hcNoContext,
      NewItem('C~a~scade',   '',   kbNoKey, cmCascade,	hcNoContext,
      NewItem('~N~ext',      'F6', kbF6,    cmNext,    hcNoContext,
      NewItem('~P~revious', 'Shift-F6', kbShiftF6, cmPrev, hcNoContext,
      NewItem('~C~lose', 'Alt-F3',  kbAltF3, cmClose,	hcNoContext,
      nil)))))))),
    nil))))
  ));
end;


  { ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ }

var  R	: TRect;
     W	: PWindow;
     i	: integer;
     MyApp  : TMyApp;

Begin
  PxInit;

  New(SeriesTbl, Init('SERTEST', 0, TRUE));

  If SeriesTbl^.Status = PxERR_TABLENOTFOUND then  { create a test table }
    begin
    SeriesTbl^.Reset;  { clear the error }
    SeriesTbl^.Build('SERTEST');
    SeriesTbl^.Open('SERTEST', 0, TRUE);

    { populate the test table with dummy data }
    For i := 1 to 50 do
      begin
      SeriesTbl^.Book := i;
      SeriesTbl^.Release := 0;
      Str(i:4, SeriesTbl^.Series);
      SeriesTbl^.Series := 'TEST' + SeriesTbl^.Series;
      SeriesTbl^.Title := 'Test Title';
      If random(2) = 0 then
	SeriesTbl^.Author := 'Dr. Jeckll'
       else
	SeriesTbl^.Author := 'Mr. Hyde';

      SeriesTbl^.PubDate.Month := random(12) + 1;
      SeriesTbl^.PubDate.Year  := 1997 - random(25);

      SeriesTbl^.DueDate.Month := random(12) + 1;
      SeriesTbl^.DueDate.Day   := random(28) + 1;
      SeriesTbl^.DueDate.Year  := 1997;

      SeriesTbl^.RecAppend;
      writeln(SeriesTbl^.GetText(79));
      end;
    end;

  MyApp.Init;

  AssignWinRect(R, 0, 0);
  New(W, Init(R, 'Work Window', wnNextAvail));
  With W^ do
    begin
    Options := Options or ofTileable; { must be tileable for AssignWinRect }
    GetExtent(R);		  { create new rectangle for editor object }
    R.Grow(-1,-1);			      { shrink -1 to avoid borders }
    Inc(R.A.Y, 2);			 { make room for TDmxLabels object }
    Insert(New(PPxObjDMX,
	   Init(SeriesTbl,
		R,
		New(PDmxFLabels, InitInsert(W, SeriesTbl^.DmxLabel)),
		New(PDmxRecInd, InitInsert(W, 10)),
		W^.StandardScrollBar(sbHorizontal),
		W^.StandardScrollBar(sbVertical)
		)
	));
    end;
  DeskTop^.Insert(Application^.ValidView(W));
  Message(Desktop, evCommand, cmDMX_Unlock, nil);

  MyApp.Run;
  MyApp.Done;


  Dispose(SeriesTbl, Done);

  PxExit;
End.
