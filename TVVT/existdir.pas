program existdir;
{ Confirms the existence of a directory given on the command
line. Returns errorlevel 2 on error, 1 if not found, 0 if
found. }

uses
dos;

var
s : searchrec;

begin
if paramcount <> 1 then
begin
writeln('Syntax: EXISTDIR directory');
halt(2);
end
else
begin
findfirst(paramstr(1),Directory,S);
while (Doserror = 0) and ((Directory and S.Attr) = 0) do
findnext(S);
if Doserror <> 0 then
begin
Writeln('Directory not found.');
halt(1);
end
else
begin
Writeln('Directory found.');
halt(0);
end;
end;
end.