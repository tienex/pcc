{ Free Pascal - Hello World with Modern Features }
program HelloFreePascal;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  { Generic list example (FreePascal extension) }
  generic TList<T> = class
  private
    FItems: array of T;
  public
    procedure Add(const Item: T);
    function Count: Integer;
  end;

procedure TList.Add(const Item: T);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := Item;
end;

function TList.Count: Integer;
begin
  Result := Length(FItems);
end;

var
  greeting: string;
  i: integer;

begin
  greeting := 'Hello from Free Pascal!';
  WriteLn(greeting);

  // Demonstrate break/continue
  for i := 1 to 10 do
  begin
    if i = 5 then
      continue;
    if i > 8 then
      break;
    WriteLn('Count: ', i);
  end;

  WriteLn('Done!');
end.
