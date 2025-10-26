{ Delphi Object Pascal - Hello World }
unit HelloDelphi;

interface

uses
  System, SysUtils;  // Delphi units

type
  TGreeter = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    procedure Greet;
    property Name: string read FName write FName;
  end;

implementation

constructor TGreeter.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TGreeter.Greet;
begin
  WriteLn('Hello from Delphi, ', FName, '!');
end;

end.
