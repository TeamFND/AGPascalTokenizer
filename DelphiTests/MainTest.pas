unit MainTest;

interface
uses
  DUnitX.TestFramework,
  System.Classes,
  WinAPI.Windows,
  SysUtils,
  AG.PascalTokenizer;

type
  [TestFixture]
  TMyTestObject = class(TObject) 
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    [Test]
    procedure Test2;
    [Test]
    procedure Test3;
    // Test with TestCase Atribute to supply parameters.
  end;

implementation

procedure TMyTestObject.Test1;
var
  input:TStrings;
  tokenizer:TPasTokenizer;
  token:TToken;
begin
  input:= TStringList.Create();
  input.LoadFromFile('..\..\MainTest.pas');
  tokenizer:=TPasTokenizer.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.get_next;
    TDUnitX.CurrentRunner.Log(TLogLevel.Information, token.Text);
  end;
  //sleep(10000);
end;

procedure TMyTestObject.Test2;
var
  s:string;
begin
  s:=#39'kek'#39;
  if not is_string(s) then
    raise Exception.Create('Is string error 1');
  s:='s:=12334;';
  if is_string(s) then
    raise Exception.Create('Is string error 2');
end;

procedure TMyTestObject.Test3;
var
  s:string;
begin
  s:='{ asdasdasd }';
  if not is_comment(s) then
    raise Exception.Create('Is comment error 1');
  s:='(* s:=12334;*)';
  if not is_comment(s) then
    raise Exception.Create('Is comment error 2');
  s:='// s:=12334;*)';
  if not is_comment(s) then
    raise Exception.Create('Is comment error 3');
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
