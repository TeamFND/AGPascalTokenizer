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
    [Test]
    procedure Test4;
    [Test]
    procedure TestParallel;
    [Test]
    procedure Test5;
    // Test with TestCase Atribute to supply parameters.
  end;

implementation

procedure TMyTestObject.Test1;
var
  input:TStrings;
  tokenizer:TAGPasTokenizer;
  token:TAGToken;
begin
  input:= TStringList.Create();
  input.LoadFromFile('..\..\MainTest.pas');
  tokenizer:=TAGPasTokenizer.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.GetNext;
    TDUnitX.CurrentRunner.Log(TLogLevel.Information, token.Text);
  end;
end;

procedure TMyTestObject.Test2;
var
  s:string;
begin
  s:=#39'kek'#39;
  if not IsString(s) then
    raise Exception.Create('Is string error 1');
  s:='s:=12334;';
  if IsString(s) then
    raise Exception.Create('Is string error 2');
end;

procedure TMyTestObject.Test3;
var
  s:string;
begin
  s:='{ asdasdasd }';
  if not IsComment(s) then
    raise Exception.Create('Is comment error 1');
  s:='(* s:=12334;*)';
  if not IsComment(s) then
    raise Exception.Create('Is comment error 2');
  s:='// s:=12334;*)';
  if not IsComment(s) then
    raise Exception.Create('Is comment error 3');
end;

procedure TMyTestObject.Test4;
var
  input:TStrings;
  tokenizer:TAGPasTokenizerStack;
  token:TAGToken;
begin
  input:= TStringList.Create();
  input.LoadFromFile('..\..\MainTest.pas');
  tokenizer:=TAGPasTokenizerStack.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.Pop;
    TDUnitX.CurrentRunner.Log(TLogLevel.Information, token.Text);
  end;
end;

procedure TMyTestObject.Test5;
var
  input:TStrings;
  tokenizer:TAGPasTokenizerStack;
  token:TAGToken;
begin
  input:= TStringList.Create();
  input.LoadFromFile('..\..\MainTest.pas');
  tokenizer:=TAGPasTokenizerStack.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.Pop;
    TDUnitX.CurrentRunner.Log(TLogLevel.Information, token.Text);
  end;
end;

procedure TMyTestObject.TestParallel;
var
  input:TStrings;
  tokenizer:TAGPasTokenizerStack;
  token:TAGToken;
  begin
  input:= TStringList.Create();
  input.LoadFromFile('..\..\MainTest.pas');
  tokenizer:=TAGPasTokenizerParallelStack.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.Pop;
    TDUnitX.CurrentRunner.Log(TLogLevel.Information, token.Text);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
