unit MainTest;

interface
uses
  DUnitX.TestFramework,
  System.Classes,
  WinAPI.Windows,
  AG.PascalTokeniser;

type
  [TestFixture]
  TMyTestObject = class(TObject) 
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
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

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
