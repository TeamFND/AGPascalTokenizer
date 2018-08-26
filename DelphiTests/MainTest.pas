unit MainTest;

interface
uses
  DUnitX.TestFramework;

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
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
