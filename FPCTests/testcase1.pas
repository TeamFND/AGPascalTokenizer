unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils,GuiTestRunner, testregistry,AG.PascalTokenizer;

type

  TTestCase1= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestCase1.TestHookUp;
var
  input:TStrings;
  tokenizer:TPasTokenizer;
  token:TToken;
begin
  input:= TStringList.Create();
  input.LoadFromFile('testcase1.pas');
  tokenizer:=TPasTokenizer.Create(input);
  token.ended:=False;
  while not token.ended do
  begin
    token:=tokenizer.get_next;
    TestRunner.MemoLog.Append(token.Text);
  end;
end;



initialization

  RegisterTest(TTestCase1);
end.

