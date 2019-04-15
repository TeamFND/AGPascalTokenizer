program Demo;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

{$R *.res}


uses
  {$IF not defined(FPC)}
  //FastMM4,
  {$endif}
  SysUtils,
  Classes,
  AG.PascalTokenizer in '../AG.PascalTokenizer.pas';

procedure Test(sList:TStrings; toStream:TStream; sync: boolean);
var tokenStack: TAGPasTokenizerStack;
  s:UnicodeString;
begin

  tokenStack := nil;
  try
    if sync then
      tokenStack := TAGPasTokenizerStack.Create(sList)
    else
      tokenStack:= TAGPasTokenizerParallelStack.Create(sList);

    while not tokenStack.ended do begin
      s := tokenStack.Pop.Text+#10;
      toStream.Write(Pointer(s)^, Length(s)*2);
    end;
  finally
    FreeAndNil(tokenStack);
  end;
end;

var start: Cardinal;
  sList: TStringList;
   sw:TFileStream;
begin
  TObject.Create();//утечка
  try
    sList := TStringList.Create();
    try
      sList.LoadFromFile('c:\prosoft\RIO\source\rtl\common\System.Classes.pas');
      sw:=TFileStream.Create('tokens1.txt', fmCreate or fmOpenWrite);

      start := TThread.GetTickCount;
      Test(sList,sw, true);

      start := TThread.GetTickCount - start;
      Writeln('Sync Done in ', start, ' ms');
      sw.Free;
      sw:=TFileStream.Create('tokens2.txt', fmCreate or fmOpenWrite);
      start := TThread.GetTickCount;
      Test(sList,sw, false);

      start := TThread.GetTickCount - start;
      sw.Free;
      Writeln('Sync Done in ', start, ' ms');

    finally
      sList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
