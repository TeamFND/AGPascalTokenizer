unit AG.PascalTokenizer;

interface

uses
  {$IFDEF FPC}
    SysUtils,Classes
  {$ELSE}
    System.Generics.Collections,System.SysUtils,System.Classes
  {$ENDIF};

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

type
  TAGTokenizerPos = record
    x, y: integer;
  end;

  TAGToken = record
    Text: string;
    &begin, &end: TAGTokenizerPos;
    ended: boolean;
    {$IFNDEF FPC}constructor Create(Text: string; &begin, &end: TAGTokenizerPos;ended:boolean);{$ENDIF}
  end;

  TAGPasTokenizer = class
  strict protected
    s: TStrings;
    y: integer;
    x: integer;
    function DoReadable():boolean;
    function IsReadable():boolean;
    function NextReadable():boolean;
    procedure SkipSpaces();
    function GetPos():TAGTokenizerPos;
    procedure SetPos(pos:TAGTokenizerPos);
  public
    ended: boolean;
    function GetNext(): TAGToken;
    // procedure read_next();
    constructor Create(input:TStrings);
    property Pos:TAGTokenizerPos read GetPos write SetPos;
  end;

  {$IFNDEF FPC}
  TAGPasTokenizerStack=class
  strict protected
    type
      GetCall=reference to function(Tokenizer:TAGPasTokenizer):TAGToken;
    var
      Stack:TStack<TAGToken>;
      Tokenizer:TAGPasTokenizer;
      Get:GetCall;
    function GetLast():TAGToken;
    function IsEnded():Boolean;
  public
    constructor Create(input:TStrings;GetComments:boolean=True);
    procedure Push(t:TAGToken);
    function Pop():TAGToken;
    property Last:TAGToken read GetLast write Push;
    property Ended:Boolean read IsEnded;
  end;
  {$ENDIF}

function IsComment(s: string): boolean;
function IsName(s: string): boolean;
function IsString(s: string): boolean;

implementation

{$IFNDEF FPC}
constructor TAGToken.Create(Text: string; &begin, &end: TAGTokenizerPos;
  ended: boolean);
begin
  Self.Text := Text;
  Self.&begin := &begin;
  Self.&end := &end;
  Self.ended := ended;
end;
{$ENDIF}

const
  SYMS1 = '()[]/|\@#=><:;,.$+-*^';
  SPACES = #12#10#13#9#11' ';
  NO_NAME_SYMS = SYMS1 + SPACES + '{}';
  CHARS_ID0 = '&abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  CHARS_ID = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_';
  fix = {$IFDEF NEXTGEN}-1{$ELSE}0{$ENDIF};

var
  SYMS2:{$IFDEF FPC}TStringList{$ELSE}TList<string>{$ENDIF}; // array[0..8]of string=();

function IsComment(s:string):boolean;
begin
  Result:=(s.startswith('{') or s.startswith('(*') or s.startswith('//'));
end;

function IsName(s:string):boolean;
var
  i: integer;
begin
  if length(s) <= 0 then
    Exit(False);
  if s = '&' then
    Exit(False);
  if not CHARS_ID0.Contains(s[1 + fix]) then
    Exit(False);
  for i := 1 to length(s) do
  begin
    if not CHARS_ID.Contains(s[i]) then
      Exit(False);
  end;
  Result:=True;
end;

function IsString(s: string):boolean;
begin
  Result:=s.StartsWith(#39);
end;

function TAGPasTokenizer.DoReadable(): boolean;
begin
  if not IsReadable() then
  begin
    if (y + 1 = s.Count) then
      ended := True
    else
    begin
      inc(y);
      x := 1+Fix;
      while s[y]='' do
      begin
	if y + 1 = s.Count then
	begin
	  ended := True;
	  break;
	end;
	inc(y);
      end;
    end;
    Exit(True);
  end
  else
    Exit(False);
end;

function TAGPasTokenizer.IsReadable(): boolean;
begin
  Exit(length(s[y])+1+Fix > x);
end;

function TAGPasTokenizer.NextReadable(): boolean;
begin
  inc(x);
  Result := DoReadable();
end;

procedure TAGPasTokenizer.SkipSpaces();
begin
  DoReadable();
  if not ended then
  begin
    while SPACES.Contains(s[y][x]) do
      NextReadable();
  end;
end;

function TAGPasTokenizer.GetPos(): TAGTokenizerPos;
begin
  Result.x := x;
  Result.y := y;
end;

procedure TAGPasTokenizer.SetPos(pos:TAGTokenizerPos);
begin
  y:=Pos.x;
  x:=Pos.y;
  ended:=False;
  DoReadable();
end;

function TAGPasTokenizer.GetNext(): TAGToken;
var
  l,i,last_i0:integer;
  ml,ss,line:string;
  now_sym,next_sym:char;
  f{$IFDEF FPC},ff{$ENDIF}:boolean;
  begin_pos:TAGTokenizerPos;
begin
  ml:='';
  ss:='';
  f:=True;
  begin_pos:=GetPos();
  while f and not ended do
  begin
    line:=s[y];
    now_sym:=line[x];
    l:=length(line);
    if x<>l+Fix then
      next_sym:=line[x+1]
    else
      next_sym:=#0;
    if ml='' then
    begin
      if now_sym='/' then
      begin
        if next_sym='/' then
        begin
          for i:=x to l+Fix do
            ss:=ss+line[i];
          x:=l+Fix;
          break;
        end;
      end
      else if now_sym='{' then
      begin
        ml:='}';
        ss:=now_sym;
        last_i0:=y;
      end
      else if now_sym='(' then
      begin
        if next_sym='*' then
        begin
          ml:=')';
          inc(x);
          last_i0:=y;
          ss:=now_sym+next_sym;
        end
        else
        begin
          ss:='(';
          inc(x);
          break;
        end;
      end
      else
      begin
        if SYMS1.Contains(now_sym)then
        begin
          ss:=now_sym;
          inc(x);
          if SYMS2.
            {$IFDEF FPC}
              IndexOf(now_sym+next_sym)<>-1
            {$ELSE}
              Contains(now_sym+next_sym)
            {$ENDIF}then
          begin
            inc(x);
            ss:=ss+next_sym;
          end;
        break;
        end
        else if now_sym=#39 then
        begin
          ss:=#39;
          inc(x);
          if next_sym<>'' then
          begin
            ss:=ss+next_sym;
            while line[x] <> #39 do
            begin
              inc(x);
              if not IsReadable() then
              begin
                dec(x);
                break;
              end;
              ss:=ss+line[x];
            end;
            inc(x);
          end;
          break;
        end
        else
        begin
          while not NO_NAME_SYMS.Contains(line[x]) do
          begin
            ss:=ss+line[x];
            inc(x);
            if not IsReadable() then
              break;
          end;
          break;
        end;
      end;
    end
    else
    begin
      while last_i0<>y do
      begin
        ss:=ss+#10;
        inc(last_i0);
      end;
      ss:=ss+now_sym;
      if now_sym=ml then
        if ml='}' then
        begin
          inc(x);
          break;
        end
        else if(x<>0)and(line[x-1]='*')then
        begin
          inc(x);
          break;
        end;
    end;
    NextReadable();
  end;
  {$IFDEF FPC}
  Result.Text:=ss;
  Result.&begin:=begin_pos;
  Result.&end:=GetPos;
  Result.ended:=ended;
  {$ELSE}
  Result:=TAGToken.Create(ss,begin_pos,GetPos,ended);
  {$ENDIF}
  SkipSpaces;
end;

constructor TAGPasTokenizer.Create(input:TStrings);
begin
  s:=input;
  y:=0;
  x:=1+fix;
  ended:=False;
  SkipSpaces;
end;

{$IFNDEF FPC}
{TAGPasTokenizerStack}

function TAGPasTokenizerStack.GetLast():TAGToken;
begin
if Stack.Count<>0 then
  Result:=Stack.Peek
else
begin
  Result:=Get;
  Stack.Push(Result);
end;
end;

function TAGPasTokenizerStack.IsEnded():Boolean;
begin

end;

constructor TAGPasTokenizerStack.Create(input:TStrings;GetComments:boolean=True);
begin
Stack:=TStack<TAGToken>.Create();
Tokenizer:=TAGPasTokenizer.Create(input);
if GetComments then
  Get:=function(Tokenizer:TAGPasTokenizer):TAGToken
    begin
      Result:=Tokenizer.GetNext;
    end
else
  Get:=function(Tokenizer:TAGPasTokenizer):TAGToken
    begin
      while True do
      begin
        Result:=Tokenizer.GetNext;
        if Result.ended or not IsComment(Result.Text) then
          break;
      end;
    end;
end;

procedure TAGPasTokenizerStack.Push(t:TAGToken);
begin
Stack.Push(t);
end;

function TAGPasTokenizerStack.Pop():TAGToken;
begin
if Stack.Count<>0 then
  Result:=Stack.Pop
else
  Result:=Get;
end;

{$ENDIF}

initialization
SYMS2 := {$IFDEF FPC}TStringList{$ELSE}TList<string>{$ENDIF}.Create();
SYMS2.Add('>=');
SYMS2.Add('<=');
SYMS2.Add('<>');
SYMS2.Add(':=');
SYMS2.Add('..');
SYMS2.Add('-=');
SYMS2.Add('+=');
SYMS2.Add('/=');
SYMS2.Add('*=');
SYMS2.Add('**');
SYMS2.Add('><');
SYMS2.Add('(.');
SYMS2.Add('.)');
SYMS2.Add('<<');
SYMS2.Add('>>');
end.
