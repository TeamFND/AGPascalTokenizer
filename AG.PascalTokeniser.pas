unit AG.PascalTokeniser;

interface

uses
  System.Generics.Collections, System.SysUtils,
  System.Classes;

type
  TTokenizerPos = record
    x, y: integer;
  end;

  TToken = record
    Text: string;
    &begin, &end: TTokenizerPos;
    ended: boolean;
    constructor Create(Text: string; &begin, &end: TTokenizerPos;
      ended: boolean);
  end;

  TPasTokenizer = class
  private
    s: TStrings;
    y: integer;
    x: integer;
    ended: boolean;
    function _do_readable(): boolean;
    function _is_readable(): boolean;
    function _next_readable(): boolean;
    procedure _skip_spaces();
    function _get_pos(): TTokenizerPos;
    procedure _set_pos(i0: integer; i1: integer);
  public
    function get_next(): TToken;
    // procedure read_next();
    // procedure is_ended();
    constructor Create(input:TStrings);
  end;

  {PasTokenizerStack = class
  private
    stack: TStack<integer>;
    // _pop
    procedure _get_with_comments();
    procedure _get_without_comments();
  public
    procedure push(s: string);
    procedure pop();
    procedure read_last();
    procedure is_ended();
  end;}

function is_comment(s: string): boolean;
function is_name(s: string): boolean;
function is_string(s: string): boolean;

implementation

constructor TToken.Create(Text: string; &begin, &end: TTokenizerPos;
  ended: boolean);
begin
  Self.Text := Text;
  Self.&begin := &begin;
  Self.&end := &end;
  Self.ended := ended;
end;

const
  SYMS1 = '()[]/|\@#=><:;,.$+-*';
  SPACES = #12#10#13#9#11' ';
  NO_NAME_SYMS = SYMS1 + SPACES + '{}';
  CHARS_ID0 = '&abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  CHARS_ID = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_';
  fix = {$IFDEF NEXTGEN}-1{$ELSE}0{$ENDIF};

var
  SYMS2: TList<String>; // array[0..8]of string=();

function is_comment(s: string): boolean;
begin
  // TODO
end;

function is_name(s: string): boolean;
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
end;

function is_string(s: string): boolean;
begin
  // TODO
end;

function TPasTokenizer._do_readable(): boolean;
begin
  if not _is_readable() then
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

function TPasTokenizer._is_readable(): boolean;
begin
  Exit(length(s[y])+1+Fix > x);
end;

function TPasTokenizer._next_readable(): boolean;
begin
  inc(x);
  Result := _do_readable();
end;

procedure TPasTokenizer._skip_spaces();
begin
  _do_readable();
  if not ended then
  begin
    while SPACES.Contains(s[y][x]) do
      _next_readable();
  end;
end;

function TPasTokenizer._get_pos(): TTokenizerPos;
begin
  Result.x := x;
  Result.y := y;
end;

procedure TPasTokenizer._set_pos(i0: integer; i1: integer);
begin
  y := i0;
  x := i1;
  ended := False;
  _do_readable();
end;

function TPasTokenizer.get_next(): TToken;
var
  l,i,last_i0:integer;
  ml,ss,line:string;
  now_sym,next_sym:char;
  f,str_changed:boolean;
  begin_pos:TTokenizerPos;
begin
  ml := '';
  ss := '';
  f := True;
  str_changed := True;
  begin_pos := _get_pos();
  while f and not ended do
  begin
    line := s[y];
    now_sym := line[x];
    l := length(line);
    if x<>l+Fix then
      next_sym := line[x + 1]
    else
      next_sym := #0;
    if ml='' then
    begin
      if now_sym = '/' then
      begin
        if next_sym = '/' then
        begin
          for i:=x to l+Fix do
            ss:=ss+line[i];
          x := l+Fix;
          break;
        end;
      end
      else if now_sym = '{' then
      begin
        ml := '}';
        ss := now_sym;
        last_i0 := y;
      end
      else if now_sym = '(' then
      begin
        if next_sym = '*' then
        begin
          ml := ')';
          inc(x);
          last_i0 := y;
          ss := now_sym + next_sym;
        end
        else
        begin
          ss := '(';
          inc(x);
          break;
        end;
      end
      else
      begin
        if SYMS1.Contains(now_sym) then
        begin
          ss := now_sym;
          inc(x);
          if SYMS2.Contains(now_sym + next_sym) then
          begin
            inc(x);
            ss := ss + next_sym;
          end;
          break;
        end
        else if now_sym = #39 then
        begin
          ss := #39;
          inc(x);
          if next_sym <> '' then
          begin
            ss := ss + next_sym;
            while line[x] <> #39 do
            begin
              inc(x);
              if not _is_readable() then
              begin
                dec(x);
                break;
              end;
              ss := ss + line[x];
            end;
            inc(x);
          end;
          break;
        end
        else
        begin
          while not NO_NAME_SYMS.Contains(line[x]) do
          begin
            ss := ss + line[x];
            inc(x);
            if not _is_readable() then
              break;
          end;
          break;
        end;
      end;
    end
    else
    begin
      while last_i0 <> y do
      begin
        ss := ss + #10;
        inc(last_i0);
      end;
      if ss[length(ss) + fix] = #10 then
      begin
        ss[length(ss) + fix] := now_sym;
        ss := ss + #10;
      end;
      if now_sym = ml then
        if ml = '}' then
        begin
          inc(x);
          break;
        end
        else if (x <> 0) and (line[x - 1] = '*') then
        begin
          inc(x);
          break;
        end;
    end;
    _next_readable();
  end;
  Result := TToken.Create(ss, begin_pos, _get_pos, ended);
  _skip_spaces;
end;

constructor TPasTokenizer.Create(input:TStrings);
begin
  s:=input;
  y:=0;
  x:=1+fix;
  ended:=False;
  _skip_spaces;
end;

initialization
SYMS2 := TList<string>.Create();
SYMS2.Add('>=');
SYMS2.Add('<=');
SYMS2.Add('<>');
SYMS2.Add(':=');
SYMS2.Add('..');
SYMS2.Add('-=');
SYMS2.Add('+=');
SYMS2.Add('/=');
SYMS2.Add('*=');

end.
