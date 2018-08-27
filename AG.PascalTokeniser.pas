unit AG.PascalTokeniser;

interface

uses
  System.Generics.Collections,
  System.Classes;

function is_comment(s:string);
function is_name(s:string);
function is_string(s:string);

const
  SYMS1 = ['(',')','[',']','/','|','\','@','#','=','>','<',':',';',',','.','$','+','-','*'];
  SYMS2 = ['>=','<=','<>',':=','..','-=','+=','/=','*='];
  SPACES = [#12,#10,#13,#9,#11,' '];
  NO_NAME_SYMS = SYMS1 + SPACES + ['{','}'];
  CHARS_ID0 = '&abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  CHARS_ID = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_';

type
  PasTokenizer=class
    private
      s:TStrings;
      y:integer;
      x:integer;
      ended:boolean;
      procedure _do_readable();
      procedure _is_readable();
      procedure _next_readable();
      procedure _skip_spaces();
      procedure _get_pos();
      procedure _set_pos(i0:integer; i1:integer);
    public
      procedure get_next();
      procedure read_next();
      procedure is_ended();
  end;
  PasTokenizerStack=class
    private
      stack:TStack<integer>;
      // _pop
      procedure _get_with_comments();
      procedure _get_without_comments();
    public
      procedure push(s:string);
      procedure pop();
      procedure read_last();
      procedure is_ended();
  end;

implementation

function is_comment(s:string);
begin
  // TODO
end;

function is_name(s:string);
var
  i:integer;
begin
  if length(s) <= 0 then Exit(False);
  if s = '&' then Exit(False);
  if not (s[0] in CHARS_ID0) then Exit(False);
  for i := 1 to length(s) do
  begin
    if not (s[i] in CHARS_ID) then
      Exit(False);
  end;
end;

function is_string(s);
begin
  // TODO
end;

class function PasTokenizer._do_readable();
begin
  if not _is_readable() then
  begin
    if (y+1 = Length(s)) then
    begin
     ended = True;
    end
    else begin
      inc(y);
      x = 0;
      while not s[y] = '' do
      begin
        if y+1 = length(s) then
        begin
          ended = True;
          break;
        end;
        inc(y);
      end;
    end;
    Exit(True);
  end else Exit(False);
end;

class function PasTokenizer._is_readable();
begin
  Exit(length(s[y]) > x);
end;

class function PasTokenizer._next_readable();
begin
  inc(x);
  Exit(_do_readable());
end;

class function PasTokenizer._skip_spaces();
begin
  _do_readable();
  if not ended then
  begin
    while s[y][x] in SPACES do
    _next_readable();
  end;
end;

class function PasTokenizer._get_pos();
begin
  Exit(y, x);
end;

class function PasTokenizer._set_pos(i0:integer; i1:integer);
begin
  y = i0;
  x = i1;
  ended = False;
  _do_readable();
end;

class function PasTokenizer.get_next();
var
  begin_pos:integer;
  l:integer;
  last_i0:integer;
  m1:string = '';
  ss:string = '';
  line:string;
  now_sym:char;
  next_sym:char;
  f:boolean = True;
  str_changed:boolean = True;
begin
  begin_pos = _get_pos();
  while f and not ended do
  begin
    line = s[y];
    now_sym = line[x];
    l = length(line);
    if x+1 <> 1 then
    begin
      next_sym = line[x+1];
    end else begin
      next_sym = '';
    end;
    if m1 = '' then
    begin
      if now_sym = '/' then
      begin
        if next_sym = '/' then
        begin
          ss = line[x];
          x = 1;
          break;
        end;
      end
      else if now_sym = '{' then
      begin
        m1 = '}';
        ss = [now_sym];
        last_i0 = y;
      end
      else if now_sym = '(' then
      begin
        // TODO
      end;
      // TODO
    end;
  end;
end;
end.
