unit AG.PascalTokenizer;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

  uses
    SysUtils, Classes, IniFiles, SyncObjs, Generics.Collections;


  type
    TAGTokenizerPos = record
      x, y: integer;
    end;

    TAGToken = record
      Text:string;
      &begin, &end: TAGTokenizerPos;
      ended: boolean;
      procedure Make(const Text:string;&begin, &end: TAGTokenizerPos;ended: boolean);
    end;

    TAGPasTokenizer = class
      strict protected
        FStrings: TStrings;
        FLineIx: integer;
        x: integer;
        function DoReadable(): boolean;
        function IsReadable(): boolean;
        function NextReadable(): boolean;
        procedure SkipSpaces();
        function GetPos(): TAGTokenizerPos;
        procedure SetPos(pos: TAGTokenizerPos);
      public
        ended: boolean;
        function GetNext(): TAGToken;
        // procedure read_next();
        constructor Create(input: TStrings);
        property pos: TAGTokenizerPos read GetPos write SetPos;
    end;

    TAGPasTokenizerStack = class
      strict protected
      type
        GetCall = function(Tokenizer: TAGPasTokenizer): TAGToken of object;

      var
        Stack: TQueue<TAGToken>;
        Tokenizer: TAGPasTokenizer;
        Get: GetCall;
        IsEnd: boolean;
        function GetLast(): TAGToken;virtual;
        function GetWithComments(Tokenizer: TAGPasTokenizer): TAGToken;
        function GetWithoutComments(Tokenizer: TAGPasTokenizer): TAGToken;
        destructor Destroy;override;
      protected
        function GetCachedCount: integer;inline;
      public
        constructor Create(input: TStrings;GetComments: boolean = True);
        procedure Push(const t: TAGToken);virtual;
        function Pop(): TAGToken;virtual;
        property Last: TAGToken read GetLast write Push;
        property ended: boolean read IsEnd;
    end;

    TAGPasTokenizerParallelStack = class(TAGPasTokenizerStack)
      strict protected
      type
        TWorkerThread = class(TThread)
          strict protected
            FStack: TAGPasTokenizerParallelStack;
            procedure Execute;override;
          public
            Idling: boolean;
            constructor Create(const Stack: TAGPasTokenizerParallelStack);
        end;

      var
        FWorker: TWorkerThread;
        FStackLock: TCriticalSection;
        function AddTokenToStack: boolean;
        function GetLast(): TAGToken;override;
        procedure EnsureThreadDone();
        destructor Destroy;override;
      protected
        FStackHalfMax: integer;
        FSignal: TEvent;
        procedure Push(const t: TAGToken);override;
      public
        constructor Create(const input: TStrings;GetComments: boolean = True;
        stackMax: integer = 1000);
        function Pop(): TAGToken;override;
    end;

  function IsComment(s:string): boolean;
  function IsName(s:string): boolean;
  function IsString(const s:string): boolean;

implementation

  procedure TAGToken.Make(const Text:string;&begin, &end: TAGTokenizerPos;ended:
  boolean);
  begin
    Self.Text := Text;
    Self.&begin := &begin;
    Self.&end := &end;
    Self.ended := ended;
  end;

  const
    SYMS1 = '()[]/|\@#=><:;,.$+-*^';
    SPACES = #12#10#13#9#11' ';
    NO_NAME_SYMS = SYMS1 + SPACES + '{}';
    CHARS_ID0 = '&abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
    CHARS_ID = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_';
    fix = {$IFDEF NEXTGEN}-1{$ELSE}0{$ENDIF};

  var
    SYMS2: THashedStringList;

  function IsComment(s:string): boolean;
  begin
    Result :=(s.startswith('{')or s.startswith('(*')or s.startswith('//'));
  end;

  function IsName(s:string): boolean;
  var
    i: integer;
  begin
    if length(s)<= 0 then
      Exit(False);
    if s = '&' then
      Exit(False);
    if not CHARS_ID0.Contains(s[1 + fix])then
      Exit(False);
    for i := 1 to length(s)do
    begin
      if not CHARS_ID.Contains(s[i])then
        Exit(False);
    end;
    Result := True;
  end;

  function IsString(const s:string): boolean;
  begin
    Result := s.startswith(#39);
  end;

  function TAGPasTokenizer.DoReadable(): boolean;
  begin
    if not IsReadable()then
    begin
      if(FLineIx + 1 = FStrings.Count)then
        ended := True
      else
      begin
        inc(FLineIx);
        x := 1 + fix;
        while FStrings[FLineIx]<= '' do
        begin
          if FLineIx + 1 = FStrings.Count then
          begin
            ended := True;
            break;
          end;
          inc(FLineIx);
        end;
      end;
      Exit(True);
    end
    else
      Exit(False);
  end;

  function TAGPasTokenizer.IsReadable(): boolean;
  begin
    Result := x <= length(FStrings[FLineIx])+ fix;
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
      while SPACES.Contains(FStrings[FLineIx][x])do
        NextReadable();
    end;
  end;

  function TAGPasTokenizer.GetPos(): TAGTokenizerPos;
  begin
    Result.x := x;
    Result.y := FLineIx;
  end;

  procedure TAGPasTokenizer.SetPos(pos: TAGTokenizerPos);
  begin
    FLineIx := pos.x;
    x := pos.y;
    ended := False;
    DoReadable();
  end;

  function TAGPasTokenizer.GetNext(): TAGToken;
  var
    l,  last_i0: integer;
    ml, ss, line:string;
    now_sym, next_sym: char;
    f: boolean;
    begin_pos: TAGTokenizerPos;
  begin
    ml := '';
    ss := '';
    f := True;
    begin_pos := GetPos();
    while f and not ended do
    begin
      line := FStrings[FLineIx];
      now_sym := line[x];
      l := length(line);
      if x < l + fix then
        next_sym := line[x + 1]
      else
        next_sym := #0;

      if ml = '' then
      begin
        if now_sym = '/' then
        begin
          if next_sym = '/' then
          begin
            ss := Copy(line, x, l);
            x := l + 1 + fix;
            break;
          end;
        end
        else if now_sym = '{' then
        begin
          ml := '}';
          ss := now_sym;
          last_i0 := FLineIx;
        end
        else if now_sym = '(' then
        begin
          if next_sym = '*' then
          begin
            ml := ')';
            inc(x);
            last_i0 := FLineIx;
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
          if SYMS1.Contains(now_sym)then
          begin
            ss := now_sym;
            inc(x);
            if SYMS2.IndexOf(now_sym + next_sym)<>-1 then begin
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
              while line[x]<> #39 do
              begin
                inc(x);
                if not IsReadable()then
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
            while not NO_NAME_SYMS.Contains(line[x])do
            begin
              ss := ss + line[x];
              inc(x);
              if not IsReadable()then
                break;
            end;
            break;
          end;
        end;
      end
      else
      begin
        while last_i0 <> FLineIx do
        begin
          ss := ss + #10;
          inc(last_i0);
        end;
        ss := ss + now_sym;
        if now_sym = ml then
          if ml = '}' then
          begin
            inc(x);
            break;
          end
          else if(x <> 0)and(line[x - 1]= '*')then
          begin
            inc(x);
            break;
          end;
      end;
      NextReadable();
    end;
    Result.Make(ss, begin_pos, GetPos, ended);
    SkipSpaces;
  end;

  constructor TAGPasTokenizer.Create(input: TStrings);
  begin
    FStrings := input;
    FLineIx := 0;
    x := 1 + fix;
    ended := False;
    SkipSpaces;
  end;

  { TAGPasTokenizerStack }

  destructor TAGPasTokenizerStack.Destroy;
  begin
    FreeAndNil(Stack);
    FreeAndNil(Tokenizer);
    inherited;
  end;

  function TAGPasTokenizerStack.GetCachedCount: integer;
  begin
    Result := Stack.Count
  end;

  function TAGPasTokenizerStack.GetLast(): TAGToken;
  begin
    if Stack.Count <> 0 then
      Result := Stack.Peek
    else
    begin
      Result := Get(Tokenizer);
      Stack.Enqueue(Result);
    end;
  end;

  function TAGPasTokenizerStack.GetWithComments(
  Tokenizer: TAGPasTokenizer): TAGToken;
  begin
    Result := Tokenizer.GetNext;
  end;

  function TAGPasTokenizerStack.GetWithoutComments(
  Tokenizer: TAGPasTokenizer): TAGToken;
  var done: boolean;
  begin
    repeat
      Result := Tokenizer.GetNext;
    until Result.ended or not IsComment(Result.Text);
    IsEnd := Result.ended;
  end;

  constructor TAGPasTokenizerStack.Create(input: TStrings;
  GetComments: boolean = True);
  begin
    Stack := TQueue<TAGToken>.Create();
    Tokenizer := TAGPasTokenizer.Create(input);

    if GetComments then
      Get := GetWithComments
    else
      Get := GetWithoutComments;
  end;

  procedure TAGPasTokenizerStack.Push(const t: TAGToken);
  begin
    Stack.Enqueue(t);
  end;

  function TAGPasTokenizerStack.Pop(): TAGToken;
  begin
    if Stack.Count > 0 then
      Result := Stack.Dequeue
    else
      Result := Get(Tokenizer);
    IsEnd := Result.ended;
  end;

  { TAGPasTokenizerParallelStack }

  function TAGPasTokenizerParallelStack.AddTokenToStack(): boolean;
  var tkn: TAGToken;
  begin
    FStackLock.Enter;
    tkn := Get(Tokenizer);
    Result := tkn.ended;
    Stack.Enqueue(tkn);
    FStackLock.Leave;
  end;

  constructor TAGPasTokenizerParallelStack.Create(const input: TStrings;
  GetComments:
  boolean = True;stackMax: integer = 1000);
  begin
    inherited Create(input, GetComments);
    FStackHalfMax := stackMax shr 1;
    FStackLock := TCriticalSection.Create();
    FSignal := TEvent.Create(nil, False, True, 'ag_tkn_work_signal');
    FWorker := TWorkerThread.Create(Self);
  end;

  destructor TAGPasTokenizerParallelStack.Destroy;
  begin
    EnsureThreadDone();
    FreeAndNil(FStackLock);
    inherited;
  end;

  procedure TAGPasTokenizerParallelStack.EnsureThreadDone;
  begin
    if not FWorker.Terminated then begin
      FWorker.Terminate;
      FSignal.SetEvent();// stop idling
      FWorker.WaitFor();
    end;
    FreeAndNil(FWorker);
    FreeAndNil(FSignal);
  end;

  function TAGPasTokenizerParallelStack.GetLast: TAGToken;
  begin
    FStackLock.Enter;
    try
      Result := inherited GetLast();
    finally
      FStackLock.Leave;
    end;
  end;

  function TAGPasTokenizerParallelStack.Pop: TAGToken;
  var
    doReplentishStack, wasRead: boolean;
  begin

    FStackLock.Enter;

    wasRead := Stack.Count > 0;
    if wasRead then
      Result := Stack.Dequeue
    else
      Result := Get(Tokenizer);

    doReplentishStack := FWorker.Idling and(Stack.Count < FStackHalfMax);
    FStackLock.Leave;

    IsEnd := Result.ended;
    if doReplentishStack then
      FSignal.SetEvent;

  end;

  procedure TAGPasTokenizerParallelStack.Push(const t: TAGToken);
  begin
    FStackLock.Enter;
    try
      inherited Push(t);
    finally
      FStackLock.Leave;
    end;
  end;

  { TAGPasTokenizerParallelStack.TWorkerThread }

  constructor TAGPasTokenizerParallelStack.TWorkerThread.Create(const Stack:
  TAGPasTokenizerParallelStack);
  begin
    FStack := Stack;
    inherited Create(False, 4 * 4096);
  end;

  procedure TAGPasTokenizerParallelStack.TWorkerThread.Execute;
  var Count: integer;
    isDone: boolean;
    max: integer;
  begin
    Count := FStack.GetCachedCount;
    max := FStack.FStackHalfMax * 2;
    repeat
      isDone := FStack.AddTokenToStack();
      inc(Count);
      while not Terminated and(Count >= max)do begin
        Count := FStack.GetCachedCount();// fetch real count
        if Count < max then
          break;
        Idling := True;
        FStack.FSignal.WaitFor(1000);
        Count := FStack.GetCachedCount();
      end;
      Idling := False;
    until Terminated or isDone or FStack.ended;
  end;

initialization

  SYMS2 := THashedStringList.Create();
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
  finalization
  FreeAndNil(SYMS2);

end.
