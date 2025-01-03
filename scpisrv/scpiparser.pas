(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ScpiParser;

(* SCPI support functions.                                      MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  EInvalidSCPI= class(Exception);

(* Take a <Program Message> which comprises one or more <Program Message Units>
  with ; as a separator. Expect that the first PMU is rooted even if it is not
  preceded by :, subsequent PMUs might be rooted in which case they are preceded
  by :. Break the PM up into a sequence of PMUs, represented by a TList of
  TStringLists, to be freed by the caller (see ReConstruct()). Raise
  EInvalidSCPI with an appropriate message on error.
*)
function DeConstruct(const programMessage: AnsiString): TList;

(* Take a <Program Message Unit> comprising multiple nodes separated by : with
  :* as a special case, plus an optional final value separated by '? ' or ' '
  with '?' as a special case. Separate the value or ? into a distinct node.
  Raise EInvalidSCPI with an appropriate message on error.
*)
function DeTail(programMessageUnit: TStringList): TStringList;

(* Take a <Program Message> comprising multiple <Program Message Units> and
  deTail each. Raise EInvalidSCPI with an appropriate message on error.
*)
function DeTail(programMessage: TList): TList;

(* Take a <Program Message> which comprises one or more <Program Message Units>
  where the first is rooted. Working from first to last, ensure that every
  PMU is rooted. Raise EInvalidSCPI with an appropriate message on error.

  Each PMU is assumed to end either with a node followed by a space and a value
  or ?, or with a node followed by :* which is used when a callback is being
  registered. Assume that the bulk of the preceding PMU (i.e. other than its
  final one or two elements) might need to be propagated if a PMU is not root-
  relative.
*)
function ReRoot(programMessage: TList): TList;

(* Merge multiple nodes into a <Program Message Unit> using : and space as
  separators; the content will be assumed to be root-relative but is not marked
  with a leading : . The output of this will typically be examined to select a
  callback into instrument-specific code. The PMU will be left represented by a
  single StringList item. Raise EInvalidSCPI with an appropriate
  message on error.
*)
function ReTail(programMessageUnit: TStringList): TStringList;

(* For each <Program Message Unit> making up a <Program Message>, merge the
  constituent nodes into a <Program Message Unit> using : and space as
  separators; the content will be assumed to be root-relative but is not marked
  with a leading : . The output of this will typically be examined to select a
  callback into instrument-specific code. The PMU will be left represented by a
  single StringList item. Raise EInvalidSCPI with an appropriate message on error.
*)
function ReTail(programMessage: TList): TList;

(* Merge multiple <Program Message Units> into a <Program Message> using ; as a
  separator. The output of this will typically be used to register a callback
  in instrument-specific code. The PM and its content will be destroyed by
  default. Raise EInvalidSCPI with an appropriate message on error.
*)
function ReConstruct(var programMessage: TList; freeList: boolean= true): AnsiString;

procedure ScpiDump(programMessageUnit: TStringList; indent: integer= 0; const header: AnsiString= '');
procedure ScpiDump(programMessage: TList; indent: integer= 0; const header: AnsiString= '');


implementation

{$define DEBUG }
{ define DEBUGONLY }

{$ifdef DEBUGONLY }
{$define DEBUG    }
{$endif DEBUGONLY }


uses
  StrUtils;

const
(* This is used internally to identify the distinction between a value wildcard
  (i.e. following a space) and a wildcard following a colon. Note that this is
  an AnsiChar, even if subsequently prettified using UTF-8 etc. by dump().
*)
  valueWildcard= #$1f;

type
  abortSet= set of AnsiChar;


(* This is for internal debugging.
*)
function asString(pmu: TStringList): string;

var
  i: integer;

begin
  result := '';                         (* We very much do not want any odd     *)
  for i := 0 to pmu.Count - 1 do begin  (* quotes etc. introduced by CommaText. *)
    if i > 0 then
      result += ',';
    result += pmu[i]
  end
end { asString } ;


(* This is for internal debugging.
*)
function asString(pm: TList): string;

var
  i: integer;

begin
  result := '';
  for i := 0 to pm.Count - 1 do begin
    if i > 0 then
      result += '.';
    Assert(TObject(pm[i]) is TStringList, 'Internal error: bad list item.');
    result += asString(TObject(pm[i]) as TStringList)
  end
end { asString } ;


(* Break a string on the indicated delimiter, returning a TStringList (to be
  freed by the caller) containing zero or more substrings. On error, raise
  EInvalidSCPI with an appropriate error message and any allocation freed.

  If the indicated delimiter is quoted, using either single- or double-quotes,
  or escaped using \, it is treated as part of the current substring. Quotes
  may themselves be embedded in strings either by being repeated (Pascal-style),
  or by being of the opposite type of quote (Modula-2 style), or using a \
  escape (C-style). Everything at or beyond the abort, if defined, is appended
  to the final substring.
*)
function scpiSplit(const command: AnsiString; delimiter: AnsiChar= ';';
                                                abort: AbortSet= []): TStringList;

var
  sq: boolean= false;
  dq: boolean= false;
  i: integer;
  token: AnsiString;

begin
  if (delimiter = '''') or (delimiter = '"') then
    raise EInvalidSCPI.Create('Invalid delimiter in splitter');
  result := TStringList.Create;
  if command = '' then
    exit;
  try
    if Pos(delimiter, command) < 1 then begin
      result.Append(command);
      exit
    end;
    result.Append('');
    i := 1;
    while i <= Length(command) do begin

(* Special case: a delimiter at the start of the string is preserved, so that   *)
(* we can subsequently recognise a <Program Message Unit> that is explicitly    *)
(* root-relative.                                                               *)

      if (i = 1) and (command[i] = delimiter) then begin
        result[result.Count - 1] := delimiter;
        i += 1;
        continue
      end;

(* Special case: handle two-character sequences which are invariably passed     *)
(* verbatim.                                                                    *)

      token := Copy(command, i, 2);
      case token of
        '''''',
        '""',
        '\''',
        '\"': begin
                result[result.Count - 1] := result[result.Count - 1] + token;
                i += 2;
                continue
              end
      otherwise
      end;

(* When considering single characters, treat everything at or after an abort    *)
(* character as indivisible (typically a value); quotes toggle unless nested    *)
(* and if either type of quote is active a delimiter is not treated as          *)
(* significant.                                                                 *)

      token := command[i];
      if (abort <> []) and (token[1] in abort) and not (sq or dq) then begin
        result[result.Count - 1] := result[result.Count - 1] + Copy(command, i, MaxInt);
        break
      end;
      case token of
        '''': begin
                result[result.Count - 1] := result[result.Count - 1] + token;
                if not dq then
                  sq := not sq
              end;
        '"':  begin
                result[result.Count - 1] := result[result.Count - 1] + token;
                if not sq then
                  dq := not dq
              end
      otherwise
         if (token = delimiter) and not (sq or dq) then
           result.Append('')
         else
           result[result.Count - 1] := result[result.Count - 1] + token
      end;
      i += 1
    end
  except
    result.Free;
    raise
  end
end { scpiSplit } ;


procedure ScpiDump(programMessageUnit: TStringList; indent: integer= 0; const header: AnsiString= '');

{$ifdef DEBUG }
var
  i: integer;

begin
  WriteLn(ErrOutput, '::: ' + header);
  for i := 0 to programMessageUnit.Count - 1 do
    if programMessageUnit[i] = valueWildcard then
      WriteLn(ErrOutput, PadLeft('', indent) + '§')
    else
      WriteLn(ErrOutput, PadLeft('', indent) + programMessageUnit[i]);
  Flush(ErrOutput)
{$else        }
begin
{$endif DEBUG }
end { ScpiDump } ;


procedure ScpiDump(programMessage: TList; indent: integer= 0; const header: AnsiString= '');

{$ifdef DEBUG }
var
  i: integer;

begin
  WriteLn(ErrOutput, ';;; ' + header);
  for i := 0 to programMessage.Count - 1 do
    ScpiDump(TStringList(programMessage[i]), indent + 2)
{$else        }
begin
{$endif DEBUG }
end { ScpiDump } ;


(* Take a <Program Message> which comprises one or more <Program Message Units>
  with ; as a separator. Expect that the first PMU is rooted even if it is not
  preceded by :, subsequent PMUs might be rooted in which case they are preceded
  by :. Break the PM up into a sequence of PMUs, represented by a TList of
  TStringLists, to be freed by the caller (see ReConstruct()). Raise
  EInvalidSCPI with an appropriate message on error.
*)
function DeConstruct(const programMessage: AnsiString): TList;

var
  i: integer;
  pmuList: TStringList;

begin
  if programMessage = '' then
    exit(nil);
  result := TList.Create;
  try
    pmuList := scpiSplit(programMessage, ';');
    try
      for i := 0 to pmuList.Count - 1 do
        result.Add(scpiSplit(pmuList[i], ':', [' ', '?']))
    finally
      FreeAndNil(pmuList)
    end;
  except
    for i := 0 to result.Count - 1 do
      TStringList(result[i]).Free;
    result.Free;
    raise
  end
end { DeConstruct };


(* Take a <Program Message Unit> comprising multiple nodes separated by : with
  :* as a special case, plus an optional final value separated by '? ' or ' '
  with '?' as a special case. Separate the value or ? into a distinct node.
  Raise EInvalidSCPI with an appropriate message on error.
*)
function DeTail(programMessageUnit: TStringList): TStringList;

var
  final, operation: integer;
  finalNode: string;
  value: string= '';

begin
  result := programMessageUnit;
  final := programMessageUnit.Count - 1;
  finalNode := programMessageUnit[final];
  operation := Pos('? ', finalNode);
  if operation < 1 then
    operation := Pos('?', finalNode);
  if operation < 1 then
    operation := Pos(' ', finalNode);

(* The value is separated by a space. However I'm also trying to handle syntax  *)
(* that is strictly invalid where a value appears without a node indicating     *)
(* what it should be used for, e.g.                                             *)
(*                                                                              *)
(*      FREQ:SLEW:AUTO ON;3 MHZ/S                                               *)
(*                                                                              *)
(* At the very least, it is desirable to keep the numeric value and the unit    *)
(* together, even if it is later rejected as an error.                          *)

  if (operation > 0) and not (finalNode[1] in ['.', '0'..'9']) then begin
    value := Copy(finalNode, operation, MaxInt);
    if value = '*' then
      value := valueWildcard;
    SetLength(finalNode, operation - 1)
  end else
    if finalNode[Length(finalNode)] = '?' then begin
      value := '?';
      SetLength(finalNode, Length(finalNode) - 1)
    end;
  if value <> '' then begin
    programMessageUnit[final] := finalNode;
    programMessageUnit.Append(value)
  end
end { DeTail } ;


(* Take a <Program Message> comprising multiple <Program Message Units> and
  deTail each. Raise EInvalidSCPI with an appropriate message on error.
*)
function DeTail(programMessage: TList): TList;

var
  i: integer;

begin
  result := programMessage;
  for i := 0 to programMessage.Count - 1 do
    DeTail(TStringList(programMessage[i]))
end { DeTail };


(* Take a <Program Message> which comprises one or more <Program Message Units>
  where the first is rooted. Working from first to last, ensure that every
  PMU is rooted. Raise EInvalidSCPI with an appropriate message on error.

  Each PMU is assumed to end either with a node followed by a space and a value
  or ?, or with a node followed by :* which is used when a callback is being
  registered. Assume that the bulk of the preceding PMU (i.e. other than its
  final one or two elements) might need to be propagated if a PMU is not root-
  relative.
*)
function ReRoot(programMessage: TList): TList;

var
  root, scratch: TStringList;
  i: integer;
  node: AnsiString;

begin
  result := programMessage;
  root := TStringList.Create;
  try
    for i := 0 to programMessage.Count - 1 do begin

(* The first PMU is described as being implicitly root relative, but there are  *)
(* also examples where the first PMU is a single-node command e.g. INITiate. As *)
(* such load the initial root when possible, not necessarily from the first PMU *)
(* particularaly if this is shorter than reasonable. For the remainder, check   *)
(* the first character of the zeroth node. If it's : then the PMU is explicitly *)
(* root-relative.                                                               *)
(*                                                                              *)
(* This also, as a special case, tolerates a PMU that starts with a number such *)
(* as                                                                           *)
(*      FREQ:SLEW:AUTO ON;3 MHZ/S                                               *)
(*                                                                              *)
(* At the very least, it is desirable to keep the numeric value and the unit    *)
(* together, even if it is later rejected as an error.                          *)

      if (TStringList(programMessage[i]).Count < 2) and not
                        (TStringList(programMessage[i])[0][1] in ['.', '0'..'9']) then
        continue;
      if (root.Count = 0) or (TStringList(programMessage[i])[0][1] = ':') then begin
        root.Clear;
        root.Assign(TStringList(programMessage[i]));
        if root[0][1] = ':' then begin
          node := root[0];
          Delete(node, 1, 1);
          root[0] := node
        end;
        if (root[root.Count - 1] = '*') or (root[root.Count - 1] = valueWildcard) then begin
          node := root[root.Count - 1]; (* This for debugging                   *)
          root.Delete(root.Count - 1)
        end else begin
          node := root[root.Count - 1]; (* This for debugging                   *)
          root.Delete(root.Count - 1);
          node := root[root.Count - 1]; (* This for debugging                   *)
          root.Delete(root.Count - 1)
        end;
        if i <> 0 then begin
          node := TStringList(programMessage[i])[0];
          Delete(node, 1, 1);
          TStringList(programMessage[i])[0] := node
        end
      end else begin

(* This PMU is not root-relative, so use the previous root as a prefix.         *)

        scratch := TStringList.Create;
        try
          scratch.Assign(root);
          scratch.AddStrings(TStringList(programMessage[i]))
        finally
          TStringList(programMessage[i]).Free;
          programMessage[i] := scratch
        end
      end
    end
  finally
    root.Free
  end
end { ReRoot } ;


(* Merge multiple nodes into a <Program Message Unit> using : and space as
  separators; the content will be assumed to be root-relative but is not marked
  with a leading : . The output of this will typically be examined to select a
  callback into instrument-specific code. The PMU will be left represented by a
  single StringList item. Raise EInvalidSCPI with an appropriate
  message on error.
*)
function ReTail(programMessageUnit: TStringList): TStringList;

var
  i: integer;

begin
  result := programMessageUnit;
  for i := 1 to programMessageUnit.Count - 1 do begin
    if programMessageUnit[i] = '' then  (* Shouldn't happen                     *)
      continue;
    case programMessageUnit[i] of
      '?':           programMessageUnit[0] := programMessageUnit[0] + programMessageUnit[i];
      '*':           programMessageUnit[0] := programMessageUnit[0] + ':' + programMessageUnit[i];
      valueWildcard: programMessageUnit[0] := programMessageUnit[0] + '*'
    otherwise
      if i <> programMessageUnit.Count - 1 then
        programMessageUnit[0] := programMessageUnit[0] + ':' + programMessageUnit[i]
      else
        case programMessageUnit[i][1] of
          '.', '*',
          '0'..'9': programMessageUnit[0] := programMessageUnit[0] + ' ' + programMessageUnit[i];
          ' ', '?': programMessageUnit[0] := programMessageUnit[0] + programMessageUnit[i]
        otherwise
          programMessageUnit[0] := programMessageUnit[0] + ':' + programMessageUnit[i]
        end
    end
  end;
  for i := programMessageUnit.Count - 1 downto 1 do
    programMessageUnit.Delete(i)
end { ReTail } ;


(* For each <Program Message Unit> making up a <Program Message>, merge the
  constituent nodes into a <Program Message Unit> using : and space as
  separators; the content will be assumed to be root-relative but is not marked
  with a leading : . The output of this will typically be examined to select a
  callback into instrument-specific code. The PMU will be left represented by a
  single StringList item. Raise EInvalidSCPI with an appropriate message on error.
*)
function ReTail(programMessage: TList): TList;

var
  i: integer;

begin
  result := programMessage;
  for i := 0 to programMessage.Count - 1 do
    reTail(TStringList(programMessage[i]))
end { ReTail } ;


(* Merge multiple <Program Message Units> into a <Program Message> using ; as a
  separator. The output of this will typically be used to register a callback
  in instrument-specific code. The PM and its content will be destroyed by
  default. Raise EInvalidSCPI with an appropriate message on error.
*)
function ReConstruct(var programMessage: TList; freeList: boolean= true): AnsiString;

var
  i, j: integer;

begin
  result := '';
  for i := 0 to programMessage.Count - 1 do begin
    if TStringList(programMessage[i]).Count <> 1 then begin
      for j := 0 to programMessage.Count - 1 do
        TStringList(programMessage[j]).Free;
      FreeAndNil(programMessage);
      raise EInvalidSCPI.Create('Program Message has not been re-tailed')
    end;
    if i = 0 then

(* The first PMU doesn't get a : prefix.                                        *)

      result := TStringList(programMessage[0])[0]
    else

(* PMUs which only comprise a single node are assumed to be mandatory commands  *)
(* to inspect registers i.e. *STB? etc. so don't get a : prefix, the remainder  *)
(* do.                                                                          *)

      if PosSet([' ', ':', '?'], TStringList(programMessage[i])[0]) < 1 then
        result := result + ';' + TStringList(programMessage[i])[0]
      else
        result := result + ';:' + TStringList(programMessage[i])[0]
  end;
  if freeList then begin
    for j := 0 to programMessage.Count - 1 do
      TStringList(programMessage[j]).Free;
    FreeAndNil(programMessage)
  end
end { ReConstruct } ;


procedure testSplit(const command: AnsiString; delimiter: AnsiChar= ':');

var
  pm: TList;
  reconstructed: string;
  i: integer;

begin
  WriteLn(ErrOutput, '    ', command);
  pm := DeConstruct(command);
  DeTail(pm); // Splits trailing ? (among other things)

(* At this point the original program message is completely broken down, and we *)
(* can fairly easily scan through it either building a list of the short and    *)
(* long forms of each node (typically during the Register() function, noting    *)
(* that the last node represents a value, a query, or a wildcard), or checking  *)
(* and expanding the nodes typically during the handling of a message.          *)

  ReRoot(pm);
  ScpiDump(pm, 0, command);

(* At this point each Program Message Unit is identified unambiguously relative *)
(* to the root. This is typically where callbacks would be triggered.           *)

  ReTail(pm);
//  ScpiDump(pm, 0, command);
  reconstructed := ReConstruct(pm);
  WriteLn(ErrOutput, '=== ', reconstructed);

(* This errs on the side of caution in that it doesn't ignore a difference      *)
(* caused by a non-first PMU being re-rooted.                                   *)
(*                                                                              *)
(* This is not an error:                                                        *)
(*                                                                              *)
(*     FREQ:STAR 3 MHZ;STOP 5 MHZ                                               *)
(* ...                                                                          *)
(* === FREQ:STAR 3 MHZ;:FREQ:STOP 5 MHZ                                         *)
(* === ----------------^                                                        *)
(*                                                                              *)
(* This is an error:                                                            *)
(*                                                                              *)
(*     SYSTem:HELP:SYNTax? SYSTem:HELP:SYNTax?                                  *)
(* ...                                                                          *)
(* === SYSTem:HELP:SYNTax:? SYSTem:HELP:SYNTax?                                 *)
(* === ------------------^                                                      *)

  if reconstructed <> command then begin
    Write(ErrOutput, '=== ');
    i := 1;
    while (i <= Length(command)) and (i <= Length(reconstructed)) and (reconstructed[i] = command[i]) do begin
        Write(ErrOutput, '-');
        i += 1
      end;
    WriteLn(ErrOutput, '^')
  end;

(* The reconstructed Program Message should be very similar to the original,    *)
(* except that every Program Message Unit is explicitly root-relative. As a     *)
(* side-effect, the parsed representation is freed by default (or in all cases  *)
(* of error).                                                                   *)

  WriteLn(ErrOutput)
end { testSplit } ;


procedure testSplitting;

begin
{$ifdef DEBUGONLY }

(* Test cases from https://www.ivifoundation.org/docs/scpi-99.pdf               *)

  testSplit('INITiate;*OPC;*CLS', ';');         (* 4.1.3.2                      *)

(* "The first command is always referenced to the root node. Subsequent         *)
(* commands, however, are referenced to the same tree level as the previous     *)
(* command in a message unit."                                                  *)

  testSplit('FREQ:STAR 3 MHZ;STOP 5 MHZ', ';'); (* 6.2.4                        *)

(* Leading colon resets a subsequent command to be referenced to the root.      *)

  testSplit('FREQ:STAR 3 MHZ;:FREQ:STOP 5 MHZ', ';');
  testSplit('FREQ:STAR 3 MHZ;SLEW:AUTO ON', ';');
  testSplit('FREQ:SLEW:AUTO ON;STOP 5 MHZ', ';');
  testSplit('FREQ:START 3 MHZ;BAND 1 MHZ', ';');
  testSplit('FREQ:START 3 MHz;:BAND A', ';');
  testSplit('DISP:STAT ON;DATA "Hello, world!"', ';');

(* Test a couple of queries derived from the above.                             *)

  testSplit('FREQ:START 3 MHZ;BAND 1 MHZ', ';');
  testSplit('FREQ:START?', ';');
  testSplit('FREQ:BAND?', ';');
  testSplit('FREQ:START 3 MHZ;BAND?', ';');
  testSplit('FREQ:START 3 MHz;:BAND A', ';');
  testSplit('FREQ:START?', ';');
  testSplit('BAND?', ';');
  testSplit('FREQ:START 3 MHz;:BAND?', ';');

(* This is strictly invalid, since there is no header (node etc.) preceding the *)
(* value. It might be possible to bodge it by assuming that a leading digit     *)
(* indicates the start of a value (i.e. as an alternative to a space), but it   *)
(* will remain problematic.                                                     *)

  testSplit('FREQ:SLEW:AUTO ON;3 MHZ/S', ';');

(* Test that the saved root is updated as needed.                               *)

  testSplit('FREQ:SLEW:AUTO ON;:DISP:STAT ON;DATA "Slew auto on"', ';');

(* These are the forms typically used when registering a callback.              *)

  testSplit('FREQuency:STARt *', ';');
  testSplit('FREQuency:STARt?', ';');
  testSplit('DISPlay:*', ';');

(* There are a number of cases where there may be something "value-like" after  *)
(* the question mark which indicates a query. The first example here is from    *)
(* Volume 1 of the specification, the others my extension to return the syntax  *)
(* of a command.                                                                *)

  testSplit('SYSTem:TIME? MAX,MAX,MAX');
  testSplit('SYSTem:HELP:SYNTax? SYSTem');
  testSplit('SYSTem:HELP:SYNTax? SYSTem:HELP');
  testSplit('SYSTem:HELP:SYNTax? SYSTem:HELP:SYNTax');
  testSplit('SYSTem:HELP:SYNTax? SYSTem:HELP:SYNTax?');

(* There are other cases- not necessarily in the specification, but encountered *)
(* in real life, which don't have a value. Several of the ones below were       *)
(* problematic at r22, as marked.                                               *)

  testSplit('SENSe:INITiate?', ';');
  testSplit('SENSe:INITiate', ';');
  testSplit('SENSe:FETCh1?', ';');
  testSplit('SENSe:FETCh?', ';');
  testSplit('INITiate?', ';');
  testSplit('INITiate', ';');
  testSplit('FETCh1?', ';');
  testSplit('FETCh?', ';');

  halt
{$endif DEBUGONLY }
end { testSplitting } ;


{$ifdef DEBUGONLY }
begin
  testSplitting
{$endif DEBUGONLY }
end.

