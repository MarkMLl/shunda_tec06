(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

unit consoleapp;

(* This is a simple and incomplete program to read data from a Shunda Tec-06    *)
(* battery tester and send it to stdout; it borrows heavily from older work for *)
(* a Mastech 2115B multimeter.                                  MarkMLl         *)
(*                                                                              *)
(* THINGS IT DOES:                                                              *)
(*                                                                              *)
(* * Open port and read data at non-standard rate.                              *)
(* * Output data, one reading per line. Note: -F minimally tested.              *)
(* * Plot graph on screen.                                                      *)
(* * Respond to SCPI. Note: minimally tested.                                   *)
(*                                                                              *)
(* THINGS IT DOES NOT DO:                                                       *)
(*                                                                              *)
(* * Attempt to sense communications speed automatically.                       *)
(* * Test any interface device other than a genuine FTDI chip.                  *)
(* * Attempt to send any commands to the tester (believed impossible).          *)
(* * Save the data or graph (see DS112 project).                                *)
(* * Scale the graph for printing etc. (ditto).                                 *)

{ TODO : Save graph (.png, .gif) and data. }
{ TODO : Scale graph to various screen/paper sizes }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Serial;

type
  TWriteLn= procedure(const s: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const projName: string);

(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; const portName: string;
                                        var pleaseStop: boolean; writer: TWriteLn= nil): integer;

(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;


implementation

uses
  StrUtils, IniFilesAbout, LocatePorts, Termio, BaseUnix, Errors, ScpiServer,
                                                SyncObjs;

const
  portSpeed= 124000;
  ignoreSpeedError= true;

var
  scpiPort: integer= -2;
  scpi: TScpiServer= nil;
  scpiLock: TCriticalSection= nil;
  scpiFetch: string;


(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

begin
  WriteLn();
  WriteLn(projName + ' ' + AboutText());
  WriteLn()
end { DoVersion } ;


(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const projName: string);

begin
  WriteLn();
  WriteLn('Usage: tec06 [OPTIONS]... DEVICE');
  WriteLn();
  WriteLn('Output readings from a Shunda Tec-06 battery tester.');
  WriteLn();
  WriteLn('DEVICE is the name of a serial device such as /dev/ttyUSB0. Note that this');
  WriteLn('must support non-standard high-speed Baud rates, so far the only device which');
  WriteLn('has been found suitable is a genuine (i.e. not a counterfeit) FTDI FT232.');
  WriteLn();
{$ifdef LCL }
  WriteLn('If there is no explicit option or device an interactive GUI screen will');
  WriteLn('be presented. Supported options are as below:');
{$else      }
  WriteLn('Supported options are as below:');
{$endif LCL }
  WriteLn();
  WriteLn('  --version      Version information.');
  WriteLn();
  WriteLn('  --help         This help text, also reports default device.');
  WriteLn();
  WriteLn('  -F --format p  Output format for main value is defined by pattern p, which');
  WriteLn('                 is e.g. %5.2f to specify a total of five characters with two');
  WriteLn('                 digits after the decimal point. Alternatively use %x@yBzd or');
  WriteLn('                 %x@yLzd for offset x and y bytes of big/little-endian data');
  WriteLn('                 with C format %zd etc., @ by itself dumps raw data as read.');
  WriteLn();
  WriteLn('  --scpi [p]     Listen for SCPI commands on port p, or standard input.');
  WriteLn();
{$ifdef LCL }
  WriteLn('  -              Dummy option, ignored.');
{$else      }
  WriteLn('  - --           Dummy options, ignored.');
{$endif LCL }
  WriteLn();
  WriteLn('Exit status:');
  WriteLn();
  WriteLn(' 0  Normal termination');
  WriteLn(' 1  Cannot parse device identifier');
  WriteLn(' 2  Named device cannot be opened');
  WriteLn(' 3  Named device is unresponsive');
  WriteLn(' 4  Data access error');
  WriteLn(' 5  Data format error');
  WriteLn(' 9  Bad command-line parameters');
  WriteLn('10  Unable to create or start SCPI server');
  WriteLn()
end { DoHelp } ;


{$if not declared(SerialStruct) }
(* Translated from include/linux/serial.h, see also linux/tty_flags.h. This is  *)
(* ancient and is related to the original setserial program, it's got Ty Ts'o's *)
(* name on it which lends it some credibility; however I don't know how widely  *)
(* this is implemented, i.e. whether it is deprecated in later kernels and      *)
(* whether some devices do not support it at the driver level.                  *)
(*                                                                              *)
(* See https://forum.lazarus.freepascal.org/index.php/topic,35302.msg403957.html *)
(* but note the necessity for canary fields since the kernel might write more   *)
(* into the structure than is documented.                                       *)

type
  SerialStruct = packed record
         typ             : Cardinal;
         line            : Cardinal;
         port            : Cardinal;
         irq             : Cardinal;
         flags           : Cardinal;
         xmit_fifo_size  : Cardinal;
         custom_divisor  : Cardinal;
         baud_base       : Cardinal;
         close_delay     : Word;
         io_type         : Byte;
         reserved_char   : Byte;
         hub6            : Cardinal;
         closing_wait    : Word; // time to wait before closing
         closing_wait2   : Word; // no longer used...
         iomem_base      : PtrUInt;
         iomem_reg_shift : Word;
         port_high       : Cardinal;
         iomap_base      : Cardinal; // cookie passed into ioremap
         canary: array[0..31] of cardinal;
  end;


(*
 * For the close wait times, 0 means wait forever for serial port to
 * flush its output.  65535 means don't wait at all.
 *)

const
  ASYNC_CLOSING_WAIT_INF= 0;
  ASYNC_CLOSING_WAIT_NONE= 65535;
  birdseed: cardinal= $55555555;

(*
 * These are the supported serial types.
 *)

type
  portType= (_Unknown= 0, _8250= 1, _16450= 2, _16550= 3, _16550A= 4, _Cirrus= 5,
                _16650= 6, _16650v2= 7, _16750= 8, _Startech= 9, _16C950= 10,
                _16654= 11, _16850=12, _RSA= 13);

  ioType= (Port= 0, Hub6= 1, Mem= 2, Mem32= 3, AU= 4, TSI= 5, Mem32BE= 6, Mem16= 7);

const
  PORT_MAX= 13;
  IO_MAX= 7;

  UART_CLEAR_FIFO=          $01;
  UART_USE_FIFO=            $02;
  UART_STARTECH=            $04;
  UART_NATSEMI=             $08;

  (* Flag bits:                                                                 *)

  ASYNCB_HUP_NOTIFY=        0; (* Notify getty on hangups and closes
                                    * on the callout port *)
  ASYNCB_FOURPORT=          1; (* Set OUT1, OUT2 per AST Fourport settings *)
  ASYNCB_SAK=               2; (* Secure Attention Key (Orange book) *)
  ASYNCB_SPLIT_TERMIOS=     3; (* [x] Separate termios for dialin/callout *)
  ASYNCB_SPD_HI=            4; (* Use 57600 instead of 38400 bps *)
  ASYNCB_SPD_VHI=           5; (* Use 115200 instead of 38400 bps *)
  ASYNCB_SKIP_TEST=         6; (* Skip UART test during autoconfiguration *)
  ASYNCB_AUTO_IRQ=          7; (* Do automatic IRQ during
                                    * autoconfiguration *)
  ASYNCB_SESSION_LOCKOUT=   8; (* [x] Lock out cua opens based on session *)
  ASYNCB_PGRP_LOCKOUT=      9; (* [x] Lock out cua opens based on pgrp *)
  ASYNCB_CALLOUT_NOHUP=    10; (* [x] Don't do hangups for cua device *)
  ASYNCB_HARDPPS_CD=       11; (* Call hardpps when CD goes high  *)
  ASYNCB_SPD_SHI=          12; (* Use 230400 instead of 38400 bps *)
  ASYNCB_LOW_LATENCY=      13; (* Request low latency behaviour *)
  ASYNCB_BUGGY_UART=       14; (* This is a buggy UART, skip some safety
                                    * checks.  Note: can be dangerous! *)
  ASYNCB_AUTOPROBE=        15; (* [x] Port was autoprobed by PCI/PNP code *)
  ASYNCB_MAGIC_MULTIPLIER= 16; (* Use special CLK or divisor *)
  ASYNCB_LAST_USER=        16;

  ASYNC_HUP_NOTIFY=        (1 << ASYNCB_HUP_NOTIFY);
  ASYNC_FOURPORT=          (1 << ASYNCB_FOURPORT);
  ASYNC_SAK=               (1 << ASYNCB_SAK);
  ASYNC_SPLIT_TERMIOS=     (1 << ASYNCB_SPLIT_TERMIOS);
  ASYNC_SPD_HI=            (1 << ASYNCB_SPD_HI);
  ASYNC_SPD_VHI=           (1 << ASYNCB_SPD_VHI);
  ASYNC_SKIP_TEST=         (1 << ASYNCB_SKIP_TEST);
  ASYNC_AUTO_IRQ=          (1 << ASYNCB_AUTO_IRQ);
  ASYNC_SESSION_LOCKOUT=   (1 << ASYNCB_SESSION_LOCKOUT);
  ASYNC_PGRP_LOCKOUT=      (1 << ASYNCB_PGRP_LOCKOUT);
  ASYNC_CALLOUT_NOHUP=     (1 << ASYNCB_CALLOUT_NOHUP);
  ASYNC_HARDPPS_CD=        (1 << ASYNCB_HARDPPS_CD);
  ASYNC_SPD_SHI=           (1 << ASYNCB_SPD_SHI);
  ASYNC_LOW_LATENCY=       (1 << ASYNCB_LOW_LATENCY);
  ASYNC_BUGGY_UART=        (1 << ASYNCB_BUGGY_UART);
  ASYNC_AUTOPROBE=         (1 << ASYNCB_AUTOPROBE);
  ASYNC_MAGIC_MULTIPLIER=  (1 << ASYNCB_MAGIC_MULTIPLIER);

  ASYNC_FLAGS=             ((1 << (ASYNCB_LAST_USER + 1)) - 1);
  ASYNC_DEPRECATED=        (ASYNC_SPLIT_TERMIOS + ASYNC_SESSION_LOCKOUT +
                ASYNC_PGRP_LOCKOUT + ASYNC_CALLOUT_NOHUP + ASYNC_AUTOPROBE);
  ASYNC_SPD_CUST=          (ASYNC_SPD_HI + ASYNC_SPD_VHI);
  ASYNC_SPD_WARP=          (ASYNC_SPD_HI + ASYNC_SPD_SHI);
  ASYNC_SPD_MASK=          (ASYNC_SPD_HI + ASYNC_SPD_VHI + ASYNC_SPD_SHI);
  ASYNC_USR_MASK=          (ASYNC_SPD_MASK + ASYNC_CALLOUT_NOHUP + ASYNC_LOW_LATENCY);

  ASYNC_DEPRECATED_N= ASYNC_FLAGS - ASYNC_DEPRECATED;
  ASYNC_SPD_CUST_N= ASYNC_FLAGS -ASYNC_SPD_CUST;
  ASYNC_SPD_WARP_N= ASYNC_FLAGS - ASYNC_SPD_WARP;
  ASYNC_SPD_MASK_N= ASYNC_FLAGS - ASYNC_SPD_MASK;
  ASYNC_USR_MASK_N= ASYNC_FLAGS - ASYNC_USR_MASK;
{$endif                         }


  (* This uses a newer API introduced- I believe- by kernel 2.6, but never
    widely implemented.
  *)
  function setNonStandardSpeedA(port: TSerialHandle; speed: longint; verbose: boolean= false): boolean;

  // Put non-standard speed setting code in SerialComms and note it for possible
  // inclusion in Serial.pp.

  {
  Placeholder: discussion of arbitrary speeds on Linux >=2.6
  https://sourceware.org/bugzilla/show_bug.cgi?id=10339 in particular comment 5.
  This (interestingly) also mentions MIDI's 31,250 defining it as 1/32 Mbps.

  There's a whole lot of stuff not defined by FPC, including BOTHER (for most CPUs)
  and termios2. See /usr/include/asm-generic/termbits.h
  }

  const
{$if not declared(TCGETS2) }
    TCGETS2= $542A;                     (* Linux include/uapi/asm-generic/ioctls.h *)
    TCSETS2= $542B;
//    TCSETSW2= $542C;
//    TCSETSF2= $542D;
{$endif                    }

  var
    tios: TTermios;                     (* Actually termios2 from around 2.6.4  *)
    rc, err: integer;

  begin
    rc := fpIoctl(port, TCGETS2, @tios);
    result := rc = 0;
    if rc <> 0 then begin
      err := fpGetErrno;
      if verbose then
        WriteLn(ErrOutput, 'API error: cannot get initial (standard) speed, error '
                                        + IntToStr(err) + ', "' + StrError(err) +'".');
      exit
    end;
    if (tios.c_ispeed <> 115200) and verbose then
      WriteLn(ErrOutput, 'API error: initial input standard speed mismatch.');
    if (tios.c_ospeed <> 115200) and verbose then
      WriteLn(ErrOutput, 'API error: initial output standard speed mismatch.');
    tios.c_ispeed := speed;
    tios.c_ospeed := speed;
    rc := fpIoctl(port, TCSETS2, @tios);
    result := result and (rc = 0);
    if (rc <> 0) and verbose then begin
      err := fpGetErrno;
      WriteLn(ErrOutput, 'API error: cannot set required (non-standard) speed, error '
                                        + IntToStr(err) + ', "' + StrError(err) +'".')
    end
  end { setNonStandardSpeedA } ;


  (* This uses the legacy API provided for setserial etc.
  *)
  function setNonStandardSpeedB(port: TSerialHandle; speed: longint; verbose: boolean= false): boolean;

  var
    serInfo: SerialStruct;
    tios: termios;
    rc, err: integer;

  begin
    FillByte(serInfo, SizeOf(serInfo), 0);
    rc := fpIoctl(port, TIOCGSERIAL, @serinfo);
    result := rc = 0;
    if (rc <> 0) and verbose then begin
      err := fpGetErrno;
      WriteLn(ErrOutput, 'API error: cannot get serial information block, error '
                                        + IntToStr(err) + ', "' + StrError(err) +'".');
      exit
    end;
    serInfo.flags := serInfo.flags and ASYNC_SPD_MASK_N;
    serInfo.flags := serInfo.flags or ASYNC_SPD_CUST;
    serInfo.custom_divisor := Round(serInfo.baud_base / speed);
    rc := fpIoctl(port, TIOCSSERIAL, @serinfo);
    result := rc = 0;
    if (rc <> 0) and verbose then begin
      err := fpGetErrno;
      WriteLn(ErrOutput, 'API error: cannot set serial information block, error '
                                        + IntToStr(err) + ', "' + StrError(err) +'".');
      exit
    end;
    tcgetattr(port, tios);
    tios.c_cflag := B38400;
    tios.c_ispeed := tios.c_cflag;
    tios.c_ospeed := tios.c_ispeed;
    rc := tcsetattr(port, TCSANOW, tios);
    result := result and (rc = 0);
    if (rc <> 0) and verbose then begin
      err := fpGetErrno;
      WriteLn(ErrOutput, 'API error: cannot set required (non-standard) speed, error '
                                        + IntToStr(err) + ', "' + StrError(err) +'".')
    end
  end { setNonStandardSpeedB } ;


(* Set a non-standard speed of 128 kBit/sec. On error exit without saving state,
  this may be caused by a device which has no arbitrary-speed API or by Linux
  <v2.6.20: I'm thinking in particular of things like the SheevaPlug or NSLU2
  here which were shipped in that era and might possibly be still found with
  the original kernel and libraries.

   If we can't set the speed then consider that (128,000 - 115,200) / 128,000 is
  10%. That's 10 bits before the timing error definitely hits, so since this is
  async hence resets the timing on every start bit it might /possibly/ be OKish
  for 10 bits (i.e. start bit plus 8 data bits plus parity).
*)
function setNonStandardSpeed(port: TSerialHandle; speed: longint; verbose: boolean= false): boolean;

begin
  result := setNonStandardSpeedA(port, speed, verbose);
  if not result then
    result := setNonStandardSpeedB(port, speed, verbose)
end { setNonStandardSpeed } ;


(* Wait until there is a gap in the data to make sure we don't try to treat data
  as a sync byte;
*)
procedure waitDataGap(port: TSerialHandle);

var
  scratch: byte;

begin
  while SerReadTimeout(port, scratch, 10) > 0 do        (* 10 mSec timeout      *)
    Sleep(1)
end { waitDataGap } ;


type
  byteArray= array of byte;


operator + (const a: byteArray; const b: byte): byteArray;

begin
  result := a;
  SetLength(result, Length(result) + 1);
  result[High(result)] := b
end { + } ;


(* Output data that we've got from a feature request or a query. This is either
  a block of hex/ASCII data in the conventional layout, or is formatted
  according to a layout string where

         %      Indicates start of layout string
         n @    Buffer is zero-based, advance to this index
  either n L    n bytes of little-endian data follow
  or     n B    n bytes of big-endian data follow
  or     n H    n bytes of host-endian data follow
         xxx    C-style format string, leading % assumed

  Assume that this requires three components required in that order, and with n
  being a decimal number. This is intended as a general aid to investigating
  HID behaviour, I'm writing it primarily to support a TEMPer sensor.
*)
procedure diagOutput(const rawBlock: byteArray; l: integer; const diagOutputFormat: string);

var
  i, j, scratchInt: integer;
  expectMaths: boolean;
  pattern, numAsString, savedStr: string;
  savedChar: char;
  accumulator: double;


  function hex(b: byte): string; inline;

  begin
    result := ' ' + LowerCase(IntToHex(b, 2))
  end { hex } ;


  (* Parse off anything that looks like a signed or unsigned integer or real
    plus the # character and space.
  *)
  function parseNum(var str: string): string;

  var
    valid: set of char;

  begin
    result := '';
    valid := [' ', '#', '.', '0'..'9'];
    if not expectMaths then
      valid += ['+', '-'];
    while (str <> '') and (str[1] in valid) do begin
      result += str[1];
      Delete(str, 1, 1)
    end
  end { parseNum } ;


  procedure accumulateData(littleEndian: boolean);

  begin
    if littleEndian then begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i + 1] shl 8) + rawBlock[i];
        3: scratchInt := (rawBlock[i + 2] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i];
        4: scratchInt := (rawBlock[i + 3] shl 24) + (rawBlock[i + 2] shl 16) +
                                (rawBlock[i + 1] shl 8) + rawBlock[i]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end else begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i] shl 8) + rawBlock[i + 1];
        3: scratchInt := (rawBlock[i] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i + 2];
        4: scratchInt := (rawBlock[i] shl 24) + (rawBlock[i + 1] shl 16) +
                                (rawBlock[i + 2] shl 8) + rawBlock[i + 3]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end
  end { accumulateData } ;


begin
  if diagOutputFormat = '' then begin
    for i := 0 to SizeOf(rawBlock) div 16 do
      if i * 16 < l then begin
        Write(LowerCase(IntToHex(I * 16, 4)), ' ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            Write(hex(rawBlock[(i * 16) + j]));
        Write('  ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            if (rawBlock[(i * 16) + j] >= $20) and (rawBlock[(i * 16) + j] < $7f) then
              Write(Chr(rawBlock[(i * 16) + j]))
            else
              Write('·');               (* Dot                                  *)
        WriteLn
      end
  end else begin
    pattern := diagOutputFormat;
    while pattern <> '' do
      case pattern[1] of
        '\': begin                      (* Control characters, and literal %    *)
               case pattern[2] of
                 'b': Write(#$08);
                 'n': WriteLn;
                 'r': Write(#$0d);
                 't': Write(#$09)
               otherwise                (* Second character verbatim            *)
                 Write(pattern[2])
               end;
               Delete(pattern, 1, 2)
             end;
        '%': begin                      (* Formatted output, uses a nested loop *)
               i := 0;                  (* Index into array, default zero       *)
               j := 2;                  (* Number of bytes to read, default 2   *)
               accumulator := 0.0;
               savedChar := #$00;       (* These bypass any arithmetic operations *)
               savedStr := '';
               expectMaths := false;
               Delete(pattern, 1, 1);   (* Leading %                            *)
               while pattern <> '' do begin
                 numAsString := parseNum(pattern); (* Deletes chars parsed off  *)
                 if pattern = '' then begin (* Treat this as literal output,    *)
                   Write(numAsString);  (* we'll have lost an earlier %.        *)
                   exit
                 end;
                 try
                   case pattern[1] of
                     '(': expectMaths := true; (* Are + and - maths operators   *)
                     ')': expectMaths := false; (* or part of the format string? *)
                     '@': if numAsString <> '' then
                            i := StrToInt(numAsString);
                     'L': accumulateData(true);
                     'B': accumulateData(false);
(*$IFDEF ENDIAN_LITTLE *)
                     'H': accumulateData(true);         (* Host-endian          *)
(*$ELSE                *)
                     'H': accumulateData(false);
(*$ENDIF ENDIAN_LITTLE *)
                     '+': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator += scratchInt
                          end;
                     '-': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator -= scratchInt
                          end;
                     '*': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator *= scratchInt
                          end;
                     '/': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt
                          end;
                     '%': begin         (* Not a modulus: round to precision    *)
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt;
                            accumulator := Round(accumulator);
                            accumulator *= scratchInt
                          end

(* Assume that anything else is a printf()-style type character, except that    *)
(* this does not have provision for a prefix character since that's been        *)
(* handled explicitly by the L or B command.                                    *)
(*                                                                              *)
(* I am resisting the temptation of putting in any additional operators here,   *)
(* for example some of the sexy APL ones, not so much because the sort of       *)
(* manipulation they imply is well outside the original scope of this program   *)
(* (which was, after all, a GUI to configure a mouse) but because most of them  *)
(* imply the availability of variable or array support which is quite simply    *)
(* impractical without using a completely different notation.                   *)

                   otherwise
                     case pattern[1] of
                       'c':      Write(Format('%' + numAsString + pattern[1], [savedChar]));
                       's':      Write(Format('%' + numAsString + pattern[1], [savedStr]));
                       'e', 'E', 'f',
                       'g', 'G': Write(Format('%' + numAsString + pattern[1], [accumulator]));
                       'x':      Write(LowerCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')));
                       'X':      Write(UpperCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')))
                     otherwise
                       Write(FormatEx('%' + numAsString + pattern[1], [Round(accumulator)], IsoTimeOnly, 2))
                     end;
                     Delete(pattern, 1, 1);
                     Break              (* Back to outer loop                   *)
                   end;
                   Delete(pattern, 1, 1) (* Stay in inner loop                  *)
                 except
                   Write(#$0d);         (* Start of line                        *)
                   diagOutput(rawBlock, l, '') (* Something wrong               *)
                 end
               end                      (* Inner loop (handles % format)        *)
             end
      otherwise                         (* Anything else                        *)
        Write(pattern[1]);
        Delete(pattern, 1, 1)
      end;
      WriteLn                           (* Outer loop ends immediately after this *)
  end
end { diagOutput } ;


type
  Tmessage= array[0..14] of byte;


(* Output either as a data dump (format is @ by itself) or with C-style
  formatting applied to each byte or word.
*)
function outputFormattedRaw(message: TMessage; const formatString: string): boolean;

begin
  result := true;
  if Length(formatString) <= 2 then
    diagOutput(message, SizeOf(message), '')
  else
    diagOutput(message, SizeOf(message), formatString)
end { outputFormattedRaw } ;


(* Output using an explicit C-style format string. If this ends with whitespace
  or a control character (tab or newline) then don't append an implicit newline.
*)
function outputFormattedRescaled(value: double; const formatString: string): boolean;

var
  ln: boolean= true;

begin
  result := true;
  try
    if Length(formatString) > 2 then begin
      if formatString[Length(formatString)] = ' ' then
        ln := true;
      if formatString[Length(formatString) - 1] = '\' then
        ln := true
    end;
    Write(FormatEx(formatString, [value]));
    if ln then
      WriteLn;
  except
    result := false
  end
end { outputFormattedRescaled } ;


(* Format a 14-byte message to individual variables, with reference to
  https://www.sevarg.net/2018/01/20/reverse-engineering-tec-06-serial/
*)
function unpack(const message: Tmessage; var mA, mV, mVT: word; var mAH: cardinal;
                                        var r: word; var status: byte): boolean;

begin
  result := (message[0] = $aa) and (message[1] = $6a) and (message[14] = $ac);
  if not result then
    exit;
  mA := (((message[2] << 8) + message[3]) - 17) * 10;
  mV := (message[4] << 8) + message[5] - $200;
  mVT := (message[6] << 8) + message[7];
  mAH := (message[8] << 16) + (message[9] << 8) + message[10];
  r := (message[11] << 8) + message[12] - 20;
  status := message[13];

(* This lock isn't strictly necessary, since the main thread is being used to   *)
(* both update the (global) variables showing the meter's state, and dispatch   *)
(* SCPI sommands.                                                               *)

  if Assigned(scpi) then begin
    scpiLock.Enter;
    try
      scpiFetch := Format('%1.3f,%1.3f,%1.3f,%d,%1.3f,%d',
                                [mA/1000, mV/1000, mVT/1000, mAH, r/1000, status])
    finally
      scpiLock.Leave
    end
  end
end { unpack } ;


(* If the format string contains an @ then process the raw buffer. Otherwise
  extract the main value and scale to a double, then format it.
*)
function outputFormatted(message: Tmessage; const formatString: string): boolean;

var
  mA, mV, mVT, mO: word;
  mAH: cardinal;
  status: byte;
  value: double;

begin
  if (formatString = '') or (Pos('@', formatString) > 0) then
    result := outputFormattedRaw(message, formatString)
  else begin
    if not unpack(message, mA, mV, mVT, mAH, mO, status) then
      exit(false);
    value := mAH;
    result := outputFormattedRescaled(value, formatString)
  end
end { outputFormatted } ;


(* Format a 14-byte message into text, with reference to
  https://www.sevarg.net/2018/01/20/reverse-engineering-tec-06-serial/
*)
function autoFormatted(message: Tmessage; noUnits: boolean= false): string;

const
  lastStatus: byte= $ff;                (* Static variable                      *)
  start: TDateTime= 0.0;                (* Static variable                      *)
  stop: TDateTime= 0.0;                 (* Static variable                      *)
  paused: TDateTime= 0.0;               (* Static variable                      *)
  messageEvery= 60 / 50;

var
  mA, mV, mVT, mO: word;
  mAH: cardinal;
  status: byte;


  (* This is used for voltages etc.
  *)
  function toStr(w: word): string;

  begin
    result := Format('%1.3f', [w / 1000])
  end { toStr } ;


  (* This is used for the mAH counter.
  *)
  function toStr(c: cardinal): string;

  begin
    result := IntToStr(c)
  end { toStr } ;


  (* This is used for elapsed hours and minutes, including the case where more
    than 24 hours have elapsed.
  *)
  function toStr(hm: TDateTime): string;

  var
    h, m: string;

  begin
    hm := hm * 24;
    h := IntToStr(Trunc(hm));           (* Single digit hour is acceptable      *)
    hm := Frac(hm) * 60;
    m := IntToStr(Round(hm));
    if Length(m) < 2 then               (* Minutes are always two digits        *)
      m := '0' + m;
    result := h + ':' + m
  end { toStr } ;


  (* Similar to toStr() above, but appends seconds.
  *)
  function toStrSec(hms: TDateTime): string;

  var
    h, m, s: string;

  begin
    hms := hms * 24;
    h := IntToStr(Trunc(hms));          (* Single digit hour is acceptable      *)
    hms := Frac(hms) * 60;
    m := IntToStr(Trunc(hms));
    if Length(m) < 2 then               (* Minutes are always two digits        *)
      m := '0' + m;
    result := h + ':' + m;
    hms := Frac(hms) * 60;
    s := IntToStr(Round(hms));
    if Length(s) < 2 then               (* Seconds are always two digits        *)
      s := '0' + s;
    result := h + ':' + m + ':' + s
  end { toStrSec } ;


  (* Convert a time interval into a cleaned up fractional hour.
  *)
  function C(dt: TDateTime): string;

  var
    fraction: double;

  begin
    fraction := 1 / (dt * HoursPerDay);
    if (fraction > 0.225) and (fraction < 0.275) then
      result := '0.25'
    else
      if (fraction > 0.725) and (fraction < 0.775) then
        result := '0.75'
      else
        if fraction < 10.0 then
          result := Format('%1.1f', [dt])
        else
          result := IntToStr(Round(dt))
  end { C } ;


begin
  if message[0] <> $aa then
    exit('');                           (* Bad sync byte                        *)
  result := IsoFormatDateTime(UTC_Now(), IsoTimeOnly, 0) + '|'; (* Date, time   *)
  if not unpack(message, mA, mV, mVT, mAH, mO, status) then
    exit('');

(* I believe that measurement and reporting run asynchronously, there appear to *)
(* be brief "stopped" states even if these are not the result of a button press *)
(* on the instrument.                                                           *)

(* If the state was completed and we move to stopped, assume that it's a reset  *)
(* before a run.                                                                *)

  if (lastStatus = 3) and (status = 2) then     (* Completed -> stopped         *)
    start := 0.0;

(* If the state was completed and we move to running, assume that it's a start. *)

  if (lastStatus = 3) and (status = 1) then begin (* Completed -> running       *)
    start := UTC_Now();
    paused := 0.0
  end;

(* If the state was stopped and we have not been running, and we move to        *)
(* running, assume that it's a start.                                           *)

  if (lastStatus = 2) and (status = 1) then     (* Stopped -> running           *)
    if start = 0.0 then begin
      start := UTC_Now();
      paused := 0.0
    end;

(* If the state was running and we move to completed, assume that it's a stop.  *)

  if (lastStatus = 1) and (status = 3) then     (* Running -> completed         *)
    stop := UTC_Now();

(* If the state was running and we move to stopped, assume that it's the start  *)
(* of a pause which might be brief or might extend to one or more seconds.      *)

  if (lastStatus = 1) and (status = 2) then
    paused += (0.5 * messageEvery) / SecsPerDay;

(* If the state was stopped and continues to be stopped, assume that it's       *)
(* another 1-second pause (rather than potentially some fraction of a second).  *)

  if (lastStatus = 2) and (status = 2) then
    paused += (1.0 * messageEvery) / SecsPerDay;

(* If the state was stopped and we move to running, and we have accumulated     *)
(* some pause time indicating that this is not the initial start, assume that   *)
(* the pause might have been brief or might have extended to one or more        *)
(* seconds.                                                                     *)

  if (lastStatus = 2) and (status = 1) then
    if paused > 0.0 then
      paused += (0.5 * messageEvery) / SecsPerDay;
  lastStatus := status;

(* Note that the pause time above is roughly in integer seconds rather than     *)
(* trying to track the beginning and ending of every event. However it gets     *)
(* scaled slightly due to the pragmatic observation that the instrument sends   *)
(* slightly fewer than one message per second.                                  *)
(*                                                                              *)
(* If output is to be written to stdout or a file then append units. If it's to *)
(* go to a GUI etc. then assume the numbers will be parsed back to binary as    *)
(* well as being displayed, so it will be faster to apppend units only as       *)
(* needed.                                                                      *)

  if noUnits then begin
    result += toStr(mA) + '|';
    result += toStr(mV) + '|';
    result += toStr(mVT) + '|';
    result += toStr(mAH) + '|';
    result += toStr(mO) + '|'
  end else begin
    result += toStr(mA) + 'A|';
    result += toStr(mV) + 'V|';
    result += toStr(mVT) + 'Vt|';
    result += toStr(mAH) + 'mAH|';
    result += toStr(mO) + 'Ω|'
  end;
  case status of
    1: result += 'Running';
    2: result += 'Stopped';
    3: result += 'Completed'
  otherwise
    result += IntToStr(status)
  end;

(* Append the time the test has been running, or the time it took to run to     *)
(* completion allowing for any pauses (which make the result unreliable by      *)
(* introducing extra time for reactants to diffuse to a new equilibrium).       *)
(*                                                                              *)
(* If completed, append an estimate of the test rate: so if the elapsed time is *)
(* one hour the rate will be 1C, if it's two hours it will be 0.5C and so on.   *)
(* It would in principle be possible to use this to iterate to one of the       *)
(* "standard" rates (i.e. 0.1C for lead-acid or alkaline, 0.2C for NiMH, 1C for *)
(* lithium etc.), but in practice unless characterising a bulk purchase of new  *)
(* cells the wear and tear due to repeated cycling would almost certainly be    *)
(* counterproductive.                                                           *)

// I'm not very happy with this. If the tester decides to go to stopped rather
// than completed state then if paused (a seconds count) is in the equations
// below then it will continue to count down. This is undesirable since (a) it
// messes up any display of voltage recovery on the graph and (b) in the code
// as it stands -ve seconds don't borrow properly.

  if start > 0.0 then
    if noUnits then // GUI
      case status of
        1,                                              (* Running              *)
        2: result += ' ' + toStrSec((UTC_Now() - start) { - paused } ); (* Stopped     *)
        3: result += ' ' + toStrSec((stop - start) - paused); (* Completed        *)
      otherwise
      end
    else // Non-GUI
      case status of
        1,                                              (* Running              *)
        2: result += ' ' + toStr((UTC_Now() - start) - paused); (* Stopped      *)
        3: begin
             result += ' ' + toStr((stop - start) - paused); (* Completed       *)
             result += ' (' + C((stop - start) - paused) + 'C)'
           end
      otherwise
      end
end { autoFormatted } ;


var
  debugLevel: integer= 0;
  formatString: string= '';
  onceOnly: boolean= false;


(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; const portName: string;
                                        var pleaseStop: boolean; writer: TWriteLn= nil): integer;
const
  scpiPrompt= false;

var
  i, lastI: integer;
  waitingFirstSync: boolean;
  resync: jmp_buf;
  message: Tmessage;
  scratch: string;

begin
  try
    if IsATTY(portHandle) < 1 then
      WriteLn(ErrOutput, 'Device ' + portName + ' is not a TTY, continuing');
    SerSetParams(portHandle, 115200, 8, EvenParity, 1, []);
    if not setNonStandardSpeed(portHandle, portSpeed) then
      if ignoreSpeedError then
        WriteLn(ErrOutput, 'Device ' + portName + ' cannot be set to 128K, continuing')
      else begin
        WriteLn(ErrOutput, 'Device ' + portName + ' cannot be set to 128k');
        exit(2)
      end;

(********************************************************************************)
(*                                                                              *)
(* DIRE WARNING: even if the speed appears to have been set correctly, we have  *)
(* no guarantee that it actually /has/ been set correctly since we don't know   *)
(* (a) whether the driver (kernel module) reads the speed setting back from the *)
(* interface device and (b) whether the interface device is truthful when asked *)
(* what speed it's set to.                                                      *)
(*                                                                              *)
(* The original Windows package includes drivers for a CP210 and PL2303 which   *)
(* implies that (at least on Windows) these can be set to 128k, and on Linux it *)
(* is generally believed that /genuine/ FT232 chips can be set to an arbitrary  *)
(* speed. However both the PL2303 and FT232 are known to be afflicted by clones *)
(* and counterfeits, and there is absolutely no guarantee that these will work  *)
(* correctly.                                                                   *)
(*                                                                              *)
(********************************************************************************)

    lastI := SetJmp(resync);
    waitDataGap(portHandle);
    waitingFirstSync := true;
    repeat
      i := -1;

(* There's two possibilities here. If we're not confident that we're seeing a   *)
(* stream of contiguous data then be prepared to wait several seconds for a     *)
(* sync byte, otherwise be less tolerant.                                       *)

      if waitingFirstSync then
        if SerReadTimeout(portHandle, message[0], 5000) = 0 then
          case lastI of
            0,
            1: exit(3);                 (* Unresponsive                         *)
          otherwise
            exit(4)                     (* Access error, message not intact     *)
          end
        else begin end                  (* Continue with check that it's $55    *)
      else
        if SerReadTimeout(portHandle, message[0], 2000) = 0 then
          LongJmp(resync, 1);
      waitingFirstSync := false;
      if message[0] <> $aa then
        continue;

(* Anything that isn't a sync byte has been discarded. Read the remainder of    *)
(* the message.                                                                 *)

      for i := 1 to High(message) do
        if SerReadTimeout(portHandle, message[i], 100) = 0 then
          LongJmp(resync, i + 1);

(* We have been passed a pointer to pleaseStop rather than a fixed value, if    *)
(* it's been set outside this function while we've been waiting for and reading *)
(* the message then don't call the writer since this would interfere with GUI   *)
(* cleanup.                                                                     *)

      if pleaseStop then
        break;
      if debugLevel > 1 then begin
        Write(ErrOutput, '#');
        for i := 0 to High(message) do
          Write(ErrOutput, ' ' + LowerCase(HexStr(message[i], 2)));
        WriteLn(ErrOutput);
        Flush(ErrOutput)
      end;
      if Assigned(writer) then begin
        scratch := autoFormatted(message, true);
        if scratch = '' then
          exit(5);                      (* Format error                         *)
        writer(scratch)
      end else

(* We have a 9-byte message. If there is an explicit format string then apply   *)
(* it, otherwise just try to do the right thing.                                *)

        if formatString <> '' then
          if not outputFormatted(message, formatString) then
            exit(5)                     (* Format error                         *)
          else begin end
        else begin
          scratch := autoFormatted(message, false);
          if scratch = '' then
            exit(5);                    (* Format error                         *)
          WriteLn(scratch)
        end;

(* Only output a single line if an SCPI server has been requested. Even this    *)
(* might be excessive.                                                          *)

      if Assigned(scpi) and not scpi.Finished then begin

(* If Run() hasn't been called to activate the thread, which depends on the     *)
(* thread manager being imported into the main unit at compilation, then it's   *)
(* necessary to call Poll() regularly. Don't expect this to perform well.       *)

        if scpi.Suspended then
          scpi.Poll(NonBlocking);

(* This lock isn't strictly necessary, since the main thread is being used to   *)
(* both update the (global) variables showing the meter's state, and dispatch   *)
(* SCPI sommands.                                                               *)

        if scpi.CommandsAvailable() > 0 then begin
          scpiLock.Enter;
          try
            while scpi.Dispatch() do
              Sleep(10)
          finally
            scpiLock.Leave
          end
        end
      end;
      if onceOnly then                  (* Debugging option, -ve level          *)
        break
    until pleaseStop { Dave }           (* Or signal from keyboard              *)
  finally
    SerClose(portHandle)
  end
end { RunConsoleApp2 } ;


{$macro on  }
{$define IS_SCPI_SYNTAX__:= Pos(' SYNTAX', command) = Length(command) - Length(' SYNTAX') + 1 }
{$define SCPI_COMMAND_NO_SYNTAX__:= Copy(command, 1, Length(command) - Length(' SYNTAX')) }
{$define SYNTAX_REQUESTED_FOR__:= Copy(SCPI_COMMAND_NO_SYNTAX__, Pos(' ', SCPI_COMMAND_NO_SYNTAX__) + 1, MaxInt) }

function scpiDoNothing(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    if Pos(' ', SCPI_COMMAND_NO_SYNTAX__) > 0 then
      scpi.Respond('No syntax for "' + SYNTAX_REQUESTED_FOR__ + '"', true)
    else
      scpi.Respond('No syntax for "' + SCPI_COMMAND_NO_SYNTAX__ + '"', true)
  end else
    scpi.Respond('Do not understand "' + command + '"', true)
end { scpiDoNothing } ;


function scpiDoHalt(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else begin
    scpi.Destroy;
    Halt
  end
end { scpiDoHalt } ;


(* Response should be four fields with manufacturer, model (without "Model "
  text etc.), serial number, and firmware/revision info of all subsystems.

  Assume that response fields may be empty but must remain comma-delimited, and
  that trailing (but not embedded) spurious commas may be suppressed.
*)
function scpiDoIdentify(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else
    scpi.Respond('Shunda,Tec-06')
end { scpiDoIdentify } ;


(* SYSTem:HELP:SYNTAX? results in each of the registered commands being passed  *)
(* ' SYNTAX' (note leading space) as its parameter. Hence                       *)
(*                                                                              *)
(* Returns the syntax of the specified command                                  *)
(*                                                                              *)
(* Syntax SYSTem:HELP:SYNTax? <command_header>                                  *)
(*                                                                              *)
(* Example SYST:HELP:SYNT? INP                                                  *)
(*                                                                              *)
(* The above command returns                                                    *)
(*                                                                              *)
(*      INPut#:COUPling GND|DC\n                                                *)
(*      INPut#:COUPling? [OPTions|DEFault]\n                                    *)
(*      INPut#:GAIN MINimum|MAXimum|                                            *)
(*                  UP|DOWN|DEFault|<value>\n                                   *)
(*      INPut#:GAIN? [OPTions|DEFault]\n                                        *)
(*                                                                              *)
(* Assume that the pattern to be matched here is strictly that returned by an   *)
(* earlier SYSTem:HELP:HEADers? command. Note the special cases here to handle  *)
(* the HEADers and SYNTax commands themselves.                                  *)

// https://koctas-img.mncdn.com/mp/pdf/5000818338/4acb9313c3954507b3f33b10fcd84ec3.pdf


function scpiDoReportFetch(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' Report captured data.', true)
  end else
    scpi.Respond(scpiFetch, true)
end { scpiDoReportFetch } ;


(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;

var
  portHandle: TSerialHandle= InvalidSerialHandle;
  dontStop: boolean= false;
  i: integer;

begin
  result := 3;                          (* Unresponsive is a good default       *)
  i := 1;
  while i <= ParamCount() do begin
    case ParamStr(i) of
      '-',                              (* Placeholder only                     *)
      '--',                             (* This doesn't work with GUI/LCL       *)
      '--debug':      if i = ParamCount() then begin
                        WriteLn(stderr, 'Debug level has no parameter');
                        exit(9)         (* Missing debug level                  *)
                      end else begin
                        i += 1;
                        try
                          debugLevel := Abs(StrToInt(ParamStr(i)));
                          onceOnly := StrToInt(ParamStr(i)) < 0
                        except
                          WriteLn(stderr, 'Debug level not numeric');
                          exit(9)       (* Bad debug level                      *)
                        end
                      end;
      '-F',
      '--format':     if i = ParamCount() then begin
                        WriteLn(stderr, 'Format string has no parameter');
                        exit(9)         (* Missing format string                *)
                      end else begin
                        i += 1;
                        formatString := ParamStr(i)
                      end;
      '--scpi':       if i = ParamCount() then
                        scpiPort := -1
                      else begin
                        i += 1;
                        if Lowercase(ParamStr(i)) = 'scpi-telnet' then
                          scpiPort := 5024      (* Gospel according to NMap     *)
                        else
                          if Lowercase(ParamStr(i)) = 'scpi-raw' then
                            scpiPort := 5025    (* Gospel according to NMap     *)
                          else
                            if (not TryStrToInt(ParamStr(i), scpiPort)) or
                                        (scpiPort < 0) or (scpiPort > 65535) then begin
                              WriteLn(stderr, 'Bad SCPI port number');
                              exit(1)   (* Bad SCPI port                        *)
                            end
                      end
    otherwise
      if i <> ParamCount() then begin
        WriteLn(stderr, 'Bad device name');
        exit(1)                         (* Bad device name                      *)
      end else
        portName := ParamStr(i)
    end;
    i += 1
  end;
  portHandle := SerOpenLocked(portName);
  if portHandle = InvalidSerialHandle then begin
    WriteLn(stderr, 'Device ' + portName + ' cannot be opened');
    exit(2)                             (* Cannot be opened                     *)
  end;
  result := 0;
  if debugLevel > 0 then
    WriteLn(ErrOutput, '# Using port ', portName);

(* If requested, start an SCPI server.                                          *)

  if scpiPort >= -1 then begin
    if debugLevel > 0 then
      if scpiPort = -1 then
        WriteLn(stderr, '# Starting SCPI daemon on standard I/O')
      else
        WriteLn(stderr, '# Starting SCPI daemon on port ', scpiPort);

(* Even though we're not showing this host's IP addresses yet, we can usefully  *)
(* display the port number early so that if the daemon can't be started we know *)
(* the potential clash.                                                         *)

    scpi := TScpiServer.Create(scpiPort);
    if Assigned(scpi) then begin
      if debugLevel > 0 then
        if (scpiPort > -1) and (scpi.OwnAddr <> '') then
          if Pos(' ', scpi.OwnAddr) > 0 then
            WriteLn(StdErr, '# Listening on IP addresses ', scpi.OwnAddr)
          else
            WriteLn(StdErr, '# Listening on IP address ', scpi.OwnAddr);
      scpiLock := TCriticalSection.Create;
      scpi.BlankIsHelp := true;
      scpi.HelpIsHelp := true;
      scpi.HelpQIsHelp := true;
      scpi.Register('', @scpiDoNothing); (* Default does nothing              *)
      scpi.Register('*HALT', @scpiDoHalt);
      scpi.Register('SYSTem:HELP:HEADers?', nil);
      scpi.Register('*IDN', @scpiDoIdentify);
      scpi.Register('FETCH?', @scpiDoReportFetch);

(* As an alternative, we could use e.g.                                         *)
(*                                                                              *)
(*      scpi.Register('SENSe:VALUe#?', @scpiDoReportValue);                     *)
(*                                                                              *)
(* and rely on the handler looking at the command.                              *)

(* If Run() hasn't been called to activate the thread, which depends on the     *)
(* thread manager being imported into the main unit at compilation, then it's   *)
(* necessary to call Poll() regularly. Don't expect this to perform well.       *)

      if not scpi.Run() then begin
        result := 10;
        if scpiPort < 0 then
          WriteLn(stderr, 'Unable to run SCPI server on stdin')
        else
          WriteLn(stderr, 'Unable to run SCPI server on port ', scpiPort)
      end
    end else begin
      result := 10;
      WriteLn(stderr, 'Unable to create SCPI server')
    end
  end;
  if result = 0 then
    result := RunConsoleApp2(portHandle, portName, dontStop);
  case result of
    3: WriteLn(stderr, 'No data waiting for sync byte');
    4: WriteLn(stderr, 'No data reading message');
    5: WriteLn(stderr, 'Error formatting message')
  otherwise

(* Assume that whatever command we specified on the commandline, defaulting to  *)
(* a display of the model number, has been successful.                          *)

  end
end { RunConsoleApp };


begin
  Assert(Format('%1.1f', [0.17]) = '0.2', 'Format doesn''round floats properly: 0.17 -> ' + Format('%1.1f', [0.17]))
end.

