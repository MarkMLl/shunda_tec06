(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program tec06;

(* Read data from a Shunda TEC-06, either as a console program or with a GUI.   *)
(*                                                              MarkMLl.        *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
{$ifdef LCL }
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, tec06code, graphscalingcode,
{$endif LCL }
  consoleapp, ScpiServer;

var
  Tec06Port: string= '';
  i: integer;

{$ifdef LCL }
{$R *.res}
{$endif LCL }

begin
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--version' then begin
      DoVersion('Tec06');
      Halt(0)
    end;
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--help' then begin
      DoHelp('Tec06');
      Halt(0)
    end;
  if ParamCount > 0 then
    Tec06Port := ParamStr(ParamCount());
{$ifdef LCL }
  if ParamCount() > 0 then  (* If GUI is available, activated by no parameter   *)
{$endif LCL }
    Halt(RunConsoleApp(Tec06Port));

(* The objective here is to minimise the amount of manually-inserted text so as *)
(* to give the IDE the best chance of managing form names etc. automatically. I *)
(* try, I don't always succeed...                                               *)

{$ifdef LCL }
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTec06Form, Tec06Form);
  Application.CreateForm(TFormGraphScaling, FormGraphScaling);
  Application.Run;
{$endif LCL }
end.

