(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

unit tec06code;

(* This is a graphical wrapper for the code which reads a Shunda Tec-06 battery *)
(* tester.                                                      MarkMLl         *)

(* Icons for toolbar- such as it is- are from https://www.iconfinder.com/icons. *)
(* The application icon is from https://iconscout.com/icons. To be honest the   *)
(* toolbar looks pretty pointless, but I've added it for visual consistency     *)
(* with the original program: refer to the (somewhat sketchy) Chinese-language  *)
(* manual at https://github.com/Syonyk/TEC06.                                   *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Grids, ExtCtrls, TAGraph, TASeries, TATransformations;

type
  String31= string[31];

  { TTec06Form }

  TTec06Form = class(TForm)
    ButtonConnect: TButton;
    Chart1: TChart;
    Chart1LineSeriesVolts: TLineSeries;
    Chart1LineSeriesAmps: TLineSeries;
    ChartAxisTransformationsLeft: TChartAxisTransformations;
    ChartAxisTransformationsLeftAutoScaleAxisTransformLeft: TAutoScaleAxisTransform;
    ChartAxisTransformationsRight: TChartAxisTransformations;
    ChartAxisTransformationsRightAutoScaleAxisTransformRight: TAutoScaleAxisTransform;
    ComboBox1: TComboBox;
    GroupBoxTrend: TGroupBox;
    GroupBoxNow: TGroupBox;
    GroupBoxConnection: TGroupBox;
    IdleTimerScpi: TIdleTimer;
    ImageListButtons: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItemHelpAbout: TMenuItem;
    MenuItemGraphScaling: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemTool: TMenuItem;
    MenuItemSetting: TMenuItem;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButtonSearch: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonQuit: TToolButton;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure IdleTimerScpiTimer(Sender: TObject);
    procedure MenuItemGraphScalingClear(Sender: TObject);
    procedure MenuItemGraphScalingClick(Sender: TObject);
    procedure MenuItemHelpAboutClick(Sender: TObject);
    procedure OnAfterShow(afterShowParam: PtrInt);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1Resize(Sender: TObject);
    procedure ToolButtonQuitClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure ToolButtonSearchClick(Sender: TObject);
  strict private

  protected
    fcommsThread: TThread;

    (* Output the values read from the meter in text form. This is called, via
      Synchronize(), by the background communications thread.
    *)
    procedure ReadingTxt(const readings: string);
  public

    (* Message box optimised for position.
    *)
    FUNCTION MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                            buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;
  end;

var
  Tec06Form: TTec06Form;

implementation

{$R *.lfm}

uses
  ConsoleApp, Serial, LocatePorts, StrUtils, GraphScalingCode, IniFilesAbout;

const
  ProjName= 'Tec06';
  Tec06CodeMagicNumber= 2024120920;

(********************************************************************************)

type
  TcommsThread= class(TThread)
  strict private
    serialHandle: TSerialHandle;
    serialName: String31;
    messageStr: string;
    procedure msgShim;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; mySerialHandle: TSerialHandle;
                                                        mySerialName: String31);
    procedure writerShim;
  end;

  TTec06FormH=                          (* Avoid circular definition            *)
    class helper for TTec06Form
    protected
      function CommsThread(): TCommsThread; inline;
    end;

(* This helper avoids problems with circular references, in particular many
  occurrences of things like TCommsThread(Tec06Form.CommsThread).Synchronize()
  in favour of the somewhat more concise Tec06Form.CommsThread.Synchronize().
*)
function TTec06FormH.CommsThread(): TCommsThread; inline;

begin
  result := fCommsThread as TcommsThread
end { TTec06FormH.CommsThread } ;


(********************************************************************************)

(* These are used to pass values between the background thread and the main     *)
(* (GUI) thread. They'd normally be local to the thread object, but can't be in *)
(* this case since the RunConsoleApp2() function hence the writer callback are  *)
(* "traditional" Pascal rather than being "methods" in an object.               *)

var
  readings: string;
  stopLoop: boolean= false;


constructor TcommsThread.Create(CreateSuspended: Boolean; mySerialHandle: TSerialHandle;
                                                        mySerialName: String31);

begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
  serialHandle := mySerialHandle;
  serialName := mySerialName
end { TcommsThread.Create } ;


(* Output the values read from the meter in text form. This is called, via
  Synchronize(), by the background communications thread.
*)
procedure TTec06Form.ReadingTxt(const readings: string);

const
  clAmber= $000090ff;
  clDarkOrange= $008CFF;                (* From turbopower_ipro/ipcss.inc       *)
  clOrange= $00A5FF;
  clOrangeRed= $0045FF;
  clLowSatRed= $008080ff;
  clLowSatGreen= $0080ff80;

var
  status: string;
  mins: double;


  (* HMS string to minutes. This explicitly handles intervals > 24 hours.
  *)
  function HmsToMins(const s: string): double;

  begin
    result := 60 * StrToInt(Trim(ExtractWord(1, s, [':'])));
    result += StrToInt(Trim(ExtractWord(2, s, [':'])));
    result += (StrToInt(Trim(ExtractWord(3, s, [':']))) / 60)
  end { HmsToMins } ;


begin
  StringGrid1.Cells[1, 0] := Trim(ExtractWord(2, readings, ['|'])); (* Current  *)
  StringGrid1.Cells[1, 1] := Trim(ExtractWord(3, readings, ['|'])); (* Voltage  *)
  StringGrid1.Cells[1, 2] := Trim(ExtractWord(4, readings, ['|']));
  StringGrid1.Cells[1, 3] := Trim(ExtractWord(5, readings, ['|']));
  StringGrid1.Cells[1, 4] := Trim(ExtractWord(6, readings, ['|']));
  StringGrid1.Cells[1, 5] := '[No data]';
  status := Trim(ExtractWord(7, readings, ['|']));
  Label1.Caption := Trim(ExtractWord(1, status, [' ']));
  status := Trim(ExtractWord(2, status, [' ']));
  if status <> '' then begin
    Label2.Caption := status;
    mins := HmsToMins(status)
  end else begin
    Label2.Caption := '0:00:00';
    mins := 0.0
  end;
  case Label1.Caption of
    'Stopped':   Label1.Font.Color := clRed;
    'Running':   Label1.Font.Color := clAmber;
    'Completed': Label1.Font.Color := clGreen
  otherwise
    Label1.Font.Color := clBlack
  end;

// This would probably be a good place to consider rescaling the graph to keep
// the voltage and current lines somewhat below the maximum, provided that the
// user hasn't _overridden_ this manually.

  if Label1.Font.Color = clAmber then begin
    TLineSeries(Chart1.Series[0]).AddXY(mins, StrToFloat(StringGrid1.Cells[1, 1]), '', clRed); (* Volts *)
    TLineSeries(Chart1.Series[1]).AddXY(mins, StrToFloat(StringGrid1.Cells[1, 0]), '', clGreen)  (* Amps *)
  end else begin
    TLineSeries(Chart1.Series[0]).AddXY(mins, StrToFloat(StringGrid1.Cells[1, 1]), '', clLowSatRed); (* Volts *)
    TLineSeries(Chart1.Series[1]).AddXY(mins, StrToFloat(StringGrid1.Cells[1, 0]), '', clLowSatGreen)  (* Amps *)
  end
end { TTec06Form.ReadingTxt } ;


(* Provided that it is called via Synchronize(), this executes in the context of
  the main (GUI) thread.
*)
procedure TcommsThread.msgShim;

begin
  Tec06Form.MessageDlgOpt(messageStr, mtError, [mbOk], 0);
  Tec06Form.ButtonConnectClick(nil)    (* Help GUI recover from error          *)
end { TcommsThread.msgShim } ;


(* Provided that it is called via Synchronize(), this executes in the context of
  the main (GUI) thread.
*)
procedure TcommsThread.writerShim;

begin
  Tec06Form.ReadingTxt(readings)
end { TcommsThread.writerShim } ;


(* This is called by the datacomms loop in the datacomms thread roughly every
  second, and passes the unpacked data to the main (GUI) thread via Synchronize()
  and a shim procedure (immediately above).
*)
procedure writer(const s: string);

// This isn't very efficient since it is basically processing text output which
// has been parsed and scaled from the binary data received from meter, but
// since this is a background thread I'm not particularly bothered by that.

begin
  readings := s;
  Tec06Form.CommsThread.Synchronize(@Tec06Form.CommsThread.writerShim)
end { writer } ;


(* Main datacomms code. Because RunConsoleApp2() may run either in the context
  of this background thread or a simple console program, its writer parameter
  is a top-level procedure rather than being a method in the thread.
*)
procedure TcommsThread.Execute;

begin
  case RunConsoleApp2(serialHandle, serialName, stopLoop, @writer) of
    3: messageStr := 'No data waiting for sync byte';
    4: messageStr := 'No data reading message';
    5: messageStr := 'Error formatting message'
  otherwise
    messageStr := ''
  end;
  if (messageStr <> '') and not stopLoop then
    Synchronize(@msgShim);
  messageStr := ''
end { TcommsThread.Execute } ;

(********************************************************************************)

{ TTec06Form }

procedure TTec06Form.StringGrid1Resize(Sender: TObject);

begin
//  StringGrid1.DefaultRowHeight := StringGrid1.Height div 6;
  StringGrid1.ColWidths[0] := Trunc(StringGrid1.Width * 7 / 16);
  StringGrid1.ColWidths[1] := Trunc(StringGrid1.Width * 6 / 16)
end { TTec06Form.StringGrid1Resize } ;


procedure TTec06Form.ToolButtonQuitClick(Sender: TObject);

begin
  Close
end { TTec06Form.ToolButtonQuitClick } ;


procedure TTec06Form.ToolButtonSaveClick(Sender: TObject);

begin
  MessageDlgOpt('Not yet implemented', mtWarning, [mbYes, mbNo], 0)
end { TTec06Form.ToolButtonSaveClick } ;


procedure TTec06Form.ToolButtonSearchClick(Sender: TObject);

var
  i: integer;
  portNames: TStringList;

begin
  portNames := ListPorts();
  try
    for i := 0 to portNames.Count - 1 do begin
      if Pos('ttyS', portnames[i]) = 0 then
        ComboBox1.Items.Append(portnames[i])
    end;
    ComboBox1.ItemIndex := ComboBox1.Items.Count - 1
  finally
    FreeAndNil(portNames)
  end
end { TTec06Form.ToolButtonSearchClick };


(* This is called when the wm_After_Show message is dequeued, which is
  guaranteed to be after form creation has completed.
*)
procedure TTec06Form.OnAfterShow(afterShowParam: PtrInt);

var
  exitCode: integer;

begin
  Assert(afterShowParam = Tec06CodeMagicNumber, 'Internal error: TTec06Form bad magic number');

(* This is probably the place to tweak the height of GroupBoxConnection so that *)
(* the rows fill the stringgrid neatly. There is a list of known widgetset      *)
(* names at the start of IniFilesAbout implementation part.                     *)

  StringGrid1.DefaultRowHeight := StringGrid1.Height div StringGrid1.RowCount;
  GroupBoxNow.ClientHeight := (StringGrid1.DefaultRowHeight * StringGrid1.RowCount) - StringGrid1.GridLineWidth +
        StringGrid1.BorderSpacing.Top + StringGrid1.BorderSpacing.Bottom +
        Label1.Height + GroupBoxNow.BorderSpacing.Top + GroupBoxNow.BorderSpacing.Bottom
(*$IFDEF LCLGTK2 *)
                                                                - 2
(*$ENDIF         *)
(*$IFDEF LCLQT5  *)
(*$ENDIF         *)                                             ;

(* Parse the commandline and optionally start an SCPI server.                   *)

  exitCode := ParseParams();
  if exitCode < 0 then
    exitCode := ScpiStart();
  if exitCode > 0 then begin
    MessageDlgOpt('Commandline error ' + IntToStr(exitCode), mtError, [mbOK], 0);
    Close
  end;

(* The only device that I've found that will run at the required non-standard   *)
(* speed (124kBits/sec) is a genuine- /not/ a counterfeit- FTDI FT232, however  *)
(* this might be caused by the microcontroller's power drain so I'm accepting   *)
(* anything here other than a PC's onboard 8250-derivatives which are limited   *)
(* to 115k.                                                                     *)

  ToolButtonSearchClick(nil)
end { TTec06Form.OnAfterShow } ;


procedure TTec06Form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  temp: string;

begin
  temp:= Application.MainForm.Caption;
  IF Pos(' ', temp) > 0 THEN
    SetLength(temp, Pos(' ', temp) - 1);
  temp:= 'Terminate ' + temp + '?';
  CanClose:= MessageDlgOpt(temp, mtWarning, [mbYes, mbNo], 0) = mrYes
end { TTec06Form.FormCloseQuery } ;


procedure TTec06Form.IdleTimerScpiTimer(Sender: TObject);

begin
  IdleTimerScpi.Enabled := false;
  try
    ScpiDispatch
  finally
    IdleTimerScpi.Enabled := true
  end
end { TTec06Form.IdleTimerScpiTimer } ;


procedure TTec06Form.MenuItemGraphScalingClear(Sender: TObject);

begin
  Chart1.AxisList[0].Range.Min := 0.0;
  Chart1.AxisList[0].Range.Max := 4.5;
  Chart1.AxisList[1].Range.Min := 0;
  Chart1.AxisList[1].Range.Max := 60;
  Chart1.AxisList[2].Range.Min := 0;
  Chart1.AxisList[2].Range.Max := 1.0
end { TTec06Form.MenuItemGGraphScalingClear } ;


procedure TTec06Form.MenuItemGraphScalingClick(Sender: TObject);

begin
  FormGraphScaling.MaskEdit1.Value := Chart1.AxisList[0].Range.Min;
  FormGraphScaling.MaskEdit2.Value := Chart1.AxisList[0].Range.Max;
  FormGraphScaling.MaskEdit5.Value := Chart1.AxisList[1].Range.Min;
  FormGraphScaling.MaskEdit6.Value := Chart1.AxisList[1].Range.Max;
  FormGraphScaling.MaskEdit3.Value := Chart1.AxisList[2].Range.Min;
  FormGraphScaling.MaskEdit4.Value := Chart1.AxisList[2].Range.Max;
  if FormGraphScaling.ShowModal() = mrOK then begin
    Chart1.AxisList[0].Range.Min := FormGraphScaling.MaskEdit1.Value;
    Chart1.AxisList[0].Range.Max := FormGraphScaling.MaskEdit2.Value;
    Chart1.AxisList[1].Range.Min := FormGraphScaling.MaskEdit5.Value;
    Chart1.AxisList[1].Range.Max := FormGraphScaling.MaskEdit6.Value;
    Chart1.AxisList[2].Range.Min := FormGraphScaling.MaskEdit3.Value;
    Chart1.AxisList[2].Range.Max := FormGraphScaling.MaskEdit4.Value
  end
end { TTec06Form.MenuItemGraphScalingClick } ;


procedure TTec06Form.MenuItemHelpAboutClick(Sender: TObject);

var
  about: string;

begin
  about := AboutText();
  MessageDlgOpt(ProjName + ' ' + about, mtConfirmation, [mbOk], 0)
end { TTec06Form.MenuItemHelpAboutClick } ;


procedure TTec06Form.ButtonConnectClick(Sender: TObject);

var
  portName: string;
  portHandle: TSerialHandle= InvalidSerialHandle;

begin
  if ButtonConnect.Tag = 0 then begin
    portName := ComboBox1.Text;
    portHandle := SerOpenLocked(portName); (* Configured and closed by thread   *)
    stopLoop := false;
    if portHandle <> InvalidSerialHandle then begin

(* The thread should free itself on termination. I'm going to be somewhat       *)
(* naughty and assume this is done correctly.                                   *)

      fcommsThread := TcommsThread.Create(false, portHandle, portName);
      StringGrid1.Cells[1, 0] := '';
      StringGrid1.Cells[1, 1] := '';
      StringGrid1.Cells[1, 2] := '';
      StringGrid1.Cells[1, 3] := '';
      StringGrid1.Cells[1, 4] := '';
      StringGrid1.Cells[1, 5] := '';

// The graph probably shouldn't be reset to nominal scaling if the user has
// _overridden_ this manually. If the user hasn't overridden it then the start
// of a connected session, or a restart (Completed -> Stopped), should prime the
// scaling to increase or decrease to keep the traces nicely visible, but after
// the first data has been received it should only increase if necessary.

//      MenuItemGraphScalingClear(sender);
      TLineSeries(Chart1.Series[0]).Clear;
      TLineSeries(Chart1.Series[1]).Clear;
      Label2.Caption := '0:00:00';
      Label1.Caption := 'Connected';
      Label1.Font.Color := clBlue;
      ButtonConnect.Caption := 'Disconnect';
      ButtonConnect.Tag := 1
    end
  end else begin
    fcommsThread.Terminate;             (* Leaves field containing rubbish      *)
    stopLoop := true;                   (* Makes sure thread loop exits promptly *)
    fcommsThread := nil;
    Label1.Caption := 'Disconnected';
    Label1.Font.Color := clBlack;
    ButtonConnect.Caption := 'Connect';
    ButtonConnect.Tag := 0
  end
end { TTec06Form.ButtonConnectClick } ;


procedure TTec06Form.FormCreate(Sender: TObject);

begin
  Application.QueueAsyncCall(@OnAfterShow, Tec06CodeMagicNumber) (* Keep at end *)
end { TTec06Form.FormCreate } ;


(* Message box optimised for position.
*)
FUNCTION TTec06Form.MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;

VAR     x, y: INTEGER;

BEGIN
  x:= (Left + Width DIV 2 + Screen.Width DIV 2) DIV 2;
  y:= (Top + Height DIV 2 + Screen.Height DIV 2) DIV 2;
  RESULT:= MessageDlgPos(aMsg, dlgType, buttons, helpCtx, x, y)
END { TTec06Form.MessageDlgOpt } ;


end.

