(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

unit GraphScalingCode;

(* Support code for a modal form which allows graphing parameters to be set.    *)
(*                                                              MarkMLl         *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, Spin;

type

  { TFormGraphScaling }

  TFormGraphScaling = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MaskEdit1: TFloatSpinEdit;
    MaskEdit2: TFloatSpinEdit;
    MaskEdit3: TFloatSpinEdit;
    MaskEdit4: TFloatSpinEdit;
    MaskEdit5: TSpinEdit;
    MaskEdit6: TSpinEdit;
    procedure Label1Click(Sender: TObject);
  private

  public

  end;

var
  FormGraphScaling: TFormGraphScaling;

implementation

{$R *.lfm}

{ TFormGraphScaling }

procedure TFormGraphScaling.Label1Click(Sender: TObject);

begin

end;

end.

