unit DK_NumberInputForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Spin,

  DK_Math, DK_CtrlUtils, DK_SpinEdit, DK_InputImages, DK_InputUtils;

type

  { TDKNumberInputForm }

  TDKNumberInputForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    TitleLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public

  end;

  function DoInputInt64(const ATitle: String;
                  var AValue: Int64;
                  const AMinValue: Int64 = 0;
                  const AMaxValue: Int64 = 0;
                  const AIncrement: Int64 = 1;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  function DoInputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer = 2;
                  const AMinValue: Double = 0;
                  const AMaxValue: Double = 0;
                  const AIncrement: Double = 1;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

{$R *.lfm}

procedure SetSpinEditPosition(const AForm: TDKNumberInputForm;
                              const ASpinEdit: TCustomFloatSpinEdit);
begin
  ASpinEdit.Parent:= AForm;
  ASpinEdit.AnchorSide[akTop].Side:= asrBottom;
  ASpinEdit.AnchorSide[akTop].Control:= AForm.TitleLabel;
  ASpinEdit.BorderSpacing.Top:= 12;
  ASpinEdit.AnchorSide[akLeft].Side:= asrLeft;
  ASpinEdit.AnchorSide[akLeft].Control:= AForm;
  ASpinEdit.BorderSpacing.Left:= 8;
  ASpinEdit.AnchorSide[akRight].Side:= asrRight;
  ASpinEdit.AnchorSide[akRight].Control:= AForm;
  ASpinEdit.BorderSpacing.Right:= 8;
  ASpinEdit.Anchors:= [akLeft, akTop, akRight];
end;

function DoInputInt64(const ATitle: String;
                  var AValue: Int64;
                  const AMinValue: Int64 = 0;
                  const AMaxValue: Int64 = 0;
                  const AIncrement: Int64 = 1;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKNumberInputForm;
  SpinEdit: TDKSpinEdit;

  procedure SpinEditCreate;
  begin
    SpinEdit:= TDKSpinEdit.Create(Form);
    SetSpinEditPosition(Form, SpinEdit);

    if AMinValue<>0 then
      SpinEdit.MinValue:= Max(AMinValue, Int64.MinValue)
    else
      SpinEdit.MinValue:= Int64.MinValue;
    if AMaxValue<>0 then
      SpinEdit.MaxValue:= Min(AMaxValue, Int64.MaxValue)
    else
      SpinEdit.MaxValue:= Int64.MaxValue;
    SpinEdit.Value:= AValue;
    SpinEdit.Increment:= AIncrement;
  end;

begin
  Result:= False;

  Form:= TDKNumberInputForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);
    Form.TitleLabel.Caption:= ATitle;

    SpinEditCreate;

    if Form.ShowModal=mrOK then
    begin
      AValue:= SpinEdit.Value;
      Result:= True;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

function DoInputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer = 2;
                  const AMinValue: Double = 0;
                  const AMaxValue: Double = 0;
                  const AIncrement: Double = 1;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKNumberInputForm;
  SpinEdit: TFloatSpinEdit;

  procedure SpinEditCreate;
  begin
    SpinEdit:= TFloatSpinEdit.Create(Form);
    SetSpinEditPosition(Form, SpinEdit);

    if AMinValue<>0 then
      SpinEdit.MinValue:= Max(AMinValue, Double.MinValue)
    else
      SpinEdit.MinValue:= Double.MinValue;
    if AMaxValue<>0 then
      SpinEdit.MaxValue:= Min(AMaxValue, Double.MaxValue)
    else
      SpinEdit.MaxValue:= Double.MaxValue;
    SpinEdit.DecimalPlaces:= ADecimalPlaces;
    SpinEdit.Value:= AValue;
    SpinEdit.Increment:= AIncrement;
  end;

begin
  Result:= False;

  Form:= TDKNumberInputForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);
    Form.TitleLabel.Caption:= ATitle;

    SpinEditCreate;

    if Form.ShowModal=mrOK then
    begin
      AValue:= SpinEdit.Value;
      Result:= True;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

{ TDKNumberInputForm }

procedure TDKNumberInputForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
  Width:= MIN_FORM_WIDTH;
  Height:= MIN_FORM_HEIGHT;
end;

procedure TDKNumberInputForm.FormShow(Sender: TObject);
begin
  DKInputImages.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);
  FormToScreenCenter(Self);
end;

procedure TDKNumberInputForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TDKNumberInputForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

