unit DK_IntegerInputForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,

  DK_Math, DK_SpinEdit, DK_InputImages, DK_InputConst;

type

  { TDKIntegerInputForm }

  TDKIntegerInputForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    TitleLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

implementation

{$R *.lfm}

function DoInputInt64(const ATitle: String;
                  var AValue: Int64;
                  const AMinValue: Int64 = 0;
                  const AMaxValue: Int64 = 0;
                  const AIncrement: Int64 = 1;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKIntegerInputForm;
  SpinEdit: TDKSpinEdit;

  procedure SpinEditCreate;
  begin
    SpinEdit:= TDKSpinEdit.Create(Form);
    SpinEdit.Parent:= Form;
    SpinEdit.AnchorSide[akTop].Side:= asrBottom;
    SpinEdit.AnchorSide[akTop].Control:= Form.TitleLabel;
    SpinEdit.BorderSpacing.Top:= 12;
    SpinEdit.AnchorSide[akLeft].Side:= asrLeft;
    SpinEdit.AnchorSide[akLeft].Control:= Form;
    SpinEdit.BorderSpacing.Left:= 8;
    SpinEdit.AnchorSide[akRight].Side:= asrRight;
    SpinEdit.AnchorSide[akRight].Control:= Form;
    SpinEdit.BorderSpacing.Right:= 8;
    SpinEdit.Anchors:= [akLeft, akTop, akRight];

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

  Form:= TDKIntegerInputForm.Create(nil);
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

{ TDKIntegerInputForm }

procedure TDKIntegerInputForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
  Width:= MIN_FORM_WIDTH;
  Height:= MIN_FORM_HEIGHT;
end;

procedure TDKIntegerInputForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TDKIntegerInputForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

