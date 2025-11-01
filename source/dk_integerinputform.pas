unit DK_IntegerInputForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, SpinEx,

  DK_InputImages, DK_InputConst;

type

  { TDKIntegerInputForm }

  TDKIntegerInputForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    ValueEdit: TSpinEditEx;
    TitleLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    LogID: Integer;
  end;


  function DoInputInteger(const ATitle: String;
                  var AValue: Integer;
                  const AMinValue: Integer = 0;
                  const AMaxValue: Integer = 0;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;


implementation

{$R *.lfm}

procedure SetFormParams(const AForm: TDKIntegerInputForm;
                        const ATitle, ACaption: String;
                        const AWidth, AHeight: Integer);
begin
  SetFormSizeAndCaption(AForm, ACaption, AWidth, AHeight);
  AForm.TitleLabel.Caption:= ATitle;
end;

procedure SetValueInteger(const AEdit: TSpinEditEx; const AValue, AMinValue, AMaxValue: Integer);
begin
  AEdit.MinValue:= Integer.MinValue;
  AEdit.MaxValue:= Integer.MinValue;

  if (AMinValue<>0) and (AMinValue>AEdit.MinValue) then
    AEdit.MinValue:= AMinValue;
  if (AMaxValue<>0) and (AMaxValue<AEdit.MaxValue) then
    AEdit.MaxValue:= AMaxValue;

  AEdit.Value:= AValue;
end;

function DoInputInteger(const ATitle: String;
                  var AValue: Integer;
                  const AMinValue: Integer = 0;
                  const AMaxValue: Integer = 0;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKIntegerInputForm;
begin
  Result:= False;

  Form:= TDKIntegerInputForm.Create(nil);
  try
    SetFormParams(Form, ATitle, ACaption, AWidth, AHeight);
    SetValueInteger(Form.ValueEdit, AValue, AMinValue, AMaxValue);

    if Form.ShowModal=mrOK then
    begin
      AValue:= Form.ValueEdit.Value;
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

