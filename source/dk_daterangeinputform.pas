unit DK_DateRangeInputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, Buttons, DateUtils,

  DK_Math, DK_Const, DK_CtrlUtils, DK_Dialogs, DK_InputImages, DK_InputUtils;

type

  { TDKDateRangeInputForm }

  TDKDateRangeInputForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DT1: TDateTimePicker;
    DT2: TDateTimePicker;
    Date2TitleCheckBox: TCheckBox;
    Date1TitleLabel: TLabel;
    SaveButton: TSpeedButton;
    TitleLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure Date2TitleCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure SetDT2Properties;
  public

  end;

  function DoInputDateRange(const ATitle, ADate1Title, ADate2Title: String;
                  var AIsUsedDate2: Boolean;
                  var ADate1, ADate2: TDate;
                  const AMinDate: TDate = 0;
                  const AMaxDate: TDate = 0;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

{$R *.lfm}

function DoInputDateRange(const ATitle, ADate1Title, ADate2Title: String;
                  var AIsUsedDate2: Boolean;
                  var ADate1, ADate2: TDate;
                  const AMinDate: TDate = 0;
                  const AMaxDate: TDate = 0;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKDateRangeInputForm;
begin
  Result:= False;

  Form:= TDKDateRangeInputForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);
    Form.TitleLabel.Caption:= ATitle;
    Form.Date1TitleLabel.Caption:= ADate1Title;
    Form.Date2TitleCheckBox.Caption:= ADate2Title;

    if CompareDate(AMinDate, AMaxDate)<0 then
    begin
      Form.DT1.MinDate:= AMinDate;
      Form.DT1.MaxDate:= AMaxDate;
    end;
    Form.DT1.Date:= ADate1;

    Form.Date2TitleCheckBox.Checked:= AIsUsedDate2;
    Form.SetDT2Properties;
    if AIsUsedDate2 then
      Form.DT2.Date:= ADate2;

    if Form.ShowModal=mrOK then
    begin
      AIsUsedDate2:= Form.Date2TitleCheckBox.Checked;
      ADate1:= Form.DT1.Date;
      ADate2:= Form.DT2.Date;
      Result:= True;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

{ TDKDateRangeInputForm }

procedure TDKDateRangeInputForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
  Width:= MIN_FORM_WIDTH;
  Height:= MIN_FORM_HEIGHT;

  DT1.MinDate:= 0;
  DT1.MaxDate:= INFDATE;
  DT1.Date:= DT1.MinDate;
  DT2.MinDate:= 0;
  DT2.MaxDate:= INFDATE;
  DT2.Date:= DT2.MaxDate;
end;

procedure TDKDateRangeInputForm.FormShow(Sender: TObject);
var
  SideCtrl: TControl;
begin
  if Date1TitleLabel.Width>Date2TitleCheckBox.Width then
    SideCtrl:= Date1TitleLabel
  else
    SideCtrl:= Date2TitleCheckBox;

  DT1.AnchorSide[akLeft].Side:= asrRight;
  DT1.AnchorSide[akLeft].Control:= SideCtrl;
  DT1.BorderSpacing.Left:= BORDER_SPACING_HORIZONTAL;
  DT1.Anchors:= [akLeft, akTop];

  DT2.AnchorSide[akLeft].Side:= asrRight;
  DT2.AnchorSide[akLeft].Control:= SideCtrl;
  DT2.BorderSpacing.Left:= BORDER_SPACING_HORIZONTAL;
  DT2.Anchors:= [akLeft, akTop];

  AutoSize:= True;
  AutoSize:= False;
  if Height<MIN_FORM_HEIGHT then
    Height:= MIN_FORM_HEIGHT;
  if Width<MIN_FORM_WIDTH then
    Width:= MIN_FORM_WIDTH;


  DKInputImages.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);
  FormToScreenCenter(Self);
end;

procedure TDKDateRangeInputForm.SaveButtonClick(Sender: TObject);
begin
  if Date2TitleCheckBox.Checked and (CompareDate(DT1.Date, DT2.Date)>0) then
  begin
    Inform('Дата окончания периода должна быть больше даты начала!');
    Exit;
  end;

  ModalResult:= mrOK;
end;

procedure TDKDateRangeInputForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TDKDateRangeInputForm.SetDT2Properties;
begin
  if Date2TitleCheckBox.Checked then
  begin
    DT2.Enabled:= True;
    DT2.MinDate:= DT1.MinDate;
    DT2.MaxDate:= DT1.MaxDate;
    DT2.Date:= Date;
  end
  else begin
    DT2.MinDate:= 0;
    DT2.MaxDate:= INFDATE;
    DT2.Date:= INFDATE;
    DT2.Enabled:= False;
  end;
end;

procedure TDKDateRangeInputForm.Date2TitleCheckBoxChange(Sender: TObject);
begin
  SetDT2Properties;
end;

end.

