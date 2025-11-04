unit DK_TextInputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,

  DK_Vector, DK_CtrlUtils, DK_InputImages, DK_InputUtils;

type

  { TDKTextInputForm }

  TDKTextInputForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    TextMemo: TMemo;
    SaveButton: TSpeedButton;
    TitleLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public

  end;

  function DoInputText(const ATitle: String;
                  var ALines: TStrVector;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  function DoInputText(const ATitle: String;
                  var AText: String;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

function DoInputText(const ATitle: String;
                  var ALines: TStrVector;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKTextInputForm;
begin
  Result:= False;

  Form:= TDKTextInputForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);
    Form.TitleLabel.Caption:= ATitle;
    VToStrings(ALines, Form.TextMemo.Lines);

    if Form.ShowModal=mrOK then
    begin
      ALines:= VFromStrings(Form.TextMemo.Lines);
      Result:= True;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

function DoInputText(const ATitle: String;
                  var AText: String;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  Form: TDKTextInputForm;
begin
  Result:= False;

  Form:= TDKTextInputForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);
    Form.TitleLabel.Caption:= ATitle;
    Form.TextMemo.Text:= AText;

    if Form.ShowModal=mrOK then
    begin
      AText:= Form.TextMemo.Text;
      Result:= True;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TDKTextInputForm }

procedure TDKTextInputForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
  Width:= MIN_FORM_WIDTH;
  Height:= MIN_FORM_HEIGHT;
end;

procedure TDKTextInputForm.FormShow(Sender: TObject);
begin
  DKInputImages.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);
  FormToScreenCenter(Self);
end;

procedure TDKTextInputForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TDKTextInputForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

