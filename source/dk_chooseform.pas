unit DK_ChooseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,

  DK_CtrlUtils, DK_InputImages, DK_InputConst;

type

  { TDKChooseForm }

  TDKChooseForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    MainPanel: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    VT2: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TDKChooseForm }

procedure TDKChooseForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
end;

procedure TDKChooseForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  DKInputImages.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);

  if Caption='APP_TITLE' then
    Caption:= Application.Title;

  H:= ButtonPanel.Height + VT1.Height + 50;
  if VT2.Visible then
    H:= H + VT2.Height;

  if H>MIN_FORM_HEIGHT then
    ClientHeight:= H;

  FormToScreenCenter(Self);
end;

procedure TDKChooseForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TDKChooseForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

