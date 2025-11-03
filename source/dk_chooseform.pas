unit DK_ChooseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,

  DK_CtrlUtils, DK_Vector, DK_VSTTableTools,

  DK_InputImages, DK_InputConst;

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

  function DoChoose(const ATitle1, ATitle2: String;
                  const AItems1, AItems2: TStrVector;
                  var AChooseIndex1, AChooseIndex2: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

{$R *.lfm}

function DoChoose(const ATitle1, ATitle2: String;
                const AItems1, AItems2: TStrVector;
                var AChooseIndex1, AChooseIndex2: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  Form: TDKChooseForm;
  List1, List2: TVSTStringList;
begin
  Result:= False;
  if VIsNil(AItems1) then
  begin
    AChooseIndex1:= -1;
    AChooseIndex2:= -1;
    Exit;
  end;

  Form:= TDKChooseForm.Create(nil);
  try
    SetFormSizeAndCaption(Form, ACaption, AWidth, AHeight);

    List1:= TVSTStringList.Create(Form.VT1, ATitle1, nil);
    List1.AutoHeight:= True;
    List1.Update(AItems1);
    List1.Select(AChooseIndex1);

    if VIsNil(AItems2) then
    begin
      Form.VT2.Visible:= False;
      AChooseIndex2:= -1;
    end
    else begin
      List2:= TVSTStringList.Create(Form.VT2, ATitle2, nil);
      List2.AutoHeight:= True;
      List2.Update(AItems2);
      List2.Select(AChooseIndex2);
    end;

    if Form.ShowModal=mrOK then
    begin
      AChooseIndex1:= List1.SelectedIndex;
      if Assigned(List2) then
        AChooseIndex2:= List2.SelectedIndex;
      Result:= True;
    end;

  finally
    if Assigned(List1) then FreeAndNil(List1);
    if Assigned(List2) then FreeAndNil(List2);
    FreeAndNil(Form);
  end;
end;


{ TDKChooseForm }

procedure TDKChooseForm.FormCreate(Sender: TObject);
begin
  DKInputImages:= TDKInputImages.Create(Self);
  Width:= MIN_FORM_WIDTH;
  Height:= MIN_FORM_HEIGHT;
end;

procedure TDKChooseForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  DKInputImages.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);

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

