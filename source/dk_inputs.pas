unit DK_Inputs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,

  DK_Vector, DK_VSTTableTools,

  DK_InputConst, DK_ChooseForm;

  {ОКНО С ВЫБОРОМ ИЗ ОДНОГО СПИСКА
   Кнопки   : "Выбрать" [Result=True], "Отменить" [Result=False]
   Вход:
     ATitle   - заголовок списка
     AItems   - строки списка
     ACaption - заголовок окна сообщения (если='APP_TITLE' - наименование приложения)
     AWidth   - ширина окна (если<DK_InputConst.MIN_FORM_WIDTH, то DK_InputConst.MIN_FORM_WIDTH)
     AHeight  - выстота окна - автом. подбирается по высоте списка
                (если=<DK_InputConst.MIN_FORM_HEIGHT, то DK_InputConst.MIN_FORM_HEIGHT)
   Выход:
     AChooseIndex - индекс выбранной позиции списка
     }
  function Choose(const ATitle: String;
                  const AItems: TStrVector;
                  out AChooseIndex: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  {ОКНО С ВЫБОРОМ ИЗ ДВУХ СПИСКОВ
   Кнопки   : "Выбрать" [Result=True], "Отменить" [Result=False]
   Вход:
     ATitle1, ATitle2   - заголовки списков
     AItems1, AItems2   - строки списков
     ACaption - заголовок окна сообщения (если='APP_TITLE' - наименование приложения)
     AWidth   - ширина окна (если<DK_InputConst.MIN_FORM_WIDTH, то DK_InputConst.MIN_FORM_WIDTH)
     AHeight  - выстота окна - автом. подбирается по высоте списка
                (если=<DK_InputConst.MIN_FORM_HEIGHT, то DK_InputConst.MIN_FORM_HEIGHT)
   Выход:
     AChooseIndex1, AChooseIndex2 - индексы выбранной позиции списков
     }
  function Choose(const ATitle1, ATitle2: String;
                  const AItems1, AItems2: TStrVector;
                  out AChooseIndex1, AChooseIndex2: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

function Choose(const ATitle: String;
                const AItems: TStrVector;
                out AChooseIndex: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  i: Integer;
begin
  Result:= Choose(ATitle, EmptyStr, AItems, nil, AChooseIndex, i, ACaption, AWidth, AHeight);
end;

function Choose(const ATitle1, ATitle2: String;
                const AItems1, AItems2: TStrVector;
                out AChooseIndex1, AChooseIndex2: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  Form: TDKChooseForm;
  List1, List2: TVSTStringList;
begin
  Result:= False;
  AChooseIndex1:= -1;
  AChooseIndex1:= -2;

  Form:= TDKChooseForm.Create(nil);
  try
    Form.Caption:= ACaption;
    List1:= TVSTStringList.Create(Form.VT1, ATitle1, nil);
    List2:= TVSTStringList.Create(Form.VT2, ATitle2, nil);
    try
      List1.AutoHeight:= True;
      List1.Update(AItems1);
      if VIsNil(AItems2) then
        Form.VT2.Visible:= False
      else begin
        List2.AutoHeight:= True;
        List2.Update(AItems2);
      end;

      if AWidth>MIN_FORM_WIDTH then
        Form.Width:= AWidth;
      if AHeight>MIN_FORM_HEIGHT then
        Form.Height:= AHeight;

      if Form.ShowModal=mrOK then
      begin
        AChooseIndex1:= List1.SelectedIndex;
        AChooseIndex2:= List2.SelectedIndex;
        Result:= True;
      end;

    finally
      FreeAndNil(List1);
      FreeAndNil(List2);
    end;
  finally
    FreeAndNil(Form);
  end;
end;

end.

