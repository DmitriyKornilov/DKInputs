unit DK_Inputs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,

  DK_Vector,

  DK_ChooseForm, DK_IntegerInputForm;

  {ОКНО С ВЫБОРОМ ИЗ ОДНОГО СПИСКА
   Кнопки   : "Выбрать" [Result=True], "Отменить" [Result=False]
   Параметры:
     ATitle   - заголовок списка
     AItems   - строки списка
     ACaption - заголовок окна сообщения (если='APP_TITLE' - наименование приложения)
     AWidth   - ширина окна (если<DK_InputConst.MIN_FORM_WIDTH, то DK_InputConst.MIN_FORM_WIDTH)
     AHeight  - выстота окна - автом. подбирается по высоте списка
                (если=<DK_InputConst.MIN_FORM_HEIGHT, то DK_InputConst.MIN_FORM_HEIGHT)

     AChooseIndex - индекс выбранной позиции списка
     }
  function Choose(const ATitle: String;
                  const AItems: TStrVector;
                  var AChooseIndex: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  {ОКНО С ВЫБОРОМ ИЗ ДВУХ СПИСКОВ
   Кнопки   : "Выбрать" [Result=True], "Отменить" [Result=False]
   Параметры:
     ATitle1, ATitle2   - заголовки списков
     AItems1, AItems2   - строки списков
     ACaption - заголовок окна сообщения (если='APP_TITLE' - наименование приложения)
     AWidth   - ширина окна (если<DK_InputConst.MIN_FORM_WIDTH, то DK_InputConst.MIN_FORM_WIDTH)
     AHeight  - выстота окна - автом. подбирается по высоте списка
                (если=<DK_InputConst.MIN_FORM_HEIGHT, то DK_InputConst.MIN_FORM_HEIGHT)

     AChooseIndex1, AChooseIndex2 - индексы выбранной позиции списков
     }
  function Choose(const ATitle1, ATitle2: String;
                  const AItems1, AItems2: TStrVector;
                  var AChooseIndex1, AChooseIndex2: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;



  function InputInteger(const ATitle: String;
                  var AValue: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  function InputInteger(const ATitle: String;
                  var AValue: Integer;
                  const AMinValue, AMaxValue: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

function Choose(const ATitle: String;
                const AItems: TStrVector;
                var AChooseIndex: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  ChooseIndex2: Integer;
begin
  ChooseIndex2:= -1;
  Result:= DoChoose(ATitle, EmptyStr, AItems, nil, AChooseIndex, ChooseIndex2,
                    ACaption, AWidth, AHeight);
end;

function Choose(const ATitle1, ATitle2: String;
                const AItems1, AItems2: TStrVector;
                var AChooseIndex1, AChooseIndex2: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
begin
  Result:= DoChoose(ATitle1, ATitle2, AItems1, AItems2, AChooseIndex1, AChooseIndex2,
                    ACaption, AWidth, AHeight);
end;

function InputInteger(const ATitle: String;
                  var AValue: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputInteger(ATitle, AValue, 0, 0, ACaption, AWidth, AHeight);
end;

function InputInteger(const ATitle: String;
                var AValue: Integer;
                const AMinValue, AMaxValue: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputInteger(ATitle, AValue, AMinValue, AMaxValue, ACaption, AWidth, AHeight);
end;

end.

