unit DK_Inputs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,

  DK_Vector, DK_Math,

  DK_ChooseForm, DK_NumberInputForm;

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

  {ОКНО С ВВОДОМ ЧИСЛА Integer, Int64, Single, Double (SpinEdit)
   Кнопки   : "Сохранить" [Result=True], "Отменить" [Result=False]
   Параметры:
     ATitle     - заголовок списка
     AMinValue  - минимально возможное значение вводимого числа
     AMaxValue  - максимально возможное значение вводимого числа
     AIncrement - значение приращения числа при нажатии кнопок Up/Down
     ACaption   - заголовок окна сообщения (если='APP_TITLE' - наименование приложения)
     AWidth     - ширина окна (если<DK_InputConst.MIN_FORM_WIDTH, то DK_InputConst.MIN_FORM_WIDTH)
     AHeight    - выстота окна - автом. подбирается по высоте списка
                  (если=<DK_InputConst.MIN_FORM_HEIGHT, то DK_InputConst.MIN_FORM_HEIGHT)

     AValue - значение выбранного числа
   }
  function InputInteger(const ATitle: String;
                  var AValue: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputInteger(const ATitle: String;
                  var AValue: Integer;
                  const AMinValue, AMaxValue, AIncrement: Integer;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputInt64(const ATitle: String;
                  var AValue: Int64;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputInt64(const ATitle: String;
                  var AValue: Int64;
                  const AMinValue, AMaxValue, AIncrement: Int64;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputSingle(const ATitle: String;
                  var AValue: Single;
                  const ADecimalPlaces: Integer = 2;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputSingle(const ATitle: String;
                  var AValue: Single;
                  const ADecimalPlaces: Integer;
                  const AMinValue, AMaxValue: Single;
                  const AIncrement: Double;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer = 2;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
  function InputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer;
                  const AMinValue, AMaxValue, AIncrement: Double;
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
var
  MinValue, MaxValue: Integer;
  Value: Int64;
begin
  MinValue:= Integer.MinValue;
  MaxValue:= Integer.MaxValue;

  Value:= AValue;
  Result:= DoInputInt64(ATitle, Value, MinValue, MaxValue, 1,
                        ACaption, AWidth, AHeight);
  AValue:= Value;
end;

function InputInteger(const ATitle: String;
                var AValue: Integer;
                const AMinValue, AMaxValue, AIncrement: Integer;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  MinValue, MaxValue: Integer;
  Value: Int64;
begin
  if AMinValue<>0 then
    MinValue:= Max(AMinValue, Integer.MinValue)
  else
    MinValue:= Integer.MinValue;
  if AMaxValue<>0 then
    MaxValue:= Min(AMaxValue, Integer.MaxValue)
  else
    MaxValue:= Integer.MaxValue;

  Value:= AValue;
  Result:= DoInputInt64(ATitle, Value, MinValue, MaxValue, AIncrement,
                        ACaption, AWidth, AHeight);
  AValue:= Value;
end;

function InputInt64(const ATitle: String;
                  var AValue: Int64;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputInt64(ATitle, AValue, 0, 0, 1, ACaption, AWidth, AHeight);
end;

function InputInt64(const ATitle: String;
                var AValue: Int64;
                const AMinValue, AMaxValue, AIncrement: Int64;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputInt64(ATitle, AValue, AMinValue, AMaxValue, AIncrement,
                        ACaption, AWidth, AHeight);
end;

function InputSingle(const ATitle: String;
                  var AValue: Single;
                  const ADecimalPlaces: Integer = 2;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  MinValue, MaxValue: Single;
  Value: Double;
begin
  MinValue:= Single.MinValue;
  MaxValue:= Single.MaxValue;

  Value:= AValue;
  Result:= DoInputDouble(ATitle, Value, ADecimalPlaces, MinValue, MaxValue, 1,
                        ACaption, AWidth, AHeight);
  AValue:= Value;
end;

function InputSingle(const ATitle: String;
                var AValue: Single;
                const ADecimalPlaces: Integer;
                const AMinValue, AMaxValue: Single;
                const AIncrement: Double;
                const ACaption: String = '';
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  MinValue, MaxValue: Single;
  Value: Double;
begin
  if AMinValue<>0 then
    MinValue:= Max(AMinValue, Single.MinValue)
  else
    MinValue:= Single.MinValue;
  if AMaxValue<>0 then
    MaxValue:= Min(AMaxValue, Single.MaxValue)
  else
    MaxValue:= Single.MaxValue;

  Value:= AValue;
  Result:= DoInputDouble(ATitle, Value, ADecimalPlaces, MinValue, MaxValue, AIncrement,
                        ACaption, AWidth, AHeight);
  AValue:= Value;
end;

function InputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer = 2;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputDouble(ATitle, AValue, ADecimalPlaces, 0, 0, 1, ACaption, AWidth, AHeight);
end;

function InputDouble(const ATitle: String;
                  var AValue: Double;
                  const ADecimalPlaces: Integer;
                  const AMinValue, AMaxValue, AIncrement: Double;
                  const ACaption: String = '';
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
begin
  Result:= DoInputDouble(ATitle, AValue, ADecimalPlaces, AMinValue, AMaxValue, AIncrement,
                        ACaption, AWidth, AHeight);
end;

end.

