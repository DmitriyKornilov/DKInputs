unit DK_InputUtils;

{$mode objfpc}{$H+}

interface

uses
  Forms;//  Classes, SysUtils;

const
  MIN_FORM_WIDTH = 400;
  MIN_FORM_HEIGHT = 250;

  BORDER_SPACING_HORIZONTAL = 8;
  BORDER_SPACING_VERTICAL   = 12;

  procedure SetFormSizeAndCaption(const AForm: TForm;
                                  const ACaption: String;
                                  const AWidth, AHeight: Integer);

implementation

procedure SetFormSizeAndCaption(const AForm: TForm;
                                const ACaption: String;
                                const AWidth, AHeight: Integer);
begin
  AForm.Caption:= ACaption;
  if ACaption='APP_TITLE' then
    AForm.Caption:= Application.Title;

  if AWidth>MIN_FORM_WIDTH then
    AForm.Width:= AWidth;
  if AHeight>MIN_FORM_HEIGHT then
    AForm.Height:= AHeight;
end;

end.

