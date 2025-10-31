unit DK_InputImages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Buttons,
  DK_CtrlUtils;

type

  { TDKInputImages }

  TDKInputImages = class(TDataModule)
    Glyphs24: TImageList;
    Glyphs30: TImageList;
    Glyphs36: TImageList;
    Glyphs42: TImageList;
  private

  public
    procedure ToButtons(const AButtons: array of TSpeedButton);
  end;

var
  DKInputImages: TDKInputImages;

implementation

{$R *.lfm}

{ TDKInputImages }

procedure TDKInputImages.ToButtons(const AButtons: array of TSpeedButton);
var
  i: Integer;
  L: TImageList;
begin
  L:= ChooseImageListForScreenPPI(Glyphs24, Glyphs30, Glyphs36, Glyphs42);
  for i:= 0 to High(AButtons) do
    AButtons[i].Images:= L;
end;

end.

