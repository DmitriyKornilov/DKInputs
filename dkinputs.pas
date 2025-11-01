{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKInputs;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_ChooseForm, DK_InputImages, DK_Inputs, DK_InputConst, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKInputs', @Register);
end.
