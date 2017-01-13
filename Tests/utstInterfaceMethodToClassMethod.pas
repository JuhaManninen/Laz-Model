{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstInterfaceMethodToClassMethod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type

  IMyInterface = Interface
    Function MyFunc : Integer;
  end;

  TMyClass = Class(TInterfacedObject,IMyInterface)
    Function MyOtherFunction : Integer;

    {this causes error in passrc}
    Function IMyInterface.MyFunc = MyOtherFunction;
  end;

implementation

end.

