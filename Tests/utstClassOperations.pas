{
 MODEL   STATUS Interface Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstClassOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

{ Support Type. Not part of test }
TClassType = (Class1, Class2, Class3);

{ Test for Class Variable }
TClassVariable = class
  private
    class var FClassType: TClassType;
end;



 TClassFunction = class(TClassVariable)
   public
     class function GetClassType: TClassType;
 end;

 TClassFunctionStatic = class(TClassVariable)
   public
     class function GetClassType: TClassType; static;
 end;

 TClassProcedure = class(TClassVariable)
   public
     class procedure SetClassType(const AValue: TClassType);
 end;

 TClassProcedureStatic = class(TClassVariable)
   public
     class procedure SetClassType(const AValue: TClassType); static;
 end;

 TClassConstructor = class(TClassVariable)
   public
     class constructor Create;
 end;

 TClassDestructor = class(TClassVariable)
   public
     class destructor Destroy;
 end;


implementation

end.

