unit utstClassOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TClassType = (Class1, Class2, Class3);

  TClassVariable = class
    private
      class var FClassType: TClassType;
  end;

{ Not currently implemented in FPC }

 // TClassProperty = class (TClassVariable)
 //   published
 //   class property ClassType: TClassType read FClassType write FClassType default Class1;
 // end;

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

