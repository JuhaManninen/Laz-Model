{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde
  Portions (C) 2016 Peter Dyson. Initial Lazarus port

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uModel;

{$mode objfpc}{$H+}

{
  Classes to represent the object model.
}

interface

uses
  Classes, SysUtils, Contnrs,
  uListeners, uModelEntity, uIterators;

const

  UNKNOWNPACKAGE_NAME = '«Unknown»';
  ConfigFileExt = '.essModel';

type
  TLogicPackage = class;
  TUnitPackage = class;

  { Non UML construct, needed for class operations}
  TClassOperationType = (cotNone, cotClass, cotStaticClass);

  { Non UML construct, need to transform for xmi}
  TOperationType = (otConstructor, otDestructor, otProcedure, otFunction);

  { UML Modeling Language 2.5 9.5.3 }
  TAggregationKind = (akNone, akShared, akComposite);

  { UML Modeling Language 2.5 9.4.2
    Translates to a feature on a parameter in xmi}
  TParameterDirectionKind = (dkIn, dkInOut, dkOut, dkReturn);
  //  TArgumentAccess = (argDefault, argConst, argVar, argOut, argConstRef);

  { Pascal call modifiers
    translates to Features on an Operation in xmi}
  TCallModifier = (cmDefault, cmRegister, cmCdecl, cmPascal, cmStdCall, cmSafeCall, cmInline);
  //  TCallingConvention = (ccDefault,ccRegister,ccPascal,ccCDecl,ccStdCall,ccOldFPCCall,ccSafeCall,ccSysCall);


  {using this we can keep full pascal declaration and use UML abstract concept
   to flag a method as abstract along with virtual or dysnamic}
  TMethodDirective = (mdDefault, mdVirtual, mdDynamic, mdAbstract, mdOverride, mdReintroduce, mdMessage);


  // these are more oriented to procedural pascal programming.
  //TProcedureModifier = (pmVirtual, pmDynamic, pmAbstract, pmOverride,
  //                        pmExport, pmOverload, pmMessage, pmReintroduce,
  //                        pmStatic,pmInline,pmAssembler,pmVarargs, pmPublic,
  //                        pmCompilerProc,pmExternal,pmForward);

  TCallModifiers = set of TCallModifier;

  TObjectModel = class
  private
    Listeners: TInterfaceList;
    FModelRoot: TLogicPackage;
    FUnknownPackage: TUnitPackage;
    FLocked: boolean;
    procedure CreatePackages;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
    procedure AddListener(NewListener: IUnknown);
    procedure RemoveListener(Listener: IUnknown);
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    property ModelRoot: TLogicPackage read FModelRoot;
    property Locked: boolean read FLocked;
    property UnknownPackage: TUnitPackage read FUnknownPackage;
  end;

  TFeature = class(TModelEntity);

  TClassifier = class(TModelEntity)
  private
    FFeatures: TObjectList;
    FIsPlaceholder: boolean;
  protected
    //Editor integration, each classifier can be in a separate source file in java
    FSourceFilename: String;
    function GetSourcefilename: String; override;
    procedure SetSourcefilename(const Value: String); override;
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function GetFeatures : IModelIterator;
  published
    property Features: IModelIterator read GetFeatures;
    property IsPlaceholder: boolean read FIsPlaceHolder write FIsPlaceholder;
  end;

  TParameter = class(TModelEntity)
  private
    FTypeClassifier : TClassifier;
    FDirection: TParameterDirectionKind;
    FIsConst: boolean;
    FDefaultValue: string;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  published
    property TypeClassifier : TClassifier read FTypeClassifier write FTypeClassifier;
    property Direction: TParameterDirectionKind read FDirection write Fdirection default dkIn;
    { Pascal specific addition to TParameterDirectionKind.dkIn could be set as true for
     any dkIn and not change the program functionality.}
    property IsConst: boolean read FIsConst write FIsConst default false;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  { TOperation }

  TOperation = class(TFeature)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  private
    FOperationType: TOperationType;
    FParameters: TObjectList;
    FIsAbstract: boolean;
    FReturnValue: TClassifier;
    FModifier: TCallModifier;
    FIsStatic: TClassOperationType;
    FMethodDirective: TMethodDirective;
    procedure SetOperationType(const Value: TOperationType);
    procedure SetIsAbstract(const Value: boolean);
    procedure SetReturnValue(const Value: TClassifier);
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddParameter(const NewName: string): TParameter;
    function GetParameters : IModelIterator;
    procedure AddCallModifier(AValue: TCallModifier);
  published
    property OperationType: TOperationType read FOperationType write SetOperationType;
    property IsStatic: TClassOperationType read FIsStatic write FIsStatic default cotNone;
    property IsAbstract: boolean read FIsAbstract write SetIsAbstract;
    property ReturnValue: TClassifier read FReturnValue write SetReturnValue;
    property MethodDirective: TMethodDirective read FMethodDirective write FMethodDirective default mdDefault;
    property CallModifier: TCallModifier read FModifier write FModifier;
  end;

  { TEnumLiteral }

  TEnumLiteral = class(TFeature)
  private
    FOrdVal: integer;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  published
    property OrdVal:integer read FOrdVal write FOrdVal;
  end;

  TAttribute = class(TFeature)
  private
    FTypeClassifier: TClassifier;
    FIsStatic: boolean;
    procedure SetTypeClassifier(const Value: TClassifier);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  published
    property TypeClassifier : TClassifier read FTypeClassifier write SetTypeClassifier;
    { UML 2.5 9.5.3
     < When a Property is a static attribute of a Classifier, the value or values
      are related to the Classifier itself within some execution scope >
      We need this on Attribute rather than property as Pascal can have Class
      Variables which are not Properties but are needed by Class Operations.
      Properties will inherit this.
      Used in Model to flag Class variables.
    }
    property IsStatic: boolean read FIsStatic write FIsStatic default False;
  end;

  TProperty = class(TAttribute)
  { TODO : to be specified later }
  private
    FAggregationKind: TAggregationKind;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  published
    property AggregationKind: TAggregationKind read FAggregationKind write FAggregationKind default akNone;
  end;

  TDataType = class(TClassifier)
    {From UML-spec: A descriptor of a set of values that lack identity and whose
    operations do not have side effects. Datatypes include
    primitive pre-defined types and user-definable types. Pre-defined
    types include numbers, string and time. User-definable
    types include enumerations.}
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  end;


  { TStructuredDataType }

  TStructuredDataType = class(TDataType)
  public
    function AddAttribute(const NewName: string): TAttribute;
    function GetAttributes: IModelIterator;
  end;

  { UML Modeling Language 2.5 10.2.2 }

  { TEnumeration }

  TEnumeration = class(TDataType)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    function AddLiteral(const s: string): TEnumLiteral;
  published
  end;

  TInterface = class(TClassifier)
  private
    FAncestor: TInterface;
    procedure SetAncestor(const Value: TInterface);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddOperation(const NewName: string): TOperation;
    function AddAttribute(const NewName: string): TAttribute;
    function GetOperations : IModelIterator;
    function GetAttributes : IModelIterator;
    property Ancestor: TInterface read FAncestor write SetAncestor;
    function GetImplementingClasses : IModelIterator;
  end;

  { TClass }

  TClass = class(TClassifier, IBeforeClassListener)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  private
    FAncestor: TClass;
    FImplements: TObjectList;
    procedure SetAncestor(const Value: TClass);
    //Ancestorlisteners
    procedure AncestorChange(Sender: TModelEntity);
    procedure AncestorAddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure AncestorRemove(Sender: TModelEntity);
    procedure AncestorEntityChange(Sender: TModelEntity);
    procedure IBeforeClassListener.Change = AncestorChange;
    procedure IBeforeClassListener.EntityChange = AncestorEntityChange;
    procedure IBeforeClassListener.AddChild = AncestorAddChild;
    procedure IBeforeClassListener.Remove = AncestorRemove;
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddOperation(const NewName: string): TOperation;
    function AddAttribute(const NewName: string): TAttribute;
    function AddProperty(const NewName: string): TProperty;
    function AddImplements(I: TInterface): TInterface;
    property Ancestor: TClass read FAncestor write SetAncestor;
    function GetOperations : IModelIterator;
    function GetAttributes : IModelIterator;
    function GetImplements : IModelIterator;
    function GetDescendants : IModelIterator;
    function FindOperation(O : TOperation) : TOperation;
  end;


  TAbstractPackage = class(TModelEntity)
  private
    ConfigFile : string;
  protected
    //To be able to integrate with an editor we need the location of the entity
    //Editor integration, in delphi all classes in a package are in the same file
    FSourceFilename: String;
    function GetSourcefilename: String; override;
    procedure SetSourcefilename(const Value: String); override;
  public
    constructor Create(AOwner: TModelEntity); override;
    procedure SetConfigFile(const Value : string);
    function GetConfigFile : string;
  end;

  //Represents the link between one package that uses another
  TUnitDependency = class(TModelEntity)
  public
    Package : TUnitPackage;
  end;

  { TUnitPackage }

  TUnitPackage = class(TAbstractPackage)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  private
    FClassifiers: TObjectList;
    FUnitDependencies: TObjectList;
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddClass(const NewName: string): TClass;
    function AddInterface(const NewName: string): TInterface;
    function AddDatatype(const NewName: string): TDataType;
    function AddEnumeration(const NewName: string): TEnumeration;
    function AddUnitDependency(U : TUnitPackage; AVisibility : TVisibility): TUnitDependency;
    function FindClassifier(const CName: string; RaiseException: boolean = False; TheClass : TModelEntityClass = nil; CaseSense : boolean = False): TClassifier;
    function GetClassifiers : IModelIterator;
    function GetUnitDependencies : IModelIterator;
  end;


  TLogicPackage = class(TAbstractPackage)
  private
    FPackages: TObjectList;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(AOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddUnit(const NewUnitName: string): TUnitPackage;
    //Might need a AddLogicPackage also
    function FindUnitPackage(const PName: string; RaiseException: boolean = False; CaseSense : boolean = False): TUnitPackage;
    function GetPackages : IModelIterator;
    function GetAllUnitPackages : IModelIterator;
    function GetAllClassifiers : IModelIterator;
  end;

  function AllClassesPackage : TAbstractPackage;

implementation

uses uError;


Var

_AllClassesPackage : TAbstractPackage = nil;


type
  //Used by Class.GetDescendant
  TClassDescendantFilter = class(TIteratorFilter)
  private
    Ancestor : TClass;
  public
    constructor Create(AAncestor : TClass);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  //Used by Interface.GetImplementingClasses
  TInterfaceImplementsFilter = class(TIteratorFilter)
  private
    Int : TInterface;
  public
    constructor Create(I : TInterface);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  TStrCompare = function(const S1, S2: string): Integer;

const
  CompareFunc : array[boolean] of TStrCompare = (@CompareText, @CompareStr);

{ TStructuredDataType }

function TStructuredDataType.AddAttribute(const NewName: string): TAttribute;
begin

end;

function TStructuredDataType.GetAttributes: IModelIterator;
begin

end;

{ TEnumLiteral }

class function TEnumLiteral.GetBeforeListener: TGUID;
begin
  Result:=inherited GetBeforeListener;
end;

class function TEnumLiteral.GetAfterListener: TGUID;
begin
  Result:=inherited GetAfterListener;
end;

{ TEnumeration }

class function TEnumeration.GetBeforeListener: TGUID;
begin
  Result:=inherited GetBeforeListener;
end;

class function TEnumeration.GetAfterListener: TGUID;
begin
  Result:=inherited GetAfterListener;
end;

function TEnumeration.AddLiteral(const s: string): TEnumLiteral;
begin
  Result := TEnumLiteral.Create(self);
  Result.Name:= s;
  FFeatures.Add(Result);
end;

{ TObjectModel }

constructor TObjectModel.Create;
begin
  Listeners := TInterfaceList.Create;
  CreatePackages;
end;

destructor TObjectModel.Destroy;
begin
  FreeAndNil(Listeners);
  FreeAndNil(FModelRoot);
// FUnknownPackage will be freed by FModelRoot who owns it
  inherited;
end;

procedure TObjectModel.Clear;
begin
  //Model must be locked, otherwise events will be fired back to
  //backend and diagram.
  if not FLocked then
  begin
    Lock;
    FreeAndNil(FModelRoot);
    CreatePackages;
    UnLock;
  end
  else
  begin
    FreeAndNil(FModelRoot);
    CreatePackages;
  end;
end;


procedure TObjectModel.Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
var
  I: integer;
  L,Dum: IUnknown;
begin
  if not Locked then
    for I := 0 to Listeners.Count - 1 do
    begin
      L := Listeners[I];
      case Method of
        //BeforeChange is triggered when the model will be changed from the root-level.
        mtBeforeChange:
           if L.QueryInterface(IBeforeObjectModelListener,Dum) = 0 then
             (L as IBeforeObjectModelListener).Change(nil);
        //AfterChange is triggered when the model has been changed from the root-level.
        mtAfterChange:
           if L.QueryInterface(IAfterObjectModelListener,Dum) = 0 then
             (L as IAfterObjectModelListener).Change(nil);
      else
        raise Exception.Create(ClassName + ' Eventmethod not recognized.');
      end;
    end;
end;


procedure TObjectModel.Lock;
begin
  Fire(mtBeforeChange);
  FLocked := True;
  ModelRoot.Locked := True;
end;

procedure TObjectModel.Unlock;
begin
  FLocked := False;
  ModelRoot.Locked := False;
  Fire(mtAfterChange);
end;

procedure TObjectModel.CreatePackages;
begin
  //Creates the default packages that must exist
  FModelRoot := TLogicPackage.Create(nil);
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

procedure TObjectModel.AddListener(NewListener: IUnknown);
begin
  if Listeners.IndexOf(NewListener) = -1 then
    Listeners.Add(NewListener);
end;

procedure TObjectModel.RemoveListener(Listener: IUnknown);
begin
  Listeners.Remove(Listener);
end;

{ TLogicPackage }

constructor TLogicPackage.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
  FPackages := TObjectList.Create(True);
end;

destructor TLogicPackage.Destroy;
begin
  FreeAndNil(FPackages);
  inherited;
end;

function TLogicPackage.AddUnit(const NewUnitName: string): TUnitPackage;
begin
  Result := TUnitPackage.Create(Self);
  Result.FName := NewUnitName;
  FPackages.Add(Result);
  try
    Fire(mtBeforeAddChild, Result)
  except
    FPackages.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result)
end;

class function TLogicPackage.GetAfterListener: TGUID;
begin
  Result := IAfterLogicPackageListener;
end;

class function TLogicPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeLogicPackageListener;
end;

//Searches in this and dependant logic packages after a unit with name PName.
function TLogicPackage.FindUnitPackage(const PName: string; RaiseException: boolean = False; CaseSense : boolean = False): TUnitPackage;
var
  I: integer;
  P: TAbstractPackage;
  F : TStrCompare;
begin
  F := CompareFunc[CaseSense];
  Result := nil;
  for I := 0 to FPackages.Count - 1 do
  begin
    P := FPackages[I] as TAbstractPackage;
    if (P is TLogicPackage) then
    begin
      Result := (P as TLogicPackage).FindUnitPackage(PName, RaiseException);
      if Assigned(Result) then
        Exit;
    end
    else if (P is TUnitPackage) then
    begin
      if F(P.Name,PName)=0 then
      begin
        Result := P as TUnitPackage;
        Exit;
      end;
    end;
  end;
  if not Assigned(Result) and RaiseException then
    raise Exception.Create(ClassName + '.FindUnitPackage failed: ' + PName);
end;

function TLogicPackage.GetPackages: IModelIterator;
begin
  Result := TModelIterator.Create(FPackages);
end;

//Returns all unitpackages in and below this logic package.
//Unknownpackage is excluded.
function TLogicPackage.GetAllUnitPackages: IModelIterator;
var
  List : TObjectList;

  procedure InAddNested(L : TLogicPackage);
  var
    Mi : IModelIterator;
    P : TModelEntity;
  begin
    Mi := L.GetPackages;
    while Mi.HasNext do
    begin
      P := Mi.Next;
      if P is TLogicPackage then
        InAddNested(P as TLogicPackage)
      else //Not logicpackage, must be unitpackage.
        if (P.Name<>UNKNOWNPACKAGE_NAME) then List.Add( P );
    end;
  end;

begin
  List := TObjectList.Create(False);
  try
    InAddNested(Self);
    Result := TModelIterator.Create(List,True);
  finally
    List.Free;
  end;
end;

//Returns all classifiers in and below this logic package.
function TLogicPackage.GetAllClassifiers: IModelIterator;
var
  Pmi,Cmi : IModelIterator;
  List : TObjectList;
begin
  List := TObjectList.Create(False);
  try
    Pmi := GetAllUnitPackages;
    while Pmi.HasNext do
    begin
      Cmi := (Pmi.Next as TUnitPackage).GetClassifiers;
      while Cmi.HasNext do
        List.Add( Cmi.Next );
    end;
    Result := TModelIterator.Create(List,True);
  finally
    List.Free;
  end;
end;

{ TUnitPackage }

constructor TUnitPackage.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
  FClassifiers := TObjectList.Create(True);
  FUnitDependencies := TObjectList.Create(True);
end;

destructor TUnitPackage.Destroy;
begin
  FreeAndNil(FClassifiers);
  FreeAndNil(FUnitDependencies);
  inherited;
end;

function TUnitPackage.AddClass(const NewName: string): TClass;
begin
  Result := TClass.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddInterface(const NewName: string): TInterface;
begin
  Result := TInterface.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddDatatype(const NewName: string): TDataType;
begin
  Result := TDataType.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddEnumeration(const NewName: string): TEnumeration;
begin
  Result := TEnumeration.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TUnitPackage.GetAfterListener: TGUID;
begin
  Result := IAfterUnitPackageListener;
end;

class function TUnitPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeUnitPackageListener;
end;


{
  Search for classifier in this unit, then looks in UnitDependencies if necessary.
  Used by the parser to find ancestorclass within current scope.
}
function TUnitPackage.FindClassifier(const CName: string;
  RaiseException: boolean = False;
  TheClass : TModelEntityClass = nil;
  CaseSense : boolean = False): TClassifier;
var
  Clsf : TClassifier;
  Mi : IModelIterator;
  P : TUnitPackage;
  F : TStrCompare;

  function InFind(P : TUnitPackage) : TClassifier;
  var
    Mi : IModelIterator;
  begin
    Result := nil;
    //Search in this unit
    if Assigned(TheClass) then
      Mi := TModelIterator.Create( P.GetClassifiers , TheClass )
    else
      Mi := TModelIterator.Create(P.GetClassifiers);
    while Mi.HasNext do
    begin
      Clsf := TClassifier(Mi.Next) ;
      if F(Clsf.Name,CName)=0 then
      begin
        Result := Clsf;
        Break;
      end;
    end;
  end;

begin
  F := CompareFunc[CaseSense];
  //Search in this unit
  Result := InFind(Self);
  //If nil search in public dependencies
  if not Assigned(Result) then
  begin
    Mi := GetUnitDependencies;
    while Mi.HasNext do
    begin
      P := (Mi.Next as TUnitDependency).Package;
      Result := InFind(P);
      if Assigned(Result) then
        Break;
    end;
  end;
  if not Assigned(Result) and RaiseException then
    raise Exception.Create(ClassName + '.FindClassifier failed: ' + CName);
end;

function TUnitPackage.GetClassifiers: IModelIterator;
begin
  Result := TModelIterator.Create( FClassifiers );
end;


function TUnitPackage.AddUnitDependency(U: TUnitPackage; AVisibility: TVisibility): TUnitDependency;
begin
  Assert( (U<>Self) and (U<>nil) ,ClassName + '.AddUnitDependency invalid parameter');
  Result := TUnitDependency.Create( Self );
  Result.Package := U;
  Result.Visibility := AVisibility;
  FUnitDependencies.Add( Result );
end;

function TUnitPackage.GetUnitDependencies: IModelIterator;
begin
  Result := TModelIterator.Create( FUnitDependencies );
end;

{ TClass }

constructor TClass.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
  FImplements := TObjectList.Create(False); //Only reference
end;

destructor TClass.Destroy;
begin
  //Dont touch listeners if the model is locked.
  if not Locked then
  begin
    Fire(mtBeforeRemove);
    //    if Assigned(FAncestor) then
    //      FAncestor.RemoveListener(IBeforeClassListener(Self));
  end;
  FreeAndNil(FImplements);
  inherited;
end;

function TClass.AddAttribute(const NewName: string): TAttribute;
begin
  Result := TAttribute.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TClass.AddProperty(const NewName: string): TProperty;
begin
  Result := TProperty.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
end;

function TClass.AddOperation(const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TClass.GetAfterListener: TGUID;
begin
  Result := IAfterClassListener;
end;

class function TClass.GetBeforeListener: TGUID;
begin
  Result := IBeforeClassListener;
end;

function TClass.AddImplements(I: TInterface): TInterface;
begin
  Result := I;
  FImplements.Add(I);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FImplements.Remove(I);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

procedure TClass.SetAncestor(const Value: TClass);
var
  Old: TClass;
begin
  Assert(Value <> Self, 'Tried to set self to ancestor.');
  if Value <> FAncestor then
  begin
    Old := FAncestor;
    FAncestor := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FAncestor := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TClass.AncestorAddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  ErrorHandler.Trace(Format('%s : %s : %s : %s', ['AncestorAddChild', ClassName, FName, Sender.Name]));
end;

procedure TClass.AncestorChange(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('%s : %s : %s : %s', ['AncestorChange', ClassName, FName, Sender.Name]));
end;

procedure TClass.AncestorEntityChange(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('%s : %s : %s : %s', ['AncestorEntityChange', ClassName, FName, Sender.Name]));
  Fire(mtBeforeEntityChange);
  Fire(mtAfterEntityChange);
end;

procedure TClass.AncestorRemove(Sender: TModelEntity);
begin
  ErrorHandler.Trace(Format('%s : %s : %s : %s', ['AncestorRemove', ClassName, FName, Sender.Name]));
  FAncestor.RemoveListener(IBeforeClassListener(Self));
  Ancestor := nil;
end;

function TClass.GetOperations: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TOperation);
end;

function TClass.GetAttributes: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TAttribute);
end;

function TClass.GetImplements: IModelIterator;
begin
  Result := TModelIterator.Create( FImplements );
end;

//Returns a list of classes that inherits from this class.
function TClass.GetDescendants: IModelIterator;
begin
  Result := TModelIterator.Create(
    (Root as TLogicPackage).GetAllClassifiers,
    TClassDescendantFilter.Create(Self) );
end;


{
  Finds an operation with same name and signature as parameter.
  Used by Delphi-parser to find a modelentity for a method implementation.
}
function TClass.FindOperation(O: TOperation): TOperation;
var
  Mi,Omi1,Omi2 : IModelIterator;
  O2 : TOperation;
  label Skip;
begin
  Assert(O<>nil,ClassName + '.FindOperation invalid parameter');
  Result := nil;
  Mi := GetOperations;
  while Mi.HasNext do
  begin
    O2 := Mi.Next as TOperation;
    //Compare nr of parameters
    if O.FParameters.Count<>O2.FParameters.Count then
      Continue;
    { TODO -ovk : case sensitive match? java/delphi. only delphi-parser calls this method. }
    //Compare operation name
    if CompareText(O.Name,O2.Name)<>0 then
      Continue;
    //Compare parameters
    Omi1 := O.GetParameters;
    Omi2 := O2.GetParameters;
    while Omi1.HasNext do
      if CompareText((Omi1.Next as TParameter).Name,(Omi2.Next as TParameter).Name)<>0 then
        goto Skip;
    //Ok, match
    Result := O2;
    Break;
  Skip:
  end;
end;


{ TParameter }

class function TParameter.GetAfterListener: TGUID;
begin
  Result := IAfterParameterListener;
end;

class function TParameter.GetBeforeListener: TGUID;
begin
  Result := IBeforeParameterListener;
end;

{ TOperation }


constructor TOperation.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
  FParameters := TObjectList.Create(True);
end;

destructor TOperation.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

function TOperation.AddParameter(const NewName: string): TParameter;
begin
  Result := TParameter.Create(Self);
  Result.FName := NewName;
  FParameters.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FParameters.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TOperation.GetAfterListener: TGUID;
begin
  Result := IAfterOperationListener;
end;

class function TOperation.GetBeforeListener: TGUID;
begin
  Result := IBeforeOperationListener;
end;

procedure TOperation.SetOperationType(const Value: TOperationType);
var
  Old: TOperationType;
begin
  Old := FOperationType;
  if Old <> Value then
  begin
    FOperationType := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FOperationType := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TOperation.SetIsAbstract(const Value: boolean);
var
  Old: boolean;
begin
  Old := FIsAbstract;
  if Old <> Value then
  begin
    FIsAbstract := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FIsAbstract := Old;
      raise;
    end {try};
    Fire(mtAfterEntityChange);
  end;
end;

procedure TOperation.SetReturnValue(const Value: TClassifier);
var
  Old: TClassifier;
begin
  Old := FReturnValue;
  if Old <> Value then
  begin
    FReturnValue := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FReturnValue := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

function TOperation.GetParameters: IModelIterator;
begin
  Result := TModelIterator.Create( FParameters );
end;

procedure TOperation.AddCallModifier(AValue: TCallModifier);
begin
  FModifier := AValue;
end;

{ TAttribute }

class function TAttribute.GetAfterListener: TGUID;
begin
  Result := IAfterAttributeListener;
end;

class function TAttribute.GetBeforeListener: TGUID;
begin
  Result := IBeforeAttributeListener;
end;

procedure TAttribute.SetTypeClassifier(const Value: TClassifier);
var
  Old: TClassifier;
begin
  Old := FTypeClassifier;
  if Old <> Value then
  begin
    FTypeClassifier := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FTypeClassifier := Old;
      raise;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

{ TProperty }

class function TProperty.GetAfterListener: TGUID;
begin
  Result := IAfterPropertyListener;
end;

class function TProperty.GetBeforeListener: TGUID;
begin
  Result := IBeforePropertyListener;
end;

{ TClassifier }

constructor TClassifier.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
  FFeatures := TObjectList.Create(True);
  if Assigned(uModelEntity.CurrentSourcefilename) then
    FSourceFilename := uModelEntity.CurrentSourcefilename^;
end;

destructor TClassifier.Destroy;
begin
  FFeatures.Free;
  inherited;
end;

function TClassifier.GetFeatures: IModelIterator;
begin
  Result := TModelIterator.Create( FFeatures );
end;

function TClassifier.GetSourcefilename: String;
begin
  Result := inherited GetSourcefilename;
end;

procedure TClassifier.SetSourcefilename(const Value: String);
begin
  FSourceFilename := Value;
end;

{ TInterface }

constructor TInterface.Create(AOwner: TModelEntity);
begin
  inherited Create(AOwner);
end;

destructor TInterface.Destroy;
begin
  inherited;
end;

function TInterface.AddOperation(const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TInterface.GetAfterListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;

class function TInterface.GetBeforeListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;

function TInterface.GetOperations: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TOperation);
end;

procedure TInterface.SetAncestor(const Value: TInterface);
begin
  Assert(Value <> Self, 'Tried to set self to ancestor.');
  FAncestor := Value;
end;

//Returns a list of classes that implements this interface.
function TInterface.GetImplementingClasses: IModelIterator;
begin
  Result := TModelIterator.Create(
    (Root as TLogicPackage).GetAllClassifiers,
    TInterfaceImplementsFilter.Create(Self) );
end;

function TInterface.AddAttribute(const NewName: string): TAttribute;
begin
  Result := TAttribute.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
    raise;
  end;
  Fire(mtAfterAddChild, Result);
end;

function TInterface.GetAttributes : IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TAttribute);
end;

{ TDataType }

class function TDataType.GetAfterListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;

class function TDataType.GetBeforeListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;

{ TAbstractPackage }

constructor TAbstractPackage.Create(AOwner: TModelEntity);
begin
  inherited;
  if Assigned(uModelEntity.CurrentSourcefilename) then
    FSourceFilename := uModelEntity.CurrentSourcefilename^;
end;

function TAbstractPackage.GetConfigFile: string;
begin
  Result := ConfigFile;
  if (Result='') and Assigned(FOwner) then
    Result := (Owner as TAbstractPackage).GetConfigFile;
end;

function TAbstractPackage.GetSourcefilename: String;
begin
  Result := FSourceFilename;
end;

procedure TAbstractPackage.SetConfigFile(const Value: string);
begin
  if Value<>'' then
    ConfigFile := ChangeFileExt(Value,ConfigFileExt);
end;


procedure TAbstractPackage.SetSourcefilename(const Value: String);
begin
  FSourceFilename := Value;
end;

{ TClassDescendantFilter }

constructor TClassDescendantFilter.Create(AAncestor: TClass);
begin
  inherited Create;
  Self.Ancestor := AAncestor;
end;

//Returns true if M inherits from ancestor
function TClassDescendantFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is TClass) and ((M as TClass).Ancestor = Ancestor);
end;

{ TInterfaceImplementsFilter }

constructor TInterfaceImplementsFilter.Create(I: TInterface);
begin
  inherited Create;
  Int := I;
end;

//Returns true if M implements interface Int
function TInterfaceImplementsFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is TClass) and ((M as TClass).FImplements.IndexOf(Int)<>-1);
end;


//Unique Flag-instance, if Integrator.CurrentEntity=AllClassesPackage then show all classes
function AllClassesPackage : TAbstractPackage;
begin
  if _AllClassesPackage=nil then
    _AllClassesPackage := TAbstractPackage.Create(nil);
  Result := _AllClassesPackage;
end;

finalization
FreeAndNil(_AllClassesPackage);

end.

