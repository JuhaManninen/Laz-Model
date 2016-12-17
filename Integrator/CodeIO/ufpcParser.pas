{
  ESS-Model
  Copyright (C) 2016 Peter Dyson. Initial Lazarus port
  Portions (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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
unit ufpcParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCodeParser, uModel, uModelEntity, PParser, PasTree;


type

  { TSimpleEngine }

  TSimpleEngine = class(TPasTreeContainer)
  public
    constructor Create;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;


  { TfpcParser }

  TfpcParser = class(TCodeParser)
    private
      FOM: TObjectModel;
      FUnit: TUnitPackage;
      fFilename: string;
      FGlobalDefines: TStringList;

      function getVisibility(vis: TPasMemberVisibility): TVisibility;
      function getProcType(pt: TProcType): TOperationType;
      function getClassifier(s: String): TClassifier;

      procedure ParseProject(M: TPasModule);
      procedure ParseUnit(M: TPasModule);
      procedure GetUnits(u: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure GetClasses(c: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure PopulateMembers(ths: TClass; mems: TFPList); overload;
      procedure PopulateMembers(intf: TInterface; mems: TFPList); overload;
      procedure PopulateClass(ths: TClass; cls: TPasClassType);
      procedure PopulateInterface(intf: TInterface; cls: TPasClassType);
      procedure AddProcedure(op: TOperation; proc: TPasProcedure);
      procedure AddConstructor(op: TOperation; proc: TPasConstructor);
      procedure AddDestructor(op: TOperation; proc: TPasDestructor);
      procedure AddFunction(op: TOperation; proc: TPasFunction);
      procedure AddProperty(prop: TProperty; pproc: TPasProperty);
    public
      procedure ParseFileWithDefines( AModel: TAbstractPackage; AOM: TObjectModel; const GlobalDefines: TStringList );
    published
      property Filename: string read fFilename write fFilename;

  end;


implementation

uses
  Dialogs;
  { TSimpleEngine }

constructor TSimpleEngine.Create;
begin
  self.InterfaceOnly := True;
  self.NeedComments := False;
 inherited;
end;

function TSimpleEngine.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;


{ TfpcParser }

// TODO EXTEND The model
function TfpcParser.getVisibility(vis: TPasMemberVisibility): TVisibility;
begin
  case vis of
    visPublic, visDefault: Result := viPublic;
    visStrictPrivate, visPrivate: Result := viPrivate;
    visStrictProtected, visProtected: Result := viProtected;
    visPublished: Result := viPublished;
    visAutomated:
  end;
end;

// TODO EXTEND The model
function TfpcParser.getProcType(pt: TProcType): TOperationType;
begin
  case pt of
    ptProcedure,ptClassProcedure: Result := TOperationType.otProcedure;
    ptFunction,ptClassFunction: Result := TOperationType.otFunction;
    ptConstructor,ptClassConstructor: Result := TOperationType.otConstructor;
    ptDestructor, ptClassDestructor: Result := TOperationType.otDestructor;
//    ptOperator, ptClassOperator: ;
  end;
end;



procedure TfpcParser.ParseProject(M: TPasModule);
begin

end;

procedure TfpcParser.ParseUnit(M: TPasModule);
var
  intf: TInterfaceSection;
begin
   FUnit := (FModel as TLogicPackage).AddUnit(M.Name);
   FUnit.Sourcefilename := Filename;
   intf := M.InterfaceSection;
   GetUnits(intf.UsesList);
   GetClasses(intf.Classes);

end;

procedure TfpcParser.GetUnits(u: TFPList; AVisibility: TVisibility;
  Recurse: Boolean);
var
  i: integer;
  prs: TfpcParser;
  str: TStream;
  fullName, uName: string;
begin
  If Assigned(u) and (u.Count > 0) then
     for i := 0 to u.Count- 1 do
     begin
       uName := TPasElement(u.Items[i]).Name;
       if Assigned(NeedPackage) and (FOM.ModelRoot.FindUnitPackage(uName) = nil) then
       begin
         fullName := NeedPackage(uName, str{%H-}, Recurse);
         if Fullname <> '' then
         begin
           prs := TfpcParser.Create;
           prs.FileName := fullName;
           prs.NeedPackage := NeedPackage;
           try
             try
               prs.ParseFileWithDefines(FModel, FOM, FGlobalDefines);
             except
               on E : EParseError do
                 ShowMessage(E.Message);
             end;
           finally
             FreeAndNil(prs);
           end;
         end;
       end;
       if (FOM.ModelRoot.FindUnitPackage(uName) <> nil) and Recurse then
         FUnit.AddUnitDependency(FOM.ModelRoot.FindUnitPackage(uName),AVisibility);

     end;
end;

procedure TfpcParser.GetClasses(c: TFPList; AVisibility: TVisibility;
  Recurse: Boolean);
var
  i: integer;
  cls: TPasClassType;
  ths: TClass;
  intf: TInterface;
begin
  If Assigned(c) and (c.Count > 0) then
     for i := 0 to c.Count- 1 do
     begin
        cls := TPasClassType(c.Items[i]);

        case cls.ObjKind of
          okObject, okClass:
          begin
            ths := FUnit.AddClass(cls.Name);
            PopulateClass(ths, cls);
          end;
          okInterface:
          begin
            intf := FUnit.AddInterface(cls.Name);
            PopulateInterface(intf, cls);
          end;
//  TODO        okGeneric, okSpecialize,
//  or NOT      okClassHelper,okRecordHelper,okTypeHelper

        end;
     end;

end;

procedure TfpcParser.PopulateMembers(ths: TClass; mems: TFPList);
var
  i: integer;
  op: TOperation;
  attr: TAttribute;
  vari: TPasVariable;
  prop: TProperty;
  cons: TPasConstructor;
begin
   for i := 0 to mems.Count-1 do
   begin
      case TPasType(mems.Items[i]).ElementTypeName of
        'procedure':
        begin
           op := ths.AddOperation(TPasType(mems.Items[i]).Name);
           AddProcedure(op,TPasProcedure(mems.Items[i]));
        end;
        'function' :
          begin
            op := ths.AddOperation(TPasType(mems.Items[i]).Name);
            AddFunction(op, TPasFunction(mems.Items[i]));
          end;

        'property'  :
          begin
             prop := ths.AddProperty(TPasType(mems.Items[i]).Name);
             AddProperty(prop, TPasProperty(mems.Items[i]));
          end;
         'variable'  :
           begin
              vari :=  TPasVariable(mems.Items[i]);
              attr := ths.AddAttribute(vari.Name);
              attr.TypeClassifier := getClassifier(vari.VarType.name);
              attr.Visibility := getVisibility(vari.Visibility);
           end;
         'constructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              AddConstructor(op, TPasConstructor(mems.Items[i]));
           end;
         'destructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              AddDestructor(op, TPasDestructor(mems.Items[i]));

           end
         else
           begin
             //op.Create(ths);
           end;

      end;
   end;
end;

procedure TfpcParser.PopulateMembers(intf: TInterface; mems: TFPList);
var
  i: integer;
  op: TOperation;
begin
  for i := 0 to mems.Count-1 do
  begin
     case TPasType(mems.Items[i]).ElementTypeName of
       'procedure':
       begin
          op := intf.AddOperation(TPasType(mems.Items[i]).Name);
          AddProcedure(op,TPasProcedure(mems.Items[i]));
       end;
       'function' :
         begin
            op := intf.AddOperation(TPasType(mems.Items[i]).Name);
            AddFunction(op, TPasFunction(mems.Items[i]));
         end;
       'variable'  :
         begin
            intf.AddAttribute(TPasType(mems.Items[i]).Name);
         end;
     end;
  end;
end;


// this has a nasty hack due to the parser parsing file by file
// even though it does things recursivly, unfortunately there are a lot
// of TStringLists, TObject references which come as parameters to
// Types and FunctionTypeDef e.g of Of TObject. etc etc. The current model
// does not allow inheritance of dataTypes.
// This is something which has to be fixed as Pascal does have type inheritance.
// So Basically if we see a T as the first letter in an indentifier
// Generate a Class in the unknown bucket instead of a DataType style
// Primitive. Otherwise we would have to chase the full heirachy all the way
// down into the fcl.
function TfpcParser.getClassifier(s: String): TClassifier;
begin
  if s = '' then s := 'Unidentified datatype';
  Result := FUnit.FindClassifier(s);
  if not Assigned(Result) then
     Result := FUnit.FindClassifier(s, False, TClass);
  if not Assigned(Result) then
       Result := FOM.UnknownPackage.FindClassifier(s, False, TClass);
  if not Assigned(Result) then
     Result := FOM.UnknownPackage.FindClassifier(s);
  if not Assigned(Result) then
  if s[1] ='T' then   // HACK ALERT
      Result := FOM.UnknownPackage.AddClass(s)
  else
      Result := FOM.UnknownPackage.AddDatatype(s);
end;

procedure TfpcParser.PopulateClass(ths: TClass; cls: TPasClassType);
var
  ans: TPasType;
  ancestor: TClass;
  cfr: TClassifier;
begin
  if Assigned(cls.AncestorType) then
  begin
    ans := TPasType(cls.AncestorType);
    ancestor := FUnit.FindClassifier(ans.Name,False,TClass) as TClass;
    if Not Assigned(ancestor) then
    begin
        cfr := FOM.UnknownPackage.FindClassifier(ans.Name);
        if Assigned(cfr) then
           TClassifier( ancestor) := cfr;
    end;
    If Assigned( ancestor) then
      ths.Ancestor := ancestor
    else
      ths.Ancestor := FOM.UnknownPackage.AddClass(ans.Name);
  end;

  if Assigned(cls.Members) then
     PopulateMembers(ths, cls.Members);

end;

procedure TfpcParser.PopulateInterface(intf: TInterface; cls: TPasClassType);
var
  classif, interf: TInterface;
begin
  if Assigned (cls.AncestorType) then
  begin
    interf := FUnit.FindClassifier(cls.AncestorType.Name,False,TInterface) as TInterface;
    if not Assigned(interf) then
    begin
      classif := FOM.UnknownPackage.FindClassifier(cls.AncestorType.Name,False,TInterface) as TInterface;
      if Assigned(classif) and (classif is TInterface) then
        interf := classif as TInterface;
    end;
    if Assigned(interf) then
      intf.Ancestor := interf
    else
      intf.Ancestor := FOM.UnknownPackage.AddInterface(cls.AncestorType.Name);
    end;
  if Assigned(cls.Members) then
     PopulateMembers(intf, cls.Members);

end;

procedure TfpcParser.AddProcedure(op: TOperation; proc: TPasProcedure);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otProcedure;
  if Assigned(proc.ProcType.Args) and  (proc.ProcType.Args.Count> 0) then
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     if Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
end;

procedure TfpcParser.AddConstructor(op: TOperation; proc: TPasConstructor);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otConstructor;
  if Assigned(proc.ProcType.Args) and  (proc.ProcType.Args.Count> 0) then
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
end;

procedure TfpcParser.AddDestructor(op: TOperation; proc: TPasDestructor);
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otDestructor;
end;

procedure TfpcParser.AddFunction(op: TOperation; proc: TPasFunction);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otFunction;
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     If Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
  op.ReturnValue := getClassifier(TPasFunctionType(proc.ProcType).ResultEl.ResultType.Name);
end;

procedure TfpcParser.AddProperty(prop: TProperty; pproc: TPasProperty);
begin
   prop.Visibility := getVisibility(pproc.Visibility);
   If Assigned (pproc.VarType) then
     prop.TypeClassifier := getClassifier(pproc.VarType.Name);

end;



procedure TfpcParser.ParseFileWithDefines(AModel: TAbstractPackage;
  AOM: TObjectModel; const GlobalDefines: TStringList);
var
  M: TPasModule;
  E: TPasTreeContainer;
begin
  FGlobalDefines := GlobalDefines;
  FModel := AModel;
  FOM := AOM;

  E := TSimpleEngine.Create;
  M := ParseSource(E, self.Filename ,'WINDOWS' ,'i386', True);
  if M is TPasProgram then
     ParseProject (M)
  else
     ParseUnit(M);

  FreeAndNil(M);
  FreeAndNil(E);
end;

end.
