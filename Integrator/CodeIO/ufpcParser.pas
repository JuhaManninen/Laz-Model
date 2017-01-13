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
  Classes, SysUtils,
  Dialogs,
  uCodeParser, uModel, uModelEntity, PParser, PasTree;


type

  { TSimpleEngine }

  TSimpleEngine = class(TPasTreeContainer)
  private
    fIsProgram: boolean;
  public
    constructor Create;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
    function FindModule(const AName: String): TPasModule; override;
    property IsProgram: boolean read fIsProgram write fIsProgram;
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
      function getInterfaceRef(s: string): TInterface;
      function GetDefaultValue(exp: TPasExpr):string;

      procedure FillEnum(tp: TPasEnumType; te: TEnumeration);

      procedure AddOpDirectives(op: TOperation; Mods:TProcedureModifiers);
      procedure ParseProject(M: TPasProgram);
      procedure ParseUnit(M: TPasModule);
      procedure GetUnits(u: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure GetClasses(c: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure GetTypes(c: TFPList);
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

function DelQuot(s:String):String;
   var i:integer;
   const s1=#39;
  begin
   Result:='';
   i:=pos(s1,s);
   while i > 0 do
    begin
     if i > 0 then delete(s,i,1);
     i:=pos(s1,s);
    end;
   //if i > 0 then delete(s,i,2);
   Result:=s;
  end;

  { TSimpleEngine }

constructor TSimpleEngine.Create;
begin
  self.InterfaceOnly := True;
  self.NeedComments := True;
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
  Result.DocComment := CurrentParser.SavedComments;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;

function TSimpleEngine.FindModule(const AName: String): TPasModule;
begin
  if fIsProgram then
     Result := TPasModule.Create(AName, nil)
  else
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



procedure TfpcParser.ParseProject(M: TPasProgram);
begin
  FUnit := (FModel as TLogicPackage).AddUnit(M.Name);
  FUnit.Sourcefilename := Filename;
  GetUnits(M.ProgramSection.UsesList);

end;

procedure TfpcParser.ParseUnit(M: TPasModule);
var
  intf: TInterfaceSection;
begin
   FUnit := (FModel as TLogicPackage).AddUnit(M.Name);
   FUnit.Sourcefilename := Self.Filename;
   FUnit.Documentation.Description := M.DocComment;
   FUnit.Visibility := viPublic;
   intf := M.InterfaceSection;
   GetUnits(intf.UsesList);
   GetTypes(intf.Types);
   GetClasses(intf.Classes);

end;

procedure TfpcParser.GetUnits(u: TFPList; AVisibility: TVisibility;
  Recurse: Boolean);
var
  i: integer;
  prs: TfpcParser;
  str: TStream;
  fullName, uName: string;
  ref: TPasElement;
begin
  If Assigned(u) and (u.Count > 0) then
     for i := 0 to u.Count- 1 do
     begin
       ref := TPasElement(u.Items[i]);
       if (ref is TPasModule) and (TPasModule(ref).Filename <> '') then
          uname := DelQuot(TPasModule(ref).FileName)
        else
          uName := ref.Name;
       if Assigned(NeedPackage) and (FOM.ModelRoot.FindUnitPackage(uName) = nil) then
       begin
         fullName := NeedPackage(uName, str{%H-}, Recurse);
         if Fullname <> '' then
         begin
           try
             prs := TfpcParser.Create;
             prs.FileName := fullName;
             prs.NeedPackage := NeedPackage;
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
            ths.SourceY := cls.SourceLinenumber;
            ths.Documentation.Description := cls.DocComment;
            ths.Visibility := viPublic;
            PopulateClass(ths, cls);
          end;
          okInterface:
          begin
            intf := FUnit.AddInterface(cls.Name);
            intf.SourceY := cls.SourceLinenumber;
            intf.Documentation.Description := cls.DocComment;
            intf.Visibility := viPublic;
            PopulateInterface(intf, cls);
          end;
//  TODO        okGeneric, okSpecialize,
//  or NOT      okClassHelper,okRecordHelper,okTypeHelper
        end;
     end;

end;


// NOTE most dt instancea may be replaced by
// other classes as model evolves. Do not factor
// common code yet.
procedure TfpcParser.GetTypes(c: TFPList);
var
  i: integer;
  tp: TPasType;
  dt: TDataType;
  te: TEnumeration;
begin
  if Assigned(c) and (c.Count > 0) then
    for i := 0 to c.Count-1 do
    begin
      tp := TPasType(c.Items[i]);
      case tp.ClassName of
        'TPasProcedureType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;
          end;
        'TPasFunctionType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;

          end;
        'TPasEnumType':
          begin
            te := TEnumeration (FUnit.AddEnumeration(tp.Name));
            te.SourceY:= tp.SourceLinenumber;
            te.Documentation.Description := tp.DocComment;
            te.Visibility := viPublic;
            FillEnum(TPasEnumType(tp), te);
          end;
        'TPasAliasType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;

          end;
        'TPasArrayType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;

          end;
        'TPasSetType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;
          end;
        'TPasRecordType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;
          end;
        'TPasPointerType':
          begin
            dt := FUnit.AddDatatype(tp.Name);
            dt.SourceY := tp.SourceLinenumber;
            dt.Documentation.Description := tp.DocComment;
            dt.Visibility := viPublic;
          end;

      else
        {$IFDEF DEBUG}
          Assert(True, 'Unhandled Data Type: ' + tp.ClassName);
        {$ENDIF DEBUG}
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
begin
   for i := 0 to mems.Count-1 do
   begin
      case TPasType(mems.Items[i]).ElementTypeName of
        'class procedure':
          begin
             op := ths.AddOperation(TPasType(mems.Items[i]).Name);
             op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
             AddProcedure(op,TPasProcedure(mems.Items[i]));
             if (pmStatic in TPasClassProcedure(mems.Items[i]).Modifiers) then
               op.IsStatic := cotStaticClass
             else
               op.IsStatic := cotClass;
          end;
        'procedure':
        begin
           op := ths.AddOperation(TPasType(mems.Items[i]).Name);
           op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
           AddProcedure(op,TPasProcedure(mems.Items[i]));
        end;
        'class function' :
          begin
            op := ths.AddOperation(TPasType(mems.Items[i]).Name);
            op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
            AddFunction(op, TPasFunction(mems.Items[i]));
             if (pmStatic in TPasClassFunction(mems.Items[i]).Modifiers) then
               op.IsStatic := cotStaticClass
             else
               op.IsStatic := cotClass;
          end;
        'function' :
          begin
            op := ths.AddOperation(TPasType(mems.Items[i]).Name);
            op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
            AddFunction(op, TPasFunction(mems.Items[i]));
          end;

        'property'  :
          begin
             prop := ths.AddProperty(TPasType(mems.Items[i]).Name);
             prop.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
             AddProperty(prop, TPasProperty(mems.Items[i]));
          end;
         'variable'  :
           begin
              vari :=  TPasVariable(mems.Items[i]);
              attr := ths.AddAttribute(vari.Name);
              attr.TypeClassifier := getClassifier(vari.VarType.name);
              attr.Visibility := getVisibility(vari.Visibility);
              attr.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              if (vmClass in vari.VarModifiers) then
                attr.IsStatic := True;
           end;
         'constructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddConstructor(op, TPasConstructor(mems.Items[i]));
           end;
         'class constructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddConstructor(op, TPasConstructor(mems.Items[i]));
              op.IsStatic := cotClass;
           end;

         'destructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddDestructor(op, TPasDestructor(mems.Items[i]));
           end;
         'class destructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddDestructor(op, TPasDestructor(mems.Items[i]));
              op.IsStatic := cotClass;
           end
         else
           begin
             {$IFDEF DEBUG}
               Assert(True, 'Missing '  + TPasType(mems.Items[i]).ElementTypeName + ' in class members');
             {$ENDIF}
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
          op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
          AddProcedure(op,TPasProcedure(mems.Items[i]));
       end;
       'function' :
         begin
            op := intf.AddOperation(TPasType(mems.Items[i]).Name);
            op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
            AddFunction(op, TPasFunction(mems.Items[i]));
         end
       else
         begin
            {$IFDEF DEBUG}
              Assert(True, 'Missing '  + TPasType(mems.Items[i]).ElementTypeName + ' in interface members');
            {$ENDIF}
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
  begin
    if s[1] ='T' then   // HACK ALERT
        Result := FOM.UnknownPackage.AddClass(s)
    else
        Result := FOM.UnknownPackage.AddDatatype(s);
    // As we have had to create this in the unknown bucket
    Result.IsPlaceholder := True;
  end;
end;

function TfpcParser.getInterfaceRef(s: string): TInterface;
begin
   Result := FUnit.FindClassifier(s,False,TInterface) as TInterface;
   if not Assigned(Result) then
   begin
     Result := FOM.UnknownPackage.FindClassifier(s,False,TInterface) as TInterface;
     if not (Assigned(Result) and (Result is TInterface)) then
     begin
        Result := FOM.UnknownPackage.AddInterface(s);
        Result.IsPlaceholder := True;
     end;
   end;
end;

function TfpcParser.GetDefaultValue(exp: TPasExpr): string;
var
  tpeBoolConst: TBoolConstExpr;
begin
  case exp.Kind of
    pekIdent:;
    pekNumber:;
    pekString:;
    pekSet:;
    pekNil:;
    pekBoolConst:
      begin
        tpeBoolConst := TBoolConstExpr(exp);
        Result := BoolToStr(tpeBoolConst.Value, true);
      end;
    pekRange:;
    pekUnary:;
    pekBinary:;
    pekFuncParams:;
    pekArrayParams:;
    pekListOfExp:;
    pekInherited:;
    pekSelf:;
  end;
end;

procedure TfpcParser.FillEnum(tp: TPasEnumType; te: TEnumeration);
var
  i: integer;
  ev: TpasEnumValue;
  tel: TEnumLiteral;
begin
 for i := 0 to tp.Values.Count-1 do
 begin
   ev := TPasEnumValue(tp.Values[i]);
   tel := te.AddLiteral(ev.Name);
   tel.SourceY:= ev.SourceLinenumber;
 // TODO  tel.OrdVal:=ev.Value???;
 end;

end;


procedure TfpcParser.PopulateClass(ths: TClass; cls: TPasClassType);
var
  ans: TPasType;
  ancestor: TClass;
  cfr: TClassifier;
  intf: TInterface;
  intfs: TFPList;
  i: integer;
begin
  if Assigned(cls.AncestorType) then
  begin
    ans := TPasType(cls.AncestorType);
    ancestor := FUnit.FindClassifier(ans.Name,False,TClass) as TClass;
    if Not Assigned(ancestor) then
    begin
        cfr := FOM.UnknownPackage.FindClassifier(ans.Name);
        if Assigned(cfr) then
        begin
           cfr.IsPlaceholder := True;
           TClassifier( ancestor) := cfr;
        end;
    end;
    If Assigned( ancestor) then
      ths.Ancestor := ancestor
    else
      begin
      ths.Ancestor := FOM.UnknownPackage.AddClass(ans.Name);
      ths.Ancestor.IsPlaceholder := True ;
      end;
  end;

  If Assigned(cls.Interfaces) then
  begin
    intfs:= cls.Interfaces;
    for i := 0 to intfs.Count -1 do
    begin
      intf := getInterfaceRef(TPasType(intfs[i]).Name);
      ths.AddImplements(intf);
    end;
  end;

  if Assigned(cls.Members) then
     PopulateMembers(ths, cls.Members);

end;

procedure TfpcParser.PopulateInterface(intf: TInterface; cls: TPasClassType);
begin
  if Assigned (cls.AncestorType) then
     intf.Ancestor := getInterfaceRef(cls.AncestorType.Name);

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
     par.SourceY := arg.SourceLinenumber;
     if Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
     if arg.Access = argConst then
       par.IsConst:= True;
     if Assigned(arg.ValueExpr) then
       par.DefaultValue:=GetDefaultValue(arg.ValueExpr);
  end;
  AddOpDirectives(op,proc.Modifiers);

end;

procedure TfpcParser.AddOpDirectives(op: TOperation; Mods: TProcedureModifiers);
begin
  if (pmAbstract in Mods) then
    op.IsAbstract:= true;

  if (pmVirtual in Mods) then
    op.MethodDirective:=mdVirtual;
  if (pmDynamic in Mods) then
    op.MethodDirective:=mdDynamic;
  if (pmReintroduce in Mods) then
    op.MethodDirective:=mdReintroduce;
  if (pmOverride in Mods) then
    op.MethodDirective:=mdOverride;
  if (pmMessage in Mods) then
    op.MethodDirective:=mdMessage;

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
     par.SourceY := arg.SourceLinenumber;
     if Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
  AddOpDirectives(op,proc.Modifiers);
end;

procedure TfpcParser.AddDestructor(op: TOperation; proc: TPasDestructor);
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otDestructor;
  AddOpDirectives(op,proc.Modifiers);
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
     par.SourceY := arg.SourceLinenumber;
     case arg.Access of
       argConst:;
       argConstRef:;
       argDefault:;  // do nothing default already
       argOut:par.Direction:=dkOut;
       argVar:par.Direction:=dkInOut;
     end;
     If Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
  AddOpDirectives(op,proc.Modifiers);
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
  s: string;
  pp: TPasProgram;
begin
  FGlobalDefines := GlobalDefines;
  FModel := AModel;
  FOM := AOM;

  E := TSimpleEngine.Create;
  s:=  ExtractFileExt(fFilename);
  if ( s = '.lpr') then
  begin
     E.InterfaceOnly := false;
     TSImpleEngine(E).IsProgram := True;
     pp:= ParseSource(E, self.Filename ,'WINDOWS' ,'i386', True) as TPasProgram;
     ParseProject (pp);
     FreeAndNil(E);
  end
  else
  begin
     M := ParseSource(E, self.Filename ,'WINDOWS' ,'i386', True);
     ParseUnit(M);
     FreeAndNil(M);
  end;
  E.Free;
end;

end.

