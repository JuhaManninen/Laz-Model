unit uClassTreeEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs, typinfo,
  ComCtrls, ExtCtrls, uModel, uModelEntity, uIterators, uViewIntegrator, uListeners ;

type

  { TClassTreeEditForm }

  TClassTreeEditForm = class(TForm)
    Splitter1: TSplitter;
    TIPropertyGrid1: TTIPropertyGrid;
    TreeView1: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    { private declarations }
    FModel: TObjectModel;
    FModelObject: TModelEntity;
    procedure SetObject(AValue: TModelEntity);
    procedure SetModel(AModel: TObjectModel);
    procedure InitTreeFromModel;
    procedure InitTreeFromClass;
    procedure InitTreeFromUnit;
  public
    { public declarations }
    property Model: TObjectModel write SetModel;
    property ModelObject: TModelEntity write SetObject;
  end;

var
  ClassTreeEditForm: TClassTreeEditForm;

implementation

{$R *.lfm}

{ TClassTreeEditForm }


procedure TClassTreeEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  Self.Hide;
  CanClose := False;
end;

procedure TClassTreeEditForm.TreeView1SelectionChanged(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  if (Assigned(TreeView1.Selected.Data)) and (TObject(TreeView1.Selected.Data) is TPersistent) then
    TIPropertyGrid1.TIObject := TPersistent(TreeView1.Selected.Data);
end;

procedure TClassTreeEditForm.SetObject(AValue: TModelEntity);
begin
    Self.FModelObject :=  AValue;
    Self.TIPropertyGrid1.TIObject := AValue;
    InitTreeFromModel;
end;

procedure TClassTreeEditForm.SetModel(AModel: TObjectModel);
begin
  FModel := AModel;
end;

procedure TClassTreeEditForm.InitTreeFromModel;
begin
    TreeView1.Items.Clear;
    if (FModelObject is TClass) then
      InitTreeFromClass;
    if (FModelObject is TUnitPackage) then
      InitTreeFromUnit;

end;

procedure TClassTreeEditForm.InitTreeFromClass;
var
  tp, tc, nod,nod1, nod2,nod3: TTreeNode;
  Pi, Mi: IModelIterator;
  ent: TModelEntity;
  attr: TAttribute;
  op: TOperation;
  par: TParameter;
begin
    with FModelObject as TClass do
    begin
      tp := TreeView1.Items.Add(nil, 'Parent: ' + Owner.Name);
      If Assigned(Ancestor) then
      begin
        tc := TreeView1.Items.AddChildObject(tp, Name + ': ' + Ancestor.Name, Ancestor);
        Mi := TModelIterator.Create(GetImplements, ioAlpha);
        if Mi.Count > 0 then
        begin
          nod := TreeView1.Items.AddChildObject(tc, 'implementors', nil);
          while Mi.HasNext do
          begin
            ent := Mi.Next;
            TreeView1.Items.AddChildObject(nod, ent.Name, ent);
          end;
        end;
      end
      else
        tc := TreeView1.Items.AddChild(tp, Name);

      Mi := TModelIterator.Create(GetAttributes);
      if Mi.Count > 0 then
      begin
        nod := TreeView1.Items.AddChildObject(tc, 'Attributes', nil);
        while Mi.HasNext do
        begin
          ent := Mi.Next;
          case  ent.ClassName of
          'TAttribute','TProperty' :
             begin
               attr := TAttribute(ent);
               nod1 := TreeView1.Items.AddChildObject(nod, attr.Name + ' : ' + attr.ClassName, attr);
               if Assigned(attr.TypeClassifier) then
                 TreeView1.Items.AddChildObject(nod1, attr.TypeClassifier.Name + ' : ' + attr.TypeClassifier.ClassName, attr.TypeClassifier);
             end;
           else
            Assert(True, 'Unhandled Attribute class ' + ent.ClassName);
          end;
        end;
      end;

      Mi := TModelIterator.Create(GetOperations);
      if Mi.Count > 0 then
      begin
        nod := TreeView1.Items.AddChildObject(tc, 'Operations', nil);
        while Mi.HasNext do
        begin
          ent := Mi.Next;
          nod1 := TreeView1.Items.AddChildObject(nod, ent.Name + ' : ' + ent.ClassName, ent);
          op := TOperation(ent);
          TreeView1.Items.AddChild(nod1,' Operation Type: ' + GetEnumName(typeinfo(TOperationType), ord(op.OperationType)));

          Pi := TModelIterator.Create(op.GetParameters);
          if Pi.Count > 0  then
            while Pi.HasNext do
            begin
              par := TParameter(Pi.Next);
              nod2 := TreeView1.Items.AddChildObject(nod1, par.Name + ' : ' + par.ClassName, par);
              if Assigned(par.TypeClassifier) then
                nod3 := TreeView1.Items.AddChildObject(nod2, par.TypeClassifier.Name + ' : ' + par.TypeClassifier.ClassName, par.TypeClassifier);
            end;
          if (op.OperationType = otFunction) then
          begin
            TreeView1.Items.AddChildObject(nod1,'Return: ' + op.ReturnValue.Name + ' : ' + op.ReturnValue.ClassName, op.ReturnValue);
          end;
        end;
      end;


    end;

end;

procedure TClassTreeEditForm.InitTreeFromUnit;
var
  tp, tc, nod,nod1, nod2,nod3: TTreeNode;
  dt: TDataType;
  clsf: TClassifier;
  i: integer;
  Mi,Pi: IModelIterator;
  tel: TEnumLiteral;
begin
    nod := TreeView1.Items.AddChildObject(nil,FModelObject.Name, FModelObject);
    with FModelObject as TUnitPackage do
    begin
      Mi := TModelIterator.Create(GetClassifiers);
      if Mi.Count > 0 then
        while Mi.HasNext do
        begin
          clsf := TClassifier(Mi.Next);
          nod1 := TreeView1.Items.AddChildObject(nod, clsf.Name + ' : ' + clsf.ClassName, clsf);
          case clsf.ClassName of
          'TDataType':
             begin
             end;
          'TClass':
             begin

             end;
          'TInterface':
             begin
             end;
          'TEnumeration':
             begin
               Pi := TModelIterator.Create(clsf.GetFeatures);
               if Pi.Count > 0 then
               while Pi.HasNext do
               begin
                 tel := TEnumLiteral(Pi.Next);
                 nod2 := TreeView1.Items.AddChildObject(nod1, tel.Name + ' : ' + tel.ClassName,tel);
               end;
             end;
          else
            Assert(true,'Unknown Classifier Type: ' + clsf.ClassName);
          end;
        end;

    end;

end;

end.

