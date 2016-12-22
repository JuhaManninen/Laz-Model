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
var
  tp, tc, nod,nod1, nod2,nod3: TTreeNode;
  s: string;
  Pi, Mi: IModelIterator;
  ent, ent1: TModelEntity;
  attr: TAttribute;
  op: TOperation;
  par: TParameter;
begin
    TreeView1.Items.Clear;
    if (FModelObject is TClass) then
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

end;

end.

