unit uClassTreeEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, uModel, uModelEntity, uIterators, uViewIntegrator, uListeners ;

type

  { TClassTreeEditForm }

  TClassTreeEditForm = class(TForm)
    Splitter1: TSplitter;
    TIPropertyGrid1: TTIPropertyGrid;
    TreeView1: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
  tp, tc, nod: TTreeNode;
  s: string;
  Mi: IModelIterator;
  ent: TModelEntity;
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
      end;
    end;

end;

end.

