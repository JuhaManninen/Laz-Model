unit uClassTreeEditIntegrator;

{$mode objfpc}{$H+}

interface

uses
  Controls,
  uViewIntegrator, uModel, uFeedback, uClassTreeEditForm;

type

  { TClassTreeEditIntegrator }

  TClassTreeEditIntegrator = class(TViewIntegrator)
  private
    MyForm: TClassTreeEditForm;
  public
    constructor Create(om: TObjectModel; AParent: TWinControl; AFeedback : IEldeanFeedback = nil); override;
    destructor Destroy; override;
    procedure CurrentEntityChanged; override;
  end;

implementation


{ TClassTreeEditIntegrator }

constructor TClassTreeEditIntegrator.Create(om: TObjectModel;
  AParent: TWinControl; AFeedback: IEldeanFeedback);
begin
  inherited Create(om, AParent, AFeedback);
  MyForm := ClassTreeEditForm;
  MyForm.Model := om;
end;

destructor TClassTreeEditIntegrator.Destroy;
begin
  inherited Destroy;
end;

procedure TClassTreeEditIntegrator.CurrentEntityChanged;
begin
  inherited CurrentEntityChanged;
  if  MyForm.Visible then
    MyForm.ModelObject := CurrentEntity;
end;

end.

