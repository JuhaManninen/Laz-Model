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


// Exporter for the "ESS-Model XML" format
unit uEmxExport;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uIntegrator, uModelEntity, uModel, uFeedback;

type
  TEmxExporter = class(TExportIntegrator)
  private
    Ids : TStringList;
    Output : TMemoryStream;
    NextId : integer;
    Feedback : IEldeanFeedback;
    procedure WritePackage(P : TAbstractPackage);
    procedure WriteLogicPackage(L : TLogicPackage);
    procedure WriteUnitPackage(U : TUnitPackage);
    procedure WriteClass(C : TClass);
    procedure WriteInterface(I : TInterface);
    procedure WriteEntityHeader(E : TModelEntity; const AName : string);
    procedure WriteFeatures(C : TClassifier);
    procedure WriteDataType(T : TDataType);
    function MakeTypeRef(C : TClassifier) : string;
    function MakeId(const S : string) : string;
    function XmlClose(const S : string) : string;
    function XmlOpen(const S : string) : string;
    function Xml(const S : string) : string;
    procedure Write(const S : string);
  public
    constructor Create(om: TObjectModel; AFeedback : IEldeanFeedback = nil); reintroduce;
    destructor Destroy; override;
    procedure InitFromModel; override;
    procedure ShowSaveDialog;
    procedure SaveTo(const FileName : string);
    function GetText : string;
  end;

implementation

{ TEmxExporter }

constructor TEmxExporter.Create(om: TObjectModel; AFeedback : IEldeanFeedback = nil);
begin
  inherited Create(om);
  Output := TMemoryStream.Create;
  Ids := TStringList.Create;
  Ids.Sorted := True;
  Ids.Duplicates := dupIgnore;
  NextId := 0;
  Self.Feedback := AFeedback;
  if Feedback=nil then
    Self.Feedback := NilFeedback;
end;

destructor TEmxExporter.Destroy;
begin
  FreeAndNil(Output);
  FreeAndNil(Ids);
  inherited;
end;

procedure TEmxExporter.InitFromModel;
begin
  //Write(XmiHeader);
  //Write( XmlOpen('Model_Management.Model') );
  WritePackage(Model.ModelRoot);
  //Write( XmlClose('Model_Management.Model') );
  //Write(XmiFooter);
  Feedback.Message('ESS-Model xmi export finished.');
end;

function TEmxExporter.MakeId(const S : string): string;
var
  I : integer;
begin
  I := Ids.IndexOf(S);
  if I=-1 then
  begin
    Inc(NextId);
    I := Ids.Add(S);
  end;
  Result := 'emx_' + IntToStr(I);
end;


procedure TEmxExporter.ShowSaveDialog;
var
  D : TSaveDialog;
  Dir : string;
begin
  D := TSaveDialog.Create(nil);
  try
    Dir := ExtractFilePath( Model.ModelRoot.GetConfigFile );
    D.DefaultExt := 'emx';
    D.InitialDir := Dir;
    D.Filter := 'ESS-Model xml files (*.emx)|*.emx|All files (*.*)|*.*';
    D.Options := D.Options + [ofOverwritePrompt];
    if D.Execute then
      SaveTo( D.FileName );
  finally
    D.Free;
  end;
end;

procedure TEmxExporter.Write(const S: string);
begin
  Output.Write(S[1],Length(S));
  Output.Write(#13#10,2);
end;


procedure TEmxExporter.WriteClass(C: TClass);
var
  Mi : IModelIterator;
begin
  WriteEntityHeader(C, 'Class');

  WriteFeatures(C);

  if Assigned(C.Ancestor) then
  begin
    Write( XmlOpen( 'Class.Ancestor') );
//    MakeGeneral(C,C.Ancestor);
    Write( XmlClose( 'Class.Ancestor') );
  end;

  //Implements
  Mi := C.GetImplements;
  if Mi.HasNext then
  begin
    Write( XmlOpen( 'Class.Implements') );
//    while Mi.HasNext do
//      MakeAbstract(C, Mi.Next as TClassifier);
    Write( XmlClose( 'Class.Implements') );
  end;

  Mi := C.GetDescendants;
  if Mi.HasNext then
  begin
    Write( XmlOpen( 'Class.Descendant') );
//    while Mi.HasNext do
//      MakeGeneral( Mi.Next as TClassifier, C);
    Write( XmlClose( 'Class.Descendant') );
  end;

  Write( XmlClose('Class') );
end;



procedure TEmxExporter.WriteFeatures(C: TClassifier);
var
  Mi : IModelIterator;
  F : TModelEntity;

  procedure WriteAttribute(A : TAttribute);
  begin
    WriteEntityHeader(A, 'Attribute');
    if Assigned(A.TypeClassifier) then
    begin
      Write( XmlOpen('TypeClassifier') );
        Write( MakeTypeRef(A.TypeClassifier) );
      Write( XmlClose('TypeClassifier') );
    end;
    Write( XmlClose('Attribute') );
  end;

  procedure WriteOperation(O : TOperation);
  var
    Mio : IModelIterator;
    P : TParameter;
  begin
    WriteEntityHeader(O, 'Operation');
      Write( XmlOpen('Parameters') );
      if Assigned(O.ReturnValue) then
      begin
        Write( XmlOpen('ReturnParameter') );
        Write( '<' + 'Parameter.kind value="return"/>');
          Write( MakeTypeRef( O.ReturnValue ) );
        Write( XmlClose('ReturnParameter') );
      end;
      Mio := O.GetParameters;
      while Mio.HasNext do
      begin
        P := Mio.Next as TParameter;
        WriteEntityHeader(P, 'Parameter');
        if Assigned(P.TypeClassifier) then
        begin
          Write( XmlOpen('Parameter.type') );
            Write( MakeTypeRef(P.TypeClassifier) );
          Write( XmlClose('Parameter.type') );
        end;
        Write( XmlClose('Parameter') );
      end;
      Write( XmlClose('Parameters') );
    Write( XmlClose('Operation') );
  end;

begin
  Mi := C.GetFeatures;
  if Mi.HasNext then
  begin
    Write( XmlOpen('Feature') );
    while Mi.HasNext do
    begin
      F := Mi.Next;
      if F is TAttribute then
        WriteAttribute(F as TAttribute)
      else if F is TOperation then
        WriteOperation(F as TOperation);
    end;
    Write( XmlClose('Feature') );
  end;
end;


procedure TEmxExporter.WriteEntityHeader(E: TModelEntity; const AName: string);
const
  VisibilityMap: array[TVisibility] of string = ('private', 'protected', 'public', 'public');
  //(viPrivate,viProtected,viPublic,viPublished);
begin
{
  <UnitPackage id="emx_17" name="LogWriter" visibility="private">
}
  Write( '<' + AName + ' id="' + MakeId(E.FullName) + '" name="' + Xml(E.Name) +
    '" visibility="' + VisibilityMap[E.Visibility] + '">' );
end;



procedure TEmxExporter.WritePackage(P: TAbstractPackage);
begin
  Feedback.Message('EMX generating package ' + P.Name + '...');
  //WriteEntityHeader(P,'Package');
    if P is TLogicPackage then
      WriteLogicPackage(P as TLogicPackage)
    else if P is TUnitPackage then
      WriteUnitPackage(P as TUnitPackage);
  //Write( XmlClose('Package') );
end;


procedure TEmxExporter.WriteLogicPackage(L: TLogicPackage);
var
  Mi : IModelIterator;
begin
  WriteEntityHeader(L,'LogicPackage');
  Mi := L.GetPackages;
  while Mi.HasNext do
    WritePackage( Mi.Next as TAbstractPackage );
  Write( XmlClose('LogicPackage') );
end;


procedure TEmxExporter.WriteUnitPackage(U: TUnitPackage);
var
  Mi : IModelIterator;
  C : TModelEntity;
begin
  WriteEntityHeader(U,'UnitPackage');
  Mi := U.GetClassifiers;
  while Mi.HasNext do
  begin
    C := Mi.Next;
    if C is TClass then
      WriteClass(C as TClass)
    else if C is TInterface then
      WriteInterface(C as TInterface)
    else if C is TDataType then
      WriteDataType(C as TDataType);
  end;
  Write( XmlClose('UnitPackage') );
end;

function TEmxExporter.XmlClose(const S: string): string;
begin
  Result := '</' + S + '>';
end;

function TEmxExporter.XmlOpen(const S: string): string;
begin
  Result := '<' + S + '>';
end;


//Writes a reference to a classifier
function TEmxExporter.MakeTypeRef(C: TClassifier) : string;
var
  S : string;
begin
  if C is TClass then
    S := 'Class'
  else if C is TDataType then
    S := 'DataType'
  else if C is TInterface then
    S := 'Interface';
  Result := '<' + S +' idref="' + MakeId(C.FullName) + '"/>';
end;


//Check that string does not contain xml-chars like < and >
function TEmxExporter.Xml(const S: string): string;
var
  I : integer;
begin
  Result := S;
  for I:=1 to Length(Result) do
    case Result[I] of
      '<' : Result[I]:='(';
      '>' : Result[I]:=')';
    end;
end;

procedure TEmxExporter.WriteInterface(I: TInterface);
{
          <Foundation.Core.ModelElement.supplierDependency>
            <Foundation.Core.Abstraction xmi.idref="xmi.37"/>
          </Foundation.Core.ModelElement.supplierDependency>
}
var
  Mi : IModelIterator;
begin
  WriteEntityHeader(I, 'Interface');
  WriteFeatures(I);

  //Implementing classes
  Mi := I.GetImplementingClasses;
  if Mi.HasNext then
  begin
    Write( XmlOpen( 'Interface.ImplementingClass') );
//    while Mi.HasNext do
//      MakeAbstract(Mi.Next as TClassifier,I);
    Write( XmlClose( 'Interface.ImplementingClass') );
  end;

  Write( XmlClose('Interface') );
end;

procedure TEmxExporter.WriteDataType(T: TDataType);
begin
  WriteEntityHeader(T, 'DataType');
  Write( XmlClose('DataType') );
end;


procedure TEmxExporter.SaveTo(const FileName: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create( FileName ,fmCreate);
  try
    F.CopyFrom(Output, 0);
  finally
    F.Free;
  end;
end;

//Returns the whole emx-file as a string.
function TEmxExporter.GetText: string;
begin
  SetLength(Result,Output.Size);
  Move(Output.Memory^,Result[1],Output.Size);
end;


end.
