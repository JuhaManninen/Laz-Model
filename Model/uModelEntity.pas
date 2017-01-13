{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter S�derman, Ville Krumlinde
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

{
  Definition of TModelEntity is in it's own unit to avoid a circular unit
  reference bwtween uModel and uListeners

  IModelIterator is defined here for the same reason.
}
unit uModelEntity;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, LCLIntf, LCLType,
  uDocumentation;

type
  TListenerMethodType = (mtBeforeChange, mtBeforeAddChild, mtBeforeRemove, mtBeforeEntityChange,
    mtAfterChange, mtAfterAddChild, mtAfterRemove, mtAfterEntityChange);

  TVisibility = (viPrivate, viProtected, viPublic, viPublished);


  {
    TModelEntity is the representation of UML 2.5 NamedElement abstract class.

    This is documented in Section 7.8.9 of the Formal Spec.

    The original code seems to assume that all NamedElements are owned at
    the package level and derive the NameSpace heirarchy from package ownership.

  }
  TModelEntity = class(TInterfacedPersistent)
  private
    function GetRoot: TModelEntity;
  protected
    // To be able to integrate with an editor we need the location of the entity
    // Sourcefilename is stored only where needed, in abstractpackage and classifier
    FSourceX, FSourceY: Integer;
    FName: string;
    FOwner: TModelEntity;
    FDocumentation : TDocumentation;
    FVisibility: TVisibility;
    Listeners: TInterfaceList;
    FLocked: boolean;
    procedure SetName(const Value: string); virtual;
    function GetFullName: string;
    class function GetBeforeListener: TGUID; virtual;
    class function GetAfterListener: TGUID; virtual;
    procedure SetVisibility(const Value: TVisibility);
    function GetLocked: boolean;
    procedure Fire(Method: TListenerMethodType; Info: TModelEntity = nil); virtual;
    function GetSourcefilename: String; virtual;
    procedure SetSourcefilename(const Value: String); virtual;
    function GetDescription: string;
    {IUnknown, beh�vs f�r att kunna vara lyssnare}
    function QueryInterface(const IID: TGUID; out Obj): HResult;
    function _AddRef: Integer;
    function _Release: Integer;
  public
    constructor Create(AOwner: TModelEntity); virtual;
    destructor Destroy; override;
    procedure AddListener(NewListener: IUnknown);
    procedure RemoveListener(Listener: IUnknown);
    property FullName: string read GetFullName;
    property Owner: TModelEntity read FOwner write FOwner;
    property Locked: boolean read GetLocked write FLocked;
    property Root : TModelEntity read GetRoot;
    property Documentation : TDocumentation read FDocumentation;
    property SourceX: Integer read FSourceX write FSourceX;
  published
    property Name: string read FName write SetName;
    property Visibility: TVisibility read FVisibility write SetVisibility;

    // TODO These are read only so are only here while developing Model
    property Sourcefilename: String read GetSourcefilename write SetSourcefilename;
    property SourceY: Integer read FSourceY write FSourceY;
    property Description : string read GetDescription;
  end;

  TModelEntityClass = class of TModelEntity;

  //Sortorder for iterators
  TIteratorOrder = (ioNone,ioVisibility,ioAlpha{,ioType});

  //Basinterface for iterators
  IModelIterator = interface(IUnknown)
    ['{42329900-029F-46AE-96ED-6D4ABBEAFD4F}']
    function HasNext : boolean;
    function Next : TModelEntity;
    procedure Reset;
    function Count : integer;
  end;

  //Basinterface for iteratorfilters
  IIteratorFilter = interface(IUnknown)
    ['{FD77FD42-456C-4B8A-A917-A2555881E164}']
    function Accept(M : TModelEntity) : boolean;
  end;

var
  CurrentSourcefilename: PString;
  CurrentSourceX: PInteger;
  CurrentSourceY: PInteger;

implementation

uses uListeners;


{ TModelEntity }

constructor TModelEntity.Create(AOwner: TModelEntity);
begin
  inherited Create;
  Self.Owner := AOwner;
  Listeners := TInterfaceList.Create;
  FDocumentation := TDocumentation.Create;
  if Assigned(CurrentSourceX) then SourceX := CurrentSourceX^;
  if Assigned(CurrentSourceY) then SourceY := CurrentSourceY^;
end;

destructor TModelEntity.Destroy;
begin
  FreeAndNil(FDocumentation);
  FreeAndNil(Listeners);
  inherited;
end;

function TModelEntity.GetFullName: string;
begin
  if Assigned(FOwner) then
    Result := FOwner.FullName + '::' + FName
  else
    Result := FName;
end;

function TModelEntity.GetLocked: boolean;
begin
  Result := FLocked or (Assigned(Owner) and Owner.Locked);
end;

procedure TModelEntity.AddListener(NewListener: IUnknown);
begin
  if self.Listeners.IndexOf(NewListener) = -1 then
    self.Listeners.Add(NewListener);
end;

procedure TModelEntity.RemoveListener(Listener: IUnknown);
begin
  self.Listeners.Remove(Listener);
end;


procedure TModelEntity.SetName(const Value: string);
var
  OldName: string;
begin
  OldName := FName;
  FName := Value;
  try
    Fire(mtBeforeEntityChange)
  except
    FName := OldName;
    raise;
  end {try};
  Fire(mtAfterEntityChange)
end;

procedure TModelEntity.SetVisibility(const Value: TVisibility);
var
  Old: TVisibility;
begin
  Old := Value;
  FVisibility := Value;
  try
    Fire(mtBeforeEntityChange)
  except
    FVisibility := Old;
    raise;
  end {try};
  Fire(mtAfterEntityChange)
end;

procedure TModelEntity.Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
var
  I: integer;
  IL: IModelEntityListener;
  L: IUnknown;
begin
  if not Locked then
    for I := 0 to Listeners.Count - 1 do
    begin
      L := Listeners[I];
      case Method of
        mtBeforeAddChild:
          if Supports(L, GetBeforeListener, IL) then
            IL.AddChild(Self, Info);
        mtBeforeRemove:
          if Supports(L, GetBeforeListener, IL) then
            IL.Remove(Self);
        mtBeforeChange:
          if Supports(L, GetBeforeListener, IL) then
            IL.Change(Self);
        mtBeforeEntityChange:
          if Supports(L, GetBeforeListener, IL) then
            IL.EntityChange(Self);
        mtAfterAddChild:
          if Supports(L, GetAfterListener, IL) then
            IL.AddChild(Self, Info);
        mtAfterRemove:
          if Supports(L, GetAfterListener, IL) then
            IL.Remove(Self);
        mtAfterChange:
          if Supports(L, GetAfterListener, IL) then
            IL.Change(Self);
        mtAfterEntityChange:
          if Supports(L, GetAfterListener, IL) then
            IL.EntityChange(Self);
      else
        raise Exception.Create(ClassName + ' Eventmethod not recognized.');
      end {case};
    end;
end;

function TModelEntity.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK
  else Result := E_NOINTERFACE
end;

function TModelEntity._AddRef: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TModelEntity._Release: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TModelEntity.GetRoot: TModelEntity;
begin
  Result := Self;
  while Result.Owner<>nil do
    Result := Result.Owner;
end;

class function TModelEntity.{%H-}GetAfterListener: TGUID;
begin
  raise Exception.Create( ClassName + '.GetAfterListener');
end;

class function TModelEntity.{%H-}GetBeforeListener: TGUID;
begin
  raise Exception.Create( ClassName + '.GetBeforeListener');
end;

function TModelEntity.GetSourcefilename: String;
begin
 Result := '';
 if Owner <> nil then Result := Owner.Sourcefilename;
end;

procedure TModelEntity.SetSourcefilename(const Value: String);
begin
 if Owner <> nil then Owner.Sourcefilename := Value;
end;

function TModelEntity.GetDescription: string;
begin
  Result := FDocumentation.Description;
end;

initialization
  CurrentSourcefilename := nil;
  CurrentSourceX := nil;
  CurrentSourceY := nil;
end.
