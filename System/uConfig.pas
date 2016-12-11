{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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

unit uConfig;

interface
{$ifdef WIN32}
uses Classes, registry;
{$endif}
{$ifdef LINUX}
uses Classes;
{$endif}

type
  //Save changed diagram layout setting
  TDiSaveSetting = (dsAlways,dsAsk,dsNever);

  TConfig = class
  private
{$ifdef WIN32}
    Reg : TRegistry;
{$endif}
{$ifdef LINUX}
{ TODO : Fix for Linux }
{$endif}
    FDiSave : TDiSaveSetting;
    FDiShowAssoc: boolean;
    FDiVisibilityFilter: integer;
    FEditorCommandLine: String;
    function ReadInt(const Key : string; const Default : integer) : integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetResourceStream(const Name : string) : TMemoryStream;
    function GetResourceText(const Name : string) : string;
  public
    IsLimitedColors : boolean;
    IsTerminating : boolean;
    property DiSave : TDiSaveSetting read FDiSave write FDiSave;
    property DiShowAssoc : boolean read FDiShowAssoc write FDiShowAssoc;
    property DiVisibilityFilter : integer read FDiVisibilityFilter write FDiVisibilityFilter;

    property EditorCommandLine: String read FEditorCommandLine write FEditorCommandLine;

    procedure WriteStr(const Key : string; const Value : string);
    function ReadStr(const Key : string; const Default : string) : string;
    procedure StoreSettings;
  end;

var
  Config: TConfig;

implementation

{$ifdef WIN32}
uses Forms,
   Windows,
   uConst,
   SysUtils;
{$endif}
{$ifdef LINUX}
uses QForms,
   uConst,
   SysUtils;
{$endif}



constructor TConfig.Create;
var
  DC : integer;
begin
  IsLimitedColors := False;
  {$IFDEF Win32}
  DC := GetDC(0);
  IsLimitedColors := GetDeviceCaps( DC ,BITSPIXEL) <= 8;
  ReleaseDC(0,DC);
  {$ENDIF}

{$ifdef WIN32}
  Reg := TRegistry.Create;
  Reg.RootKey:=HKEY_CURRENT_USER;
  Reg.OpenKey(uConst.RegKey,True);

  FDiSave := TDiSaveSetting(ReadInt('DiSave',Integer(dsAsk)));
  if FDiSave>High(TDiSaveSetting) then
    FDiSave := High(TDiSaveSetting);

  FDiShowAssoc := ReadInt('DiShowAssoc',0)<>0;
  FDiVisibilityFilter := ReadInt('DiVisibilityFilter',0);
  FEditorCommandLine := ReadStr('EditorCommandLine','');
{$endif}
{$ifdef LINUX}
{ TODO : Fix for Linux }
{$endif}
end;

destructor TConfig.Destroy;
begin
{$ifdef WIN32}
  Reg.Free;
{$endif}
{$ifdef LINUX}
{ TODO : Fix for Linux }
{$endif}
  inherited Destroy;
end;

function TConfig.GetResourceStream(const Name: string): TMemoryStream;
var
  R,Len : integer;
  P : Pointer;
begin
{$ifdef WIN32}
  R := FindResource(HInstance,PChar(Name),RT_RCDATA);
  Assert(R<>0,'GetResource: ' + Name);
  Len := SizeOfResource(HInstance,R);
  R := LoadResource(HInstance,R);
  P := LockResource(R);
  Result := TMemoryStream.Create;
  Result.SetSize(Len);
  Move(P^,Result.Memory^,Len);
  UnlockResource(R);
{$endif}
{$ifdef LINUX}
{ TODO : Fix for Linux }
{$endif}
end;

//Returnerar resurs som en sträng
function TConfig.GetResourceText(const Name: string): string;
var
  Str : TMemoryStream;
begin
  Str := GetResourceStream(Name);
  try
    SetLength(Result,Str.Size);
    Move(Str.Memory^,Result[1],Str.Size);
  finally
    Str.Free;
  end;
end;

function TConfig.ReadInt(const Key: string;
  const Default: integer): integer;
begin
{$ifdef WIN32}
  if Reg.GetDataType(Key)=rdInteger then
    Result := Reg.ReadInteger(Key)
  else
    Result := Default;
{$endif}
{$ifdef LINUX}
{ TODO : Fix for Linux }
    Result := Default;
{$endif}
end;


function TConfig.ReadStr(const Key, Default: string): string;
begin
{$ifdef WIN32}
  if Reg.GetDataType(Key)=rdString then
    Result := Reg.ReadString(Key)
  else
    Result := Default;
{$endif}
end;

procedure TConfig.WriteStr(const Key, Value: string);
begin
{$ifdef WIN32}
  Reg.WriteString(Key,Value)
{$endif}
end;

procedure TConfig.StoreSettings;
begin
  Reg.WriteInteger('DiSave',Integer(FDiSave));
  Reg.WriteBool('DiShowAssoc',FDiShowAssoc);
  Reg.WriteInteger('DiVisibilityFilter',FDiVisibilityFilter);
  Reg.WriteString('EditorCommandLine',FEditorCommandLine);
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.
