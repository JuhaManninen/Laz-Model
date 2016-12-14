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

unit uDelphiIntegrator;

{$mode objfpc}{$H+}

interface
uses Classes, uIntegrator, uModel, uDelphiParser, uCodeProvider,
  uCodeParser;

type

  // Ordinary import of delphi code, until we have two-way integration.
  TDelphiImporter = class(TImportIntegrator)
  private
    // Implementation of parser callback to retrieve a named package
    function NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False):String;
  public
    procedure ImportOneFile(const FileName : string); override;
    class function GetFileExtensions : TStringList; override;
  end;


implementation
uses SysUtils, uError;
{ TDelphiImporter }

procedure TDelphiImporter.ImportOneFile(const FileName : string);
var
  Str: TStream;
  Parser: TDelphiParser;
  GlobalDefines : TStringList;
begin
  Str := CodeProvider.LoadStream(FileName);
  if Assigned(Str) then
  begin
    GlobalDefines := TStringList.Create;

    {$ifdef WIN32}   ////FPCTODO sort this properly for fpc
    GlobalDefines.Add('MSWINDOWS');
    GlobalDefines.Add('WIN32');
    {$endif}
    {$ifdef LINUX}
    GlobalDefines.Add('LINUX');
    {$endif}

    Parser := TDelphiParser.Create;
    try
      Parser.Filename := FileName;
      Parser.NeedPackage := @NeedPackageHandler;
      Parser.ParseStreamWithDefines(Str, Model.ModelRoot, Model, GlobalDefines);
    finally
      Parser.Free;
      GlobalDefines.Free;
    end;
  end;
end;


class function TDelphiImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.pas'] := 'code';
  Result.Values['.lpr'] := 'Lazarus project';
  Result.Values['.lpk'] := 'Lazarus package';
end;

function TDelphiImporter.NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = False):String;
var
  FileName: string;
begin
  AStream := nil;
  if ExtractFileExt(AName) = '' then
    FileName := ExtractFileName(AName) + '.pas'
  else
    FileName := AName;
  FileName := CodeProvider.LocateFile(FileName);
  Result := FileName;
  if (not OnlyLookUp) and (FileName<>'') and (FilesRead.IndexOf(FileName)=-1) then
  begin
    AStream := CodeProvider.LoadStream(FileName);
    FilesRead.Add(FileName);
  end;
end;

initialization

  Integrators.Register(TDelphiImporter);

end.
