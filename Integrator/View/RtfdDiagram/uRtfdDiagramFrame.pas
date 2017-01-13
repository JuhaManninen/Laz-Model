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

unit uRtfdDiagramFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls,
  uDiagramFrame, uRtfdDiagram;

type
  TRtfdDiagramFrame = class(TDiagramFrame)
    DiaBackImage: TImage;
    VisPublicImage: TImage;
    VisPrivateImage: TImage;
    VisProtectedImage: TImage;
    procedure OpenSelectedPackageActionExecute(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent; ADiagram: TRtfdDiagram); reintroduce;
  end;

implementation

{$R *.lfm}


{ TRtfdDiagramFrame }

constructor TRtfdDiagramFrame.Create(AOwner: TComponent; ADiagram: TRtfdDiagram);
begin
  inherited Create(AOwner, ADiagram.Model);
  Self.Diagram := ADiagram;
  Diagram.OnUpdateToolbar := @OnUpdateToolbar;
end;

procedure TRtfdDiagramFrame.OpenSelectedPackageActionExecute(Sender: TObject);
begin
  inherited;
  (Diagram as TRtfdDiagram).OpenSelectedPackage;
end;

end.

