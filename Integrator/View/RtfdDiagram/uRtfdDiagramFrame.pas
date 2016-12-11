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

unit uRtfdDiagramFrame;

interface

{$ifdef LINUX}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  uDiagramFrame, QActnList, uRtfdDiagram, QMenus, QStdCtrls, QExtCtrls, uListeners,
  QButtons, QTypes, Graphics, ExtCtrls, Menus, ActnList, Controls,
  StdCtrls, Buttons, Forms;
{$endif}
{$ifdef WIN32}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uDiagramFrame, ActnList, uRtfdDiagram, Menus, StdCtrls, ExtCtrls, uListeners,
  Buttons;
{$endif}

type
  TRtfdDiagramFrame = class(TDiagramFrame)
    DiaBackImage: TImage;
    VisPublicImage: TImage;
    VisPrivateImage: TImage;
    VisProtectedImage: TImage;
    procedure OpenSelectedPackageActionExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent; Diagram: TRtfdDiagram); reintroduce;
  end;

implementation

uses uError;

{$ifdef WIN32}
{$R *.DFM}
{$endif}
{$ifdef LINUX}
{$R *.xfm}
{$endif}


{ TRtfdDiagramFrame }

constructor TRtfdDiagramFrame.Create(Owner: TComponent; Diagram: TRtfdDiagram);
begin
  inherited Create(Owner,Diagram.Model);
  Self.Diagram := Diagram;
  Diagram.OnUpdateToolbar := OnUpdateToolbar;
end;

procedure TRtfdDiagramFrame.OpenSelectedPackageActionExecute(Sender: TObject);
begin
  inherited;
  (Diagram as TRtfdDiagram).OpenSelectedPackage;
end;

end.

