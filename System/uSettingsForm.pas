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

unit uSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;


type
  TSettingsForm = class(TForm)
    OkButton: TButton;
    DiSaveCombo: TComboBox;
    Label1: TLabel;
    Button2: TButton;
    ShowAssocCheck: TCheckBox;
    VisibilityCombo: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    eEditorCommandLine: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure DelphiIDECheckClick(Sender: TObject);
    procedure ShellCheckClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    ShellChanged,IDEChanged : boolean;
    procedure ReadSettings;
    procedure SaveSettings;
  public

  end;

implementation

uses uIntegrator, uConfig;

{$R *.lfm}


procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  ReadSettings;
  IDEChanged := False;
  ShellChanged := False;
end;

procedure TSettingsForm.DelphiIDECheckClick(Sender: TObject);
begin
  IDEChanged := True;
end;


procedure TSettingsForm.ShellCheckClick(Sender: TObject);
begin
  ShellChanged := True;
end;

procedure TSettingsForm.OkButtonClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TSettingsForm.ReadSettings;
begin
  DiSaveCombo.ItemIndex := integer(Config.DiSave);
  ShowAssocCheck.Checked := Config.DiShowAssoc;
  VisibilityCombo.ItemIndex := Config.DiVisibilityFilter;
  eEditorCommandLine.Text := Config.EditorCommandLine;
end;

procedure TSettingsForm.SaveSettings;
begin
  Config.DiSave := TDiSaveSetting(DiSaveCombo.ItemIndex);
  Config.DiShowAssoc := ShowAssocCheck.Checked;
  Config.DiVisibilityFilter := VisibilityCombo.ItemIndex;
  Config.EditorCommandLine :=  eEditorCommandLine.Text;
  Config.StoreSettings;
end;

end.
