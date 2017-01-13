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

  For more info about ESS-Model please visit:
    www.essmodel.com
    www.sourceforge.net/projects/essmodel
}

program EssModel;

uses
  Forms, Interfaces, sysutils,
  uMainForm in 'System\uMainForm.pas' {MainForm},
  uMainModule in 'System\uMainModule.pas' {MainModule: TDataModule},
  uListeners in 'Model\uListeners.pas',
  uIntegrator in 'Integrator\uIntegrator.pas',
  uModel in 'Model\uModel.pas',
  uCodeProvider in 'CodeProvider\uCodeProvider.pas',
  uFileProvider in 'CodeProvider\uFileProvider.pas',
  uCodeParser in 'Integrator\CodeIO\uCodeParser.pas',
  uCodeIntegrator in 'Integrator\Code\uCodeIntegrator.pas',
  uViewIntegrator in 'Integrator\View\uViewIntegrator.pas',
  uRtfdComponents in 'Integrator\View\RtfdDiagram\uRtfdComponents.pas',
  uRtfdDiagram in 'Integrator\View\RtfdDiagram\uRtfdDiagram.pas',
  uError in 'System\uError.pas',
  uUseful in 'System\uUseful.pas',
  uModelEntity in 'Model\uModelEntity.pas',
  uParseTree in 'Integrator\CodeIO\uParseTree.pas',
  uConfig in 'System\uConfig.pas',
  uDiagramFrame in 'Integrator\View\uDiagramFrame.pas' {DiagramFrame: TFrame},
  uRtfdDiagramFrame in 'Integrator\View\RtfdDiagram\uRtfdDiagramFrame.pas' {RtfdDiagramFrame: TFrame},
  uRtfdLabel in 'Integrator\View\RtfdDiagram\uRtfdLabel.pas',
  uIterators in 'Model\uIterators.pas',
  uDocGen in 'Integrator\Doc\uDocGen.pas',
  uDocumentation in 'Model\uDocumentation.pas',
  uJavaClassImport in 'Integrator\CodeIO\JavaClass\uJavaClassImport.pas',
  uJavaClass in 'Integrator\CodeIO\JavaClass\uJavaClass.pas',
  essConnectPanel in 'Components\essConnectPanel.pas',
  uXmiExport in 'Integrator\Export\uXmiExport.pas',
  essLayout in 'Components\essLayout.pas',
  uHtmlDocGen in 'Integrator\Doc\uHtmlDocGen.pas',
  uJavaParser in 'Integrator\CodeIO\uJavaParser.pas',
  ufpcParser in 'Integrator\CodeIO\ufpcParser.pas',
  SugiyamaLayout in 'Components\SugiyamaLayout.pas',
  uConst in 'System\uConst.pas',
  uAboutForm in 'System\uAboutForm.pas' {AboutForm},
  uSettingsForm in 'System\uSettingsForm.pas' {SettingsForm},
  uFeedback in 'System\uFeedback.pas',
  uTreeViewFrame in 'Integrator\View\uTreeViewFrame.pas' {TreeViewFrame: TFrame},
  uTreeViewIntegrator in 'Integrator\View\uTreeViewIntegrator.pas',
  uClassTreeEditIntegrator in 'Integrator\View\LzmTreeEdit\uClassTreeEditIntegrator.pas',
  uClassTreeEditForm in 'Integrator\View\LzmTreeEdit\uClassTreeEditForm.pas',
  uXmiExportArgoUML in 'Integrator\Export\uXmiExportArgoUML.pas',
  uZoomFrame in 'System\uZoomFrame.pas' {ZoomFrame: TFrame},
  uEmxExport in 'Integrator\Export\uEmxExport.pas';

{$R *.res}

begin
   {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  Application.Initialize;
  Application.Title := 'essModel';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

