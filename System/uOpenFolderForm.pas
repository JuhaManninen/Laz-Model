unit uOpenFolderForm;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls;

type
  TOpenFolderForm = class(TForm)
    Label2: TLabel;
    OkButton: TButton;
    Button2: TButton;
    FileTypeCombo: TComboBox;
    PathTreeView: TShellTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
