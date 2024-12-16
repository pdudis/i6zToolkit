{*
  The ‘i6z Toolkit’ is an application for manipulating IUCLID 6 files (*.i6z).
  It provides users with the ability to preview, batch rename/move/copy such
  files, display their ‘Manifest.xml’ in Tree View layout, as well as, export
  the list to a CSV file.

  Version: 0.95-Beta

  Copyright © 2019 Petro Dudi.
  Licensed under the GPL v3.0.
  Icons from https://icons8.com.
*}

unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  ExtCtrls, Buttons;

type

  { TForm3 }

  TForm3 = class(TForm)
    btngithub: TBitBtn;
    btncoffee: TButton;
    btnclose: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure btngithubClick(Sender: TObject);
    procedure btncoffeeClick(Sender: TObject);
    procedure btncloseClick(Sender: TObject);

  private

  public

  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

// 'Buy me a coffee' button!
procedure TForm3.btncoffeeClick(Sender: TObject);
begin
  OpenURL('https://www.buymeacoffee.com/ADYsLjqfi');
end;

// Opens the application's GitHub repository page
procedure TForm3.btngithubClick(Sender: TObject);
begin
  OpenURL('https://github.com/pdudis/i6zToolkit');
end;

procedure TForm3.btncloseClick(Sender: TObject);
begin
  Form3.Close;
end;

end.

