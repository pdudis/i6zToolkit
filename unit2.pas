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

unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  laz2_XMLRead, laz2_DOM, TreeFilterEdit;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnclose: TButton;
    collapse: TButton;
    expand: TButton;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
    procedure btncloseClick(Sender: TObject);
    procedure btncloseExit(Sender: TObject);
    procedure collapseClick(Sender: TObject);
    procedure expandClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

  public

  end;

var
  Form2: TForm2;


implementation

{$R *.lfm}

{ TForm2 }

// Clear TreeView values and close window (via 'Close' button)
procedure TForm2.btncloseClick(Sender: TObject);
var
  i: integer;

begin

  for i:=TreeView1.Items.Count-1 downto 0 do
  begin
  TreeView1.Items[i].Delete;
  end;
  TreeFilterEdit1.Clear;
  Form2.Close;
end;

// Clear TreeView values and close window (via [X] or Alt+F4)
procedure TForm2.btncloseExit(Sender: TObject);
var
  i: integer;

begin
  for i:=TreeView1.Items.Count-1 downto 0 do
  begin
  TreeView1.Items[i].Delete;
  end;
  TreeFilterEdit1.Clear;
end;

// Collapse TreeView
procedure TForm2.collapseClick(Sender: TObject);
begin
  TreeView1.FullCollapse;
end;

// Expand TreeView
procedure TForm2.expandClick(Sender: TObject);
begin
  TreeView1.FullExpand;
end;

// Clear TreeView values and close
procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
  i: integer;

begin
  for i:=TreeView1.Items.Count-1 downto 0 do
  begin
  TreeView1.Items[i].Delete;
  end;
  TreeFilterEdit1.Clear;
end;

end.


