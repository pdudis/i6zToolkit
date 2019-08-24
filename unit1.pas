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

unit Unit1;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls, Zipper, laz2_XMLRead, laz2_DOM, fileutil, LazFileUtils, Unit2, Unit3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnmove: TButton;
    btncopy: TButton;
    btnabout: TButton;
    btnexit: TButton;
    btnexport2csv: TButton;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDirectoryDialog2: TSelectDirectoryDialog;
    SelectDirectoryDialog3: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    addfiles: TButton;
    xmltree: TButton;
    rename: TButton;
    clearlist: TButton;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    procedure addfilesClick(Sender: TObject);
    procedure btnaboutClick(Sender: TObject);
    procedure btncopyClick(Sender: TObject);
    procedure btnexitClick(Sender: TObject);
    procedure btnmoveClick(Sender: TObject);
    procedure btnexport2csvClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure xmltreeClick(Sender: TObject);
    procedure renameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clearlistClick(Sender: TObject);
    procedure UniqueCaptions(const MyCaption, Filename: String);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// Add default column titles & widths to ListView
procedure ListViewTitles;

var
  Col: TListColumn;

begin
  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'File';
  Col.Width := 150;

  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'Archive Type';
  Col.Width := 120;

  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'UUID';
  Col.Width := 150;

  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'Entity Name';
  Col.Width := 150;

  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'Application';
  Col.Width := 150;

  Col := Form1.ListView1.Columns.Add;
  Col.Caption := 'Author';
  Col.Width := 100;

  end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
   ListViewTitles;
   // Enable by default the 'Entity Name' renaming option
   RadioGroup1.ItemIndex:=0;
   // Display the intro text in the StatusBar
   StatusBar1.SimpleText := 'Welcome to the ‘i6z Toolkit’! Start by adding some file(s)...' + '     |     ' + 'Copyright © 2019 Petro Dudi. Licensed under the GPL v3.0.';
end;

// Allow only unique files to be added to the ListView
procedure TForm1.UniqueCaptions(const MyCaption, Filename: String);
var
  Itm: TListItem;
  X: Integer;
  PassNode: TDOMNode;
  Doc: TXMLDocument;
  UnZipper: TUnZipper;
  ManifestFile: TStringList;
  AppConfigDir: String;

begin

  AppConfigDir := GetAppConfigDir(False) + 'temp';

  for X := 0 to ListView1.Items.Count-1 do
    if SameText(ListView1.Items[X].Caption, MyCaption) then Exit;
  Itm:= ListView1.Items.Add;
  Itm.Caption:= MyCaption;

  // Extract the 'Manifest.xml' file for further processing
  ManifestFile := TStringList.Create;
  ManifestFile.Add('manifest.xml');
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := Filename;
    UnZipper.OutputPath := AppConfigDir;
    UnZipper.Examine;
    UnZipper.UnZipFiles(ManifestFile);
  finally
    UnZipper.Free;
  end;

  try

    // Get the main values from the 'Manifest.xml' file

    ReadXMLFile(Doc, AppConfigDir + '\manifest.xml');

    PassNode := Doc.DocumentElement.FindNode('general-information');
    Itm.SubItems.Add(PassNode.FindNode('archive-type').TextContent);

    PassNode := Doc.DocumentElement.FindNode('base-document-uuid');
    Itm.SubItems.Add(PassNode.TextContent);

    PassNode := Doc.DocumentElement.FindNode('contained-documents');
    Itm.SubItems.Add(PassNode.FirstChild.FindNode('name').TextContent);

    PassNode := Doc.DocumentElement.FindNode('general-information');
    Itm.SubItems.Add(PassNode.FindNode('application').TextContent);

    PassNode := Doc.DocumentElement.FindNode('general-information');
    Itm.SubItems.Add(PassNode.FindNode('author').TextContent);

  finally
    Doc.Free;
  end;
  ManifestFile.Free;
end;

// Add files to ListView
procedure TForm1.addfilesClick(Sender: TObject);
  var
     i : Integer;

begin

     StatusBar1.SimpleText := 'Please wait... Adding file(s).';

     if OpenDialog1.Execute then
     begin
       Screen.Cursor := crHourGlass;
       for i := 0 to OpenDialog1.Files.Count -1 do
          begin

            UniqueCaptions(OpenDialog1.Files.Strings[i], OpenDialog1.Files.Strings[i]);

          end;
       Screen.Cursor := crDefault;
     end;

     StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';
     // Set focus to ListView to allow keyboard actions (i.e., Ctrl+A etc.)
     ListView1.SetFocus;
end;

// Show About window
procedure TForm1.btnaboutClick(Sender: TObject);
begin
  Form3.ShowModal;
end;

// Copy selected files
procedure TForm1.btncopyClick(Sender: TObject);
  var
    Item: TListItem;
    Copied: Boolean;
    SaveCopFileDir: String;

  begin

    if ListView1.Items.Count <> 0 then

    begin

    StatusBar1.SimpleText:= 'Please wait... Copying file(s).';

    if SelectDirectoryDialog3.Execute then

    begin
      Screen.Cursor := crHourGlass;
      Copied := FALSE;
      SaveCopFileDir := SelectDirectoryDialog3.FileName;


    for Item in ListView1.Items do
    begin
      if Item.Selected then
      begin

        SetCurrentDir(SaveCopFileDir);

        if (CopyFile(Item.Caption, ExtractFilename(Item.Caption))) then
	  Copied := TRUE
	else
	  ShowMessage('Couldn''t copy file(s).');

      end;

    end;

       if Copied then
        begin
           ShowMessage('Copying process completed successfully!' + sLineBreak + 'View files in folder ' + '''' + SaveCopFileDir + '''.');
           StatusBar1.SimpleText:= '';
        end
      else
      ShowMessage('Select file(s) to copy.');
      Screen.Cursor := crDefault;
      end
    else
      ShowMessage('Action has been cancelled.');

      StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';
        end
    else
      ShowMessage('Nothing to copy!');
  end;

// Exit application with confirmation
procedure TForm1.btnexitClick(Sender: TObject);
begin
  if MessageDlg('Exit application', 'Are you sure you want to quit?', mtConfirmation,
   [mbYes, mbNo],0) = mrYes
  then Application.terminate;
end;

// Move selected files
procedure TForm1.btnmoveClick(Sender: TObject);
  var
    Item: TListItem;
    Moved: Boolean;
    SaveMovFileDir: String;

  begin

    if ListView1.Items.Count <> 0 then

     begin

    StatusBar1.SimpleText:= 'Please wait... Moving file(s).';

    if SelectDirectoryDialog2.Execute then

      begin
      Screen.Cursor := crHourGlass;
      Moved := FALSE;
      SaveMovFileDir := SelectDirectoryDialog2.FileName;

    for Item in ListView1.Items do
    begin
      if Item.Selected then
      begin

        SetCurrentDir(SaveMovFileDir);


        if (CopyFile(Item.Caption, ExtractFilename(Item.Caption))) then
          begin
          DeleteFile(Item.Caption);
          Moved := TRUE;
          end
	else
          begin
          ShowMessage('Couldn''t move file(s).');
          end;

      end;

    end;

       if Moved then
        begin
           ShowMessage('Moving process completed successfully!' + sLineBreak + 'View files in folder ' + '''' + SaveMovFileDir + '''.');
           StatusBar1.SimpleText:= '';
        end
      else
      ShowMessage('Select file(s) to move.');
      Screen.Cursor := crDefault;
      end
    else
    ShowMessage('Action has been cancelled.');

      StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';

      end
    else
     ShowMessage('Nothing to move!');


  end;

// Export ListView content to CSV file (retains sorting order)
procedure TForm1.btnexport2csvClick(Sender: TObject);

var item: TListItem;
    index: Integer;
    Save2File: TStringList;
    Line: String;

begin

  if ListView1.Items.Count <> 0 then
   begin

     StatusBar1.SimpleText:= 'Please wait... Exporting CSV file.';
     Save2File := TStringList.Create;
     Save2File.Add('File, Archive Type, UUID, Entity Name, Application, Author');

        for index := 0 to ListView1.Items.Count -1 do

          begin

           item := ListView1.Items[index];
           Line := Format('%s,%s', [item.Caption, item.SubItems.CommaText]);
           Save2File.Add(Line);

          end;


  if SaveDialog1.Execute then
      begin
       Save2File.SaveToFile(SaveDialog1.Filename);
       ShowMessage('Export to CSV process completed successfully!');
       StatusBar1.SimpleText:= '';
      end
  else
     ShowMessage('Action has been cancelled.');

    Save2File.Free;
   end
   else
   ShowMessage('Nothing to export!');
   StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';
end;

// Exit application with confirmation (from [X] / Alt+F4 action)
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Exit application', 'Are you sure you want to quit?', mtConfirmation,
   [mbYes, mbNo],0) = mrYes
  then Application.terminate
  else
    CanClose := False;
end;

// Rename selected files
procedure TForm1.renameClick(Sender: TObject);
  var
    Item: TListItem;
    Renamed: Boolean;
    SaveRenFileDir: String;
    DivChar: Integer;
    RemStringForDel: Integer;
    GoodUUID: String;
    EntNameStrList: TStringList;
    Counter: Integer;
    UniqueEntName: String;

  begin

    if ListView1.Items.Count <> 0 then

      begin

        StatusBar1.SimpleText:= 'Please wait... Renaming file(s).';

    if SelectDirectoryDialog1.Execute then

     begin

      Renamed := FALSE;
      SaveRenFileDir := SelectDirectoryDialog1.FileName;

      // Create a StringList for comparing processed filenames
      EntNameStrList := TStringList.Create;
      EntNameStrList.Add('');
      // Counter for unique filename generation (i.e., filename_1)
      Counter := 1;

    for Item in ListView1.Items do
    begin
      Screen.Cursor := crHourGlass;
      if Item.Selected then
      begin

         StatusBar1.SimpleText:= 'Please wait... Renaming file(s).';

         SetCurrentDir(SaveRenFileDir);

         case RadioGroup1.ItemIndex of
           0:
             // If 'Entity Name' option is selected
             begin
               // Check for blank Entity Names (i.e., applicable to unnamed Dossiers)
               if (Item.SubItems[2] = '') then
                 begin
                   // If blank, give it the value 'Unnamed' & add it to the StringList
                   Item.SubItems[2]:= 'Unnamed';
                  EntNameStrList.Add(Item.SubItems[2]);
                 end;

               // If processed filename exists in StringList, add an incremented number at the (i.e., '_1'
               if (EntNameStrList.IndexOf(Item.SubItems[2]) > 0) then
		        begin
		        UniqueEntName := Item.SubItems[2] + '_' + Counter.ToString;
                        CopyFile(Item.Caption, UniqueEntName + '.i6z');
		        Counter := Counter + 1;
		        end
	        else
	        CopyFile(Item.Caption, Item.SubItems[2] + '.i6z');

               Renamed := TRUE;
	       EntNameStrList.Add(Item.SubItems[2]);
             end;

           1:
             // If 'Type + Name' option is selected (same flow as above)
             begin
              if (Item.SubItems[2] = '') then
                 begin
                  Item.SubItems[2]:= 'Unnamed';
                  EntNameStrList.Add(Item.SubItems[2]);
                 end;

                 if (EntNameStrList.IndexOf(Item.SubItems[2]) > 0) then
		        begin
		        UniqueEntName := Item.SubItems[2] + '_' + Counter.ToString;
                        CopyFile(Item.Caption, Item.SubItems[0] + '-' + UniqueEntName + '.i6z');
		        Counter := Counter + 1;
		        end
               else
               CopyFile(Item.Caption, Item.SubItems[0] + '-' + Item.SubItems[2] + '.i6z');

               Renamed := TRUE;
               EntNameStrList.Add(Item.SubItems[2]);
             end;
           2:
             // If 'UUID' option is selected
             // We extract the unique value (i.e., not the '/0' or '/UUID' parts)
             begin
               // Get the UUID value
               GoodUUID := Item.SubItems[1];
               // Find the position of the '/'
               DivChar := pos('/',GoodUUID);
               // Hold the superfluous part in variable
               RemStringForDel := (GoodUUID.Length+1) - DivChar;
               // And remove the superfluous value from the UUID string
               Delete(GoodUUID, DivChar, RemStringForDel);
                CopyFile(Item.Caption, GoodUUID + '.i6z');
               Renamed := TRUE;
             end;
         end;

      end;
      Screen.Cursor := crDefault;
    end;

       if Renamed then
        begin
           ShowMessage('Renaming process completed successfully!' + sLineBreak + 'View files in folder ' + '''' + SaveRenFileDir + '''.');
           StatusBar1.SimpleText:= '';
        end
      else
      ShowMessage('Select file(s) to rename.');
      EntNameStrList.Free;
       end
    else
      ShowMessage('Action has been cancelled.');

      StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';
      end
         else
             ShowMessage('Nothing to rename!');

      end;

// Enable keyboard shortcut Ctrl+A in ListView
procedure TForm1.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
begin
  if (Shift = [ssCtrl]) and (key = ord('A')) then
  begin
     Form1.ListView1.Items.BeginUpdate;
     for i := 0 to Form1.ListView1.Items.Count-1 do
     Form1.ListView1.Items[i].Selected:=true;
     Form1.ListView1.Items.EndUpdate;
  end;
end;

// Clear ListView from added files
procedure TForm1.clearlistClick(Sender: TObject);
begin
  ListView1.Items.Clear;
  StatusBar1.SimpleText := 'Files added: ' + ListView1.Items.Count.ToString + '     |     ' + 'Use Ctrl+A to select all items or Ctrl+Click to select multiple items.';
end;

// Create and populate an XML Tree with values from 'Manifest.xml'
procedure XML2Tree(tree: TTreeView; XMLDoc: TXMLDocument);
var
  iNode: TDOMNode;

  procedure ProcessNode(Node: TDOMNode; TreeNode: TTreeNode);
  var
    cNode: TDOMNode;
    s: string;
  begin

    if Node = nil then Exit;

    if Node.NodeValue.Length > 0 then
       s := Node.NodeValue
    else
       s := Node.NodeName;

    TreeNode := tree.Items.AddChild(TreeNode, s);

    cNode := Node.FirstChild;

    while cNode <> nil do
    begin
      ProcessNode(cNode, TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  iNode := XMLDoc.DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil);
    iNode := iNode.NextSibling;
  end;
end;

// Display the 'View XML Tree' window with populated values from 'Manifest.xml'
procedure TForm1.xmltreeClick(Sender: TObject);

var
  AppConfigDir: String;
  UnZipper: TUnZipper;
  ManifestFile: TStringList;
  XMLDoc: TXMLDocument;

begin

  if ListView1.Items.Count <> 0 then

  begin

  AppConfigDir := GetAppConfigDir(False) + 'temp';

    if ListView1.Selected <> nil then
      begin
	  ManifestFile := TStringList.Create;
          ManifestFile.Add('manifest.xml');
          UnZipper := TUnZipper.Create;
          try
            UnZipper.FileName := ListView1.Selected.Caption;
            UnZipper.OutputPath := AppConfigDir;
            UnZipper.Examine;
            UnZipper.UnZipFiles(ManifestFile);
          finally
            UnZipper.Free;
          end;
          try
          ReadXMLFile(XMLDoc, AppConfigDir + '\manifest.xml');
          finally
          end;
          XML2Tree(Form2.TreeView1, XMLDoc);
          ManifestFile.Free;
          Form2.ShowModal;
        end
    else
    ShowMessage('Nothing selected!');
  end
     else
         ShowMessage('Nothing to view!');
  end;



end.

