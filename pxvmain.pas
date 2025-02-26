unit pxvMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  LCLVersion, LConvEncoding,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ShellCtrls, Grids,
  DB, DBGrids, DBCtrls, ParadoxDS, sqlite3conn, sqldb;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    btnExportSQLite3: TButton;
    cmbInputEncoding: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBMemo: TDBMemo;
    DBNavigator1: TDBNavigator;
    ImageList: TImageList;
    Label1: TLabel;
    PageControl: TPageControl;
    Panel1: TPanel;
    DataPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbIndividualFiles: TRadioButton;
    rbCombinedFile: TRadioButton;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    MemoSplitter: TSplitter;
    pgData: TTabSheet;
    pgFields: TTabSheet;
    Grid: TStringGrid;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure btnExportSQLite3Click(Sender: TObject);
    procedure cmbInputEncodingChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParadoxDatasetAfterScroll(DataSet: TDataSet);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    function ShellTreeViewSortCompare(Item1, Item2: TFileItem): integer;
  private
    ParadoxDataset: TParadoxDataset;
    procedure ExportToSQLite3(AsCombinedFile: Boolean);
    function GetInputEncoding: String;
    procedure OpenParadoxFile(const AFileName: string);
    procedure UpdateGrid;
    procedure UpdateMemo;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TypInfo, LazFileUtils;

const
  PROGRAM_NAME = 'PARADOX Viewer';

{ TMainForm }

procedure TMainForm.btnExportSQLite3Click(Sender: TObject);
begin
  if rbIndividualFiles.Checked then
    ExportToSQLite3(false);
  if rbCombinedFile.Checked then
    ExportToSQLite3(true);
end;

procedure TMainForm.cmbInputEncodingChange(Sender: TObject);
begin
  if ParadoxDataset.TableName <> '' then
    OpenParadoxFile(ParadoxDataset.TableName);
end;

procedure TMainForm.ExportToSQLite3(AsCombinedFile: Boolean);
var
  sql: String;
  F: TField;
  dt: String;
  i: Integer;
  s: String;
  P: array of variant;
  query: TSQLQuery;
  dbName: String;
  tblName: String;
begin
  SQLite3Connection.Connected := false;

  tblName := ChangeFileExt(ExtractFileName(ParadoxDataset.TableName), '');
  if AsCombinedFile then begin
    dbName := ExtractFileDir(ParadoxDataset.TableName) + '.sqlite';
    SQLite3Connection.DatabaseName := dbName;
    SQLite3Connection.Open;
    SQLTransaction.Active := true;
    sql := 'DROP TABLE IF EXISTS ' + tblName;
    SQLite3Connection.ExecuteDirect(sql);
  end else begin
    dbName := ChangeFileExt(ExtractFileName(ParadoxDataset.TableName), '.sqlite');
    if FileExists(dbName) then
      DeleteFile(dbName);
    SQLite3Connection.DatabaseName := dbName;
    SQLite3Connection.Open;
    SQLTransaction.Active := true;
  end;

  sql := Format('CREATE TABLE "%s" (', [tblName]);
  for i:=0 to ParadoxDataset.FieldCount-1 do begin
    F := ParadoxDataset.Fields[i];
    if F.DataType in [ftSmallInt, ftInteger, ftWord, ftLargeInt] then
      dt := 'INTEGER'
    else if F.DataType in [ftFloat] then
      dt := 'REAL'
    else if F.DataType in [ftDateTime] then
      dt := 'DATETIME'
    else if F.DataType in [ftDate] then
      dt := 'DATE'
    else if F.DataType in [ftTime] then
      dt := 'TIME'
    else if F.DataType = ftBoolean then
      dt := 'BOOL'
    else if F.DataType = ftMemo then
      dt := 'TEXT'
    else if F.DataType in [ftString, ftWidestring] then begin
      if F.Size > 0 then
        dt := 'VARCHAR(' + IntToStr(f.Size) + ')'
      else;
        dt := 'STRING'
    end
    else if F.DataType = ftMemo then
      dt := 'STRING'
    else
      raise Exception.CreateFmt('Export to SQLite3: field type "%s" not supported', [GetEnumName(TypeInfo(TFieldType), ord(F.DataType))]);
    if i > 0 then sql := sql + ',';
    sql := sql + Format('"%s" %s', [F.FieldName, dt]);

    // Primary key (consisting of single field) ?
    if (i = 0) and (ParadoxDataset.PrimaryKeyFieldCount = 1) then
      sql := sql + ' PRIMARY KEY'
  end;
  // Primary key (constisting of several fields)
  if (ParadoxDataset.PrimaryKeyFieldCount > 1) then begin
    sql := sql + ' PRIMARY KEY (' + ParadoxDataset.Fields[0].FieldName;
    for i:= 1 to ParadoxDataset.PrimaryKeyFieldCount-1 do
      sql := sql + ',' + ParadoxDataset.Fields[i].FieldName;
    sql := sql + ') ';
  end;
  sql := sql + ');';
  SQLite3Connection.ExecuteDirect(sql);
  SQLTransaction.Commit;

  query := TSQLQuery.Create(nil);
  try
    query.Database := SQLite3Connection;
    //query.Options := query.Options + [sqoAutoApplyUpdates, sqoAutoCommit];
    sql := Format('INSERT INTO %s (%s', [tblName, ParadoxDataset.Fields[0].FieldName]);
    s := 'VALUES (:P0';
    for i := 1 to ParadoxDataset.FieldCount-1 do begin
      sql := sql + ',' + ParadoxDataset.Fields[i].FieldName;
      s := s + ',:P' + IntToStr(i);
    end;
    query.SQL.Text := sql + ') ' + s + ');';
    ParadoxDataset.First;
    while not ParadoxDataset.EoF do begin
      for i:=0 to ParadoxDataset.Fieldcount-1 do begin
        if ParadoxDataset.Fields[i].DataType = ftMemo then
          query.Params.ParamByName('P' + IntToStr(i)).AsString := ParadoxDataset.Fields[i].AsString
        else
          query.Params.ParamByName('P' + IntToStr(i)).Value := ParadoxDataset.Fields[i].Value;
      end;
      query.ExecSQL;
      ParadoxDataset.Next;
    end;
    SQLTransaction.Commit;
  finally
    query.Free;
  end;

  if AsCombinedFile then
    ShowMessage('Table "' + tblName + '" successfully added to SQLite3 database "' + dbname + '"')
  else
    ShowMessage('SQLite3 database "' + dbname + '" created successfully.');
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  fn: String;
begin
  if ParamCount > 0 then begin
    fn := ParamStr(1);
    if DirectoryExists(fn) then
      ShellTreeView.Path := fn
    else
    begin
      ShellTreeview.Path := ExtractFilepath(fn);
      OpenParadoxFile(fn);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := PROGRAM_NAME;
  ShellListview.Mask := '*.db';
  {$IFDEF MSWINDOWS}
  {$IF LCL_FullVersion >= 2010000}
  ShellTreeView.Images := nil;
  ShellListView.SmallImages := nil;
  {$IFEND}
  {$ENDIF}

  // Avoid installation of the component from CCR
  ParadoxDataset := TParadoxDataset.Create(self);
  ParadoxDataset.AfterScroll := @ParadoxDatasetAfterScroll;
  DataSource.Dataset := ParadoxDataset;
end;

function TMainForm.GetInputEncoding: String;
var
  sa: TStringArray;
begin
  if (cmbInputEncoding.ItemIndex in [0, -1]) then
    Result := ''
  else
  begin
    sa := cmbInputEncoding.Items[cmbInputEncoding.ItemIndex].Split(' ');
    Result := Lowercase(StringReplace(sa[0], '-', '', [rfReplaceAll]));
  end;
end;

procedure TMainForm.OpenParadoxFile(const AFileName: String);
begin
  ParadoxDataset.Close;
  DBMemo.DataField := '';
  ParadoxDataset.TableName := AFileName;
  ParadoxDataset.InputEncoding := GetInputEncoding;
  ParadoxDataset.Open;
  UpdateMemo;
  UpdateGrid;

  // Update information in form
  Caption := Format('%s - %s', [PROGRAM_NAME, ExtractFileName(AFilename)]);
end;

procedure TMainForm.ParadoxDatasetAfterScroll(DataSet: TDataSet);
begin
  UpdateMemo;
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fn: String;
begin
  if Selected then
  begin
    fn := ShellListview.GetPathFromItem(ShellListview.Selected);
    ShellTreeview.MakeSelectionVisible;
    OpenParadoxFile(fn);
  end;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 0;
end;

procedure TMainForm.ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := 1;
end;

// from: https://forum.lazarus.freepascal.org/index.php/topic,61347.msg462091.html
function TMainForm.ShellTreeViewSortCompare(Item1, Item2: TFileItem): integer;
begin
  // Make sure that folders are moved to the top
  Result := ord(Item2.isFolder) - ord(Item1.isFolder);
  // Move folders beginning with underscore to the top
  if Result = 0 then
    if (pos('_', Item1.FileInfo.Name) = 1) or (pos('_', Item2.FileInfo.Name) = 1) then
      Result := AnsiCompareText(Item1.FileInfo.Name, Item2.FileInfo.Name)
    else
      Result := CompareText(Item1.FileInfo.Name, Item2.FileInfo.Name);
end;

procedure TMainForm.UpdateGrid;
var
  i, j: Integer;
begin
  Grid.RowCount := ParadoxDataset.FieldCount + 1;
  j := 1;
  for i:=0 to ParadoxDataset.FieldCount-1 do begin
    Grid.Cells[0, j] := IntToStr(i);
    Grid.Cells[1, j] := ParadoxDataset.Fields[i].FieldName;
    Grid.Cells[2, j] := GetEnumName(TypeInfo(TFieldType), integer(ParadoxDataset.Fields[i].DataType));
    Grid.Cells[3, j] := IntToStr(ParadoxDataset.Fields[i].Size);
    Grid.Cells[4, j] := BoolToStr(ParadoxDataset.Fields[i].Required, true);
    inc(j);
  end;
end;

procedure TMainForm.UpdateMemo;
var
  MF: TField;
  i: Integer;
begin
  MF := nil;
  for i:=0 to ParadoxDataset.FieldCount-1 do
    if ParadoxDataset.Fields[i].DataType = ftMemo then begin
      MF := ParadoxDataset.Fields[i];
      break;
    end;
  if MF <> nil then begin
    DBMemo.DataField := MF.FieldName;
    DBMemo.Show;
    MemoSplitter.Show;
    MemoSplitter.Top := 0;
  end else begin
    DBMemo.Hide;
    MemoSplitter.Hide;
  end;
end;

end.

