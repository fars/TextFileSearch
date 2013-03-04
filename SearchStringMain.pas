///   Sergii Sidorov 2013 @ Home/test
///
///   Created: 26.02.2013
///
///   Build: Delphi XE3 v.17 RAD Studio x32/x64 Win7
///
///
///    Future work(just for fun, if I'll have free time):
///         - use more fast Boyer-Moore-Horspool text searching algorithm instead slow ansiPos();
///             (see here http://users.dcc.uchile.cl/~rbaeza/handbook/algs/7/713b.srch.p.html)
///         - add string localization via resourcestring/TcxLocalizer for components;
///         - write simple unit test cases using
///                DUnit (http://stackoverflow.com/questions/18291/unit-testing-in-delphi-how-are-you-doing-it)
///         - add to search extra file info like modified data/time, file attributes
///                 using multi column listbox instead memo;
///           http://stackoverflow.com/questions/9209394/how-to-get-file-created-accessed-and-modified-dates-the-same-as-windows-propert
///         - make dll from search string thread module
///             (like http://delphi.about.com/od/windowsshellapi/a/dll_basics.htm);
///         - fix bugs;
///


unit SearchStringMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, ShellAPI, SearchStringThread,
  Vcl.Buttons;

function DeleteLineBreaksSpecialCharacters(const Str: string): string;

type
  TForm1 = class(TForm)
    StatusLabel: TLabel;
    EditSearchedText: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    MemoSearchResults: TMemo;
    EditPath: TEdit;
    Label1: TLabel;
    CheckBoxSubDirSrch: TCheckBox;
    Edit1: TEdit;
    Label4: TLabel;
    BitBtnStart: TBitBtn;
    BitBtnStop: TBitBtn;
    BitBtn1: TBitBtn;

    procedure MemoSearchResultsDblClick(Sender: TObject);
    procedure OpenDirFromMemoLine(Memo : TCustomMemo);
    procedure BitBtnStartClick(Sender: TObject);
    procedure BitBtnStopClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    SearchThread : TFileSearchStringThread;
    procedure WMFileSearchStringFinishedHandler (var Msg : TMessage); message WM_FILE_SEARCH_STRING_FINISHED;
    procedure WMCurrentFilePathShowHandler (var Msg : TMessage); message WM_CURRENT_FILE_PATH_SHOW;
    procedure WMNewResultAddHandler (var Msg : TMessage); message WM_NEW_RESULT_ADD;
    procedure SaveComponentToFile(Component: TComponent; const FileName: TFileName);
    procedure LoadComponentFromFile(Component: TComponent; const FileName: TFileName);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WMNewResultAddHandler (var Msg : TMessage);
begin
  MemoSearchResults.Lines.Add(SearchThread.FileName);
end;

procedure TForm1.WMFileSearchStringFinishedHandler (var Msg : TMessage);
begin
  BitBtnStart.Enabled := true;
  BitBtnStop.Enabled  := false;
  StatusLabel.Caption := 'Search finished';
end;


procedure TForm1.WMCurrentFilePathShowHandler (var Msg : TMessage);
begin
  StatusLabel.Caption := SearchThread.CurentPath;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  PathStr : String;
begin
  PathStr := ExtractFilePath(ParamStr(0));
  if not SelectDirectory('Search path select', '', PathStr)
      then Exit;

  EditPath.Text := PathStr;
end;

procedure TForm1.BitBtnStartClick(Sender: TObject);
var
  SearchedTextStr, DirStr, ExtStr : String;
  IsSrchInSubDirs                 : Boolean;
begin
  SearchedTextStr := EditSearchedText.Text;
  DirStr          := EditPath.Text;
  ExtStr          := Edit1.Text;

  if SearchedTextStr = '' then begin
    ShowMessage('Enter text to find');
    Exit;
  end;

  if DirStr  = '' then begin
    ShowMessage('Choose path first');
    Exit;
  end;

  if not DirectoryExists(DirStr)then begin
    ShowMessage(DirStr+' does not exist');
    Exit;
  end;

  if ExtStr ='' then begin
    ExtStr     :='*.*';
    Edit1.Text := ExtStr;
  end;

  IsSrchInSubDirs := CheckBoxSubDirSrch.Checked;

  MemoSearchResults.Clear;

  SearchThread := TFileSearchStringThread.Create (DirStr, SearchedTextStr, ExtStr, Self,IsSrchInSubDirs );
  SearchThread.Start;

  BitBtnStart.Enabled := false;
  BitBtnStop.Enabled := true;

end;

procedure TForm1.BitBtnStopClick(Sender: TObject);
begin

  SearchThread.Terminate;

  BitBtnStart.Enabled := true;
  BitBtnStop.Enabled  := false;

  StatusLabel.Caption := 'Search terminated by user';

end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveComponentToFile(Self,ChangeFileExt(ExtractFileName(Application.ExeName),'')+ '.ini');
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadComponentFromFile(Self,ChangeFileExt(ExtractFileName(Application.ExeName),'')+ '.ini');
  Self.Show;
end;


procedure TForm1.MemoSearchResultsDblClick(Sender: TObject);
begin
    OpenDirFromMemoLine(MemoSearchResults);
end;


procedure TForm1.OpenDirFromMemoLine(Memo : TCustomMemo) ;
var
  DirPathStr: String;
  SelectedLine : Integer;
 begin
    with Memo do
    begin

       SelectedLine := Perform(EM_LINEFROMCHAR, SelStart, 0);
       DirPathStr := ExtractFileDir(DeleteLineBreaksSpecialCharacters(Lines[SelectedLine]));

      if not DirectoryExists(DirPathStr)then begin
        ShowMessage(Text+' does not exist');
        Exit;
      end;

        ShellExecute(Application.Handle, 'open', 'explorer.exe', PChar(DirPathStr), nil, SW_NORMAL);

    end;
 end;


function DeleteLineBreaksSpecialCharacters(const Str: string): string;
 var
    PtrStrStart, PtrStrEnd: PChar;
 begin

    PtrStrStart := Pointer(Str) ;
    PtrStrEnd   := PtrStrStart + Length(Str) ;

    while PtrStrStart < PtrStrEnd do
    begin
      case PtrStrStart^ of

        #10: PtrStrStart^ := #32;

        #13: PtrStrStart^ := #32;

      end;
      Inc(PtrStrStart) ;
    end;
    Result := Str;
 End;

procedure  TForm1.SaveComponentToFile(Component: TComponent; const FileName: TFileName);
var
  FileStream : TFileStream;
  MemStream : TMemoryStream;
begin
  MemStream := nil;

  if not Assigned(Component) then
    raise Exception.Create('Component is not assigned');

  FileStream := TFileStream.Create(FileName,fmCreate);
  try
    MemStream := TMemoryStream.Create;
    MemStream.WriteComponent(Component);
    MemStream.Position := 0;
    ObjectBinaryToText(MemStream, FileStream);
  finally
    MemStream.Free;
    FileStream.Free;
  end;
end;

procedure  TForm1.LoadComponentFromFile(Component: TComponent; const FileName: TFileName);
var
  FileStream : TFileStream;
  MemStream : TMemoryStream;
  i: Integer;
begin
  MemStream := nil;

  if not Assigned(Component) then
    raise Exception.Create('Component is not assigned');

  if FileExists(FileName) then
  begin
    FileStream := TFileStream.Create(FileName,fmOpenRead);
    try
      for i := Component.ComponentCount - 1 downto 0 do
      begin
        if Component.Components[i] is TControl then
          TControl(Component.Components[i]).Parent := nil;
        Component.Components[i].Free;
      end;

      MemStream := TMemoryStream.Create;
      ObjectTextToBinary(FileStream, MemStream);
      MemStream.Position := 0;
      MemStream.ReadComponent(Component);
      Application.InsertComponent(Component);
    finally
      MemStream.Free;
      FileStream.Free;
    end;
  end;
end;

end.
