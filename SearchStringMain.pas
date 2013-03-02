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
///         - add file extension filter mask(add ansiPos() of extracted file extension );
///         - add to search extra file info like creation data/time, file attributes
///                 using multi column listbox instead memo;
///         - build in metropolis GUI style for win8;
///         - save\load settings and components last state from ini/xml file
///              like here http://stackoverflow.com/questions/3163586/best-way-to-save-restore-a-form-foss
///         - add pause button to resume/suspend search tthtread;
///         - add images to buttons;
///         - make dll from search string thread module
///             (like http://delphi.about.com/od/windowsshellapi/a/dll_basics.htm);
///         - add this project to github;
///         - fix bugs;
///


unit SearchStringMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, ShellAPI, SearchStringThread;

function DeleteLineBreaksSpecialCharacters(const Str: string): string;

type
  TForm1 = class(TForm)
    StatusLabel: TLabel;
    EditSearchedText: TEdit;
    ButtonStart: TButton;
    ButtonStop: TButton;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    MemoSearchResults: TMemo;
    EditPath: TEdit;
    ButtonDirSelect: TButton;
    Label1: TLabel;
    CheckBox3: TCheckBox;

    procedure ButtonDirSelectClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure MemoSearchResultsDblClick(Sender: TObject);
    procedure OpenDirFromMemoLine(Memo : TCustomMemo);
  private
    SearchThread : TFileSearchStringThread;
    procedure WMFileSearchStringFinishedHandler (var Msg : TMessage); message WM_FILE_SEARCH_STRING_FINISHED;
    procedure WMCurrentFilePathShowHandler (var Msg : TMessage); message WM_CURRENT_FILE_PATH_SHOW;
    procedure WMNewResultAddHandler (var Msg : TMessage); message WM_NEW_RESULT_ADD;
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
  ButtonStart.Enabled := true;
  ButtonStop.Enabled  := false;
  StatusLabel.Caption := 'Search finished';
end;


procedure TForm1.WMCurrentFilePathShowHandler (var Msg : TMessage);
begin
  StatusLabel.Caption := SearchThread.CurentPath;
end;


procedure TForm1.ButtonDirSelectClick(Sender: TObject);
var
  PathStr : String;
begin
  PathStr := ExtractFilePath(ParamStr(0));
  if not SelectDirectory('Search path select', '', PathStr)
      then Exit;

  EditPath.Text := PathStr;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  SearchedTextStr, DirStr : String;
begin
  SearchedTextStr := EditSearchedText.Text;
  DirStr          := EditPath.Text;

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

  MemoSearchResults.Clear;

  SearchThread := TFileSearchStringThread.Create (DirStr, SearchedTextStr, Self);
  SearchThread.Start;

  ButtonStart.Enabled := false;
  ButtonStop.Enabled := true;

end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin

  SearchThread.Terminate;

  ButtonStart.Enabled := true;
  ButtonStop.Enabled  := false;

  StatusLabel.Caption := 'Search terminated by user';

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

end.
