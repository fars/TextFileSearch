///   Sergii Sidorov 2013 @ Home/test
///
///   Created: 26.02.2013
///
///
///
///   Description: Simple text file search engine.
///
///   File scanning put into a TThread and
///   whenver it search in new file,find text in file or finished work
///   it send  messages to the parent form,
///   which then updates the memo with new results/changing status label/finish status
///   (Parent form contains message handlers to work with it, see SearchStringMain.pas)
///


unit SearchStringThread;

interface

uses Winapi.Messages, Winapi.Windows, Vcl.Forms, System.SysUtils, System.Variants, Vcl.FileCtrl,
  System.Classes, Math;

const WM_FILE_SEARCH_STRING_FINISHED      = WM_USER + 1;
const WM_NEW_RESULT_ADD                   = WM_USER + 2;
const WM_CURRENT_FILE_PATH_SHOW           = WM_USER + 3;
const FILE_SEARCH_BUFFER_SIZE             = 1000000;

function IsStringFindInFile(const FileNameStr, SearchStr : String) : Boolean;

type
  TFileSearchStringThread = class (TThread)
  private
    DirPathStr        : String;
    CurentFilePath    : String;
    SearchTextStr     : String;
    FResultFileName   : String;
    ExtStr            : String;
    MotherForm        : TForm;
    MutexPause        : THandle;
    IsSrchInSubDir    : Boolean;
    procedure RecursiveDirectoryStrSearchProcesing(const DirPath: String);
  protected
    procedure Execute; override;
  public
    constructor Create (const Path, SearchStr, FileExtensions : String;
                                                const TheForm: TForm;
                                                const IsSrchInSubFolders : Boolean);
    property FileName : String read FResultFileName;
    property CurentPath : String read CurentFilePath;
end;

implementation

constructor TFileSearchStringThread.Create (const Path, SearchStr, FileExtensions : String;
                                            const TheForm: TForm;
                                            const IsSrchInSubFolders : Boolean);
begin
  inherited Create (True);
  DirPathStr      := Path;
  SearchTextStr   := SearchStr;
  MotherForm      := TheForm;
  ExtStr          := FileExtensions;
  IsSrchInSubDir  := IsSrchInSubFolders;
 end;

procedure TFileSearchStringThread.Execute;
begin
    RecursiveDirectoryStrSearchProcesing(DirPathStr);
    SendMessage (MotherForm.Handle, WM_FILE_SEARCH_STRING_FINISHED, 0, 0);
end;

procedure TFileSearchStringThread.RecursiveDirectoryStrSearchProcesing(const DirPath: String);//(const aPath, aTmpl : String);

const FILE_ATTRIBUTES  =  faAnyFile - faDirectory;
const DIR_ATTRIBUTES   =  faDirectory;
const FILE_SEARCH_MASK =  '*.*';

var
  CurrentPath : String;
  FileSearchrRec : TSearchRec;
begin

  CurrentPath := IncludeTrailingBackslash(DirPath);

    if FindFirst(CurrentPath + FILE_SEARCH_MASK, FILE_ATTRIBUTES, FileSearchrRec) = 0 then
      try
        repeat

          if self.Terminated = true then Exit;

          CurentFilePath := CurrentPath + FileSearchrRec.Name;
          SendMessage (MotherForm.Handle, WM_CURRENT_FILE_PATH_SHOW, 0, 0);

          if (ExtStr = '*.*') then begin
            if IsStringFindInFile(CurentFilePath, SearchTextStr) then begin
                FResultFileName :=  CurentFilePath;
                SendMessage (MotherForm.Handle, WM_NEW_RESULT_ADD, 0, 0);
            end;
          end
         else begin
          if AnsiPos(ExtractFileExt(FileSearchrRec.Name), ExtStr) > 0 then
            if IsStringFindInFile(CurentFilePath, SearchTextStr) then begin
                FResultFileName :=  CurentFilePath;
                SendMessage (MotherForm.Handle, WM_NEW_RESULT_ADD, 0, 0);
            end;
         end;
        until FindNext(FileSearchrRec) <> 0;
      finally
        FindClose(FileSearchrRec);
      end;

  if IsSrchInSubDir then
   if FindFirst(CurrentPath + FILE_SEARCH_MASK, DIR_ATTRIBUTES, FileSearchrRec) = 0 then
      try
        repeat

          if self.Terminated = true then Exit;

          CurentFilePath := CurrentPath + FileSearchrRec.Name;

          if ((FileSearchrRec.Attr and faDirectory) <> 0)
                and (FileSearchrRec.Name <> '.')
                and (FileSearchrRec.Name <> '..') then
                RecursiveDirectoryStrSearchProcesing(CurentFilePath);

        until FindNext(FileSearchrRec) <> 0;
      finally
        FindClose(FileSearchrRec);
      end;

end;

{
  Realization is not good:

  - ansiPos() - slow;
  - not using buffering - in case of litle memories will crash;
  - repeat 4 times for encoding foolproof;

  In order to load a Unicode text file we need to know its encoding.
  If the file has a Byte Order Mark (BOM),
  then we can simply use LoadFromFile(FileNameStr)

  If the file does not have a BOM then we need to explicitly specify the encoding, e.g.

  //  StrList.LoadFromFile(FileNameStr, TEncoding.UTF8);             //UTF-8
  //  StrList.LoadFromFile(FileNameStr, TEncoding.Unicode);          //UTF-16 LE or UCS-2 LE
  //  StrList.LoadFromFile(FileNameStr, TEncoding.BigEndianUnicode); //UTF-16 BE or UCS-2 BE

}

function IsStringFindInFile(const FileNameStr, SearchStr : String) : Boolean;
var
  StrList : TStringList;
  i: Integer;
begin
  Result := False;
  StrList := TStringList.Create;
  try
    StrList.LoadFromFile(FileNameStr);
    for i := StrList.Count-1 downto 0 do
      if ansiPos(SearchStr, StrList[i])<>0 then
        Result := True;

    //In case file doesn't have BOM for foolproof

    StrList.LoadFromFile(FileNameStr, TEncoding.UTF8);
    for i := StrList.Count-1 downto 0 do
      if ansiPos(SearchStr, StrList[i])<>0 then
        Result := True;

    StrList.LoadFromFile(FileNameStr, TEncoding.Unicode);
    for i := StrList.Count-1 downto 0 do
      if ansiPos(SearchStr, StrList[i])<>0 then
        Result := True;

    StrList.LoadFromFile(FileNameStr, TEncoding.BigEndianUnicode);
    for i := StrList.Count-1 downto 0 do
      if ansiPos(SearchStr, StrList[i])<>0 then
        Result := True;

  finally
    StrList.Free;
  end;
end;

end.
