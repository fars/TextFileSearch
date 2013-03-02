object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'StringSearch'
  ClientHeight = 436
  ClientWidth = 605
  Color = clBtnFace
  Constraints.MinHeight = 474
  Constraints.MinWidth = 621
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLabel: TLabel
    Left = 0
    Top = 422
    Width = 605
    Height = 14
    Align = alBottom
    Caption = 'Push start...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 416
    ExplicitWidth = 76
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 605
    Height = 121
    Align = alTop
    Caption = 'Search parameters'
    TabOrder = 0
    DesignSize = (
      605
      121)
    object Label2: TLabel
      Left = 16
      Top = 62
      Width = 26
      Height = 13
      Caption = 'Text:'
    end
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 83
      Height = 13
      Caption = 'Search directory:'
    end
    object Label1: TLabel
      Left = 16
      Top = 106
      Width = 75
      Height = 13
      Caption = 'Search Results:'
    end
    object EditSearchedText: TEdit
      Left = 16
      Top = 79
      Width = 441
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object EditPath: TEdit
      Left = 17
      Top = 35
      Width = 441
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object ButtonStart: TButton
      Left = 495
      Top = 33
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Start'
      TabOrder = 2
      OnClick = ButtonStartClick
    end
    object ButtonStop: TButton
      Left = 495
      Top = 77
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Stop'
      Enabled = False
      TabOrder = 3
      OnClick = ButtonStopClick
    end
    object ButtonDirSelect: TButton
      Left = 464
      Top = 35
      Width = 25
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 4
      OnClick = ButtonDirSelectClick
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 152
      Width = 97
      Height = 17
      Caption = 'CheckBox3'
      TabOrder = 5
    end
  end
  object MemoSearchResults: TMemo
    Left = 0
    Top = 121
    Width = 605
    Height = 289
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 1
    OnDblClick = MemoSearchResultsDblClick
  end
end
