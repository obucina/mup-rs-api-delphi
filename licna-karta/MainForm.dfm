object Form1: TForm1
  Left = 542
  Top = 324
  Width = 657
  Height = 594
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 17
  object imgPic: TImage
    Left = 10
    Top = 256
    Width = 203
    Height = 289
    Center = True
    Proportional = True
  end
  object btnCitajKarticu: TButton
    Left = 10
    Top = 21
    Width = 203
    Height = 33
    Caption = 'Citaj LK'
    TabOrder = 0
    OnClick = btnCitajKarticuClick
  end
  object btnSaveMemoToFile: TButton
    Left = 10
    Top = 152
    Width = 203
    Height = 32
    Caption = 'Sacuvaj'
    Enabled = False
    TabOrder = 2
    OnClick = btnSaveMemoToFileClick
  end
  object btnLoadFromFile: TButton
    Left = 10
    Top = 199
    Width = 203
    Height = 32
    Caption = 'Ucitaj iz fajla'
    TabOrder = 3
    OnClick = btnLoadFromFileClick
  end
  object mmoPodaci: TTntMemo
    Left = 232
    Top = 0
    Width = 417
    Height = 562
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 1
  end
end
