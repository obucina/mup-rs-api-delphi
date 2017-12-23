object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #268'ita'#269' elektronske saobra'#263'ajne dozvole'
  ClientHeight = 452
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnCitajSDSd: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = #268'itaj SD (Sd)'
    TabOrder = 0
    OnClick = btnCitajSDSdClick
  end
  object btnCitajSDNorm: TButton
    Left = 8
    Top = 40
    Width = 185
    Height = 25
    Caption = #268'itaj SD (Norm)'
    TabOrder = 1
    OnClick = btnCitajSDNormClick
  end
  object btnSacuvajPodatkeUFajl: TButton
    Left = 8
    Top = 72
    Width = 185
    Height = 25
    Caption = 'Sa'#269'uvaj podatke u fajl'
    TabOrder = 2
    OnClick = btnSacuvajPodatkeUFajlClick
  end
  object btnUcitajPodatkeIzFajla: TButton
    Left = 8
    Top = 104
    Width = 185
    Height = 25
    Caption = 'U'#269'itaj podatke iz fajla'
    TabOrder = 3
    OnClick = btnUcitajPodatkeIzFajlaClick
  end
  object mmoPodaci: TMemo
    Left = 200
    Top = 8
    Width = 426
    Height = 435
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object SaveDialog1: TSaveDialog
    Left = 336
    Top = 384
  end
  object OpenDialog1: TOpenDialog
    Left = 560
    Top = 384
  end
end
