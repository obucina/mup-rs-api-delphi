object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #268'ita'#269' elektronske li'#269'ne karte'
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
  object Image1: TImage
    Left = 8
    Top = 72
    Width = 185
    Height = 241
    Stretch = True
  end
  object btnCitajLKEid: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = #268'itaj LK (Eid)'
    TabOrder = 0
    OnClick = btnCitajLKEidClick
  end
  object btnCitajLKNorm: TButton
    Left = 8
    Top = 40
    Width = 185
    Height = 25
    Caption = #268'itaj LK (Norm)'
    TabOrder = 1
    OnClick = btnCitajLKNormClick
  end
  object btnSacuvajFotografijuUFajl: TButton
    Left = 8
    Top = 320
    Width = 185
    Height = 25
    Caption = 'Sa'#269'uvaj fotografiju u fajl'
    TabOrder = 2
    OnClick = btnSacuvajFotografijuUFajlClick
  end
  object btnSacuvajPodatkeUFajl: TButton
    Left = 8
    Top = 352
    Width = 185
    Height = 25
    Caption = 'Sa'#269'uvaj podatke u fajl'
    TabOrder = 3
    OnClick = btnSacuvajPodatkeUFajlClick
  end
  object btnUcitajPodatkeIzFajla: TButton
    Left = 8
    Top = 384
    Width = 185
    Height = 25
    Caption = 'U'#269'itaj podatke iz fajla'
    TabOrder = 4
    OnClick = btnUcitajPodatkeIzFajlaClick
  end
  object mmoPodaci: TMemo
    Left = 200
    Top = 8
    Width = 426
    Height = 435
    ScrollBars = ssBoth
    TabOrder = 5
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
