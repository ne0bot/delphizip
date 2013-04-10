object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Delphi Zip Resource Maker'
  ClientHeight = 445
  ClientWidth = 389
  Color = clBtnFace
  Constraints.MinHeight = 375
  Constraints.MinWidth = 370
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 404
    Width = 389
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnBuildRes: TButton
      Left = 157
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Build &Res'
      TabOrder = 0
      OnClick = btnBuildResClick
    end
  end
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 389
    Height = 404
    ActivePage = TabSheet1
    Align = alClient
    Constraints.MinHeight = 280
    Constraints.MinWidth = 345
    TabOrder = 1
    OnChange = PagesChange
    OnChanging = PagesChanging
    object TabSheet1: TTabSheet
      Caption = '&Paths'
      OnShow = TabSheet1Show
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        381
        376)
      object Label1: TLabel
        Left = 14
        Top = 3
        Width = 23
        Height = 13
        Caption = 'Root'
      end
      object Label2: TLabel
        Left = 15
        Top = 229
        Width = 46
        Height = 13
        Caption = 'Resource'
      end
      object Label3: TLabel
        Left = 14
        Top = 57
        Width = 12
        Height = 13
        Caption = 'Dll'
      end
      object Label4: TLabel
        Left = 12
        Top = 165
        Width = 48
        Height = 13
        Caption = 'Language'
      end
      object Label8: TLabel
        Left = 15
        Top = 333
        Width = 22
        Height = 13
        Caption = 'UPX'
      end
      object Label9: TLabel
        Left = 14
        Top = 277
        Width = 41
        Height = 13
        Caption = 'BRCC32'
      end
      object Label5: TLabel
        Left = 14
        Top = 112
        Width = 62
        Height = 13
        Caption = 'ZMSFX?.exe'
      end
      object lblSFXVer: TLabel
        Left = 96
        Top = 112
        Width = 38
        Height = 13
        Caption = 'Version:'
      end
      object lblDllVers: TLabel
        Left = 96
        Top = 57
        Width = 35
        Height = 13
        Caption = 'Version'
      end
      object lblSFXUVer: TLabel
        Left = 208
        Top = 112
        Width = 40
        Height = 13
        Caption = 'Unicode'
      end
      object edRoot: TEdit
        Left = 14
        Top = 21
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvLowered
        BevelOuter = bvRaised
        TabOrder = 0
        Text = '..\..'
        OnChange = edRootChange
      end
      object edRes: TEdit
        Left = 14
        Top = 241
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvLowered
        BevelOuter = bvRaised
        TabOrder = 1
        Text = 'RES'
        OnChange = edResChange
      end
      object edDll: TEdit
        Left = 14
        Top = 76
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvLowered
        BevelOuter = bvRaised
        TabOrder = 2
        Text = 'DLL'
        OnChange = edDllChange
      end
      object edLang: TEdit
        Left = 14
        Top = 186
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelInner = bvLowered
        BevelOuter = bvRaised
        TabOrder = 3
        Text = 'Lang'
        OnChange = edLangChange
      end
      object btnBrowseRoot: TButton
        Left = 349
        Top = 22
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 4
        OnClick = btnBrowseRootClick
      end
      object btnBrowsDest: TButton
        Left = 349
        Top = 257
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 5
        OnClick = btnBrowsDestClick
      end
      object btnBrowseDll: TButton
        Left = 349
        Top = 69
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 6
        OnClick = btnBrowseDllClick
      end
      object btnBrowseLang: TButton
        Left = 349
        Top = 163
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 7
        OnClick = btnBrowseLangClick
      end
      object edUPX: TEdit
        Left = 14
        Top = 352
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        Text = 'UPX'
        OnChange = edUPXChange
      end
      object btnBrowseUPX: TButton
        Left = 349
        Top = 352
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 9
        OnClick = btnBrowseUPXClick
      end
      object BtnBrowseBCB: TButton
        Left = 349
        Top = 304
        Width = 25
        Height = 19
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 10
        OnClick = BtnBrowseBCBClick
      end
      object edBRCC: TEdit
        Left = 14
        Top = 296
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 11
        Text = '$(BCB)\bin'
        OnChange = edBRCCChange
      end
      object edSFX: TEdit
        Left = 14
        Top = 131
        Width = 329
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 12
        Text = 'SFX'
        OnChange = edSFXChange
      end
      object btnBrowseSFX: TButton
        Left = 349
        Top = 116
        Width = 25
        Height = 17
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 13
        OnClick = btnBrowseSFXClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'DLL'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label7: TLabel
        Left = 24
        Top = 64
        Width = 99
        Height = 13
        Caption = 'DelZipxxx..dll version'
      end
      object cbDlls: TCheckBox
        Left = 24
        Top = 16
        Width = 167
        Height = 17
        Caption = 'Store DLL as Resource'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object edDllVer: TEdit
        Left = 168
        Top = 56
        Width = 121
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object pnlDllUPX: TPanel
        Left = 24
        Top = 216
        Width = 329
        Height = 121
        Color = clInfoBk
        ParentBackground = False
        TabOrder = 2
        Visible = False
        object cbUseUPX: TCheckBox
          Left = 8
          Top = 8
          Width = 209
          Height = 17
          Caption = 'Compress with UPX'
          TabOrder = 0
        end
        object StaticText9: TStaticText
          Left = 112
          Top = 32
          Width = 76
          Height = 20
          Caption = 'WARNING'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
        object StaticText10: TStaticText
          Left = 24
          Top = 56
          Width = 278
          Height = 17
          Caption = 'Compressing the dll with UPX may cause loading problems'
          TabOrder = 2
        end
        object StaticText11: TStaticText
          Left = 32
          Top = 80
          Width = 81
          Height = 17
          Caption = ' in the debugger'
          TabOrder = 3
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Languages'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbLangs: TCheckListBox
        Left = 0
        Top = 0
        Width = 381
        Height = 348
        Align = alClient
        AutoComplete = False
        Columns = 2
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 348
        Width = 381
        Height = 28
        Align = alBottom
        TabOrder = 1
        object btnAllLang: TButton
          Left = 37
          Top = 6
          Width = 75
          Height = 20
          Caption = 'All'
          TabOrder = 0
          OnClick = btnAllLangClick
        end
        object btnNoneLang: TButton
          Left = 153
          Top = 6
          Width = 75
          Height = 20
          Caption = 'None'
          TabOrder = 1
          OnClick = btnNoneLangClick
        end
        object btnRefreshLang: TButton
          Left = 277
          Top = 6
          Width = 75
          Height = 20
          Caption = 'Refresh'
          TabOrder = 2
          OnClick = btnRefreshLangClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SFX'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label10: TLabel
        Left = 162
        Top = 11
        Width = 76
        Height = 13
        Caption = 'Stub destination'
      end
      object lblSFXVer1: TLabel
        Left = 77
        Top = 76
        Width = 52
        Height = 13
        Caption = 'lblSFXVer1'
      end
      object lblSFXUVer1: TLabel
        Left = 77
        Top = 100
        Width = 54
        Height = 13
        Caption = 'lblSFXUVer'
      end
      object Label11: TLabel
        Left = 150
        Top = 57
        Width = 52
        Height = 13
        Caption = 'Make .Res'
      end
      object lblSFXCompress: TLabel
        Left = 232
        Top = 56
        Width = 71
        Height = 13
        Caption = 'Compress UPX'
      end
      object Panel3: TPanel
        Left = -3
        Top = 310
        Width = 381
        Height = 32
        TabOrder = 0
        object BtnSFXAll: TButton
          Left = 26
          Top = 6
          Width = 75
          Height = 20
          Caption = 'All'
          TabOrder = 0
          OnClick = BtnSFXAllClick
        end
        object btnSFXNone: TButton
          Left = 153
          Top = 6
          Width = 75
          Height = 20
          Caption = 'None'
          TabOrder = 1
          OnClick = btnSFXNoneClick
        end
        object btnSFXRefresh: TButton
          Left = 273
          Top = 6
          Width = 75
          Height = 20
          Caption = 'Refresh'
          TabOrder = 2
          OnClick = btnSFXRefreshClick
        end
      end
      object lbSFXLangs: TCheckListBox
        Left = 5
        Top = 136
        Width = 373
        Height = 168
        AutoComplete = False
        Columns = 1
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
      end
      object btnSFXMake: TButton
        Left = 153
        Top = 348
        Width = 75
        Height = 25
        Caption = 'Make'
        TabOrder = 2
        OnClick = btnSFXMakeClick
      end
      object cbSFXUPX: TCheckBox
        Left = 248
        Top = 75
        Width = 97
        Height = 17
        AllowGrayed = True
        TabOrder = 3
      end
      object cbSFXRes: TCheckBox
        Left = 162
        Top = 76
        Width = 80
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object edSFXBin: TEdit
        Left = 5
        Top = 30
        Width = 309
        Height = 21
        TabOrder = 5
        OnChange = edSFXBinChange
      end
      object btnBrowseSFXBin: TButton
        Left = 352
        Top = 32
        Width = 26
        Height = 17
        Caption = '...'
        TabOrder = 6
        OnClick = btnBrowseSFXBinClick
      end
      object cbSFXURES: TCheckBox
        Left = 162
        Top = 99
        Width = 80
        Height = 17
        TabOrder = 7
      end
      object cbSFXUUPX: TCheckBox
        Left = 248
        Top = 99
        Width = 97
        Height = 17
        TabOrder = 8
      end
      object rgSFXAll: TRadioGroup
        Left = 3
        Top = 57
        Width = 68
        Height = 63
        Caption = 'In All'
        ItemIndex = 0
        Items.Strings = (
          'Ansi'
          'Unicode')
        TabOrder = 9
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Def Strs'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StaticText1: TStaticText
        Left = 0
        Top = 0
        Width = 90
        Height = 24
        Align = alTop
        Alignment = taCenter
        BevelEdges = [beBottom]
        BevelKind = bkFlat
        BorderStyle = sbsSunken
        Caption = '  Warning  '
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clInfoText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold, fsUnderline]
        ParentColor = False
        ParentFont = False
        ShowAccelChar = False
        TabOrder = 0
        Transparent = False
      end
      object Panel12: TPanel
        Left = 0
        Top = 208
        Width = 381
        Height = 168
        Align = alBottom
        BevelWidth = 2
        BorderStyle = bsSingle
        TabOrder = 7
        object btnBuildSFXDefStr: TButton
          Left = 119
          Top = 120
          Width = 139
          Height = 25
          Caption = 'Build SFX default strings'
          TabOrder = 0
          OnClick = btnBuildSFXDefStrClick
        end
        object StaticText12: TStaticText
          Left = 20
          Top = 16
          Width = 309
          Height = 20
          Caption = 'This will replace (overwrite) the following file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
        object StaticText13: TStaticText
          Left = 40
          Top = 42
          Width = 90
          Height = 17
          Caption = 'SFX\DefStr.bin'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
        end
        object StaticText14: TStaticText
          Left = 20
          Top = 65
          Width = 245
          Height = 17
          Caption = 'Only needs to be done if default language strings in'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object StaticText15: TStaticText
          Left = 40
          Top = 88
          Width = 132
          Height = 17
          Caption = 'SFXStr_US.txt are modified'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
      end
      object StaticText2: TStaticText
        Left = 40
        Top = 56
        Width = 83
        Height = 17
        Caption = 'ZMMsg18.pas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
      end
      object StaticText3: TStaticText
        Left = 20
        Top = 32
        Width = 320
        Height = 20
        Caption = 'This will replace (overwrite) the following units'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
      end
      object StaticText4: TStaticText
        Left = 40
        Top = 80
        Width = 109
        Height = 17
        Caption = 'ZMDefMsgs18.pas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
      object StaticText5: TStaticText
        Left = 20
        Top = 103
        Width = 245
        Height = 17
        Caption = 'Only needs to be done if default language strings in'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object StaticText6: TStaticText
        Left = 40
        Top = 126
        Width = 188
        Height = 17
        Caption = 'ZipMsgUS.rc or ZipMsg.h are modified!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object btnMakeUnits: TButton
        Left = 121
        Top = 161
        Width = 139
        Height = 25
        Caption = 'Build Component Units'
        TabOrder = 6
        OnClick = btnMakeUnitsClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Information'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 381
        Height = 376
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 408
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = '&Exit'
      end
    end
    object Build1: TMenuItem
      Caption = '&Build'
      object Script1: TMenuItem
        Caption = '&Script'
        Hint = 'build resource script'
      end
      object Resources1: TMenuItem
        Caption = '&Resources'
        Hint = 'build and compile resources'
        OnClick = btnBuildResClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Combined1: TMenuItem
        Caption = 'Combined'
        OnClick = Combined1Click
      end
      object Separate1: TMenuItem
        Caption = 'Separate'
        Checked = True
        Hint = 'Make separate resources files'
        OnClick = Separate1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object DefTags: TMenuItem
        Caption = '&Default Strings'
        Hint = 'Build default strings'
        OnClick = DefTagsClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object A1: TMenuItem
        Caption = 'About'
        OnClick = A1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 320
    Top = 24
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 272
    Top = 184
  end
end
