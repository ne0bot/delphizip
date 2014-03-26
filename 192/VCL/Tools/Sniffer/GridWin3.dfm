object MDIGridWin: TMDIGridWin
  Left = 197
  Top = 117
  Caption = 'ZipMaster messages'
  ClientHeight = 219
  ClientWidth = 316
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TDrawGrid
    Left = 0
    Top = 0
    Width = 316
    Height = 219
    Align = alClient
    ColCount = 7
    RowCount = 1
    FixedRows = 0
    TabOrder = 0
    OnDrawCell = GridDrawCell
    ExplicitHeight = 220
  end
end
