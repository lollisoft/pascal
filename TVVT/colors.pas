UNIT Colors;

INTERFACE

USES
  Drivers, App, Views, ColorSel;

PROCEDURE ColorDialog;

IMPLEMENTATION

PROCEDURE ColorDialog;
VAR
  D: PColorDialog;
  Groups: PColorGroup;
  Palette: PPalette;
begin
  Groups :=
    ColorGroup('Desktop',
      ColorItem('Color',                1, nil),
    ColorGroup('Menus',
      ColorItem('Normal',               2,
      ColorItem('Disabled',             3,
      ColorItem('Shortcut',             4,
      ColorItem('Selected',             5,
      ColorItem('Selected disabled',    6,
      ColorItem('Shortcut selected',    7, nil)))))),
    ColorGroup('Blue Window',
      ColorItem('Frame Passive',        8,
      ColorItem('Frame Active',         9,
      ColorItem('Frame Icon',          10,
      ColorItem('Scroll bar Page',     11,
      ColorItem('Scroll bar Arrow',    12,
      ColorItem('Text Normal',         13,
      ColorItem('Text HighLight',      14, Nil))))))),
    ColorGroup('Cyan Window',
      ColorItem('Frame Passive',       16,
      ColorItem('Frame Active',        17,
      ColorItem('Frame Icon',          18,
      ColorItem('Scroll bar Page',     19,
      ColorItem('Scroll bar Arrow',    20,
      ColorItem('Text Normal',         21,
      ColorItem('Text HighLight',      22, Nil))))))),
    ColorGroup('Gray Window',
      ColorItem('Frame Passive',       24,
      ColorItem('Frame Active',        25,
      ColorItem('Frame Icon',          26,
      ColorItem('Scroll bar Page',     27,
      ColorItem('Scroll bar Arrow',    28,
      ColorItem('Text Normal',         29,
      ColorItem('Text HighLight',      30, Nil))))))),
    ColorGroup('Dialog',
      ColorItem('Frame passive',       32,
      ColorItem('Frame active',        33,
      ColorItem('Frame icon',          34,
      ColorItem('ScrollBar page area', 35,
      ColorItem('ScrollBar controls',  36,
      ColorItem('StaticText',          37,
      ColorItem('Label normal',        38,
      ColorItem('Label selected',      39,
      ColorItem('Label shortcut',      40,
      ColorItem('Button normal',       41,
      ColorItem('Button default',      42,
      ColorItem('Button selected',     43,
      ColorItem('Button disabled',     44,
      ColorItem('Button shortcut',     45,
      ColorItem('Button shadow',       46,
      ColorItem('Cluster normal',      47,
      ColorItem('Cluster selected',    48,
      ColorItem('Cluster shortcut',    49,
      ColorItem('InputLine normal text', 50,
      ColorItem('InputLine selected text', 51,
      ColorItem('InputLine arrows',    52,
      ColorItem('History arrow',       53,
      ColorItem('History sides',       54,
      ColorItem('Hist scrollbar page area', 55,
      ColorItem('Hist scrollbar controls', 56,
      ColorItem('ListViewer normal',   57,
      ColorItem('ListViewer focused',  58,
      ColorItem('ListViewer selected', 59,
      ColorItem('ListViewer divider',  60,
      ColorItem('InfoPane',            61,
      Nil)))))))))))))))))))))))))))))),
    Nil))))));
    Palette := Application^.GetPalette;
    D := New(PColorDialog, Init(Palette^, Groups));
    IF DeskTop^.ExecView(D) = cmOk THEN BEGIN
      Palette^ := D^.Pal;
      Application^.ReDraw;
    END;
    Dispose(D, Done);
  END;

END.
