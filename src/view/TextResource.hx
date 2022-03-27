package view;

import h2d.Font;

class TextResource
{
    public static var BIZCAT: Font;

    public static function init(): Void {
        BIZCAT = hxd.Res.fonts.bizcat.toFont();
    }

    public static function make_text(parent: h2d.Object): h2d.Text {
        var text = new h2d.Text(BIZCAT, parent);
        return text;
    }
}
