import Loop;
import view.TextResource;

class Main extends hxd.App
{
    var loop: Loop;

    public static function main(): Void {
        hxd.Res.initEmbed();
        new Main();
    }

    public override function init(): Void {
        TextResource.init();
        // Loop.init(this);
        this.loop = new Loop(this);
    }

    public override function update(dt: Float): Void {
        this.loop.update();
    }
}
