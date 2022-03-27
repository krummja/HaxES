import core.Bus;
import view.Screen;


class Loop
{
    private var nes: Bus;
    private var screen: Screen;
    private var is_running: Bool;
    private var safe_run: Bool = false;

    public function new(ctx: hxd.App): Void {
        this.nes = new Bus();
        this.screen = new Screen(this.nes, ctx);
        this.is_running = true;
    }

    public function update(): Void {
        screen.update();
    }
}
