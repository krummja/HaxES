package view;
import h2d.Graphics;
using hl.Bytes;
import hxd.Key;
import core.Bus;
import core.CPU.FLAG;
import view.TextResource;


class Screen
{
    private var ob: h2d.Object;
    private var nes: Bus;
    private var ctx: hxd.App;
    private var back: Graphics;

    private var map_asm: Map<Int, String>;
    private var bg_color: Int;

    public function new(nes: Bus, ctx: hxd.App): Void {
        this.nes = nes;
        this.ctx = ctx;
        this.back = new Graphics(ctx.s2d);
        this.bg_color = 0x0000AA;

        var program = [
            0xA2, 0x0A, 0x8E, 0x00,
            0x00, 0xA2, 0x03, 0x8E,
            0x01, 0x00, 0xAC, 0x00, 
            0x00, 0xA9, 0x00, 0x18, 
            0x6D, 0x01, 0x00, 0x88, 
            0xD0, 0xFA, 0x8D, 0x02, 
            0x00, 0xEA, 0xEA, 0xEA,
        ];

        var offset = 0x8000;
        for (inst in program) {
            nes.cpuRam[offset++] = inst;
        }

        nes.cpuRam[0xFFFC] = 0x00;
        nes.cpuRam[0xFFFD] = 0x80;

        this.map_asm = nes.cpu.disassemble(0x0000, 0xFFFF);
        nes.cpu.reset();

        refresh();
    }

    private function refresh() {
        var ob = this.ctx.s2d;
        back.removeChildren();
        back.clear();
        back.beginFill(this.bg_color, 1.0);
        back.drawRect(ob.x, ob.y, ob.width, ob.height);
        back.endFill();
    }

    @:allow(Loop)
    private function update(): Void {
        // Now I can process the disassembled output and display it to the
        // HEAPS screen. ^o^

        refresh();

        if (Key.isPressed(Key.SPACE)) {
            do {
                this.nes.cpu.clock();
            } while (!nes.cpu.complete());
        }

        if (Key.isPressed(Key.R))
            this.nes.cpu.reset();

        if (Key.isPressed(Key.I))
            this.nes.cpu.irq();

        if (Key.isPressed(Key.N))
            this.nes.cpu.nmi();

        draw_ram(16, 2, 0x0000, 16, 16);
        draw_ram(16, 288, 0x8000, 16, 16);
        draw_cpu(480, 2);
        draw_code(480, 112, 26);

        var help_string = "";
        help_string += "SPACE = Step Instruction    ";
        help_string += "R = RESET    ";
        help_string += "I = IRQ    ";
        help_string += "N = NMI";
        print(10, 576, help_string);
    }

    private function draw_cpu(x: Int, y: Int): Void {
        var RED = 0xFF0000;
        var GREEN = 0x00FF00;

        print(x, y, "STATUS:", 0xFFFFFF);
        print(x + 64,  y, "N", nes.cpu.status & nes.cpu.get_value(FLAG.N) > 0 ? GREEN : RED);
        print(x + 80,  y, "V", nes.cpu.status & nes.cpu.get_value(FLAG.V) > 0 ? GREEN : RED);
        print(x + 96,  y, "-", nes.cpu.status & nes.cpu.get_value(FLAG.U) > 0 ? GREEN : RED);
        print(x + 112, y, "B", nes.cpu.status & nes.cpu.get_value(FLAG.B) > 0 ? GREEN : RED);
        print(x + 128, y, "D", nes.cpu.status & nes.cpu.get_value(FLAG.D) > 0 ? GREEN : RED);
        print(x + 144, y, "I", nes.cpu.status & nes.cpu.get_value(FLAG.I) > 0 ? GREEN : RED);
        print(x + 160, y, "Z", nes.cpu.status & nes.cpu.get_value(FLAG.Z) > 0 ? GREEN : RED);
        print(x + 178, y, "C", nes.cpu.status & nes.cpu.get_value(FLAG.C) > 0 ? GREEN : RED);
        print(x, y + 16, "PC: $" + hex(nes.cpu.pc, 4));
        print(x, y + 32, "A:  $" + hex(nes.cpu.accum, 2) + "  [" + nes.cpu.accum + "]");
        print(x, y + 48, "X:  $" + hex(nes.cpu.reg_x, 2) + "  [" + nes.cpu.reg_x + "]");
        print(x, y + 64, "X:  $" + hex(nes.cpu.reg_y, 2) + "  [" + nes.cpu.reg_y + "]");
        print(x, y + 80, "Stack P: $" + hex(nes.cpu.stkp, 4));
    }

    private function draw_ram(x: Int, y: Int, addr: Int, rows: Int, cols: Int): Void {
        var ram_x: Int = x;
        var ram_y: Int = y;

        for (_ in 0...rows) {
            var display_row: String = "$" + hex(addr, 4) + ":";

            for (_ in 0...cols) {
                display_row += " " + hex(nes.cpuRead(addr, true), 2);
                addr += 1;
            }

            print(ram_x, ram_y, display_row);
            ram_y += 16;
        }
    }

    private function draw_code(x: Int, y: Int, lines: Int): Void {
        var pc = nes.cpu.pc;
        var line_y = (lines >> 1) * 16 + y;
        
        print(x, line_y, map_asm.get(pc), 0x00FFFF);
        while (line_y < (lines * 16) + y) {
            var inst = map_asm.get(++pc);
            if (inst != null) {
                line_y += 16;
                print(x, line_y, inst);
            } else {
                continue;
            }
        }

        var pc = nes.cpu.pc;
        line_y = (lines >> 1) * 16 + y;
        while (line_y > y) {
            var inst = map_asm.get(--pc);
            if (inst != null) {
                line_y -= 16;
                print(x, line_y, inst);
            } else {
                continue;
            }
        }
    }

    private function print(x: Int, y: Int, text: String, ?color: Int): Void {
        var tf = TextResource.make_text(this.back);
        tf.text = text;
        tf.x = x;
        tf.y = y;
        tf.textColor = color == null ? 0xFFFFFF : color;
    }

    private function hex(n: Int, d: Int): String {
        var s = [for (_ in 0...d) "0"];
        var i = d - 1;
        while (i >= 0) {
            s[i] = "0123456789ABCDEF".charAt(n & 0xF);
            n >>= 4;
            i--;
        }
        var out = s.join("");
        return out;
    }
}
