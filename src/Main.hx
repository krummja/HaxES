using Lambda;
using StringTools;
import core.Bus;

class Main 
{
    public static function main(): Void {
        var bus = new Bus();
        bus.ram.set(0x0000, 0x01);
        bus.ram.set(0x0001, 0x02);
        bus.ram.set(0x0002, 0x03);
        bus.ram.set(0xFFFC, 0x00);
        bus.ram.set(0xFFFD, 0x80);
        bus.cpu.reset();

        var i = 0;
        while (i < 1)
        {
            bus.cpu.clock();
            var output = bus.cpu.disassemble(0x0000, 0x0002);
            trace(output);
            i++;
        }
    }
}
