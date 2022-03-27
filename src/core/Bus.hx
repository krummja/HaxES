package core;

import haxe.ds.Vector;

class Bus
{
    public var ram(default, null): Vector<Int>;
    public var cpu(default, null): CPU;

    public function new(): Void {
        ram = new Vector(64 * 1024);
        this.cpu = new CPU();
        this.cpu.connectBus(this);
    }

    public function write(addr: Int, data: Int): Void {
        if (addr >= 0x0000 && addr <= 0xFFFF)
            ram[addr] = data;
    }
    
    public function read(addr: Int, ?readOnly: Bool = false): Int {
        if (addr >= 0x0000 && addr <= 0xFFFF)
            return ram[addr];
        return 0x00;
    }
}
