package core;

import haxe.ds.Vector;

class Bus
{
    public var cpuRam(default, null): Vector<Int>;
    public var cpu(default, null): CPU;
    public var ppu(default, null): PPU;
    public var cartridge(default, null): Cartridge;

    private var systemClockCounter: Int = 0;

    public function new(): Void {
        cpuRam = new Vector(2048);
        for (i in 0...2048)
            cpuRam[i] = 0x00;
        this.cpu = new CPU();
        this.cpu.connectBus(this);
    }

    public function cpuWrite(addr: Int, data: Int): Void {
        if (addr >= 0x0000 && addr <= 0x1FFF) {
            cpuRam[addr & 0x07FF] = data;
        }
        else if (addr >= 0x2000 && addr <= 0x3FFF) {
            ppu.cpuWrite(addr & 0x0007, data);
        }
    }
    
    public function cpuRead(addr: Int, ?readOnly: Bool = false): Int {
        var data: Int = 0x00;
        if (addr >= 0x0000 && addr <= 0x1FFF) {
            data = cpuRam[addr & 0x07FF];
        }
        else if (addr >= 0x2000 && addr <= 0x3FFF) {
            data = ppu.cpuRead(addr & 0x0007, readOnly);
        }
        return data;
    }

    public function insertCartridge(cartridge: Cartridge): Void {
        this.cartridge = cartridge;
        ppu.connectCartridge(cartridge);
    }

    public function reset(): Void {
        cpu.reset();
        systemClockCounter = 0;
    }

    public function clock(): Void {}
}
