package core;

using haxe.UInt64;

class PPU
{
    private var cartridge(default, null): Cartridge;

    public function new(): Void {}

    public function connectCartridge(cartridge: Cartridge): Void {
        this.cartridge = cartridge;
    }

    public function clock(): Void {}

    @:allow(core.Bus)
    private function cpuRead(addr: Int, readOnly: Bool = false): Int { 
        var data: Int = 0x00;

        switch (addr) {
            case 0x0000:    // Control
            case 0x0001:    // Mask
            case 0x0002:    // Status
            case 0x0003:    // OAM Address
            case 0x0004:    // OAM Data
            case 0x0005:    // Scroll
            case 0x0006:    // PPU Address
            case 0x0007:    // PPU Data
        }

        return data;
     }

    @:allow(core.Bus)
    private function cpuWrite(addr: Int, data: Int): Void {
        switch (addr) {
            case 0x0000:    // Control
            case 0x0001:    // Mask
            case 0x0002:    // Status
            case 0x0003:    // OAM Address
            case 0x0004:    // OAM Data
            case 0x0005:    // Scroll
            case 0x0006:    // PPU Address
            case 0x0007:    // PPU Data
        }
    }

    private function ppuRead(addr: Int, readonly: Bool = false): Int { 
        var data: Int = 0x00;
        addr &= 0x3FFF;
        return data;
    }

    private function ppuWrite(addr: Int, data: Int): Void {
        addr &= 0x3FFF;
    }
}
