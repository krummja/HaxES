package core;


class Cartridge
{
    public function new(): Void {}

    private function cpuRead(addr: Int, readOnly: Bool = false): Int { return 0; }
    private function cpuWrite(addr: Int, data: Int): Void {}

    private function ppuRead(addr: Int, readonly: Bool = false): Int { return 0; }
    private function ppuWrite(addr: Int, data: Int): Void {}
}
