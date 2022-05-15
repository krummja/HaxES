package core;

import view.Screen;
import sys.io.FileInput;
import macros.InitMacros;
import haxe.ds.Vector;
import haxe.ds.EnumValueMap;

enum FLAG { C; Z; I; D; B; U; V; N; }


typedef Instruction = {
    n: String,          // Name
    o: () -> Int,       // Operate
    m: () -> Int,       // Address Mode
    c: Int,             // Cycles
}

// Emulation of a 6502 Microprocessor
class CPU 
{
    public var fetched: Int = 0x00;
    public var temp: Int = 0x0000;
    public var addr_abs: Int = 0x0000;
    public var addr_rel: Int = 0x00;
    public var opcode: Int = 0x00;
    public var cycles: Int = 0;

    public var accum(default, default): Int = 0x00;      // Accumulator
    public var reg_x(default, default): Int = 0x00;      // X Register
    public var reg_y(default, default): Int = 0x00;      // Y Register
    public var stkp(default, default): Int = 0x00;       // Stack Pointer
    public var pc(default, default): Int = 0x0000;       // Program Counter
    public var status(default, default): Int = 0x00;     // Status
    
    private var bus: Bus;
    private var flags: EnumValueMap<FLAG, Int>;
    private var values: Map<FLAG, Int>;
    private var INSTRUCTION: Instruction;
    private var lookup: Vector<Instruction>;

    public function new(): Void {
        this.flags = [ 
            FLAG.C => (1 << 0),         // Carry Bit
            FLAG.Z => (1 << 1),         // Zero
            FLAG.I => (1 << 2),         // Disable Interrupts
            FLAG.D => (1 << 3),         // Decimal Mode
            FLAG.B => (1 << 4),         // Break
            FLAG.U => (1 << 5),         // Unused
            FLAG.V => (1 << 6),         // Overflow
            FLAG.N => (1 << 7),         // Negative
        ];

        var a = this;
        this.lookup = InitMacros.InitVector(
            { n: "BRK", o: a.BRK, m: a.IMM, c: 7 },{ n: "ORA", o: a.ORA, m: a.IZX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 3 },{ n: "ORA", o: a.ORA, m: a.ZP0, c: 3 },{ n: "ASL", o: a.ASL, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "PHP", o: a.PHP, m: a.IMP, c: 3 },{ n: "ORA", o: a.ORA, m: a.IMM, c: 2 },{ n: "ASL", o: a.ASL, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "ORA", o: a.ORA, m: a.ABS, c: 4 },{ n: "ASL", o: a.ASL, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BPL", o: a.BPL, m: a.REL, c: 2 },{ n: "ORA", o: a.ORA, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "ORA", o: a.ORA, m: a.ZPX, c: 4 },{ n: "ASL", o: a.ASL, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "CLC", o: a.CLC, m: a.IMP, c: 2 },{ n: "ORA", o: a.ORA, m: a.ABY, c: 4 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "ORA", o: a.ORA, m: a.ABX, c: 4 },{ n: "ASL", o: a.ASL, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },
            { n: "JSR", o: a.JSR, m: a.ABS, c: 6 },{ n: "AND", o: a.AND, m: a.IZX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "BIT", o: a.BIT, m: a.ZP0, c: 3 },{ n: "AND", o: a.AND, m: a.ZP0, c: 3 },{ n: "ROL", o: a.ROL, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "PLP", o: a.PLP, m: a.IMP, c: 4 },{ n: "AND", o: a.AND, m: a.IMM, c: 2 },{ n: "ROL", o: a.ROL, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "BIT", o: a.BIT, m: a.ABS, c: 4 },{ n: "AND", o: a.AND, m: a.ABS, c: 4 },{ n: "ROL", o: a.ROL, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BMI", o: a.BMI, m: a.REL, c: 2 },{ n: "AND", o: a.AND, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "AND", o: a.AND, m: a.ZPX, c: 4 },{ n: "ROL", o: a.ROL, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "SEC", o: a.SEC, m: a.IMP, c: 2 },{ n: "AND", o: a.AND, m: a.ABY, c: 4 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "AND", o: a.AND, m: a.ABX, c: 4 },{ n: "ROL", o: a.ROL, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },
            { n: "RTI", o: a.RTI, m: a.IMP, c: 6 },{ n: "EOR", o: a.EOR, m: a.IZX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 3 },{ n: "EOR", o: a.EOR, m: a.ZP0, c: 3 },{ n: "LSR", o: a.LSR, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "PHA", o: a.PHA, m: a.IMP, c: 3 },{ n: "EOR", o: a.EOR, m: a.IMM, c: 2 },{ n: "LSR", o: a.LSR, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "JMP", o: a.JMP, m: a.ABS, c: 3 },{ n: "EOR", o: a.EOR, m: a.ABS, c: 4 },{ n: "LSR", o: a.LSR, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BVC", o: a.BVC, m: a.REL, c: 2 },{ n: "EOR", o: a.EOR, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "EOR", o: a.EOR, m: a.ZPX, c: 4 },{ n: "LSR", o: a.LSR, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "CLI", o: a.CLI, m: a.IMP, c: 2 },{ n: "EOR", o: a.EOR, m: a.ABY, c: 4 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "EOR", o: a.EOR, m: a.ABX, c: 4 },{ n: "LSR", o: a.LSR, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },
            { n: "RTS", o: a.RTS, m: a.IMP, c: 6 },{ n: "ADC", o: a.ADC, m: a.IZX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 3 },{ n: "ADC", o: a.ADC, m: a.ZP0, c: 3 },{ n: "ROR", o: a.ROR, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "PLA", o: a.PLA, m: a.IMP, c: 4 },{ n: "ADC", o: a.ADC, m: a.IMM, c: 2 },{ n: "ROR", o: a.ROR, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "JMP", o: a.JMP, m: a.IND, c: 5 },{ n: "ADC", o: a.ADC, m: a.ABS, c: 4 },{ n: "ROR", o: a.ROR, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BVS", o: a.BVS, m: a.REL, c: 2 },{ n: "ADC", o: a.ADC, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "ADC", o: a.ADC, m: a.ZPX, c: 4 },{ n: "ROR", o: a.ROR, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "SEI", o: a.SEI, m: a.IMP, c: 2 },{ n: "ADC", o: a.ADC, m: a.ABY, c: 4 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "ADC", o: a.ADC, m: a.ABX, c: 4 },{ n: "ROR", o: a.ROR, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },
            { n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "STA", o: a.STA, m: a.IZX, c: 6 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "STY", o: a.STY, m: a.ZP0, c: 3 },{ n: "STA", o: a.STA, m: a.ZP0, c: 3 },{ n: "STX", o: a.STX, m: a.ZP0, c: 3 },{ n: "???", o: a.XXX, m: a.IMP, c: 3 },{ n: "DEY", o: a.DEY, m: a.IMP, c: 2 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "TXA", o: a.TXA, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "STY", o: a.STY, m: a.ABS, c: 4 },{ n: "STA", o: a.STA, m: a.ABS, c: 4 },{ n: "STX", o: a.STX, m: a.ABS, c: 4 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },
            { n: "BCC", o: a.BCC, m: a.REL, c: 2 },{ n: "STA", o: a.STA, m: a.IZY, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "STY", o: a.STY, m: a.ZPX, c: 4 },{ n: "STA", o: a.STA, m: a.ZPX, c: 4 },{ n: "STX", o: a.STX, m: a.ZPY, c: 4 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },{ n: "TYA", o: a.TYA, m: a.IMP, c: 2 },{ n: "STA", o: a.STA, m: a.ABY, c: 5 },{ n: "TXS", o: a.TXS, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "???", o: a.NOP, m: a.IMP, c: 5 },{ n: "STA", o: a.STA, m: a.ABX, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },
            { n: "LDY", o: a.LDY, m: a.IMM, c: 2 },{ n: "LDA", o: a.LDA, m: a.IZX, c: 6 },{ n: "LDX", o: a.LDX, m: a.IMM, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "LDY", o: a.LDY, m: a.ZP0, c: 3 },{ n: "LDA", o: a.LDA, m: a.ZP0, c: 3 },{ n: "LDX", o: a.LDX, m: a.ZP0, c: 3 },{ n: "???", o: a.XXX, m: a.IMP, c: 3 },{ n: "TAY", o: a.TAY, m: a.IMP, c: 2 },{ n: "LDA", o: a.LDA, m: a.IMM, c: 2 },{ n: "TAX", o: a.TAX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "LDY", o: a.LDY, m: a.ABS, c: 4 },{ n: "LDA", o: a.LDA, m: a.ABS, c: 4 },{ n: "LDX", o: a.LDX, m: a.ABS, c: 4 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },
            { n: "BCS", o: a.BCS, m: a.REL, c: 2 },{ n: "LDA", o: a.LDA, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "LDY", o: a.LDY, m: a.ZPX, c: 4 },{ n: "LDA", o: a.LDA, m: a.ZPX, c: 4 },{ n: "LDX", o: a.LDX, m: a.ZPY, c: 4 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },{ n: "CLV", o: a.CLV, m: a.IMP, c: 2 },{ n: "LDA", o: a.LDA, m: a.ABY, c: 4 },{ n: "TSX", o: a.TSX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },{ n: "LDY", o: a.LDY, m: a.ABX, c: 4 },{ n: "LDA", o: a.LDA, m: a.ABX, c: 4 },{ n: "LDX", o: a.LDX, m: a.ABY, c: 4 },{ n: "???", o: a.XXX, m: a.IMP, c: 4 },
            { n: "CPY", o: a.CPY, m: a.IMM, c: 2 },{ n: "CMP", o: a.CMP, m: a.IZX, c: 6 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "CPY", o: a.CPY, m: a.ZP0, c: 3 },{ n: "CMP", o: a.CMP, m: a.ZP0, c: 3 },{ n: "DEC", o: a.DEC, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "INY", o: a.INY, m: a.IMP, c: 2 },{ n: "CMP", o: a.CMP, m: a.IMM, c: 2 },{ n: "DEX", o: a.DEX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "CPY", o: a.CPY, m: a.ABS, c: 4 },{ n: "CMP", o: a.CMP, m: a.ABS, c: 4 },{ n: "DEC", o: a.DEC, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BNE", o: a.BNE, m: a.REL, c: 2 },{ n: "CMP", o: a.CMP, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "CMP", o: a.CMP, m: a.ZPX, c: 4 },{ n: "DEC", o: a.DEC, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "CLD", o: a.CLD, m: a.IMP, c: 2 },{ n: "CMP", o: a.CMP, m: a.ABY, c: 4 },{ n: "NOP", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "CMP", o: a.CMP, m: a.ABX, c: 4 },{ n: "DEC", o: a.DEC, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },
            { n: "CPX", o: a.CPX, m: a.IMM, c: 2 },{ n: "SBC", o: a.SBC, m: a.IZX, c: 6 },{ n: "???", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "CPX", o: a.CPX, m: a.ZP0, c: 3 },{ n: "SBC", o: a.SBC, m: a.ZP0, c: 3 },{ n: "INC", o: a.INC, m: a.ZP0, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 5 },{ n: "INX", o: a.INX, m: a.IMP, c: 2 },{ n: "SBC", o: a.SBC, m: a.IMM, c: 2 },{ n: "NOP", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.SBC, m: a.IMP, c: 2 },{ n: "CPX", o: a.CPX, m: a.ABS, c: 4 },{ n: "SBC", o: a.SBC, m: a.ABS, c: 4 },{ n: "INC", o: a.INC, m: a.ABS, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },
            { n: "BEQ", o: a.BEQ, m: a.REL, c: 2 },{ n: "SBC", o: a.SBC, m: a.IZY, c: 5 },{ n: "???", o: a.XXX, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 8 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "SBC", o: a.SBC, m: a.ZPX, c: 4 },{ n: "INC", o: a.INC, m: a.ZPX, c: 6 },{ n: "???", o: a.XXX, m: a.IMP, c: 6 },{ n: "SED", o: a.SED, m: a.IMP, c: 2 },{ n: "SBC", o: a.SBC, m: a.ABY, c: 4 },{ n: "NOP", o: a.NOP, m: a.IMP, c: 2 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 },{ n: "???", o: a.NOP, m: a.IMP, c: 4 },{ n: "SBC", o: a.SBC, m: a.ABX, c: 4 },{ n: "INC", o: a.INC, m: a.ABX, c: 7 },{ n: "???", o: a.XXX, m: a.IMP, c: 7 }
        );
    }

    public function complete(): Bool {
        return cycles == 0;
    }

    // ADDRESSING MODES

    // Implied
    public function IMP(): Int { 
        fetched = accum;
        return 0; 
    }

    // Immediate
    public function IMM(): Int { 
        addr_abs = pc++;
        return 0; 
    }

    // Zero-Page Addressing
    public function ZP0(): Int {
        addr_abs = read(pc);
        pc++;
        addr_abs &= 0x00FF;
        return 0; 
    }

    // Zero-Page Addressing with X Register Offset
    public function ZPX(): Int { 
        addr_abs = (read(pc) + reg_x);
        pc++;
        addr_abs &= 0x00FF;
        return 0; 
    }

    // Zero-Page Addressing with Y Register Offset
    public function ZPY(): Int { 
        addr_abs = (read(pc) + reg_y);
        pc++;
        addr_abs &= 0x00FF;
        return 0; 
    }

    public function REL(): Int { 
        addr_rel = read(pc);
        pc++;
        if ((addr_rel & 0x80) > 0)
            addr_rel |= 0xFF00;
        return 0; 
    }

    public function ABS(): Int { 
        var lo = read(pc);
        pc++;
        var hi = read(pc);
        pc++;
        addr_abs = (hi << 8) | lo;
        return 0; 
    }

    public function ABX(): Int { 
        var lo = read(pc);
        pc++;
        var hi = read(pc);
        pc++;

        addr_abs = (hi << 8) | lo;
        addr_abs += reg_x;
        
        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0; 
    }

    public function ABY(): Int { 
        var lo = read(pc);
        pc++;
        var hi = read(pc);
        pc++;

        addr_abs = (hi << 8) | lo;
        addr_abs += reg_y;
        
        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0; 
    }

    // Indirect Addressing (Pointer-like implementation)
    public function IND(): Int { 
        var ptr_lo = read(pc);
        pc++;
        var ptr_hi = read(pc);
        pc++;

        var ptr = (ptr_hi << 8) | ptr_lo;

        if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
            addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
        else // Behave normally
            addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
        
        return 0; 
    }

    public function IZX(): Int { 
        var t = read(pc);
        pc++;

        var lo = read(t + reg_x & 0x00FF);
        var hi = read(t + reg_x + 1 & 0x00FF);

        addr_abs = (hi << 8) | lo;
        return 0;
    }

    public function IZY(): Int { 
        var t = read(pc);
        pc++;

        var lo = read(t & 0x00FF);
        var hi = read((t + 1) & 0x00FF);

        addr_abs = (hi << 8) | lo;
        addr_abs += reg_y;

        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0;
    }

    // OPCODES

    // Addition
    public function ADC(): Int { 
        // Grab the data that we are adding to the accumulator
        fetch();

        // Add is performed in 16-bit domain for emulation to capture any 
        // carry bit, which will existing in bit 8 of the 16-bit word
        temp = accum + fetched + get_flag(C);
        set_flag(C, temp > 255);
        set_flag(Z, (temp & 0x00FF) == 0);
        set_flag(N, cast(temp & 0x80));
        set_flag(V, cast((~(accum ^ fetched) & (accum ^ temp)) & 0x0080));
        accum = temp & 0x00FF;
        return 1; 
    }

    // Instruction: Bitwise Logic AND
    // Function:    A = A & M
    // Flags Out:   N, Z
    public function AND(): Int {
        fetch();
        accum = accum & fetched;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 1; 
    }

    // Instruction: Arithmetic Shift Left
    // Function:    A = C <- (A << 1) <- 0
    // Flags Out:   N, Z, C
    public function ASL(): Int { 
        fetch();
        temp = fetched << 1;
        set_flag(C, (temp & 0xFF00) > 0);
        set_flag(Z, (temp & 0x00FF) == 0x00);
        set_flag(N, cast(temp & 0x80));
        if (lookup[opcode].m == IMP)
            accum = temp & 0x00FF;
        else
            write(addr_abs, cast(temp & 0x00FF));
        return 0; 
    }

    // Instruction: Branch if Carry Clear
    // Function:    if (C == 0) pc = address
    public function BCC(): Int { 
        if (get_flag(C) == 0) 
            {
                cycles++;
                addr_abs = pc + addr_rel;
    
                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                    cycles ++;
    
                pc = addr_abs;
            }
        return 0; 
    }

    // Instruction: Branch if Carry Set
    // Function:    if (C == 1) pc = address
    public function BCS(): Int { 
        if (get_flag(C) == 1) 
        {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles ++;

            pc = addr_abs;
        }
        return 0; 
    }

    // Instruction: Branch if Equal
    // Function:    if (Z == 1) pc = address
    public function BEQ(): Int { 
        if (get_flag(Z) == 1) {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            
            pc = addr_abs;
        }
        return 0; 
    }

    public function BIT(): Int { 
        fetch();
        temp = accum & fetched;
        set_flag(Z, (temp & 0x00FF) == 0x00);
        set_flag(N, cast(fetched & (1 << 7)));
        set_flag(V, cast(fetched & (1 << 6)));
        return 0; 
    }

    // Instruction: Branch if Negative
    // Function:    if (N == 1) pc = address
    public function BMI(): Int { 
        if (get_flag(N) == 1) {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            
            pc = addr_abs;
        }
        return 0; 
    }

    // Instruction: Branch if Not Equal
    // Function:    if (Z == 0) pc = address
    public function BNE(): Int { 
        if (get_flag(Z) == 0) {
            cycles++;
            
            var signed = pc + addr_rel;
            addr_abs = signed & 0xFFFF;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            
            pc = addr_abs;
        }
        return 0; 
    }

    // Instruction: Branch if Positive
    // Function:    if (N == 0) pc = address
    public function BPL(): Int { 
        if (get_flag(N) == 0) {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            
            pc = addr_abs;
        }
        return 0; 
    }

    // Instruction: Break
    // Function:    Program Sourced Interrupt
    public function BRK(): Int { 
        pc++;

        set_flag(I, true);
        write(0x0100 + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(0x0100 + stkp, pc & 0x00FF);
        stkp--;

        set_flag(B, true);
        write(0x0100 + stkp, status);
        stkp--;
        set_flag(B, false);

        pc = read(0xFFFE) | (read(0xFFFF) << 8);
        return 0; 
    }

    // Instruction: Branch if Overflow Clear
    // Function:    if (V == 0) pc = address
    public function BVC(): Int { 
        if (get_flag(V) == 0) {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            pc = addr_abs;
        }
        return 0; 
    }

    // Instruction: Branch if Overflow Set
    // Function:    if (V == 1) pc = address
    public function BVS(): Int { 
        if (get_flag(V) == 1) {
            cycles++;
            addr_abs = pc + addr_rel;

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;
            pc = addr_abs;
        }
        return 0; 
    }
    
    // Instruction: Clear Carry Flag
    // Function:    C = 0
    public function CLC(): Int { 
        set_flag(C, false);
        return 0; 
    }

    // Instruction: Clear Decimal Flag
    // Function:    D = 0
    public function CLD(): Int { 
        set_flag(D, false);
        return 0; 
    }

    // Instruction: Clear Interrupt Flag
    // Function:    I = 0
    public function CLI(): Int { 
        set_flag(I, false);
        return 0; 
    }

    // Instruction: Clear Overflow Flag
    // Function:    V = 0
    public function CLV(): Int { 
        set_flag(V, false);
        return 0; 
    }

    // Instruction: Compare Accumulator
    // Function:    C <- A >= M     Z <- (A - M) == 0
    // Flags Out:   N, C, Z
    public function CMP(): Int { 
        fetch();
        temp = accum - fetched;
        set_flag(C, accum >= fetched);
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x0080));
        return 1; 
    }

    // Instruction: Compare X Register
    // Function:    C <- X >= M     Z <- (X - M) == 0
    // Flags Out:   N, C, Z
    public function CPX(): Int { 
        fetch();
        temp = reg_x - fetched;
        set_flag(C, reg_x >= fetched);
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x080));
        return 0; 
    }

    // Instruction: Compare Y Register
    // Function:    C <- Y >= M     Z <- (Y - M) == 0
    // Flags Out:   N, C, Z
    public function CPY(): Int { 
        fetch();
        temp = reg_x - fetched;
        set_flag(C, reg_x >= fetched);
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x080));
        return 0; 
    }

    // Instruction: Decrement Value at Memory Location
    // Function:    M = M - 1
    // Flags Out:   N, Z
    public function DEC(): Int { 
        fetch();
        temp = fetched - 1;
        write(addr_abs, temp & 0x00FF);
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x0080));
        return 0; 
    }

    // Instruction: Decrement X Register
    // Function:    X = X - 1
    // Flags Out:   N, Z
    public function DEX(): Int { 
        reg_x--;
        set_flag(Z, reg_x == 0x00);
        set_flag(N, cast(reg_x & 0x80));
        return 0; 
    }

    // Instruction: Decrement Y Register
    // Function:    Y = Y - 1
    // Flags Out:   N, Z
    public function DEY(): Int { 
        reg_y--;
        set_flag(Z, reg_y == 0x00);
        set_flag(N, cast(reg_y & 0x80));
        return 0; 
    }

    // Instruction: Bitwise Logic XOR
    // Function:    A = A ^ M
    // Flags Out:   N, Z
    public function EOR(): Int { 
        fetch();
        accum = accum ^ fetched;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 1; 
    }

    // Instruction: Increment Value at Memory Location
    // Function:    M = M + 1
    // Flags Out:   N, Z
    public function INC(): Int { 
        fetch();
        temp = fetched + 1;
        write(addr_abs, cast(temp & 0x00FF));
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x0080));
        return 0; 
    }

    // Instruction: Increment X Register
    // Function:    X = X + 1
    // Flags Out:   N, Z
    public function INX(): Int { 
        reg_x++;
        set_flag(Z, reg_x == 0x00);
        set_flag(N, cast(reg_x & 0x80));
        return 0;
    }

    // Instruction: Increment Y Register
    // Function:    Y = Y + 1
    // Flags Out:   N, Z
    public function INY(): Int { 
        reg_y++;
        set_flag(Z, reg_y == 0x00);
        set_flag(N, cast(reg_y & 0x80));
        return 0;
    }

    // Instruction: Jump to Location
    // Function:    pc = address
    public function JMP(): Int { 
        pc = addr_abs;
        return 0; 
    }

    // Instruction: Jump to Sub-Routine
    // Function:    Push current pc to stack, pc = address
    public function JSR(): Int { 
        pc--;

        write(0x0100 + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(0x0100 + stkp, pc & 0x00FF);
        stkp--;

        pc = addr_abs;
        return 0; 
    }

    // Instruction: Load the Accumulator
    // Function:    A = M
    // Flags Out:   N, Z
    public function LDA(): Int { 
        fetch();
        accum = fetched;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 1; 
    }

    // Instruction: Load the X Register
    // Function:    X = M
    // Flags Out:   N, Z
    public function LDX(): Int { 
        fetch();
        reg_x = fetched;
        set_flag(Z, reg_x == 0x00);
        set_flag(N, cast(reg_x & 0x80));
        return 1; 
    }

    // Instruction: Load the Y Register
    // Function:    Y = M
    // Flags Out:   N, Z
    public function LDY(): Int { 
        fetch();
        reg_y = fetched;
        set_flag(Z, reg_y == 0x00);
        set_flag(N, cast(reg_y & 0x80));
        return 1; 
    }

    public function LSR(): Int { 
        fetch();
        set_flag(C, cast(fetched & 0x0001));
        temp = fetched >> 1;
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x0080));
        if (lookup[opcode].m == IMP)
            accum = temp & 0x00FF;
        else
            write(addr_abs, temp & 0x00FF);
        return 0;
    }

    public function NOP(): Int { 
        switch (opcode) {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
                return 1;
        }
        return 0; 
    }

    // Instruction: Bitwise Logic OR
    // Function:    A = A | M
    // Flags Out:   N, Z
    public function ORA(): Int { 
        fetch();
        accum = accum | fetched;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 1; 
    }

    // Instruction: Push Accumulator to Stack
    // Function:    A -> Stack
    public function PHA(): Int { 
        write(0x0100 + stkp, accum);
        stkp--;
        return 0; 
    }

    // Instruction: Push Status Register to Stack
    // Function:    status -> stack
    // Flags Out:   Break flag is set to 1 before push
    public function PHP(): Int { 
        write(0x0100 + stkp, status | cast(B, Int) | cast(U, Int));
        set_flag(B, false);
        set_flag(U, false);
        stkp--;
        return 0;
    }

    // Instruction: Pop Accumulator off Stack
    // Function:    A <- stack
    // Flags Out:   N, Z
    public function PLA(): Int { 
        stkp++;
        accum = read(0x0100 + stkp);
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 0; 
    }

    // Instruction: Pop Status Register off Stack
    // Function:    status <- stack
    public function PLP(): Int { 
        stkp++;
        status = read(0x0100 + stkp);
        set_flag(U, true);
        return 0; 
    }

    public function ROL(): Int { 
        fetch();
        temp = (fetched << 1) | get_flag(C);
        set_flag(C, cast(temp & 0xFF00));
        set_flag(Z, (temp & 0x00FF) == 0x0000);
        set_flag(N, cast(temp & 0x0080));
        if (lookup[opcode].m == IMP)
            accum = temp & 0x00FF;
        else
            write(addr_abs, temp & 0x00FF);
        return 0; 
    }

    public function ROR(): Int { 
        fetch();
        temp = (get_flag(C) << 7) | (fetched >> 1);
        set_flag(C, cast(fetched & 0x01));
        set_flag(Z, (temp & 0x00FF) == 0x00);
        set_flag(N, cast(temp & 0x0080));
        if (lookup[opcode].m == IMP)
            accum = temp & 0x00FF;
        else
            write(addr_abs, temp & 0x00FF);
        return 0; 
    }

    // Return from Interrupt
    public function RTI(): Int { 
        stkp++;
        status = read(0x0100 + stkp);
        status &= ~cast(B);
        status &= ~cast(U);

        stkp++;
        pc = read(0x0100 + stkp);
        stkp++;
        pc |= read(0x0100 + stkp) << 8;
        return 0; 
    }

    public function RTS(): Int { 
        stkp++;
        pc = read(0x0100 + stkp);
        stkp++;
        pc |= read(0x0100 + stkp) << 8;

        pc++;
        return 0; 
    }

    // Subtraction
    public function SBC(): Int { 
        fetch();
        var value = fetched ^ 0x00FF;
        temp = accum + value + get_flag(C);
        set_flag(C, cast(temp & 0xFF00));
        set_flag(V, cast((temp ^ accum) & (temp ^ value) & 0x0080));
        set_flag(N, cast(temp & 0x0080));
        accum = temp & 0x00FF;
        return 1;
    }

    // Instruction: Set Carry Flag
    // Function:    C = 1
    public function SEC(): Int { 
        set_flag(C, true);
        return 0; 
    }

    // Instruction: Set Decimal Flag
    // Function:    D = 1
    public function SED(): Int { 
        set_flag(D, true);
        return 0; 
    }

    // Instruction: Set Interrupt Flag
    // Function:    I = 1
    public function SEI(): Int {
        set_flag(I, true);
        return 0; 
    }

    // Instruction: Store Accumulator at Address
    // Function:    M = A
    public function STA(): Int { 
        write(addr_abs, accum);
        return 0; 
    }

    // Instruction: Store X Register at Address
    // Function:    M = X
    public function STX(): Int { 
        write(addr_abs, reg_x);
        return 0; 
    }

    // Instruction: Store Y Register at Address
    // Function:    M = Y
    public function STY(): Int { 
        write(addr_abs, reg_y);
        return 0; 
    }

    // Instruction: Transfer Accumulator to X Register
    // Function:    X = A
    // Flags Out:   N, Z
    public function TAX(): Int { 
        reg_x = accum;
        set_flag(Z, reg_x == 0x00);
        set_flag(N, cast(reg_x & 0x80));
        return 0; 
    }

    // Instruction: Transfer Accumulator to Y Register
    // Function:    Y = A
    // Flags Out:   N, Z
    public function TAY(): Int { 
        reg_y = accum;
        set_flag(Z, reg_y == 0x00);
        set_flag(N, cast(reg_y & 0x80));
        return 0; 
    }

    // Instruction: Transfer Stack Pointer to X Register
    // Function:    X = stack pointer
    // Flags Out:   N, Z
    public function TSX(): Int { 
        reg_x = stkp;
        set_flag(Z, reg_x == 0x00);
        set_flag(N, cast(reg_x & 0x80));
        return 0; 
    }

    // Instruction: Transfer X Register to Accumulator
    // Function:    A = X
    // Flags Out:   N, Z
    public function TXA(): Int { 
        accum = reg_x;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 0; 
    }

    // Instruction: Transfer X Register to Stack Pointer
    // Function:    stack pointer = X
    public function TXS(): Int { 
        stkp = reg_x;
        return 0; 
    }

    // Instruction: Transfer Y Register to Accumulator
    // Function:    A = Y
    // Flags Out:   N, Z
    public function TYA(): Int { 
        accum = reg_y;
        set_flag(Z, accum == 0x00);
        set_flag(N, cast(accum & 0x80));
        return 0; 
    }

    // This function captures illegal opcodes
    public function XXX(): Int { 
        return 0; 
    }

    public function clock(): Void {
        if (this.cycles == 0) {
            opcode = read(pc);
            set_flag(U, true);
            pc++;

            // Get starting number of cycles
            cycles = lookup[opcode].c;
            var additional_cycle1 = lookup[opcode].m();
            var additional_cycle2 = lookup[opcode].o();
            
            cycles += (additional_cycle1 & additional_cycle2);
            set_flag(U, true);
        }

        cycles--;
        // trace(this.cycles);
    }

    public function reset(): Void {
        // Get address to set program counter to
        addr_abs = 0xFFFC;
        var lo = read(addr_abs + 0);
        var hi = read(addr_abs + 1);

        // Set it
        pc = (hi << 8) | lo;

        // Reset internal registers
        accum = 0;
        reg_x = 0;
        reg_y = 0;
        stkp = 0xFD;
        status = 0x00;

        // Clear internal helper variables
        addr_rel = 0x0000;
        addr_abs = 0x0000;
        fetched = 0x00;

        // Reset takes time
        cycles = 8;
    }

    public function irq(): Void {
        if (get_flag(I) == 0) {
            write(0x0100 + stkp, (pc >> 8) & 0x00FF);
            stkp--;
            write(0x0100 + stkp, cast(pc & 0x00FF));
            stkp--;

            set_flag(B, false);
            set_flag(U, true);
            set_flag(I, true);
            write(0x0100 + stkp, status);
            stkp--;

            addr_abs = 0xFFFE;
            var lo = read(addr_abs + 0);
            var hi = read(addr_abs + 1);
            pc = (hi << 8) | lo;

            cycles = 7;
        }
    }

    public function nmi(): Void {
        write(0x0100 + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(0x0100 + stkp, cast(pc & 0x00FF));
        stkp--;

        set_flag(B, false);
        set_flag(U, true);
        set_flag(I, true);
        write(0x0100 + stkp, status);
        stkp--;

        addr_abs = 0xFFFE;
        var lo = read(addr_abs + 0);
        var hi = read(addr_abs + 1);
        pc = (hi << 8) | lo;

        cycles = 8;
    }

    // INTERNALS

    private function fetch(): Int { 
        if (!(lookup[opcode].m == IMP))
            fetched = read(addr_abs);
        return fetched;
    }

    @:allow(core.Bus)
    private function connectBus(bus: Bus): Void {
        this.bus = bus;
    }

    private function write(addr: Int, data: Int): Void {
        this.bus.cpuWrite(addr, data);
    }

    private function read(addr: Int): Int { 
        return this.bus.cpuRead(addr, false);
    }

    @:allow(view.Screen)
    private function get_value(flag: FLAG): Int {
        return flags.get(flag);
    }

    private function get_flag(flag: FLAG): Int {
        return ((status & flags.get(flag)) > 0) ? 1 : 0;
    }

    private function set_flag(flag: FLAG, v: Bool): Void {
        if (v == true) {
            status |= flags.get(flag);
        } else {
            status &= ~flags.get(flag);
        }
    }

    @:allow(view.Screen)
    private function disassemble(start: Int, stop: Int): Map<Int, String>
    {
        var addr = start;
        var value = 0x00;
        var lo = 0x00;
        var hi = 0x00;
        var map_lines = new Map<Int, String>();
        var line_addr = 0;

        function hex(n: Int, d: Int): String {
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

        while (addr <= stop) {
            line_addr = addr;
            var inst = "$" + hex(addr, 4) + ": ";
            var opcode = bus.cpuRead(addr, true);
            addr++;
            inst += lookup[opcode].n + " ";

            if (lookup[opcode].m == IMP) {
                inst += " {IMP}";
            }
            else if (lookup[opcode].m == IMM) {
                value = bus.cpuRead(addr, true); addr++;
                inst += "#$" + hex(value, 2) + " {IMM}";
            }
            else if (lookup[opcode].m == ZP0) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = 0x00;
                inst += "$" + hex(lo, 2) + " {ZP0}";
            }
            else if (lookup[opcode].m == ZPX) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = 0x00;
                inst += "$" + hex(lo, 2) + ", X {ZPX}";
            }
            else if (lookup[opcode].m == ZPY) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = 0x00;
                inst += "$" + hex(lo, 2) + ", Y {ZPY}";
            }
            else if (lookup[opcode].m == IZX) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = 0x00;
                inst += "$" + hex(lo, 2) + ", X) {IZX}";
            }
            else if (lookup[opcode].m == IZY) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = 0x00;
                inst += "$" + hex(lo, 2) + ", Y) {IZY}";
            }
            else if (lookup[opcode].m == ABS) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = bus.cpuRead(addr, true); addr++;
                inst += "$" + hex((hi << 8) | lo, 4) + " {ABS}";
            }
            else if (lookup[opcode].m == ABX) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = bus.cpuRead(addr, true); addr++;
                inst += "$" + hex((hi << 8) | lo, 4) + ", X {ABX}";
            }
            else if (lookup[opcode].m == ABY) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = bus.cpuRead(addr, true); addr++;
                inst += "$" + hex((hi << 8) | lo, 4) + ", Y {ABY}";
            }
            else if (lookup[opcode].m == IND) {
                lo = bus.cpuRead(addr, true); addr++;
                hi = bus.cpuRead(addr, true); addr++;
                inst += "($" + hex((hi << 8) | lo, 4) + ") {IND}";
            }
            else if (lookup[opcode].m == REL) {
                value = bus.cpuRead(addr, true); addr++;
                inst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
            }

            map_lines[line_addr] = inst;
        }

        return map_lines;
    }
}

// End of File
