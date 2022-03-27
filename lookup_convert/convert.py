
import os
import re

path = os.path.relpath("./")
target = os.path.join(path, "convert.md")
outfile = os.path.join(path, "outfile.md")

def parse():
    with open(target) as file:
        lines = [line for line in file]
        symbols = [line.split(",") for line in lines]
        symbols = [[sym.strip("}{ ") for sym in arr] for arr in symbols]
        
        groups = []
        for arr in symbols:
            groups += arr

        for i, sym in enumerate(groups):
            if sym == "\n":
                groups.pop(i)
                
        output = []
        
        for i in range(0, len(groups)-1, 4):
            output.append(groups[i:i+4])
            
        return output
    

def assemble_instruction(output):
    instructions = ""
    
    i = 0
    for instruction in output:
        name = instruction[0]
        operate = instruction[1]
        mode = instruction[2]
        cycles = instruction[3]
        
        istr = ''
        istr += '{ '
        istr += f'n: {name}, '
        istr += f'o: {operate}, '
        istr += f'm: {mode}, '
        istr += f'c: {cycles} '
        istr += ' },'
        
        if i == 15:
            istr += '\n'
            i = 0
        else:
            i += 1
            
        instructions += istr

    return instructions


if __name__ == '__main__':
    
        output = parse()
        instructions = assemble_instruction(output)
        with open(outfile, "x") as file:
            file.write(instructions)
        