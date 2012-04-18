from __future__ import print_function

import re
import sys
import array

import parcon as p

BASIC_OPCODES = ['SET', 'ADD', 'SUB', 'MUL', 'DIV',
                 'MOD', 'SHL', 'SHR', 'AND', 'BOR', 
                 'XOR', 'IFE', 'IFN', 'IFG', 'IFB']
JSR_OPCODE = 'JSR'

REGISTERS = ['A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J']
SPECIAL_REGISTERS = ['POP', 'PEEK', 'PUSH', 'SP', 'PC', 'O']

HEXCHARS = '0123456789ABCDEFabcdef'

VALUE_NEXT_WORD_INDIRECT = 0x1e
VALUE_NEXT_WORD_LITERAL = 0x1f

class DCPU16AsmParser(object):
    def __init__(self):
        self._parser = self._build_parser()

    def parse(self, rawcode):
        return self._parser(rawcode)

    def _build_parser(self):

        def SignificantAnyCase(x):
            return p.Regex(re.compile(x, re.I))[lambda x: x.upper()]

        def FirstAnyCase(lst):
            return p.First([SignificantAnyCase(x) for x in lst])

        newline = p.First('\n', '\r\n')
        comment = p.Literal(';') + p.Exact((p.AnyChar() - newline)[...])[''.join]
        whitespace = p.CharIn(' \t') | comment

        identifier = ( p.Exact(p.Alpha() + p.Alphanum()[...]) )[p.flatten][''.join]

        hex_char = p.CharIn(HEXCHARS)
        hex_literal = ( '0x' + hex_char[1:4] )[''.join][lambda x: int(x, 16)]

        decimal_literal = (+p.Digit())[''.join][int]
        char_literal = ("'" + p.Alphanum() + "'")[ord]

        literal = hex_literal | decimal_literal | char_literal

        immediate = literal

        basic_opcode = FirstAnyCase(BASIC_OPCODES)
        jsr_opcode = SignificantAnyCase(JSR_OPCODE)

        register = p.Keyword(FirstAnyCase(REGISTERS), p.Not(p.Alphanum()))
        special_register = p.Keyword(FirstAnyCase(SPECIAL_REGISTERS), p.Not(p.Alphanum()))

        register_indirect = '[' + register + ']'
        register_offset = ((immediate['offset'] + '+' + register['register']) | (register['register'] + '+' + immediate['offset']))[dict]
        register_offset_indirect = '[' + register_offset + ']'

        immediate_indirect = '[' + immediate + ']'

        value = ( (register | special_register)['register']
                | identifier['identifier']
                | register_indirect['register_indirect']
                | register_offset_indirect['register_offset_indirect']
                | immediate['immediate']
                | immediate_indirect['immediate_indirect'])

        values = (value + ',' + value)

        label = ':' + identifier

        instruction = ( (basic_opcode + values) 
                      | (jsr_opcode + value))

        def dict_line(x):
            if x is None:
                return None
            if isinstance(x, p.Pair):
                return dict((x,))
            else:
                return dict(x)

        line = (-label['label'] + -instruction['instruction'])[dict_line] + newline

        program = line[...] + p.End()

        return lambda x: program.parse_string(x, whitespace=whitespace)

class DCPU16Assembler(object):
    def __init__(self, ast):
        self._ast = ast
        self._program = array.array('H')
        self._labels = {}
        self._label_references = []

    def assemble(self):
        for line in self._ast:
            if not line:
                continue

            if 'label' in line:
                self._labels[line['label']] = len(self._program)

            if 'instruction' in line:
                self._program.extend(self._assemble_instruction(line['instruction']))
        # Fix up label references.
        for label, loc in self._label_references:
            if label in self._labels:
                self._program[loc] = self._labels[label]
            else:
                raise StandardError('Undefined label "%s".' % (label,))
        return self._program

    def _assemble_instruction(self, instr):
        opcode = instr[0]
        values = instr[1:]
        if opcode in BASIC_OPCODES:
            o = BASIC_OPCODES.index(opcode) + 1
            a, aextend, alabel = self._encode_value(values[0])
            b, bextend, blabel = self._encode_value(values[1])
            self._program.append(b << 10 | a << 4 | o)

            if aextend:
                self._program.append(aextend)
                if alabel:
                    self._label_references.append((alabel, len(self._program)-1))

            if bextend:
                self._program.append(bextend)
                if blabel:
                    self._label_references.append((blabel, len(self._program)-1))

        elif opcode == 'JSR':
            o = 0x01
            a, aextend, alabel = self._encode_value(values[0])
            self._program.append(a << 10 | o << 4)

            if aextend:
                self._program.append(aextend)
                if alabel:
                    self._label_references.append((alabel, len(self._program)-1))

        return []

    def _encode_value(self, value):
        vtype = value[0]

        if vtype == 'register':
            reg = value[1]
            if reg in REGISTERS:
                return REGISTERS.index(reg), None, None
            elif reg in SPECIAL_REGISTERS:
                return SPECIAL_REGISTERS.index(reg) + 0x18, None, None
            else:
                raise StandardError('Unhandled register %s.' % (reg,))

        elif vtype == 'register_indirect':
            reg = value[1]
            if reg in REGISTERS:
                return 0x08 + REGISTERS.index(reg), None, None
            else:
                raise StandardError('Can only indirect basic registers.')

        elif vtype == 'register_offset_indirect':
            reg = value[1]['register']
            offset = value[1]['offset']
            if reg in REGISTERS:
                return 0x10 + REGISTERS.index(reg), offset, None
            else:
                raise StandardError('Can only offset basic registers.')

        elif vtype == 'identifier':
            return VALUE_NEXT_WORD_LITERAL, 0xdead, value[1]

        elif vtype == 'immediate_indirect':
            return VALUE_NEXT_WORD_INDIRECT, value[1], None

        elif vtype == 'immediate':
            immediate = value[1]
            if immediate < 0x1f:
                return 0x20 + immediate, None, None
            else:
                return VALUE_NEXT_WORD_LITERAL, value[1], None

        else:
            raise StandardError('Unhandled value type %s.' % (vtype,))


def main():
    assert len(sys.argv) > 1

    parser = DCPU16AsmParser()
    rawcode = file(sys.argv[1]).read()
    ast = parser.parse(rawcode)

    assembler = DCPU16Assembler(ast)
    program = assembler.assemble()

    program.byteswap()
    with file(sys.argv[2], 'wb+') as output:
        program.tofile(output)

    #program.extend([0] * (8 - len(program) % 8))
    #for i in range(len(program) / 8):
    #    print('%04x: ' % (i*8) + ' '.join('%04x' % d for d in program[i*8:(i+1)*8]))

def rawcode_to_ast(rawcode):
        return program.parse_string(rawcode, whitespace=whitespace)

if __name__ == '__main__':
    main()
