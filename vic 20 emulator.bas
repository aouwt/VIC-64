'my (very ambitious) vic-20 emulator....

TYPE flagType
    c AS _UNSIGNED _BYTE 'Carry
    z AS _UNSIGNED _BYTE 'Zero
    i AS _UNSIGNED _BYTE 'interrupt disable
    d AS _UNSIGNED _BYTE 'decimal mode
    b AS _UNSIGNED _BYTE 'break
    __unused AS _UNSIGNED _BYTE
    v AS _UNSIGNED _BYTE 'overflow
    s AS _UNSIGNED _BYTE 'sign
END TYPE

TYPE regType
    pgCount AS _UNSIGNED INTEGER
    stk AS _UNSIGNED _BYTE
    acc AS _UNSIGNED _BYTE
    iX AS _UNSIGNED _BYTE
    iY AS _UNSIGNED _BYTE
    f AS flagType
END TYPE

DIM mem(65535) AS _UNSIGNED _BYTE, reg AS regType, I AS _UNSIGNED INTEGER
CONST base64CharSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", Pi2 = 6.283185307179586
CONST clock6502 = 1020000 '1.02 mhz=1020000 hz
PRINT "Launching initKernal..."
reg.f.__unused = 1: reg.stk = &HFF


REM Memory map:

REM |Key========================================================================|
REM |? 'I don't understand (yet)                                                |
REM |C 'Cartridge decoded (according to Wikipedia; I don't understand this also)|
REM |V 'Can change locations                                                    |
REM |R 'Read-only                                                               |
REM |===========================================================================|

REM ?   0000-03FF: 'Jump vectors, etc.     (BASIC working memory)
CONST mem_stk = &H0100
REM C   0400-0FFF: 'Expansion
REM V   1000-1DFF: 'User BASIC memory      (for unexpanded/3k expansion, otherwise screen memory)
REM V   1E00-1FFF: 'Screen memory          (for unexpanded/3k expansion, otherwise user BASIC memory)

REM C   2000-3FFF: 'Expansion blk 1

REM C   4000-5FFF: 'Expansion blk 2

REM C   6000-7FFF: 'Expansion blk 3

REM ?   8000-8FFF: 'ROM Character bitmap
CONST mem_chr = -&H8000

REM     9000-93FF: 'I/O blk 0
REM       9000-900F: 'VIC control registers
CONST mem_vic1 = -&H900A, mem_vic2 = -&H900B, mem_vic3 = -&H900C, mem_vicNoise = -&H900D, mem_vicVol = -&H900E
REM   ?   9110-912F: 'VIA chips I/O control registers
REM V   9400-95FF: 'Color RAM (if expansion at blk 1)
REM V   9600-97FF: 'Color RAM normally
REM C   9800-9BFF: 'I/O blk 2
REM C   9C00-9FFF: 'I/O blk 3

REM CR  A000-BFFF: 'Expansion ROM

REM R   C000-DFFF: 'BASIC ROM
CONST mem_BASIC = -&HC000

REM R   E000-FFFF: 'Kernal ROM
CONST mem_kernal = -&HE000

initIntROMs
CLS
emulate6502


'$INCLUDE:'vic 20 emu data.bas'

SUB emulate6502
    SHARED reg AS regType, I AS _UNSIGNED INTEGER
    I = mem_kernal + 1
    DO
        'CLS
        SELECT CASE getB(I - 1)
            CASE &H00 'BREAK! (BRK)
                'wt 7
                'PRINT "BRK @ $"; HEX$(I); ", press any key to continue"
                'SLEEP

                'Add memory to acc w/carry (ADC)
            CASE &H69 'Immeadiate           'haha 69 funny number
                reg.acc = c8(getB(I) + reg.acc): I = I + 1
                wt 2
            CASE &H65 'Zero Page
                reg.acc = c8(zPg + reg.acc)
                wt 3
            CASE &H75 'Zero Page Indexed x
                reg.acc = c8(zPgX + reg.acc)
                wt 4
            CASE &H60 'Absolute
                reg.acc = c8(ab + reg.acc)
                wt 4
            CASE &H70 'Absolute indexed x
                reg.acc = chk8(abX + reg.acc)
                wt 4
            CASE &H79 'Absolute indexed y
                reg.acc = chk8(abY + reg.acc)
                wt 4
            CASE &H61 'Indirect indexed x
                reg.acc = c8(indX + reg.acc)
                wt 6
            CASE &H71 'indirect indexed y
                reg.acc = chk8(indY + reg.acc)
                wt 5


                'AND memory with acc (AND)
            CASE &H29
                reg.acc = cZ(reg.acc AND getB(I)): I = I + 1
                wt 2
            CASE &H25
                reg.acc = cZ(reg.acc AND zPg)
                wt 3
            CASE &H35
                reg.acc = cZ(reg.acc AND zPgX)
                wt 4
            CASE &H2D
                reg.acc = cZ(reg.acc AND ab)
                wt 4
            CASE &H3D
                reg.acc = chk8(reg.acc AND abX)
                wt 4
            CASE &H39
                reg.acc = chk8(reg.acc AND abY)
                wt 4
            CASE &H21
                reg.acc = reg.acc AND indX
                wt 6
            CASE &H31
                reg.acc = reg.acc AND indY
                wt 5


                'Shift left one bit (ASL)
            CASE &H0A
                reg.acc = ASL(reg.acc)
                wt 2
            CASE &H06
                reg.acc = ASL(zPg)
                wt 5
            CASE &H16
                reg.acc = ASL(zPgX)
                wt 6
            CASE &H0E
                reg.acc = ASL(ab)
                wt 6
            CASE &H1E
                reg.acc = ASL(abX)
                wt 7

                '...

            CASE &H18 'Clear carry bit (CLC)
                reg.f.c = 0
                wt 2
            CASE &HD8 'Clear Decimal mode (CLD)
                reg.f.d = 0
                wt 2
            CASE &H58 'clear interrupt disable (CLI)
                reg.f.i = 0
                wt 2
            CASE &HB8 'clear overflow (CLO)
                reg.f.v = 0
                wt 2

                '...

                'Decrement memory (DEC)
            CASE &HC6 'zero page
                a~% = getB(I)
                putB a~%, getB(a~%) - 1
                wt 5
                I = I + 1
            CASE &HD6 'zero page x
                a~% = sZPgX
                putB a~%, getB(a~%) - 1
                wt 6
            CASE &HCE 'absolute     'HEE
                a~% = sAb
                putB a~%, getB(a~%) - 1
                wt 6
            CASE &HDE 'absolute X
                a~% = sAbX
                putB a~%, getB(a~%) - 1
                wt 7


            CASE &HCA 'Decrement index X (DEX)
                reg.iX = reg.iX - 1
                wt 2
            CASE &H88 'decrement index y (DEY)
                reg.iY = reg.iY - 1
                wt 2


                'XOR memory (EOR)
            CASE &H49
                reg.acc = reg.acc XOR getB(I)
                wt 2
                I = I + 1
            CASE &H45
                reg.acc = reg.acc XOR zPg
                wt 3
            CASE &H55
                reg.acc = reg.acc XOR zPgX
                wt 4
            CASE &H40
                reg.acc = reg.acc XOR ab
                wt 4
            CASE &H50
                reg.acc = chk8(reg.acc XOR abX)
                wt 4
            CASE &H59
                reg.acc = chk8(reg.acc XOR abY)
                wt 4
            CASE &H41
                reg.acc = reg.acc XOR indX
                wt 6
            CASE &H51
                reg.acc = chk8(reg.acc XOR indY)
                wt 5


                'incriment memory  (INC)
            CASE &HE6 'zero page
                a~% = getB(I)
                putB a~%, getB(a~%) + 1
                wt 5
                I = I + 1
            CASE &HF6 'zero page x
                a~% = sZPgX
                putB a~%, getB(a~%) + 1
                wt 6
            CASE &HEE 'absolute     'HEE
                a~% = sAb
                putB a~%, getB(a~%) + 1
                wt 6
            CASE &HFE 'absolute X
                a~% = sAbX
                putB a~%, getB(a~%) + 1
                wt 7


            CASE &HE8 'increment index x (INX)
                reg.iX = reg.iX + 1
                wt 2
            CASE &HC8 'increment index y (INY)
                reg.iY = reg.iY + 1
                wt 2


                'Jump (JMP)
            CASE &H4C
                I = ab
                wt 3
            CASE &H6C 'indirect (only used by this instruction)
                a~% = b2i(getB(I), getB(I + 1))
                I = I + 2
                I = b2i(getB(a~%), getB(a~% + 1))
                wt 5


                'Jump Subroutine (JSR)
            CASE &H20
                push i2b(I)
                push I
                I = ab
                wt 6


                'Load memory to acc (LDA)
            CASE &HA9
                reg.acc = getB(I): I = I + 1
                wt 2
            CASE &HA5
                reg.acc = zPg
                wt 3
            CASE &HB5
                reg.acc = zPgX
                wt 4
            CASE &HAD
                reg.acc = ab
                wt 4
            CASE &HBD
                reg.acc = chk8(abX)
                wt 4
            CASE &HB9
                reg.acc = chk8(abY)
                wt 4
            CASE &HA1
                reg.acc = indX
                wt 6
            CASE &HB1
                reg.acc = chk8(indY)
                wt 5


                'load x with memory (LDX)
            CASE &HA2
                reg.iX = getB(I): I = I + 1
                wt 2
            CASE &HA6
                reg.iX = zPg
                wt 3
            CASE &HB6
                reg.iX = zPgY
                wt 4
            CASE &HAE
                reg.iX = ab
                wt 4
            CASE &HBE
                reg.iX = chk8(abY)
                wt 4


                'load y with memory (LDY)
            CASE &HA0
                reg.iY = getB(I): I = I + 1
                wt 2
            CASE &HA4
                reg.iY = zPg
                wt 3
            CASE &HB4
                reg.iY = zPgX
                wt 4
            CASE &HAC
                reg.iY = ab
                wt 4
            CASE &HBC
                reg.iY = chk8(abX)
                wt 4

                '...

                'OR w/ accumilator (ORA)
            CASE &H09
                reg.acc = reg.acc OR getB(I): I = I + 1
                wt 2
            CASE &H05
                reg.acc = reg.acc OR zPg
                wt 3
            CASE &H15
                reg.acc = reg.acc OR zPgX
                wt 4
            CASE &H0D
                reg.acc = reg.acc OR ab
                wt 4
            CASE &H10
                reg.acc = chk8(reg.acc OR abX)
                wt 4
            CASE &H19
                reg.acc = chk8(reg.acc OR abY)
                wt 4
            CASE &H01
                reg.acc = reg.acc OR indX
                wt 6
            CASE &H11
                reg.acc = reg.acc OR indY
                wt 5


            CASE &H48 'push accumilator (PHA)
                push reg.acc
                wt 3

                '...

            CASE &H68 'pull accumilator (PLA)
                reg.acc = pull
                wt 4

                '...

            CASE &H60 'return from subroutine (RTS)
                I = b2i(pull, pull)
                wt 6

                '...

            CASE &H38 'set carry flag (SEC)
                reg.f.c = 1
                wt 2
            CASE &HF8 'set decimal mode (SED)
                reg.f.d = 1
                wt 2
            CASE &H78 'set interrupt disable (SEI)
                reg.f.i = 1
                wt 2


                'store accumilator in memory  (STA)
            CASE &H85
                putB getB(I), reg.acc
                wt 3
            CASE &H95
                putB sZPgX, reg.acc
                wt 4
            CASE &H80
                putB sAb, reg.acc
                wt 4
            CASE &H90
                putB sAbX, reg.acc
                wt 5
            CASE &H99
                putB sAbY, reg.acc
                wt 5
            CASE &H81
                putB sIndX, reg.acc
                wt 6
            CASE &H91
                putB sIndY, reg.acc
                wt 6

                '...

            CASE &HEA 'Nothing (NOP)
                wt 2

            CASE ELSE: PRINT USING "Opcode \\ not supported @ \  \"; HEX$(getB(I - 1)); HEX$(I - 1) ': SLEEP
                'CASE ELSE: PRINT "#";
        END SELECT
        I = I + 1
    LOOP
END SUB

SUB emuSnd
    f1# = (3995 / (127 - getB(mem_vic1))) / _SNDRATE
    f2# = (7990 / (127 - getB(mem_vic2))) / _SNDRATE
    f3# = (15980 / (127 - getB(mem_vic3))) / _SNDRATE
    n# = (31960 / (127 - getB(mem_vicNoise))) / _SNDRATE
    a = VAL("&H" + RIGHT$(HEX$(getB(mem_vicVol)), 1)) / 16
    FOR i = 0 TO 0.2 * _SNDRATE
        _SNDRAW a * SGN(SIN(Pi2 * f1# * i))
    NEXT
END SUB

SUB drawScnL (scL~%%)

END SUB

SUB wt (cycles~%%)
    _LIMIT clock6502 / cycles~%%
    IF _SNDRAWLEN < 0.1 THEN emuSnd
    'PRINT "EXECUTION!"
END SUB


FUNCTION getB~%% (l~%)
    SHARED mem() AS _UNSIGNED _BYTE
    getB~%% = mem(l~%)
    'PRINT getB~%%
END FUNCTION

FUNCTION vicB~%% (l~%)
    SHARED mem() AS _UNSIGNED _BYTE
    SELECT CASE l~%
        CASE 0 TO 8191: vicB~%% = getB(l~% + 32768)
        CASE 8192 TO 16363: vicB~%% = getB(l~% + 8192)
        CASE ELSE: PRINT "VIC address out of range"
    END SELECT
END FUNCTION

SUB putB (l~%, v~%%)
    SHARED mem() AS _UNSIGNED _BYTE
    mem(l~%) = v~%%
END SUB


SUB push (d~%%)
    SHARED mem() AS _UNSIGNED _BYTE, reg AS regType
    mem(mem_stk + reg.stk) = d~%%
    reg.stk = reg.stk - 1
END SUB

FUNCTION pull~%%
    SHARED mem() AS _UNSIGNED _BYTE, reg AS regType
    reg.stk = reg.stk + 1
    pull~%% = mem(mem_stk + reg.stk)
END FUNCTION


FUNCTION b2i~% (i2~%%, i1~%%) 'Converts two bytes to an integer
    h$ = HEX$(i1~%%)
    IF LEN(h$) = 1 THEN h$ = "0" + h$
    h2$ = HEX$(i2~%%)
    IF LEN(h2$) = 1 THEN h2$ = "0" + h2$
    b2i~% = VAL("&H" + h$ + h2$)
END FUNCTION
FUNCTION i2b~%% (i~%) 'gets the high byte of an integer
    REM you use it like this:
    REM i2b(i)             'for high byte
    REM i mod 256/i~%%=i~% 'for low byte
    i2b~%% = INT(i~% / &H100)
END FUNCTION


FUNCTION chk8~%% (i~%) 'checks for carry
    SHARED reg AS regType
    IF i~% > 255 THEN reg.f.c = 1: wt 1 ELSE IF i~% = 0 THEN reg.f.z = 1 ELSE reg.f.z = 0
    chk8~%% = i~%
END FUNCTION
FUNCTION chk16~% (i~&) 'checks for carry in integers
    SHARED reg AS regType
    IF i~& > 65535 THEN reg.f.c = 1: wt 1 ELSE IF i~& = 0 THEN reg.f.z = 1 ELSE reg.f.z = 0
    chk16~% = i~&
END FUNCTION

FUNCTION c8~%% (i~%) 'checks for carry w/o wait
    SHARED reg AS regType
    IF i~% > 255 THEN reg.f.c = 1 ELSE IF i~% = 0 THEN reg.f.z = 1 ELSE reg.f.z = 0
    c8~%% = i~%
END FUNCTION
FUNCTION c16~% (i~&)
    SHARED reg AS regType
    IF i~& > 65535 THEN ref.f.c = 1 ELSE IF i~& = 0 THEN reg.f.z = 1 ELSE reg.f.z = 0
    c16~% = i~&
END FUNCTION
FUNCTION cZ~& (i~&) 'checks if zero
    SHARED reg AS regType
    IF i~& = 0 THEN reg.f.z = 1 ELSE reg.f.z = 0
    cZ~& = i~&
END FUNCTION


FUNCTION ASL~%% (v~%%)
    SHARED reg AS regType
    b$ = BIN8(v~%%)
    IF LEFT$(b$, 1) = "1" THEN reg.f.c = 1
    ASL~%% = VAL("&B" + MID$(b$, 2) + "0")
END FUNCTION


'Addressing modes:
FUNCTION zPg~% 'zero page
    SHARED I AS _UNSIGNED INTEGER
    zPg~% = getB(getB(I))
    I = I + 1
END FUNCTION
FUNCTION zPgX~% 'zero page indexed x
    SHARED reg AS regType, I AS _UNSIGNED INTEGER
    zPgX~% = getB(getB(I) + reg.iX)
    I = I + 1
END FUNCTION
FUNCTION zPgY~% 'zero page indexed y
    SHARED reg AS regType, I AS _UNSIGNED INTEGER
    zPgY~% = getB(getB(I) + reg.iY)
    I = I + 1
END FUNCTION

FUNCTION sZPgX~% 'zero page indexed x
    SHARED reg AS regType, I AS _UNSIGNED INTEGER
    sZPgX~% = getB(I) + reg.iX
    I = I + 1
END FUNCTION
FUNCTION sZPgY~% 'zero page indexed y
    SHARED reg AS regType, I AS _UNSIGNED INTEGER
    sZPgY~% = getB(I) + reg.iY
    I = I + 1
END FUNCTION


FUNCTION ab~% 'absolute
    SHARED I AS _UNSIGNED INTEGER
    ab~% = getB(b2i(getB(I), getB(I + 1)))
    I = I + 2
END FUNCTION
FUNCTION abX~% 'absolute indexed x
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    abX~% = getB(b2i(getB(I), getB(I + 1)) + reg.iX)
    I = I + 2
END FUNCTION
FUNCTION abY~% 'absolute indexed y
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    abY~% = getB(b2i(getB(I), getB(I + 1)) + reg.iY)
    I = I + 2
END FUNCTION

FUNCTION sAb~% 'absolute mem
    SHARED I AS _UNSIGNED INTEGER
    sAb~% = b2i(getB(I), getB(I + 1))
    I = I + 2
END FUNCTION
FUNCTION sAbX~% 'absolute mem indexed x
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    sAbX~% = b2i(getB(I), getB(I + 1)) + reg.iX
    I = I + 2
END FUNCTION
FUNCTION sAbY~% 'absolute mem indexed y
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    sAbY~% = b2i(getB(I), getB(I + 1)) + reg.iY
    I = I + 2
END FUNCTION


'for indirect goto JMP 6C
FUNCTION indX~% 'indirect indexed x
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    a~%% = getB(getB(I) + reg.iX)
    indX~% = b2i(getB(a~%%), getB(a~%% + 1))
    I = I + 1
END FUNCTION
FUNCTION indY~% 'indirect indexed y
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    a~%% = getB(I)
    indY~% = b2i(getB(a~%%), getB(a~%% + 1)) + reg.iY
    I = I + 1
END FUNCTION

FUNCTION sIndX~% 'indirect mem indexed x
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    a~%% = getB(getB(I) + reg.iX)
    sIndX~% = b2i(getB(a~%%), getB(a~%% + 1))
    I = I + 1
END FUNCTION
FUNCTION sIndY~% 'indirect mem indexed y
    SHARED I AS _UNSIGNED INTEGER, reg AS regType
    a~%% = getB(I)
    sIndY~% = b2i(getB(a~%%), getB(a~%% + 1)) + reg.iY
    I = I + 1
END FUNCTION

