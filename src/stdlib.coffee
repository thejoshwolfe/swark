
exports.printc = '''
; printc
    set z, pop
    set [0x8fff], pop
    set pc, z
'''

exports.prints = '''
; prints
    set z, pop
    set a, pop        ; pointer to string object
    set b, a
    add b, [b]        ; b points to end of string - 1
    add a, 1          ; a points to first char of string

    :_prints_loop
        ifg a, b
            set pc, z
        set [0x8fff], [a]     ; print character
        add a, 1
        set pc, _prints_loop
'''
