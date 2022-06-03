-- testing LL(k) on lexicalistic rules

def Main = { T; END }

def T = First
    all_sound_off     = @Match [ 0x78; 0x00 ]
    reset_controllers = @Match [ 0x79; 0x00 ]
    local_control_off = @Match [ 0x7A; 0x00 ]
    local_control_on  = @Match [ 0x7A; 0x7f ]
    all_notes_off     = @Match [ 0x7B; 0x00 ]
    omni_off          = @Match [ 0x7C; 0x00 ]
    omni_on           = @Match [ 0x7D; 0x00 ]
    mono_on           = { $[0x7E]; $$ = $[0x00..0x10] }
    poly_on           = @Match [ 0x7F; 0x00 ]

