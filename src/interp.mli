(** Interpret an AAL program, yielding the result value and animation
    program *)
val interp_prog : Exp.prog -> (Interptypes.value * Ais.anim_prog)
