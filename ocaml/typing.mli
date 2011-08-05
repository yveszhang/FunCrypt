open Types

type typing_frame = fc_context * fc_program

exception FC_type_error of fc_position * typing_frame * type_error

val is_sub_type : cslr_type -> cslr_type -> bool

val infer : fc_context -> fc_term -> bool -> cslr_type

val infer_prog : fc_context -> fc_program -> bool -> cslr_type

val infer_string : fc_context -> string -> bool -> cslr_type

val typing_frame_to_string : typing_frame -> string

