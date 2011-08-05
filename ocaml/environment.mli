open Types

val build_fc_environment : fc_parsing_result -> bool -> fc_environment 

val typing_parsing_result : fc_parsing_result -> bool -> fc_parsing_result

val unfold_type_in_fc_term : (string * cslr_type) list -> fc_term -> fc_term  

