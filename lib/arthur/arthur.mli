
(** The internal abstract configuration type for the linter  *)
type config

(** Create a configuration from a filename *)
val parse_file_config : string -> (config, string) Result.t

(** Create a configuration from a string *)
val parse_string_config : string -> (config, string) Result.t

(** Pretty print a config to a string *)
val pp_config : config -> string 

(** Print a configuration file to screen *)
val print_config : config -> unit

(** Print a plan of the configuration to screen 
    This will print a description of how the linter will interpret the plan
*)
val print_config_plan : config -> unit

(** Given a configuration c, 
          a filename f, 
          a string s representing the function name,
          an assoc. list of string, _ pairs,
          eval_config c f s returns an assoc. list of string,_ pairs where 
    the strings in the output list represent rules that satisfy the configuration 
    for the given file and function name. Basically, what checks to apply to the 
    function inside the file.
 *)
val eval_config : config -> string -> string -> (string * 'a) list -> (string * 'a) list 

(** Creates a default config *)
val default : config

(** Writes a config back to file *)
val write_config :  ?file:string -> config -> unit 


(** Checks a configuration file *)
val check_config : unit 
