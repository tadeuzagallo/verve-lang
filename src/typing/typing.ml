let check program =
  try
    let ty, _, s = Typing_decl.check_decls Env.default_env program.Absyn.body in
    Env.apply s ty
  with Type_error.Error e ->
    Type_error.report_error Format.err_formatter e;
    Format.pp_print_newline Format.err_formatter ();
    exit 1
