open Console;

let result = ref("");

let print = stuff => {
  result := result.contents ++ stuff ++ " ";
};

let rec string_of_ident = (expr: Longident.t) => {
  switch (expr) {
  | Migrate_parsetree.Ast_406.Longident.Lident(string) => string
  | [@implicit_arity] Migrate_parsetree.Ast_406.Longident.Ldot(a, b) =>
    string_of_ident(a) ++ "." ++ b
  | [@implicit_arity] Migrate_parsetree.Ast_406.Longident.Lapply(a, b) =>
    string_of_ident(a) ++ string_of_ident(b)
  };
};

// Currently it only prints constant values from the parsetree and writes them down

let write = (~filename, data) => {
  let oc = open_out(filename);
  Printf.fprintf(oc, "%s\n", data);
  close_out(oc);
};

let printTrees = (~filename, ptree, ttree) => {
  let parseTreeFormatter =
    Format.formatter_of_out_channel(open_out(filename ++ ".ast"));

  Printast.implementation(parseTreeFormatter, ptree);

  let typedFormatter =
    Format.formatter_of_out_channel(open_out(filename ++ ".typed.ast"));

  Printtyped.implementation(typedFormatter, ttree);
};

let compile = (~filename: string, str: Typedtree.structure) => {
  // reset result, just in case
  result := "";

  let rec parseExpr = (mapper: Ast_mapper.mapper, expr: Parsetree.expression) =>
    switch (expr.pexp_desc) {
    | Parsetree.Pexp_constant(const) =>
      // we are currently only interested in constant values
      switch (const) {
      | [@implicit_arity] Parsetree.Pconst_integer(string, _) =>
        string |> print
      | Parsetree.Pconst_char(char) => char |> Char.escaped |> print
      | [@implicit_arity] Parsetree.Pconst_string(string, _) =>
        string |> print
      | [@implicit_arity] Parsetree.Pconst_float(string, _) => string |> print
      };

      expr;

    | Parsetree.Pexp_ident(ident) =>
      let str_item = Analysis.get_structure_from_loc(ident.loc, str);
      switch (str_item) {
      | None => expr
      | Some((flags, binding)) =>
        switch (binding.pvb_attributes |> Utils.extractAsm) {
        | None => binding.pvb_expr
        | Some(asm) =>
          asm |> List.iter(op_code_string => op_code_string |> print);
          expr;
        }
      };

    | [@implicit_arity] Parsetree.Pexp_let(_, _, _)
    | Parsetree.Pexp_function(_)
    | [@implicit_arity] Parsetree.Pexp_fun(_, _, _, _)
    | [@implicit_arity] Parsetree.Pexp_apply(_, _)
    | [@implicit_arity] Parsetree.Pexp_match(_, _)
    | [@implicit_arity] Parsetree.Pexp_try(_, _)
    | Parsetree.Pexp_tuple(_)
    | [@implicit_arity] Parsetree.Pexp_construct(_, _)
    | [@implicit_arity] Parsetree.Pexp_variant(_, _)
    | [@implicit_arity] Parsetree.Pexp_record(_, _)
    | [@implicit_arity] Parsetree.Pexp_field(_, _)
    | [@implicit_arity] Parsetree.Pexp_setfield(_, _, _)
    | Parsetree.Pexp_array(_)
    | [@implicit_arity] Parsetree.Pexp_ifthenelse(_, _, _)
    | [@implicit_arity] Parsetree.Pexp_sequence(_, _)
    | [@implicit_arity] Parsetree.Pexp_while(_, _)
    | [@implicit_arity] Parsetree.Pexp_for(_, _, _, _, _)
    | [@implicit_arity] Parsetree.Pexp_constraint(_, _)
    | [@implicit_arity] Parsetree.Pexp_coerce(_, _, _)
    | [@implicit_arity] Parsetree.Pexp_send(_, _)
    | Parsetree.Pexp_new(_)
    | [@implicit_arity] Parsetree.Pexp_setinstvar(_, _)
    | Parsetree.Pexp_override(_)
    | [@implicit_arity] Parsetree.Pexp_letmodule(_, _, _)
    | [@implicit_arity] Parsetree.Pexp_letexception(_, _)
    | Parsetree.Pexp_assert(_)
    | Parsetree.Pexp_lazy(_)
    | [@implicit_arity] Parsetree.Pexp_poly(_, _)
    | Parsetree.Pexp_object(_)
    | [@implicit_arity] Parsetree.Pexp_newtype(_, _)
    | Parsetree.Pexp_pack(_)
    | [@implicit_arity] Parsetree.Pexp_open(_, _, _)
    | Parsetree.Pexp_extension(_)
    | Parsetree.Pexp_unreachable =>
      Ast_mapper.default_mapper.expr(mapper, expr)
    }

  and mapper = {
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) => {
      // we are currently only interested in mapping over expressions
      parseExpr(
        mapper,
        expr,
      );
    },
  };

  // transform typedtree and parsetree, it faster and simpler, if we need type info, we will query the typedtree later
  let parsetree = Untypeast.untype_structure(str);

  // print Typedtree and Parsetree to know what's going on, can also use https://astexplorer.net/ but not 100% correct

  printTrees(~filename, parsetree, str);

  // just the default mapper from the ocaml compiler libs to map over every element of the parsetree
  mapper.structure(mapper, parsetree) |> ignore;

  // save the compile result as a .script file
  write(~filename=filename ++ ".script", result.contents);
};
