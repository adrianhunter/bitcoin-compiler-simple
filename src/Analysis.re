open Console;

let getCmt = path => {
  switch (path |> Cmt_format.read_cmt) {
  | cmt => cmt
  };
};

let getEnv = (cmt: Cmt_format.cmt_infos) => {
  switch (cmt.cmt_annots) {
  | Implementation(a) => a.str_final_env
  };
};

let getCmtPath = path => {
  let in_source = true;
  let suffix = ".res";
  let cmtpath = {
    let path =
      in_source
        ? {
          let r = Str.regexp("/src/");
          Str.replace_first(r, "/src/../lib/bs/src/", path) ++ ".cmt";
        }
        : {
          let r = Str.regexp("/../../src");
          Str.replace_first(r, "/src", path) ++ ".cmt";
        };
    let r = Str.regexp(suffix);
    path |> Str.replace_first(r, "");
  };
  cmtpath;
};

let getStr = (cmt: Cmt_format.cmt_infos) => {
  switch (cmt.cmt_annots) {
  | Implementation(a) => a |> Untypeast.untype_structure
  };
};
let getTypedStr = (cmt: Cmt_format.cmt_infos) => {
  switch (cmt.cmt_annots) {
  | Implementation(a) => a
  };
};

let getCmtFromLoc = path => {
  let in_source = true;
  let suffix = ".res";
  let cmtpath = {
    let path =
      in_source
        ? {
          let r = Str.regexp("/tests/");
          Str.replace_first(r, "/tests/../lib/bs/tests/", path) ++ ".cmt";
        }
        : {
          let r = Str.regexp("/../../src");
          Str.replace_first(r, "/src", path) ++ ".cmt";
        };
    let r = Str.regexp(suffix);
    path |> Str.replace_first(r, "");
  };
  cmtpath |> getCmt;
};

let get_structure_item = (valdecla: Types.value_description) => {
  let loc = valdecla.val_loc;
  let (line, col, path) =
    switch (loc) {
    | {loc_start, loc_end} => (
        loc_start.pos_lnum,
        loc_start.pos_cnum - loc_start.pos_bol - 4,
        loc_start.pos_fname,
      )
    };
  let cmtPath = getCmtPath(path);
  let result = ref(None);
  try(
    {
      let cmt: Cmt_format.cmt_infos = getCmt(cmtPath);
      let str = getStr(cmt);
      str
      |> List.iter((stri: Parsetree.structure_item) =>
           switch (stri.pstr_desc) {
           | [@implicit_arity] Parsetree.Pstr_value(flag, bindings) =>
             let _ =
               bindings
               |> List.map((binding: Parsetree.value_binding) => {
                    switch (binding.pvb_loc) {
                    | {loc_start, loc_end, loc_ghost} =>
                      if (loc_start.pos_lnum == line
                          && loc_start.pos_cnum
                          - loc_start.pos_bol == col) {
                        result := Some((flag, binding));
                      }
                    }
                  });
             ();
           | [@implicit_arity] Parsetree.Pstr_eval(_, _) => ()
           | Parsetree.Pstr_primitive(_) => ()
           | [@implicit_arity] Parsetree.Pstr_type(_, _) => ()
           | Parsetree.Pstr_typext(_) => ()
           | Parsetree.Pstr_exception(_) => ()
           | Parsetree.Pstr_module(_) => ()
           | Parsetree.Pstr_recmodule(_) => ()
           | Parsetree.Pstr_modtype(_) => ()
           | Parsetree.Pstr_open(_) => ()
           | Parsetree.Pstr_class(_) => ()
           | Parsetree.Pstr_class_type(_) => ()
           | Parsetree.Pstr_include(_) => ()
           | Parsetree.Pstr_attribute(_) => ()
           | [@implicit_arity] Parsetree.Pstr_extension(_, _) => ()
           }
         );
    }
    |> ignore
  ) {
  | e => ()
  };

  result.contents;
};

let get_value_declaration = (locSearch: Location.t, ttree) => {
  let result = ref(None);

  let mapper = {
    ...Tast_mapper.default,

    expr: (mapper, expr) => {
      switch (expr.exp_desc) {
      | Texp_ident(a, {loc as identLoc}, valueDecla) =>
        let _ =
          if (identLoc == locSearch) {
            result := Some(valueDecla);
          };

        expr;
      | _ => Tast_mapper.default.expr(mapper, expr)
      };
    },
  };

  mapper.structure(mapper, ttree) |> ignore;

  result.contents;
};

let get_structure_from_loc = (loc: Location.t, ttree) => {
  open Location;
  let result = ref(None);
  let value_decla = get_value_declaration(loc, ttree);
  switch (value_decla) {
  | Some(valueDecla) =>
    let loc = valueDecla.val_loc;
    let (line, col, path) =
      switch (loc) {
      | {loc_start, loc_end} => (
          loc_start.pos_lnum,
          loc_start.pos_cnum - loc_start.pos_bol - 4,
          loc_start.pos_fname,
        )
      };
    let cmtPath = getCmtPath(path);
    try(
      {
        let cmt: Cmt_format.cmt_infos = getCmt(cmtPath);
        let str = getStr(cmt);
        str
        |> List.iter((stri: Parsetree.structure_item) => {
             switch (stri.pstr_desc) {
             | [@implicit_arity] Parsetree.Pstr_value(flag, bindings) =>
               let _ =
                 bindings
                 |> List.map((binding: Parsetree.value_binding) => {
                      switch (binding.pvb_loc) {
                      | {loc_start, loc_end, loc_ghost} =>
                        if (loc_start.pos_lnum == line
                            && loc_start.pos_cnum
                            - loc_start.pos_bol == col) {
                          result := Some((flag, binding));
                        }
                      }
                    });
               ();
             | [@implicit_arity] Parsetree.Pstr_eval(_, _) => ()
             | Parsetree.Pstr_primitive(_) => ()
             | [@implicit_arity] Parsetree.Pstr_type(_, _) => ()
             | Parsetree.Pstr_typext(_) => ()
             | Parsetree.Pstr_exception(_) => ()
             | Parsetree.Pstr_module(_) => ()
             | Parsetree.Pstr_recmodule(_) => ()
             | Parsetree.Pstr_modtype(_) => ()
             | Parsetree.Pstr_open(_) => ()
             | Parsetree.Pstr_class(_) => ()
             | Parsetree.Pstr_class_type(_) => ()
             | Parsetree.Pstr_include(_) => ()
             | Parsetree.Pstr_attribute(_) => ()
             | [@implicit_arity] Parsetree.Pstr_extension(_, _) => ()
             }
           });
      }
      |> ignore
    ) {
    | e => ()
    };
    result.contents;
  | None => result.contents
  };
};

let get_typed_structure_item = (valdecla: Types.value_description) => {
  let loc = valdecla.val_loc;
  let (line, col, path) =
    switch (loc) {
    | {loc_start, loc_end} => (
        loc_start.pos_lnum,
        loc_start.pos_cnum - loc_start.pos_bol - 4,
        loc_start.pos_fname,
      )
    };
  let cmtPath = getCmtPath(path);
  let result = ref(None);
  try(
    {
      let cmt: Cmt_format.cmt_infos = getCmt(cmtPath);
      let str = getTypedStr(cmt);
      str.str_items
      |> List.iter((stri: Typedtree.structure_item) => {
           switch (stri.str_desc) {
           | [@implicit_arity] Typedtree.Tstr_eval(_, _) => ()
           | [@implicit_arity] Typedtree.Tstr_value(flag, bindings) =>
             let _ =
               bindings
               |> List.map((binding: Typedtree.value_binding) => {
                    switch (binding.vb_loc) {
                    | {loc_start, loc_end, loc_ghost} =>
                      if (loc_start.pos_lnum == line
                          && loc_start.pos_cnum
                          - loc_start.pos_bol == col) {
                        result := Some((flag, binding));
                      }
                    }
                  });
             ();
           | Typedtree.Tstr_primitive(_) => ()
           | [@implicit_arity] Typedtree.Tstr_type(_, _) => ()
           | Typedtree.Tstr_typext(_) => ()
           | Typedtree.Tstr_exception(_) => ()
           | Typedtree.Tstr_module(_) => ()
           | Typedtree.Tstr_recmodule(_) => ()
           | Typedtree.Tstr_modtype(_) => ()
           | Typedtree.Tstr_open(_) => ()
           | Typedtree.Tstr_class(_) => ()
           | Typedtree.Tstr_class_type(_) => ()
           | Typedtree.Tstr_include(_) => ()
           | Typedtree.Tstr_attribute(_) => ()
           };

           ();
         }); 
    }  
    |> ignore
  ) {
  | e => ()
  };

  result.contents;
};
