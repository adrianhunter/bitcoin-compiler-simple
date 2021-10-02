let getAttr = (string, pvb_attributes) =>
  pvb_attributes
  |> List.find_opt(a => {
       let (asd, _) = a;

       let {Location.txt} = asd;

       txt == string;
     });

open Parsetree;

let rec string_of_ident = (expr: Longident.t) => {
  switch (expr) {
  | Longident.Lident(string) => string
  | [@implicit_arity] Longident.Ldot(a, b) => string_of_ident(a) ++ "." ++ b
  | [@implicit_arity] Longident.Lapply(a, b) =>
    string_of_ident(a) ++ string_of_ident(b)
  };
};

let extractAsm = (pvb_attributes: Parsetree.attributes) => {
  let result = ref([]);

  let add = code => {
    result := result.contents @ [code];
    ();
  };

  let rec findStuff = expr => {
    switch (expr.pexp_desc) {
    | Parsetree.Pexp_ident(_) => ()
    | Parsetree.Pexp_constant(_) => ()
    | [@implicit_arity] Parsetree.Pexp_let(_, _, _) => ()
    | Parsetree.Pexp_function(_) => ()
    | [@implicit_arity] Parsetree.Pexp_fun(_, _, _, _) => ()
    | [@implicit_arity] Parsetree.Pexp_apply(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_match(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_try(_, _) => ()
    | Parsetree.Pexp_tuple(_) => ()
    | [@implicit_arity] Parsetree.Pexp_construct({txt}, _) =>
      let stringOf = string_of_ident(txt);
      stringOf |> add;
      ();
    | [@implicit_arity] Parsetree.Pexp_variant(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_record(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_field(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_setfield(_, _, _) => ()
    | Parsetree.Pexp_array(arrayContent) =>
      arrayContent |> List.iter(item => item |> findStuff)
    | [@implicit_arity] Parsetree.Pexp_ifthenelse(_, _, _) => ()
    | [@implicit_arity] Parsetree.Pexp_sequence(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_while(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_for(_, _, _, _, _) => ()
    | [@implicit_arity] Parsetree.Pexp_constraint(_, _) => ()
    | [@implicit_arity] Parsetree.Pexp_coerce(_, _, _) => ()
    | [@implicit_arity] Parsetree.Pexp_send(_, _) => ()
    | Parsetree.Pexp_new(_) => ()
    | [@implicit_arity] Parsetree.Pexp_setinstvar(_, _) => ()
    | Parsetree.Pexp_override(_) => ()
    | [@implicit_arity] Parsetree.Pexp_letmodule(_, _, _) => ()
    | [@implicit_arity] Parsetree.Pexp_letexception(_, _) => ()
    | Parsetree.Pexp_assert(_) => ()
    | Parsetree.Pexp_lazy(_) => ()
    | [@implicit_arity] Parsetree.Pexp_poly(_, _) => ()
    | Parsetree.Pexp_object(_) => ()
    | [@implicit_arity] Parsetree.Pexp_newtype(_, _) => ()
    | Parsetree.Pexp_pack(_) => ()
    | [@implicit_arity] Parsetree.Pexp_open(_, _, _) => ()
    | Parsetree.Pexp_extension(_) => ()
    | Parsetree.Pexp_unreachable => ()
    };
  };
  switch (pvb_attributes |> getAttr("asm")) {
  | None => None
  | Some((loc, payload)) =>
    switch (payload) {
    | Parsetree.PStr(str) =>
      str
      |> List.iter(stri => {
           switch (stri.pstr_desc) {
           | [@implicit_arity] Parsetree.Pstr_eval(expr, wow) =>
             findStuff(expr)
           | [@implicit_arity] Parsetree.Pstr_value(_, _) => ()
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
           };

           ();
         });


      Some(result.contents);
    | Parsetree.PSig(_) => failwith("......... shit!")
    | Parsetree.PTyp(_) => failwith("......... shit!")
    | [@implicit_arity] Parsetree.PPat(_, _) => failwith("......... shit!")
    }
  };
};
