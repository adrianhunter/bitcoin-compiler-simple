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

let opcode_of_string = s => {
  CompilerTypes.(
    switch (s) {
    | "OP_0" => OP_0
    | "OP_1" => OP_1
    | "OP_2" => OP_2
    | "OP_3" => OP_3
    | "OP_4" => OP_4
    | "OP_5" => OP_5
    | "OP_6" => OP_6
    | "OP_7" => OP_7
    | "OP_8" => OP_8
    | "OP_9" => OP_9
    | "OP_10" => OP_10
    | "OP_11" => OP_11
    | "OP_12" => OP_12
    | "OP_13" => OP_13
    | "OP_14" => OP_14
    | "OP_15" => OP_15
    | "OP_16" => OP_16
    | "OP_FALSE" => OP_FALSE
    | "OP_PUSHDATA1" => OP_PUSHDATA1
    | "OP_PUSHDATA2" => OP_PUSHDATA2
    | "OP_PUSHDATA4" => OP_PUSHDATA4
    | "OP_1NEGATE" => OP_1NEGATE
    | "OP_TRUE" => OP_TRUE
    | "OP_NOP" => OP_NOP
    | "OP_VER" => OP_VER
    | "OP_IF" => OP_IF
    | "OP_NOTIF" => OP_NOTIF
    | "OP_VERIF" => OP_VERIF
    | "OP_VERNOTIF" => OP_VERNOTIF
    | "OP_ELSE" => OP_ELSE
    | "OP_ENDIF" => OP_ENDIF
    | "OP_VERIFY" => OP_VERIFY
    | "OP_RETURN" => OP_RETURN
    | "OP_TOALTSTACK" => OP_TOALTSTACK
    | "OP_FROMALTSTACK" => OP_FROMALTSTACK
    | "OP_2DROP" => OP_2DROP
    | "OP_2DUP" => OP_2DUP
    | "OP_3DUP" => OP_3DUP
    | "OP_2OVER" => OP_2OVER
    | "OP_2ROT" => OP_2ROT
    | "OP_2SWAP" => OP_2SWAP
    | "OP_IFDUP" => OP_IFDUP
    | "OP_DEPTH" => OP_DEPTH
    | "OP_DROP" => OP_DROP
    | "OP_DUP" => OP_DUP
    | "OP_NIP" => OP_NIP
    | "OP_OVER" => OP_OVER
    | "OP_PICK" => OP_PICK
    | "OP_ROLL" => OP_ROLL
    | "OP_ROT" => OP_ROT
    | "OP_SWAP" => OP_SWAP
    | "OP_TUCK" => OP_TUCK
    | "OP_CAT" => OP_CAT
    | "OP_SPLIT" => OP_SPLIT
    | "OP_NUM2BIN" => OP_NUM2BIN
    | "OP_BIN2NUM" => OP_BIN2NUM
    | "OP_SIZE" => OP_SIZE
    | "OP_INVERT" => OP_INVERT
    | "OP_AND" => OP_AND
    | "OP_OR" => OP_OR
    | "OP_XOR" => OP_XOR
    | "OP_EQUAL" => OP_EQUAL
    | "OP_EQUALVERIFY" => OP_EQUALVERIFY
    | "OP_ADD" => OP_ADD
    | "OP_1ADD" => OP_1ADD
    | "OP_1SUB" => OP_1SUB
    | "OP_2MUL" => OP_2MUL
    | "OP_2DIV" => OP_2DIV
    | "OP_NEGATE" => OP_NEGATE
    | "OP_ABS" => OP_ABS
    | "OP_NOT" => OP_NOT
    | "OP_0NOTEQUAL" => OP_0NOTEQUAL
    | "OP_SUB" => OP_SUB
    | "OP_MUL" => OP_MUL
    | "OP_DIV" => OP_DIV
    | "OP_MOD" => OP_MOD
    | "OP_LSHIFT" => OP_LSHIFT
    | "OP_RSHIFT" => OP_RSHIFT
    | "OP_BOOLAND" => OP_BOOLAND
    | "OP_BOOLOR" => OP_BOOLOR
    | "OP_NUMEQUAL" => OP_NUMEQUAL
    | "OP_NUMEQUALVERIFY" => OP_NUMEQUALVERIFY
    | "OP_NUMNOTEQUAL" => OP_NUMNOTEQUAL
    | "OP_LESSTHAN" => OP_LESSTHAN
    | "OP_GREATERTHAN" => OP_GREATERTHAN
    | "OP_LESSTHANOREQUAL" => OP_LESSTHANOREQUAL
    | "OP_GREATERTHANOREQUAL" => OP_GREATERTHANOREQUAL
    | "OP_MIN" => OP_MIN
    | "OP_MAX" => OP_MAX
    | "OP_WITHIN" => OP_WITHIN
    | "OP_RIPEMD160" => OP_RIPEMD160
    | "OP_SHA1" => OP_SHA1
    | "OP_SHA256" => OP_SHA256
    | "OP_HASH160" => OP_HASH160
    | "OP_HASH256" => OP_HASH256
    | "OP_CODESEPARATOR" => OP_CODESEPARATOR
    | "OP_CHECKSIG" => OP_CHECKSIG
    | "OP_CHECKSIGVERIFY" => OP_CHECKSIGVERIFY
    | "OP_CHECKMULTISIG" => OP_CHECKMULTISIG
    | "OP_CHECKMULTISIGVERIFY" => OP_CHECKMULTISIGVERIFY
    | "OP_NOP2" => OP_NOP2
    | "OP_NOP3" => OP_NOP3
    | "OP_PUBKEYHASH" => OP_PUBKEYHASH
    | "OP_PUBKEY" => OP_PUBKEY
    | "OP_INVALIDOPCODE" => OP_INVALIDOPCODE
    | wow => failwith("unsupported opcode " ++ wow)
    }
  );
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

      //    opcode_of_string(stringOf) |> add;

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

      // let result = str |> Pprintast.string_of_structure;
      // let reg = Str.regexp(";;");
      // let result = result |> Str.replace_first(reg, "");

      //         ""
      //       }) |> String.concat(" ");

      Some(result.contents);
    | Parsetree.PSig(_) => failwith("......... shit!")
    | Parsetree.PTyp(_) => failwith("......... shit!")
    | [@implicit_arity] Parsetree.PPat(_, _) => failwith("......... shit!")
    }
  };
};
