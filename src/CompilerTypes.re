type argVal =
  | String(string)
  | Int(int)
  | None;
type arg = {
  value: argVal,
  label: string,
  stack_position: int,
};

type funArg = {
  label: string,
  type_: string,
};

type recordField = {
  name: string,
  index: int,
  code: list(opcode),
}
and opcode =
  | OP_0
  | OP_1
  | OP_2
  | OP_3
  | OP_4
  | OP_5
  | OP_6
  | OP_7
  | OP_8
  | OP_9
  | OP_10
  | OP_11
  | OP_12
  | OP_13
  | OP_14
  | OP_15
  | OP_16
  | OP_FALSE
  | OP_PUSHDATA1
  | OP_PUSHDATA2
  | OP_PUSHDATA4
  | OP_1NEGATE
  | OP_TRUE
  | OP_NOP
  | OP_VER
  | OP_IF
  | OP_NOTIF
  | OP_VERIF
  | OP_VERNOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
  | OP_TOALTSTACK
  | OP_FROMALTSTACK
  | OP_2DROP
  | OP_2DUP
  | OP_3DUP
  | OP_2OVER
  | OP_2ROT
  | OP_2SWAP
  | OP_IFDUP
  | OP_DEPTH
  | OP_DROP
  | OP_DUP
  | OP_NIP
  | OP_OVER
  | OP_PICK
  | OP_ROLL
  | OP_ROT
  | OP_SWAP
  | OP_TUCK
  | OP_CAT
  | OP_SPLIT
  | OP_NUM2BIN
  | OP_BIN2NUM
  | OP_SIZE
  | OP_INVERT
  | OP_AND
  | OP_OR
  | OP_XOR
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_1ADD
  | OP_1SUB
  | OP_2MUL
  | OP_2DIV
  | OP_NEGATE
  | OP_ABS
  | OP_NOT
  | OP_0NOTEQUAL
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_MOD
  | OP_LSHIFT
  | OP_RSHIFT
  | OP_BOOLAND
  | OP_BOOLOR
  | OP_NUMEQUAL
  | OP_NUMEQUALVERIFY
  | OP_NUMNOTEQUAL
  | OP_LESSTHAN
  | OP_GREATERTHAN
  | OP_LESSTHANOREQUAL
  | OP_GREATERTHANOREQUAL
  | OP_MIN
  | OP_MAX
  | OP_WITHIN
  | OP_RIPEMD160
  | OP_SHA1
  | OP_SHA256
  | OP_HASH160
  | OP_HASH256
  | OP_CODESEPARATOR
  | OP_CHECKSIG
  | OP_CHECKSIGVERIFY
  | OP_CHECKMULTISIG
  | OP_CHECKMULTISIGVERIFY
  | OP_NOP2
  | OP_NOP3
  | OP_PUBKEYHASH
  | OP_PUBKEY
  | OP_INVALIDOPCODE
  | NONE
  | INIT_VAR({
      name: string,
      binding: list(opcode),
    })
  | INIT_FUN_ARG({name: string})
  // | ASM(string)
  | USE_VAR(string)
  | INT(int)
  | INT32(int32)
  | INT64(int64)
  | NATIVE_INT(nativeint)
  | CHAR(char)
  | STRING(string)
  | FLOAT(float)
  | FUNCTION(string)
  | CALL_FUNCTION(string, list(arg))
  | EXTENSION(string, list(arg))
  | Return(list(opcode))
  | Exp_record({
      name: string,
      fields: list(recordField),
    })
  | Exp_setField({
      varname: string,
      fieldname: string,
    });

type propType =
  | String(string)
  | IntProp(int)
  | Function(string);

type extension = {
  name: string,
  args: list(arg),
};

type functionType =
  | NativeFun
  | ExtensionFun(string)
  | CustomFun(string);

type functionn = {
  name: string,
  path: list(string),
  mutable code: list(opcode),
  args: list(funArg),
  deploy_args: list(funArg),
  returnType: string,
  mutable extensions: list(extension),
  mutable functions: list(functionn),
};
type functions = list(functionn);

type moduleDeclaration = {
  mutable name: string,
  mutable functions,
  mutable modules: list(moduleDeclaration),
};

type current =
  | Function(functionn)
  | Module(moduleDeclaration);

type variable = {name: string};

type context = {
  mutable filepath: string,
  mutable functions,
  mutable isMainFunction: bool,
  mutable moduleDeclaration,
  mutable current,
  mutable isContract: bool,
  mutable variables: list(variable),
  mutable stack: list(variable),
  addStackItem: variable => unit,
  addVariable: variable => unit,
  useVariable: variable => list(opcode),
  // mutable parentContext: current,
  addFunction: functionn => unit,
  setCurrent: current => unit,
  unsetCurrent: unit => unit,
  stackCurrent: unit => current,
  mutable path: list(string),
  addPath: string => unit,
  reset: unit => unit,
  mutable lastExpr: Typedtree.expression,
  mutable lastExprScript: list(opcode)

};

type result = {
  code: string,
  adapter: string,
  desc: string,
};
