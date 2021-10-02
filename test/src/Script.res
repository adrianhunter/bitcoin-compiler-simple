@asm([OP_SUB, OP_DUDE])
let fooo = (a, b) => {
  %raw("{}")
}

// open Opcode
// open Stack

// let stack = %raw("[]")

@asm(OP_ADD)
let add = (stack, a, b) => {
  stack
}
@asm(OP_1ADD)
let add1 = a => {
  %raw("{}")
}
@asm(OP_SUB1)
let sub1 = stack => {
  stack
}
@asm(OP_SIZE)
let size = (stack): int => {
  stack |> ignore
  %raw("1")
}
@asm(OP_HASH160)
let hash160 = stack => {
  stack
}
@asm(OP_DUP)
let dup = stack => {
  stack
}
let verify = stack => {
  stack
}
@asm(OP_CHECKSIG)
let checksig = (sig, pubkey): bool => {
  true
}
@asm(OP_HASH256)
let sha256 = stack => {
  %raw("{}")
}
@asm(OP_NUM2BIN)
let num2bin = (int: int, b: int): bytes => {
  Obj.magic(int)
}
@asm(OP_BIN2NUM)
let bin2num = (bytes: bytes): int => {
  Obj.magic(bytes)
}

@asm(OP_BIN2NUM)
let int_of_string = (string: string): int => {
  (Obj.magic(string): int)
}

let string_of_bytes = (bytes: bytes): string => {
  (Obj.magic(bytes): string)
}
let bytes_of_string = (bytes: string): bytes => {
  Obj.magic(bytes)
}

@asm(OP_CAT)
let cat = (bytes: bytes, bytes: bytes): bytes => {
  %raw("{}")
}
@asm(OP_SPLIT)
let split = (stack, post: int) => {
  stack
}
@asm(OP_DROP)
let drop = stack => {
  stack
}
@asm(OP_INVERT)
let invert = stack => {
  stack
}

@asm(OP_EQUAL)
let equal = (a, b): bool => {
  %raw("{}")
}

@asm(OP_SWAP)
let swap = stack => {
  stack
}
exception NotEqualverify(string)
@asm(OP_EQUALVERIFY)
let equalverify = (stack, value) => {
  stack
}

/////////////////////////////////////////////////
/////////////// Infix operators ////////////////////
/////////////////////////////////////////////////
/////////////////////////////////////////////////
@asm(OP_EQUAL)
let \"==" = (a, b) => {
  a === b
}
@asm(OP_EQUAL)
let \"=" = (a, b) => {
  a == b
}
@asm(OP_AND)
let \"&&" = (a, b) => {
  a && b
}
@asm(OP_OR)
let \"||" = (a, b) => {
  a || b
}
@asm(OP_ADD)
let \"+" = (a: int, b: int): int => {
  a + b
}
@asm(OP_CAT)
let \"^" = (a: string, b: string) => {
  a ++ b
}

@asm(OP_SUB)
let \"-" = (a, b) => {
  a - b
}
@asm(OP_DIV)
let \"/" = (a, b) => {
  a / b
}
@asm(OP_MUL)
let \"*" = (a, b) => {
  a * b
}
@asm(OP_GREATERTHAN)
let \">" = (a, b): bool => {
  a > b
}
@asm(OP_GREATERTHANOREQUAL)
let \">=" = (a, b): bool => {
  a >= b
}
@asm(OP_LESSTHAN)
let \"<" = (a, b): bool => {
  a < b
}
@asm(OP_LESSTHANOREQUAL)
let \"<=" = (a, b): bool => {
  a <= b
}

@asm([OP_TUCK, OP_VER])
let foo = a => {
  %raw("{}")
}

let \"= []" = a => {
  [a]
}

// None |>> (a) => a

// | "*" => MUL
// | "^" => CAT
// | ">" => OP_GREATERTHAN
// | ">=" => OP_GREATERTHANOREQUAL
// | "<" => OP_LESSTHAN
// | "<=" => OP_LESSTHANOREQUAL

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

let loop = (fn, times: int) => {
  fn(1)
}
@bitcoin
let slice = (bytes: bytes, start, end_) => {
  bytes->split(start)->drop->split(end_)
}
@bitcoin
let sliceString = (string: string, start, end_): string => {
  string->split(start)->swap->drop->split(end_ - start)
}
// module String = {
//   let length = (string) => {
//     1
//   }
// }

let \"|!" = (o, d) =>
  switch o {
  | None => failwith(d)
  | Some(v) => v
  }

let \"|?>>" = (o, fn) =>
  switch o {
  | None => None
  | Some(v) => Some(fn(v))
  }

// x => \"|?>>"(x, None)
// None -> |?>> (  x => x)
let \"|?>" = (o, fn) =>
  switch o {
  | None => None
  | Some(v) => fn(v)
  }
// None |>> 123

// 123 >>= 123
// let x = Some(14) |?> (x => None)

/////////////////////////////////////////////////
/////////////// Basic modules  ////////////////////
/////////////////////////////////////////////////
/////////////////////////////////////////////////

@bitcoin
module String = {
  @asm(OP_SIZE)
  let length = (string: string): int => {
    %raw("{}")
  }
  let slice = (string: string, start, end_): string => {
    string->split(start)->swap->drop->split(end_ - start)
  }

  @asm(OP_BIN2NUM)
  let string_of_int = (a: int, int): string => {
    (Obj.magic(a): string)
  }
}
