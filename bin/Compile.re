open Console



let argv =
  switch (Sys.argv |> Array.to_list) {
  | _ => Sys.argv
  };

let readCmt = cmtFile =>
  try(Cmt_format.read_cmt(cmtFile)) {
  | Cmi_format.Error(e) =>
    switch (e) {
    | _ => assert(false)
    }
  };

let cmtToTypedTree = inputCMT => {
  let {Cmt_format.cmt_annots} = inputCMT;

  [@warning "-8"]
  (
    switch (cmt_annots) {
    | Implementation(structure) => structure
    }
  );
};
let cwd = Sys.getcwd();
let r = Str.regexp(".bs.js");
let filename = cwd ++ "/" ++ argv[1];
let filename = Str.replace_first(r, "", filename);
let r = Str.regexp("/../../..");
let cmtfilename = Str.replace_first(r, "/bs", filename) ++ ".cmt";


let cmt = readCmt(cmtfilename);
let typedTree = cmtToTypedTree(cmt);

BitcoinCompiler.compile(~filename, typedTree);
