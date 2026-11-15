(* lib/core/data/sql.ml — Typed Parameterized SQL, Schema DDL & Embedded Relational Engine *)

type value =
  | Null
  | Int of int
  | Int64 of int64
  | Float of float
  | Text of string
  | Bool of bool

type row = (string * value) list

type dialect =
  | Sqlite
  | Postgres
  | Mysql

type query = {
  sql : string;
  params : value list;
}

let q sql params = { sql; params }

let value_to_string = function
  | Null -> "NULL"
  | Int i -> string_of_int i
  | Int64 i -> Int64.to_string i
  | Float f -> Printf.sprintf "%g" f
  | Text s -> s
  | Bool b -> if b then "true" else "false"

let get_value key (r : row) : value option =
  List.assoc_opt key r

let get_string key (r : row) : string option =
  match List.assoc_opt key r with
  | Some (Text s) -> Some s
  | Some (Int i) -> Some (string_of_int i)
  | Some (Int64 i) -> Some (Int64.to_string i)
  | Some (Float f) -> Some (Printf.sprintf "%g" f)
  | Some (Bool b) -> Some (if b then "true" else "false")
  | _ -> None

let get_int key (r : row) : int option =
  match List.assoc_opt key r with
  | Some (Int i) -> Some i
  | Some (Int64 i) -> Some (Int64.to_int i)
  | Some (Float f) -> Some ( int_of_float f )
  | Some (Text s) -> int_of_string_opt (String.trim s)
  | Some (Bool b) -> Some (if b then 1 else 0)
  | _ -> None

let get_int64 key (r : row) : int64 option =
  match List.assoc_opt key r with
  | Some (Int64 i) -> Some i
  | Some (Int i) -> Some (Int64.of_int i)
  | Some (Text s) -> Int64.of_string_opt (String.trim s)
  | _ -> None

let get_float key (r : row) : float option =
  match List.assoc_opt key r with
  | Some (Float f) -> Some f
  | Some (Int i) -> Some (float_of_int i)
  | Some (Int64 i) -> Some (Int64.to_float i)
  | Some (Text s) -> float_of_string_opt (String.trim s)
  | _ -> None

let get_bool key (r : row) : bool option =
  match List.assoc_opt key r with
  | Some (Bool b) -> Some b
  | Some (Int i) -> Some (i <> 0)
  | Some (Int64 i) -> Some (i <> 0L)
  | Some (Text s) ->
    let l = String.lowercase_ascii (String.trim s) in
    if l = "true" || l = "1" || l = "t" then Some true
    else if l = "false" || l = "0" || l = "f" then Some false
    else None
  | _ -> None

let value_to_json = function
  | Null -> Json.Null
  | Int i -> Json.Int i
  | Int64 i ->
    if i >= Int64.of_int min_int && i <= Int64.of_int max_int then Json.Int (Int64.to_int i)
    else Json.Float (Int64.to_float i)
  | Float f -> Json.Float f
  | Text s -> Json.String s
  | Bool b -> Json.Bool b

let value_of_json = function
  | Json.Null -> Null
  | Json.Bool b -> Bool b
  | Json.String s -> Text s
  | Json.Int i -> Int i
  | Json.Float f ->
    let rounded = Float.round f in
    if Float.abs (f -. rounded) < 1e-9 && Float.abs f < 1073741823.0 then
      Int (int_of_float rounded)
    else
      Float f
  | Json.Array _ as j -> Text (Json.to_string j)
  | Json.Object _ as j -> Text (Json.to_string j)

let row_to_json (r : row) : Json.t =
  Json.Object (List.map (fun (k, v) -> (k, value_to_json v)) r)

let row_of_json (j : Json.t) : (row, string) result =
  match j with
  | Json.Object fields ->
    Ok (List.map (fun (k, v) -> (k, value_of_json v)) fields)
  | _ -> Error "Expected JSON object for SQL row"

let rows_to_json (rows : row list) : Json.t =
  Json.Array (List.map row_to_json rows)

let rows_of_json (j : Json.t) : (row list, string) result =
  match j with
  | Json.Array items ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs ->
        (match row_of_json x with
         | Ok r -> loop (r :: acc) xs
         | Error e -> Error e)
    in
    loop [] items
  | _ -> Error "Expected JSON array of SQL rows"

(* Safe SQL string escaping: removes NUL bytes and doubles single quotes *)
let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    if c = '\x00' then ()
    else if c = '\'' then Buffer.add_string buf "''"
    else Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let format_literal (dialect : dialect) (v : value) : string =
  match v with
  | Null -> "NULL"
  | Int i -> string_of_int i
  | Int64 i -> Int64.to_string i
  | Float f ->
    if Float.is_nan f || Float.is_infinite f then "NULL"
    else Printf.sprintf "%.15g" f
  | Text s -> "'" ^ escape_string s ^ "'"
  | Bool b ->
    (match dialect with
     | Postgres -> if b then "TRUE" else "FALSE"
     | Sqlite | Mysql -> if b then "1" else "0")

(* Rewrites placeholders between ? and $1..$n according to target SQL dialect *)
let compile (dialect : dialect) (query : query) : string * value list =
  let sql = query.sql in
  let len = String.length sql in
  let buf = Buffer.create (len + 16) in
  let idx = ref 0 in
  let in_quote = ref false in
  let i = ref 0 in
  while !i < len do
    let c = sql.[!i] in
    if c = '\'' then begin
      Buffer.add_char buf c;
      if !in_quote && !i + 1 < len && sql.[!i + 1] = '\'' then begin
        Buffer.add_char buf '\'';
        incr i
      end else
        in_quote := not !in_quote;
      incr i
    end else if not !in_quote && c = '?' then begin
      incr idx;
      (match dialect with
       | Postgres -> Buffer.add_string buf (Printf.sprintf "$%d" !idx)
       | Sqlite | Mysql -> Buffer.add_char buf '?');
      incr i
    end else if not !in_quote && c = '$' && !i + 1 < len && sql.[!i + 1] >= '1' && sql.[!i + 1] <= '9' then begin
      let j = ref (!i + 1) in
      while !j < len && sql.[!j] >= '0' && sql.[!j] <= '9' do
        incr j
      done;
      let num_str = String.sub sql (!i + 1) (!j - !i - 1) in
      (match dialect with
       | Postgres ->
         Buffer.add_char buf '$';
         Buffer.add_string buf num_str
       | Sqlite | Mysql ->
         Buffer.add_char buf '?');
      i := !j
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  (Buffer.contents buf, query.params)

(* Safe parameter interpolation for CLI/text-based execution *)
let interpolate (dialect : dialect) (query : query) : string =
  let sql = query.sql in
  let params_arr = Array.of_list query.params in
  let n_params = Array.length params_arr in
  let len = String.length sql in
  let buf = Buffer.create (len + 32) in
  let pos_idx = ref 0 in
  let in_quote = ref false in
  let i = ref 0 in
  while !i < len do
    let c = sql.[!i] in
    if c = '\'' then begin
      Buffer.add_char buf c;
      if !in_quote && !i + 1 < len && sql.[!i + 1] = '\'' then begin
        Buffer.add_char buf '\'';
        incr i
      end else
        in_quote := not !in_quote;
      incr i
    end else if not !in_quote && c = '?' then begin
      let v = if !pos_idx < n_params then params_arr.(!pos_idx) else Null in
      incr pos_idx;
      Buffer.add_string buf (format_literal dialect v);
      incr i
    end else if not !in_quote && c = '$' && !i + 1 < len && sql.[!i + 1] >= '1' && sql.[!i + 1] <= '9' then begin
      let j = ref (!i + 1) in
      while !j < len && sql.[!j] >= '0' && sql.[!j] <= '9' do
        incr j
      done;
      let num = int_of_string (String.sub sql (!i + 1) (!j - !i - 1)) in
      let v = if num >= 1 && num <= n_params then params_arr.(num - 1) else Null in
      Buffer.add_string buf (format_literal dialect v);
      i := !j
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* Schema & DDL Generator *)

type col_type =
  | Col_integer
  | Col_bigint
  | Col_real
  | Col_text
  | Col_boolean
  | Col_timestamp

type column_def = {
  name : string;
  col_type : col_type;
  primary_key : bool;
  autoincrement : bool;
  not_null : bool;
  unique : bool;
  default_sql : string option;
  references : (string * string) option;
}

type table_schema = {
  table_name : string;
  columns : column_def list;
  indexes : (string * string list * bool) list;
}

let col
    ?(primary_key = false)
    ?(autoincrement = false)
    ?(not_null = false)
    ?(unique = false)
    ?default_sql
    ?references
    name
    col_type =
  {
    name;
    col_type;
    primary_key;
    autoincrement;
    not_null = not_null || primary_key;
    unique;
    default_sql;
    references;
  }

let table ?(indexes = []) table_name columns =
  { table_name; columns; indexes }

let col_type_sql (dialect : dialect) (c : column_def) : string =
  if c.primary_key && c.autoincrement then
    match dialect with
    | Postgres -> "SERIAL PRIMARY KEY"
    | Sqlite -> "INTEGER PRIMARY KEY AUTOINCREMENT"
    | Mysql -> "INT AUTO_INCREMENT PRIMARY KEY"
  else
    let base =
      match (dialect, c.col_type) with
      | (_, Col_integer) -> "INTEGER"
      | (_, Col_bigint) -> "BIGINT"
      | (Postgres, Col_real) -> "DOUBLE PRECISION"
      | (_, Col_real) -> "REAL"
      | (_, Col_text) -> "TEXT"
      | (Sqlite, Col_boolean) -> "INTEGER"
      | (_, Col_boolean) -> "BOOLEAN"
      | (Sqlite, Col_timestamp) -> "TEXT"
      | (_, Col_timestamp) -> "TIMESTAMPTZ"
    in
    let pk = if c.primary_key then " PRIMARY KEY" else "" in
    let nn = if c.not_null && not c.primary_key then " NOT NULL" else "" in
    let uq = if c.unique && not c.primary_key then " UNIQUE" else "" in
    let df =
      match c.default_sql with
      | Some d -> " DEFAULT " ^ d
      | None -> ""
    in
    let fk =
      match c.references with
      | Some (tbl, col_name) -> Printf.sprintf " REFERENCES %s(%s)" tbl col_name
      | None -> ""
    in
    base ^ pk ^ nn ^ uq ^ df ^ fk

let create_table_ddl (dialect : dialect) (schema : table_schema) : string list =
  let cols_sql =
    String.concat ", "
      (List.map (fun c -> Printf.sprintf "%s %s" c.name (col_type_sql dialect c)) schema.columns)
  in
  let create_tbl =
    Printf.sprintf "CREATE TABLE IF NOT EXISTS %s (%s);" schema.table_name cols_sql
  in
  let idx_stmts =
    List.map (fun (idx_name, cols, is_unique) ->
      let uq = if is_unique then "UNIQUE " else "" in
      Printf.sprintf "CREATE %sINDEX IF NOT EXISTS %s ON %s (%s);"
        uq idx_name schema.table_name (String.concat ", " cols)
    ) schema.indexes
  in
  create_tbl :: idx_stmts

(* Embedded Pure-OCaml Relational Engine *)

module Embedded = struct
  type table_data = {
    mutable schema : table_schema option;
    mutable rows : row list;
    mutable next_id : int;
  }

  type db = {
    tables : (string, table_data) Hashtbl.t;
    persist : (string -> string -> unit) option;
    load : (string -> string option) option;
  }

  let create ?persist ?load () : db =
    { tables = Hashtbl.create 16; persist; load }

  let sync_to_storage (db : db) (table_name : string) (t : table_data) : unit =
    match db.persist with
    | None -> ()
    | Some save ->
      let payload =
        Json.Object [
          ("next_id", Json.Int t.next_id);
          ("rows", rows_to_json t.rows);
        ]
      in
      save ("__sql_table:" ^ table_name) (Json.to_string payload)

  let ensure_table (db : db) (table_name : string) : table_data =
    let key = String.lowercase_ascii (String.trim table_name) in
    match Hashtbl.find_opt db.tables key with
    | Some t -> t
    | None ->
      let initial = { schema = None; rows = []; next_id = 1 } in
      (match db.load with
       | Some loader ->
         (match loader ("__sql_table:" ^ key) with
          | Some raw ->
            (match Json.parse raw with
             | Ok (Json.Object fields) ->
               (match List.assoc_opt "next_id" fields with
                | Some (Json.Int n) -> initial.next_id <- n
                | Some (Json.Float n) -> initial.next_id <- int_of_float n
                | _ -> ());
               (match List.assoc_opt "rows" fields with
                | Some r_json ->
                  (match rows_of_json r_json with
                   | Ok rs -> initial.rows <- rs
                   | Error _ -> ())
                | None -> ())
             | _ -> ())
          | None -> ())
       | None -> ());
      Hashtbl.replace db.tables key initial;
      initial

  let define_table (db : db) (schema : table_schema) : unit =
    let t = ensure_table db schema.table_name in
    t.schema <- Some schema;
    sync_to_storage db (String.lowercase_ascii schema.table_name) t

  let compare_values (a : value) (b : value) : int =
    match (a, b) with
    | (Null, Null) -> 0
    | (Null, _) -> -1
    | (_, Null) -> 1
    | (Int x, Int y) -> Int.compare x y
    | (Int64 x, Int64 y) -> Int64.compare x y
    | (Int x, Int64 y) -> Int64.compare (Int64.of_int x) y
    | (Int64 x, Int y) -> Int64.compare x (Int64.of_int y)
    | (Float x, Float y) -> Float.compare x y
    | (Int x, Float y) -> Float.compare (float_of_int x) y
    | (Float x, Int y) -> Float.compare x (float_of_int y)
    | (Bool x, Bool y) -> Bool.compare x y
    | (Text x, Text y) -> String.compare x y
    | (x, y) -> String.compare (value_to_string x) (value_to_string y)

  let insert (db : db) (table_name : string) (r : row) : (int, string) result =
    let key = String.lowercase_ascii (String.trim table_name) in
    let t = ensure_table db key in
    let assigned_id = t.next_id in
    let r_with_pk =
      match t.schema with
      | Some schema ->
        let pk_col = List.find_opt (fun c -> c.primary_key) schema.columns in
        (match pk_col with
         | Some pk when not (List.mem_assoc pk.name r) || List.assoc pk.name r = Null ->
           t.next_id <- assigned_id + 1;
           (pk.name, Int assigned_id) :: List.remove_assoc pk.name r
         | Some pk ->
           (match get_int pk.name r with
            | Some id when id >= t.next_id -> t.next_id <- id + 1
            | _ -> ());
           r
         | None ->
           if not (List.mem_assoc "id" r) then begin
             t.next_id <- assigned_id + 1;
             ("id", Int assigned_id) :: r
           end else r)
      | None ->
        if not (List.mem_assoc "id" r) then begin
          t.next_id <- assigned_id + 1;
          ("id", Int assigned_id) :: r
        end else begin
          (match get_int "id" r with
           | Some id when id >= t.next_id -> t.next_id <- id + 1
           | _ -> ());
          r
        end
    in
    let validation_err =
      match t.schema with
      | None -> None
      | Some schema ->
        List.find_map (fun (c : column_def) ->
          let v_opt = List.assoc_opt c.name r_with_pk in
          if c.not_null && (v_opt = None || v_opt = Some Null) && c.default_sql = None then
            Some (Printf.sprintf "NOT NULL constraint failed: %s.%s" schema.table_name c.name)
          else if (c.unique || c.primary_key) && v_opt <> None && v_opt <> Some Null then
            let v = Option.get v_opt in
            let dup =
              List.exists (fun existing ->
                match List.assoc_opt c.name existing with
                | Some ev -> compare_values ev v = 0
                | None -> false
              ) t.rows
            in
            if dup then
              Some (Printf.sprintf "UNIQUE constraint failed: %s.%s" schema.table_name c.name)
            else None
          else None
        ) schema.columns
    in
    match validation_err with
    | Some err -> Error err
    | None ->
      t.rows <- t.rows @ [ r_with_pk ];
      sync_to_storage db key t;
      let ret_id = Option.value (get_int "id" r_with_pk) ~default:assigned_id in
      Ok ret_id

  let select
      (db : db)
      (table_name : string)
      ?(where : (row -> bool) option)
      ?(order_by : (string * [ `Asc | `Desc ]) option)
      ?(limit : int option)
      ?(offset : int option)
      () : row list =
    let t = ensure_table db table_name in
    let filtered =
      match where with
      | None -> t.rows
      | Some pred -> List.filter pred t.rows
    in
    let sorted =
      match order_by with
      | None -> filtered
      | Some (col_name, dir) ->
        List.sort (fun r1 r2 ->
          let v1 = Option.value (List.assoc_opt col_name r1) ~default:Null in
          let v2 = Option.value (List.assoc_opt col_name r2) ~default:Null in
          let cmp = compare_values v1 v2 in
          match dir with `Asc -> cmp | `Desc -> -cmp
        ) filtered
    in
    let after_offset =
      match offset with
      | None | Some 0 -> sorted
      | Some off ->
        let rec drop n = function
          | [] -> []
          | _ :: xs when n > 0 -> drop (n - 1) xs
          | xs -> xs
        in
        drop off sorted
    in
    match limit with
    | None -> after_offset
    | Some lim ->
      let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs when n > 0 -> take (n - 1) (x :: acc) xs
        | _ -> List.rev acc
      in
      take lim [] after_offset

  let join
      (db : db)
      ~(left_table : string)
      ~(right_table : string)
      ~(on : row -> row -> bool) : (row * row) list =
    let left_rows = (ensure_table db left_table).rows in
    let right_rows = (ensure_table db right_table).rows in
    let acc = ref [] in
    List.iter (fun lr ->
      List.iter (fun rr ->
        if on lr rr then acc := (lr, rr) :: !acc
      ) right_rows
    ) left_rows;
    List.rev !acc

  let update
      (db : db)
      (table_name : string)
      ~(where : row -> bool)
      ~(set : row -> row) : (int, string) result =
    let key = String.lowercase_ascii (String.trim table_name) in
    let t = ensure_table db key in
    let count = ref 0 in
    let updated =
      List.map (fun r ->
        if where r then begin
          incr count;
          set r
        end else r
      ) t.rows
    in
    t.rows <- updated;
    sync_to_storage db key t;
    Ok !count

  let delete (db : db) (table_name : string) ~(where : row -> bool) : int =
    let key = String.lowercase_ascii (String.trim table_name) in
    let t = ensure_table db key in
    let before = List.length t.rows in
    t.rows <- List.filter (fun r -> not (where r)) t.rows;
    let removed = before - List.length t.rows in
    if removed > 0 then sync_to_storage db key t;
    removed

  let transaction (db : db) (f : db -> ('a, string) result) : ('a, string) result =
    let snapshot =
      Hashtbl.fold (fun k v acc ->
        (k, { schema = v.schema; rows = v.rows; next_id = v.next_id }) :: acc
      ) db.tables []
    in
    match f db with
    | Ok res -> Ok res
    | Error err ->
      Hashtbl.clear db.tables;
      List.iter (fun (k, v) ->
        Hashtbl.replace db.tables k v;
        sync_to_storage db k v
      ) snapshot;
      Error err

  (* Lightweight SQL parser/executor for Embedded mode so parameterized SQL queries work everywhere *)
  let strip_semi s =
    let s = String.trim s in
    if String.length s > 0 && s.[String.length s - 1] = ';' then
      String.trim (String.sub s 0 (String.length s - 1))
    else s

  let parse_literal_token (tok : string) (params : value array) (p_idx : int ref) : value =
    let t = String.trim tok in
    if t = "?" then begin
      let v = if !p_idx < Array.length params then params.(!p_idx) else Null in
      incr p_idx;
      v
    end else if String.length t >= 2 && t.[0] = '$' && t.[1] >= '1' && t.[1] <= '9' then begin
      let n = int_of_string (String.sub t 1 (String.length t - 1)) in
      if n >= 1 && n <= Array.length params then params.(n - 1) else Null
    end else if String.length t >= 2 && t.[0] = '\'' && t.[String.length t - 1] = '\'' then
      Text (String.sub t 1 (String.length t - 2))
    else if String.uppercase_ascii t = "NULL" then Null
    else if String.uppercase_ascii t = "TRUE" then Bool true
    else if String.uppercase_ascii t = "FALSE" then Bool false
    else
      match int_of_string_opt t with
      | Some i -> Int i
      | None ->
        (match float_of_string_opt t with
         | Some f -> Float f
         | None -> Text t)

  let split_csv (s : string) : string list =
    let len = String.length s in
    let buf = Buffer.create 32 in
    let acc = ref [] in
    let in_q = ref false in
    for i = 0 to len - 1 do
      let c = s.[i] in
      if c = '\'' then begin
        in_q := not !in_q;
        Buffer.add_char buf c
      end else if c = ',' && not !in_q then begin
        acc := String.trim (Buffer.contents buf) :: !acc;
        Buffer.clear buf
      end else
        Buffer.add_char buf c
    done;
    let last = String.trim (Buffer.contents buf) in
    if last <> "" then acc := last :: !acc;
    List.rev !acc

  let find_kw (upper : string) (kw : string) : int option =
    let ulen = String.length upper in
    let klen = String.length kw in
    let rec search i =
      if i + klen > ulen then None
      else if String.sub upper i klen = kw then Some i
      else search (i + 1)
    in
    search 0

  let build_where_pred (clause : string) (params : value array) (p_idx : int ref) : row -> bool =
    let c = String.trim clause in
    if c = "" then (fun _ -> true)
    else
      let ops = [ ">="; "<="; "!="; "<>"; "="; ">"; "<" ] in
      let found_op =
        List.find_map (fun op ->
          match find_kw c op with
          | Some pos ->
            let col = String.trim (String.sub c 0 pos) in
            let rhs = String.trim (String.sub c (pos + String.length op) (String.length c - pos - String.length op)) in
            Some (col, op, parse_literal_token rhs params p_idx)
          | None -> None
        ) ops
      in
      match found_op with
      | None -> (fun _ -> true)
      | Some (col_name, op, target_val) ->
        (fun r ->
          let actual = Option.value (List.assoc_opt col_name r) ~default:Null in
          let cmp = compare_values actual target_val in
          match op with
          | "=" -> cmp = 0
          | "!=" | "<>" -> cmp <> 0
          | ">" -> cmp > 0
          | ">=" -> cmp >= 0
          | "<" -> cmp < 0
          | "<=" -> cmp <= 0
          | _ -> false)

  let exec_sql (db : db) (query : query) : (int, string) result =
    let raw = strip_semi query.sql in
    let upper = String.uppercase_ascii raw in
    let params = Array.of_list query.params in
    let p_idx = ref 0 in
    if String.length upper >= 12 && String.sub upper 0 12 = "CREATE TABLE" then begin
      let after_ct =
        match find_kw upper "IF NOT EXISTS" with
        | Some pos -> String.trim (String.sub raw (pos + 13) (String.length raw - pos - 13))
        | None -> String.trim (String.sub raw 12 (String.length raw - 12))
      in
      let paren_pos = Option.value (String.index_opt after_ct '(') ~default:(String.length after_ct) in
      let tbl_name = String.trim (String.sub after_ct 0 paren_pos) in
      let _ = ensure_table db tbl_name in
      Ok 0
    end else if String.length upper >= 11 && String.sub upper 0 11 = "CREATE INDEX"
             || String.length upper >= 18 && String.sub upper 0 18 = "CREATE UNIQUE INDEX" then
      Ok 0
    else if String.length upper >= 11 && String.sub upper 0 11 = "INSERT INTO" then begin
      let rest = String.trim (String.sub raw 11 (String.length raw - 11)) in
      let rest_up = String.uppercase_ascii rest in
      match find_kw rest_up "VALUES" with
      | None -> Error "INSERT statement missing VALUES clause"
      | Some vpos ->
        let left_part = String.trim (String.sub rest 0 vpos) in
        let right_part = String.trim (String.sub rest (vpos + 6) (String.length rest - vpos - 6)) in
        let lparen = Option.value (String.index_opt left_part '(') ~default:(String.length left_part) in
        let rparen = Option.value (String.rindex_opt left_part ')') ~default:(String.length left_part) in
        let tbl_name = String.trim (String.sub left_part 0 lparen) in
        let cols =
          if lparen < rparen then
            split_csv (String.sub left_part (lparen + 1) (rparen - lparen - 1))
          else []
        in
        let v_lparen = Option.value (String.index_opt right_part '(') ~default:0 in
        let v_rparen = Option.value (String.rindex_opt right_part ')') ~default:(String.length right_part - 1) in
        let val_tokens =
          split_csv (String.sub right_part (v_lparen + 1) (max 0 (v_rparen - v_lparen - 1)))
        in
        let vals = List.map (fun tok -> parse_literal_token tok params p_idx) val_tokens in
        let rec zip cs vs =
          match (cs, vs) with
          | (c :: cs', v :: vs') -> (c, v) :: zip cs' vs'
          | _ -> []
        in
        (match insert db tbl_name (zip cols vals) with
         | Ok _ -> Ok 1
         | Error e -> Error e)
    end else if String.length upper >= 11 && String.sub upper 0 11 = "DELETE FROM" then begin
      let rest = String.trim (String.sub raw 11 (String.length raw - 11)) in
      let rest_up = String.uppercase_ascii rest in
      let (tbl_name, pred) =
        match find_kw rest_up "WHERE" with
        | None -> (rest, (fun _ -> true))
        | Some wpos ->
          let t = String.trim (String.sub rest 0 wpos) in
          let w = String.trim (String.sub rest (wpos + 5) (String.length rest - wpos - 5)) in
          (t, build_where_pred w params p_idx)
      in
      Ok (delete db tbl_name ~where:pred)
    end else if String.length upper >= 6 && String.sub upper 0 6 = "UPDATE" then begin
      let rest = String.trim (String.sub raw 6 (String.length raw - 6)) in
      let rest_up = String.uppercase_ascii rest in
      match find_kw rest_up "SET" with
      | None -> Error "UPDATE statement missing SET clause"
      | Some spos ->
        let tbl_name = String.trim (String.sub rest 0 spos) in
        let after_set = String.trim (String.sub rest (spos + 3) (String.length rest - spos - 3)) in
        let after_set_up = String.uppercase_ascii after_set in
        let (set_part, where_part) =
          match find_kw after_set_up "WHERE" with
          | None -> (after_set, "")
          | Some wpos ->
            ( String.trim (String.sub after_set 0 wpos),
              String.trim (String.sub after_set (wpos + 5) (String.length after_set - wpos - 5)) )
        in
        let assignments =
          List.filter_map (fun item ->
            match String.index_opt item '=' with
            | None -> None
            | Some eq ->
              let k = String.trim (String.sub item 0 eq) in
              let v_tok = String.trim (String.sub item (eq + 1) (String.length item - eq - 1)) in
              Some (k, parse_literal_token v_tok params p_idx)
          ) (split_csv set_part)
        in
        let pred = build_where_pred where_part params p_idx in
        update db tbl_name ~where:pred ~set:(fun r ->
          List.fold_left (fun acc (k, v) ->
            (k, v) :: List.remove_assoc k acc
          ) r assignments
        )
    end else
      Ok 0

  let query_sql (db : db) (query : query) : (row list, string) result =
    let raw = strip_semi query.sql in
    let upper = String.uppercase_ascii raw in
    let params = Array.of_list query.params in
    let p_idx = ref 0 in
    if String.length upper < 6 || String.sub upper 0 6 <> "SELECT" then
      match exec_sql db query with
      | Ok _ -> Ok []
      | Error e -> Error e
    else
      match find_kw upper "FROM" with
      | None -> Error "SELECT statement missing FROM clause"
      | Some fpos ->
        let cols_part = String.trim (String.sub raw 6 (fpos - 6)) in
        let after_from = String.trim (String.sub raw (fpos + 4) (String.length raw - fpos - 4)) in
        let after_up = String.uppercase_ascii after_from in
        let limit_pos = find_kw after_up "LIMIT" in
        let order_pos = find_kw after_up "ORDER BY" in
        let where_pos = find_kw after_up "WHERE" in
        let tbl_end =
          List.fold_left (fun acc opt ->
            match opt with Some p when p < acc -> p | _ -> acc
          ) (String.length after_from) [ where_pos; order_pos; limit_pos ]
        in
        let tbl_name = String.trim (String.sub after_from 0 tbl_end) in
        let where_pred =
          match where_pos with
          | None -> None
          | Some wp ->
            let w_end =
              List.fold_left (fun acc opt ->
                match opt with Some p when p > wp && p < acc -> p | _ -> acc
              ) (String.length after_from) [ order_pos; limit_pos ]
            in
            let clause = String.sub after_from (wp + 5) (w_end - wp - 5) in
            Some (build_where_pred clause params p_idx)
        in
        let order_spec =
          match order_pos with
          | None -> None
          | Some op ->
            let o_end =
              match limit_pos with
              | Some lp when lp > op -> lp
              | _ -> String.length after_from
            in
            let o_str = String.trim (String.sub after_from (op + 8) (o_end - op - 8)) in
            let parts = List.filter (fun s -> s <> "") (String.split_on_char ' ' o_str) in
            (match parts with
             | col :: dir :: _ when String.uppercase_ascii dir = "DESC" -> Some (col, `Desc)
             | col :: _ -> Some (col, `Asc)
             | [] -> None)
        in
        let limit_val =
          match limit_pos with
          | None -> None
          | Some lp ->
            let l_str = String.trim (String.sub after_from (lp + 5) (String.length after_from - lp - 5)) in
            (match parse_literal_token l_str params p_idx with
             | Int n -> Some n
             | _ -> int_of_string_opt l_str)
        in
        let rows = select db tbl_name ?where:where_pred ?order_by:order_spec ?limit:limit_val () in
        if cols_part = "*" then Ok rows
        else if String.uppercase_ascii cols_part = "COUNT(*)" then
          Ok [ [ ("count", Int (List.length rows)) ] ]
        else
          let wanted = split_csv cols_part in
          Ok (List.map (fun r ->
            List.filter_map (fun c ->
              match List.assoc_opt c r with
              | Some v -> Some (c, v)
              | None -> None
            ) wanted
          ) rows)
end
