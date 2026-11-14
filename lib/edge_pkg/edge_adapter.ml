(* WinterCG / Vercel Edge / Cloudflare Workers / Deno Deploy Adapter (`quartz.edge`).
   Zero Unix dependency -- wraps any `Quartz.App` into a Fetch-compatible JSON
   or direct handler callable from js_of_ocaml / wasm_of_ocaml exports. *)

open Quartz_core

let parse_url_path_and_query raw_url =
  (* Accepts full URL like "https://example.com/analytics?region=eu" or path "/analytics?..." *)
  let after_proto =
    match String.index_opt raw_url ':' with
    | Some idx when idx + 2 < String.length raw_url && String.sub raw_url (idx + 1) 2 = "//" ->
      (match String.index_from_opt raw_url (idx + 3) '/' with
       | Some slash -> String.sub raw_url slash (String.length raw_url - slash)
       | None -> "/")
    | _ -> raw_url
  in
  Http.split_url_and_query after_proto

module Make
    (C : Io.CLOCK)
    (S : Io.STORAGE with type 'a io = 'a)
    (P : Io.PUBSUB) =
struct
  module Core_app = App.Make (C) (S) (P)

  let handle_fetch_json (app : Core_app.t) (fetch_json : string) : string =
    let req =
      match Json.parse fetch_json with
      | Error _ ->
        {
          Middleware.meth = "GET";
          path = "/";
          query = [];
          headers = [];
          body = "";
          client_ip = "0.0.0.0";
        }
      | Ok j ->
        let meth =
          Option.bind (Json.get_field "method" j) Json.get_string |> Option.value ~default:"GET"
        in
        let raw_url =
          Option.bind (Json.get_field "url" j) Json.get_string |> Option.value ~default:"/"
        in
        let path, query = parse_url_path_and_query raw_url in
        let headers =
          match Json.get_field "headers" j with
          | Some (Json.Object kvs) ->
            List.filter_map
              (fun (k, v) -> Option.map (fun sv -> (k, sv)) (Json.get_string v))
              kvs
          | _ -> []
        in
        let body =
          Option.bind (Json.get_field "body" j) Json.get_string |> Option.value ~default:""
        in
        let ip = Http.extract_client_ip ~peer_ip:"0.0.0.0" headers in
        { Middleware.meth = String.uppercase_ascii meth; path; query; headers; body; client_ip = ip }
    in
    let res = Core_app.handle app req in
    let status_num = Middleware.status_code_of_res res in
    let all_headers =
      ("Content-Type", res.content_type) :: List.remove_assoc "Content-Type" res.headers
    in
    Json.to_string
      (Json.obj
         [
           ("status", Json.int status_num);
           ("headers", Json.obj (List.map (fun (k, v) -> (k, Json.string v)) all_headers));
           ("body", Json.string res.body);
         ])

  (* Generates a standard WinterCG `export default { async fetch(request, env, ctx) }` wrapper *)
  let wintercg_entry_js ~compiled_wasm_or_js_module =
    Printf.sprintf
      {|import { handleFetchJson } from %S;

export default {
  async fetch(request, env, ctx) {
    const hdrs = {};
    request.headers.forEach((v, k) => { hdrs[k] = v; });
    const body = (request.method === 'GET' || request.method === 'HEAD') ? '' : await request.text();
    const out = JSON.parse(handleFetchJson(JSON.stringify({
      method: request.method,
      url: request.url,
      headers: hdrs,
      body
    })));
    return new Response(out.body, { status: out.status, headers: out.headers });
  }
};
|}
      compiled_wasm_or_js_module
end
