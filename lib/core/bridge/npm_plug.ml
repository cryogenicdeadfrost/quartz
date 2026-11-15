(* Universal NPM & JavaScript Ecosystem Plug System (`Quartz.Npm_plug`).
   Lets standard OCaml modules mount any npm package on the client via ESM/CDN
   importmaps (vanilla JS, Web Components, or React) OR invoke npm packages on
   the server via any `Io.PLUG_TRANSPORT` worker without manual hand-porting.
   Zero Unix dependency. *)

type framework = [ `Vanilla | `React | `Web_component ]

let registered_plugs : (string, string) Hashtbl.t = Hashtbl.create 16

let register_plug_metadata ~pkg ~version =
  Hashtbl.replace registered_plugs pkg version

let list_registered_plugs () : (string * string) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) registered_plugs []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let framework_to_string = function
  | `Vanilla -> "vanilla"
  | `React -> "react"
  | `Web_component -> "webcomponent"

let component
    ~pkg
    ?(version = "latest")
    ?(export_name = "default")
    ?(framework : framework = `Vanilla)
    ?(cdn_base = "https://esm.sh")
    ?(stylesheets = [])
    ?(props : (string * Json.t) list = [])
    ?style
    ?(fallback : Dom.node list = [])
    () : Dom.node =
  register_plug_metadata ~pkg ~version;
  let props_json = Json.to_string (Json.obj props) in
  let css_attr = String.concat "," stylesheets in
  let attrs =
    ref
      [
        Dom.attr "data-q-plug" "1";
        Dom.attr "data-q-pkg" pkg;
        Dom.attr "data-q-ver" version;
        Dom.attr "data-q-export" export_name;
        Dom.attr "data-q-framework" (framework_to_string framework);
        Dom.attr "data-q-cdn" cdn_base;
        Dom.attr "data-q-props" props_json;
      ]
  in
  if css_attr <> "" then attrs := Dom.attr "data-q-css" css_attr :: !attrs;
  Dom.el "quartz-plug" ?style ~attrs:!attrs fallback

let react_component
    ~pkg
    ?version
    ?export_name
    ?cdn_base
    ?stylesheets
    ?props
    ?style
    ?fallback
    () : Dom.node =
  component ~pkg ?version ?export_name ~framework:`React ?cdn_base ?stylesheets ?props ?style ?fallback ()

let web_component
    ~tag
    ~pkg
    ?(version = "latest")
    ?(cdn_base = "https://esm.sh")
    ?(attrs = [])
    ?(props : (string * Json.t) list = [])
    ?style
    kids : Dom.node =
  register_plug_metadata ~pkg ~version;
  let base_attrs =
    [
      Dom.attr "data-q-plug" "1";
      Dom.attr "data-q-pkg" pkg;
      Dom.attr "data-q-ver" version;
      Dom.attr "data-q-export" "default";
      Dom.attr "data-q-framework" "webcomponent";
      Dom.attr "data-q-cdn" cdn_base;
      Dom.attr "data-q-props" (Json.to_string (Json.obj props));
    ]
    @ attrs
  in
  Dom.el tag ?style ~attrs:base_attrs kids

let import_map_node (entries : (string * string) list) : Dom.node =
  let json = Json.obj [ ("imports", Json.obj (List.map (fun (k, v) -> (k, Json.string v)) entries)) ] in
  Dom.el "script"
    ~attrs:[ Dom.attr "type" "importmap" ]
    [ Dom.raw_unsafe (Json.to_string json) ]

(* Typed Server-Side NPM Invocation over `Io.PLUG_TRANSPORT` *)
let call_typed
    (module T : Io.PLUG_TRANSPORT)
    ~(pkg : string)
    ~(fn : string)
    ~(encode : 'req -> Json.t)
    ~(decode : 'res Json.Decode.decoder)
    (arg : 'req) : ('res, string) result =
  let args_json = Json.to_string (encode arg) in
  match T.invoke ~pkg ~fn ~args_json with
  | Error e -> Error e
  | Ok raw -> Json.Decode.decode_string decode raw
