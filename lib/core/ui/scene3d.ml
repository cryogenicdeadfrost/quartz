(* Declarative Live 3D Scene Graph AST for Quartz (`Quartz.Scene3d`).
   Runs cleanly in pure OCaml on servers/unikernels, serializing to a structured
   JSON scene descriptor executed by `quartz.browser`'s 60fps WebGL2 / Three.js
   runtime inside `[data-q-scene3d]` canvases. *)

type vec3 = float * float * float

type geometry =
  | Box of { w : float; h : float; d : float }
  | Sphere of { radius : float; segments : int }
  | Plane of { w : float; h : float }
  | Torus of { radius : float; tube : float }

type material =
  | Pbr of { color : string; metalness : float; roughness : float; emissive : string }
  | Basic of { color : string; wireframe : bool }
  | Shader of { vertex_glsl : string; fragment_glsl : string; uniforms : (string * float) list }
  | Wgsl_shader of { vertex_wgsl : string; fragment_wgsl : string; uniforms : (string * float) list }

type light_kind =
  | Ambient of { color : string; intensity : float }
  | Point of { color : string; intensity : float; pos : vec3 }
  | Directional of { color : string; intensity : float; dir : vec3 }

type camera = {
  fov : float;
  near : float;
  far : float;
  pos : vec3;
  look_at : vec3;
}

type mesh = {
  name : string;
  geometry : geometry;
  material : material;
  pos : vec3;
  rot : vec3;
  scale : vec3;
  spin : vec3;
}

type scene = {
  background : string;
  camera : camera;
  lights : light_kind list;
  meshes : mesh list;
  renderer : [ `WebGPU | `WebGL2 | `ThreeJS ];
}

let perspective_camera ?(fov = 45.0) ?(near = 0.1) ?(far = 100.0) ?(pos = (0.0, 1.8, 5.0)) ?(look_at = (0.0, 0.0, 0.0)) () =
  { fov; near; far; pos; look_at }

let ambient_light ?(color = "#ffffff") ?(intensity = 0.35) () =
  Ambient { color; intensity }

let point_light ?(color = "#38bdf8") ?(intensity = 1.2) ?(pos = (3.0, 4.0, 3.0)) () =
  Point { color; intensity; pos }

let directional_light ?(color = "#f8fafc") ?(intensity = 0.9) ?(dir = (-1.0, -2.0, -1.5)) () =
  Directional { color; intensity; dir }

let mesh
    ?(name = "mesh")
    ?(geometry = Box { w = 1.2; h = 1.2; d = 1.2 })
    ?(material = Pbr { color = "#38bdf8"; metalness = 0.6; roughness = 0.25; emissive = "#000000" })
    ?(pos = (0.0, 0.0, 0.0))
    ?(rot = (0.0, 0.0, 0.0))
    ?(scale = (1.0, 1.0, 1.0))
    ?(spin = (0.4, 0.9, 0.15))
    () =
  { name; geometry; material; pos; rot; scale; spin }

let create
    ?(background = "#020617")
    ?(camera = perspective_camera ())
    ?(lights = [ ambient_light (); directional_light () ])
    ?(renderer = `WebGL2)
    meshes =
  { background; camera; lights; meshes; renderer }

let vec3_json (x, y, z) =
  Json.array [ Json.float x; Json.float y; Json.float z ]

let geometry_json = function
  | Box { w; h; d } ->
    Json.obj [ ("kind", Json.string "box"); ("w", Json.float w); ("h", Json.float h); ("d", Json.float d) ]
  | Sphere { radius; segments } ->
    Json.obj [ ("kind", Json.string "sphere"); ("radius", Json.float radius); ("segments", Json.int segments) ]
  | Plane { w; h } ->
    Json.obj [ ("kind", Json.string "plane"); ("w", Json.float w); ("h", Json.float h) ]
  | Torus { radius; tube } ->
    Json.obj [ ("kind", Json.string "torus"); ("radius", Json.float radius); ("tube", Json.float tube) ]

let material_json = function
  | Pbr { color; metalness; roughness; emissive } ->
    Json.obj
      [
        ("kind", Json.string "pbr");
        ("color", Json.string color);
        ("metalness", Json.float metalness);
        ("roughness", Json.float roughness);
        ("emissive", Json.string emissive);
      ]
  | Basic { color; wireframe } ->
    Json.obj [ ("kind", Json.string "basic"); ("color", Json.string color); ("wireframe", Json.bool wireframe) ]
  | Shader { vertex_glsl; fragment_glsl; uniforms } ->
    Json.obj
      [
        ("kind", Json.string "shader");
        ("vertex", Json.string vertex_glsl);
        ("fragment", Json.string fragment_glsl);
        ("uniforms", Json.obj (List.map (fun (k, v) -> (k, Json.float v)) uniforms));
      ]
  | Wgsl_shader { vertex_wgsl; fragment_wgsl; uniforms } ->
    Json.obj
      [
        ("kind", Json.string "wgsl_shader");
        ("vertexWgsl", Json.string vertex_wgsl);
        ("fragmentWgsl", Json.string fragment_wgsl);
        ("uniforms", Json.obj (List.map (fun (k, v) -> (k, Json.float v)) uniforms));
      ]

let light_json = function
  | Ambient { color; intensity } ->
    Json.obj [ ("kind", Json.string "ambient"); ("color", Json.string color); ("intensity", Json.float intensity) ]
  | Point { color; intensity; pos } ->
    Json.obj
      [ ("kind", Json.string "point"); ("color", Json.string color); ("intensity", Json.float intensity); ("pos", vec3_json pos) ]
  | Directional { color; intensity; dir } ->
    Json.obj
      [ ("kind", Json.string "directional"); ("color", Json.string color); ("intensity", Json.float intensity); ("dir", vec3_json dir) ]

let to_json (s : scene) : Json.t =
  Json.obj
    [
      ("renderer", Json.string (match s.renderer with `WebGPU -> "webgpu" | `WebGL2 -> "webgl2" | `ThreeJS -> "threejs"));
      ("background", Json.string s.background);
      ( "camera",
        Json.obj
          [
            ("fov", Json.float s.camera.fov);
            ("near", Json.float s.camera.near);
            ("far", Json.float s.camera.far);
            ("pos", vec3_json s.camera.pos);
            ("look_at", vec3_json s.camera.look_at);
          ] );
      ("lights", Json.array (List.map light_json s.lights));
      ( "meshes",
        Json.array
          (List.map
             (fun m ->
               Json.obj
                 [
                   ("name", Json.string m.name);
                   ("geometry", geometry_json m.geometry);
                   ("material", material_json m.material);
                   ("pos", vec3_json m.pos);
                   ("rot", vec3_json m.rot);
                   ("scale", vec3_json m.scale);
                   ("spin", vec3_json m.spin);
                 ])
             s.meshes) );
    ]

let to_json_string s = Json.to_string (to_json s)
