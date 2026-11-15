(* Top-level ergonomic facade for Quartz v4.0 ("One Core, Many Targets").
   `open Quartz` gives access to every core, browser, edge, mirage, vercel,
   and unix subsystem + all backward-compatible v3 hooks & UI helpers. *)

(* 1. Pure Core Subsystems (`quartz.core` -- Zero Unix dependencies) *)
module Io = Quartz_core.Io
module Json = Quartz_core.Json
module Crypto_util = Quartz_core.Crypto_util
module Cookie = Quartz_core.Cookie
module I18n = Quartz_core.I18n
module Signal = Quartz_core.Signal
module Resource = Quartz_core.Resource
module Style = Quartz_core.Style
module Palette = Quartz_core.Style.Palette
module Contrast = Quartz_core.Contrast
module Scene3d = Quartz_core.Scene3d
module Dom = Quartz_core.Dom
module Headless = Quartz_core.Headless
module Router = Quartz_core.Router
module Form = Quartz_core.Form
module Charts = Quartz_core.Charts
module Components = Quartz_core.Components
module Middleware = Quartz_core.Middleware
module Isr = Quartz_core.Isr
module Pubsub = Quartz_core.Pubsub
module Websocket = Quartz_core.Websocket
module Multipart = Quartz_core.Multipart
module Http = Quartz_core.Http
module Store = Quartz_core.Store
module Sql = Quartz_core.Sql
module Jobs = Quartz_core.Jobs
module Tui = Quartz_core.Tui
module Custom_renderer = Quartz_core.Custom_renderer
module App = Quartz_core.App
module Npm_plug = Quartz_core.Npm_plug
module Debug_registry = Quartz_core.Debug_registry

(* 2. Target Adapters (`quartz.browser`, `quartz.edge`, `quartz.mirage`, `quartz.vercel`, `quartz.unix`) *)
module Browser_runtime = Quartz_browser.Browser_runtime
module Js_bridge = Quartz_browser.Browser_runtime.Js_bridge
module Edge_adapter = Quartz_edge.Edge_adapter
module Mirage_adapter = Quartz_mirage.Mirage_adapter
module Vercel_runtime = Quartz_vercel.Vercel_runtime
module Unix_adapters = Quartz_unix.Unix_adapters
module Db_driver = Quartz_unix.Db_driver
module Server = Quartz_unix.Server
module Deploy = Quartz_unix.Deploy
module Scaffolder = Quartz_unix.Scaffolder
module Import_html = Quartz_unix.Import_html
module Import_dts = Quartz_unix.Import_dts
module Npm_server_plug = Quartz_unix.Npm_server_plug
module Vite = Quartz_unix.Vite

(* 3. Reactive & Async Hooks *)
let use_state = Signal.create
let use_signal = Signal.make
let use_memo = Signal.computed
let use_effect = Signal.watch
let use_store = Signal.create_store
let use_history = Signal.make_history
let use_lens = Signal.lens
let use_resource = Resource.make
let undo = Signal.undo
let redo = Signal.redo
let batch = Signal.batch
let ( !~ ) = Signal.( !~ )
let ( <~ ) = Signal.( <~ )

(* 4. WCAG + APCA + OKLCH Color Contrast Shortcuts *)
let contrast_ratio = Contrast.contrast_ratio
let apca_lc = Contrast.apca_lc
let pick_ink = Contrast.pick_ink
let auto_text_color = Contrast.auto_text_color
let passes_aa = Contrast.passes_aa
let passes_aaa = Contrast.passes_aaa
let passes_apca = Contrast.passes_apca
let generate_scale = Contrast.generate_scale

(* 5. Declarative HTML5, SVG, Web Components, Islands, 3D & Suspense *)
type node = Dom.node

let empty = Dom.empty
let text = Dom.text
let textf = Dom.textf
let rx = Dom.rx
let rx_text = Dom.rx_text
let show = Dom.show
let each = Dom.each
let fragment = Dom.fragment
let suspense = Dom.suspense
let custom_element = Dom.custom_element
let shadow_root = Dom.shadow_root
let view_transition = Dom.view_transition
let island = Dom.island
let canvas3d = Dom.canvas3d

let div = Dom.div
let span = Dom.span
let main = Dom.main
let section = Dom.section
let article = Dom.article
let header = Dom.header
let footer = Dom.footer
let nav = Dom.nav
let aside = Dom.aside

let h1 = Dom.h1
let h2 = Dom.h2
let h3 = Dom.h3
let h4 = Dom.h4
let p = Dom.p
let code = Dom.code
let pre = Dom.pre
let strong = Dom.strong
let em = Dom.em
let small = Dom.small
let hr = Dom.hr
let br = Dom.br

let button = Dom.button
let a = Dom.a
let form = Dom.form
let input = Dom.input
let textarea = Dom.textarea
let select = Dom.select
let option = Dom.option
let checkbox = Dom.checkbox
let label = Dom.label
let bind_input = Dom.bind_input
let bind_bool = Dom.bind_bool

let ul = Dom.ul
let ol = Dom.ol
let li = Dom.li
let table = Dom.table
let thead = Dom.thead
let tbody = Dom.tbody
let tr = Dom.tr
let th = Dom.th
let td = Dom.td
let img = Dom.img
let svg = Dom.svg

(* 6. UI Kit & Interactive Motion Shortcuts *)
let badge = Components.badge
let contrast_badge = Components.contrast_badge
let btn = Components.btn
let card = Components.card
let spotlight_card = Components.spotlight_card
let stat_card = Components.stat_card
let alert = Components.alert
let modal = Components.modal
let drawer = Components.drawer
let accordion = Components.accordion
let switch_toggle = Components.switch_toggle
let progress_bar = Components.progress_bar
let slider = Components.slider
let skeleton = Components.skeleton
let avatar = Components.avatar
let timeline = Components.timeline
let stepper = Components.stepper
let kanban_board = Components.kanban_board
let command_palette = Components.command_palette
let toast_rack = Components.toast_rack
let push_toast = Components.push_toast
let code_block = Components.code_block
let tabs = Components.tabs
let navbar = Components.navbar
let data_table = Components.data_table

(* 7. Pure-SVG Vector Charts *)
let sparkline = Charts.sparkline
let area_chart = Charts.area_chart
let bar_chart = Charts.bar_chart
let donut_ring = Charts.donut_ring
let multi_line_chart = Charts.multi_line_chart
let heatmap = Charts.heatmap
let radar_chart = Charts.radar_chart
let candlestick_chart = Charts.candlestick_chart

(* 8. Router, Middleware, ISR, PubSub & REST API Shortcuts *)
let route = Router.route
let gadt_route = Router.gadt_route
let with_layout = Router.with_layout
let link = Router.link
let navigate = Router.navigate
let breadcrumbs = Router.breadcrumbs
let query_param = Router.query_param
let set_query = Router.set_query
let api_get = Server.api_get
let api_post = Server.api_post
let use_middleware = Server.use_middleware
let publish_event = Pubsub.publish

(* 9. Multi-Target Renderers, Server & Vite-Style CLI *)
let render_html = Dom.render_to_string
let render_tui = Tui.render_tui
let render_markdown = Custom_renderer.render_markdown
let render_canvas2d = Custom_renderer.render_canvas2d
let render_json_scene = Custom_renderer.render_json_scene
let run_terminal ?width ?height app = print_string (Tui.render_tui ?width ?height (app ()))
let run_browser = Server.run_browser
let export_page = Server.export_page
let export_site = Server.export_site
let run_vite_check = Vite.run_vite_check
let run_vite_build = Vite.run_vite_build
