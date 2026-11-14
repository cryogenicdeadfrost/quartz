module Signal = Signal
module Style = Style
module Palette = Style.Palette
module Dom = Dom
module Router = Router
module Form = Form
module Components = Components
module Tui = Tui
module Server = Server

(* React / SolidJS ergonomic hooks & operators right at the top level *)
let use_state = Signal.create
let use_signal = Signal.make
let use_memo = Signal.computed
let use_effect = Signal.watch
let use_store = Signal.create_store
let batch = Signal.batch
let ( !~ ) = Signal.( !~ )
let ( <~ ) = Signal.( <~ )

(* HTML & reactive view helpers re-exported so `open Quartz` is all you need *)
type node = Dom.node

let empty = Dom.empty
let text = Dom.text
let textf = Dom.textf
let rx = Dom.rx
let rx_text = Dom.rx_text
let show = Dom.show
let each = Dom.each
let fragment = Dom.fragment

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

(* UI kit shortcuts *)
let badge = Components.badge
let btn = Components.btn
let card = Components.card
let stat_card = Components.stat_card
let alert = Components.alert
let modal = Components.modal
let tabs = Components.tabs
let navbar = Components.navbar
let data_table = Components.data_table
let sparkline = Components.sparkline
let bar_chart = Components.bar_chart

(* Mounts & compilers *)
let render_html = Dom.render_to_string
let render_tui = Tui.render_tui
let run_terminal ?width ?height app = print_string (Tui.render_tui ?width ?height (app ()))
let run_browser = Server.run_browser
let export_page = Server.export_page
let export_site = Server.export_site
