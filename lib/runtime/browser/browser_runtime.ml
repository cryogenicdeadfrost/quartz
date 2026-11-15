(* Client-side Shared Browser Runtime + Islands Hydrator + DOM Diff Patcher +
   Headless WAI-ARIA Behavior + 60fps WebGL2 Scene3d Runner + Js_bridge FFI.
   Zero Unix dependency. Compiles natively or via js_of_ocaml / wasm_of_ocaml. *)

open Quartz_core

module Js_bridge = struct
  type external_widget_spec = {
    global_name : string;
    module_url : string option;
    props : (string * Json.t) list;
  }

  let mount ~global_name ?module_url ?(props = []) ?(style = []) fallback =
    let all_props =
      match module_url with
      | Some u -> ("__moduleUrl", Json.string u) :: props
      | None -> props
    in
    Dom.external_widget ~global:global_name ~props:all_props ~style fallback
end

(* Full modular shared browser runtime chunk (`quartz-runtime.js`).
   Shared by all islands on a page so no island duplicates runtime overhead! *)
let client_runtime_js =
  {|
(function() {
  if (window.__quartzRuntimeLoaded) return;
  window.__quartzRuntimeLoaded = true;

  function resolveDomPath(root, path) {
    let cur = root;
    for (let i = 0; i < path.length; i++) {
      if (!cur || !cur.children) return null;
      cur = cur.children[path[i]];
    }
    return cur;
  }

  function applyPatches(root, patches) {
    for (let i = 0; i < patches.length; i++) {
      const p = patches[i];
      const target = resolveDomPath(root, p.path || []);
      if (!target) continue;
      if (p.op === 'set_text') {
        target.textContent = p.text;
      } else if (p.op === 'set_attr') {
        if (p.value === '') target.setAttribute(p.key, '');
        else target.setAttribute(p.key, p.value);
        if (p.key === 'value' && 'value' in target) target.value = p.value;
      } else if (p.op === 'remove_attr') {
        target.removeAttribute(p.key);
      } else if (p.op === 'replace') {
        target.outerHTML = p.html;
      } else if (p.op === 'append') {
        target.insertAdjacentHTML('beforeend', p.html);
      } else if (p.op === 'remove_child') {
        const ch = target.children[p.index];
        if (ch) target.removeChild(ch);
      }
    }
  }

  async function applyPayload(res, useViewTransition) {
    if (!res.ok) return;
    const payload = await res.json();
    const rootEl = document.getElementById('quartz-root');
    const active = document.activeElement;
    const allCtrls = rootEl ? Array.from(rootEl.querySelectorAll('input,textarea,select')) : [];
    const activeIdx = allCtrls.indexOf(active);
    const selStart = (active && typeof active.selectionStart === 'number') ? active.selectionStart : null;

    const flipBefore = {};
    if (rootEl) {
      rootEl.querySelectorAll('[data-q-flip]').forEach(function(el) {
        const k = el.getAttribute('data-q-flip');
        if (k) flipBefore[k] = el.getBoundingClientRect();
      });
    }

    const patchDom = () => {
      const styleEl = document.getElementById('quartz-styles');
      if (styleEl && payload.css !== undefined) styleEl.textContent = payload.css;
      if (rootEl) {
        if (Array.isArray(payload.patches) && rootEl.firstElementChild) {
          applyPatches(rootEl.firstElementChild, payload.patches);
        } else if (payload.html !== undefined) {
          rootEl.innerHTML = payload.html;
        }
        rootEl.querySelectorAll('[data-q-flip]').forEach(function(el) {
          const k = el.getAttribute('data-q-flip');
          const prevRect = k && flipBefore[k];
          if (prevRect && typeof el.animate === 'function') {
            const nextRect = el.getBoundingClientRect();
            const dx = prevRect.left - nextRect.left;
            const dy = prevRect.top - nextRect.top;
            if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) {
              el.animate(
                [{ transform: 'translate(' + dx + 'px,' + dy + 'px)' }, { transform: 'none' }],
                { duration: 220, easing: 'cubic-bezier(0.22, 1, 0.36, 1)' }
              );
            }
          }
        });
        if (activeIdx >= 0) {
          const nextCtrls = Array.from(rootEl.querySelectorAll('input,textarea,select'));
          const nextEl = nextCtrls[activeIdx];
          if (nextEl) {
            nextEl.focus();
            if (selStart !== null && typeof nextEl.setSelectionRange === 'function') {
              try { nextEl.setSelectionRange(selStart, selStart); } catch (_) {}
            }
          }
        }
        hydrateAll();
      }
      if (payload.path && window.location.pathname !== payload.path) {
        window.history.pushState({ path: payload.path }, '', payload.path);
      }
    };

    if (useViewTransition && typeof document.startViewTransition === 'function') {
      document.startViewTransition(patchDom);
    } else {
      patchDom();
    }
  }

  async function sendEvent(id, val, transition) {
    try {
      const res = await fetch('/__quartz_event', {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: 'id=' + encodeURIComponent(id) + '&val=' + encodeURIComponent(val || '')
      });
      await applyPayload(res, !!transition);
    } catch (e) {
      console.error('[Quartz LiveView]', e);
    }
  }

  // Typed RPC Client helper
  window.__quartzRpc = async function(path, payload, method) {
    const res = await fetch(path, {
      method: method || 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: method === 'GET' ? undefined : JSON.stringify(payload || {})
    });
    if (!res.ok) throw new Error(await res.text());
    return res.json();
  };

  // Partial Hydration Islands Registry (`Quartz.registerIsland`)
  window.__quartzIslands = window.__quartzIslands || {};
  window.Quartz = window.Quartz || {
    registerIsland: function(name, mountFn) {
      window.__quartzIslands[name] = mountFn;
      hydrateIslands();
    },
    subscribeSse: function(url, onMsg) {
      if (typeof EventSource !== 'function') return null;
      const es = new EventSource(url);
      es.onmessage = function(ev) {
        let parsed = ev.data;
        try { parsed = JSON.parse(ev.data); } catch (_) {}
        if (typeof onMsg === 'function') onMsg(parsed, ev);
      };
      return es;
    },
    intl: {
      formatDate: function(epochSec, opts) {
        try { return new Intl.DateTimeFormat(undefined, opts || { dateStyle: 'medium' }).format(new Date(epochSec * 1000)); }
        catch (_) { return new Date(epochSec * 1000).toISOString(); }
      },
      formatNumber: function(num, currency) {
        try {
          const opt = currency ? { style: 'currency', currency: currency } : {};
          return new Intl.NumberFormat(undefined, opt).format(num);
        } catch (_) { return String(num); }
      },
      pluralCategory: function(n, locale) {
        try { return new Intl.PluralRules(locale || undefined).select(n); }
        catch (_) { return n === 1 ? 'one' : 'other'; }
      },
      compare: function(a, b, locale) {
        try { return new Intl.Collator(locale || undefined).compare(a, b); }
        catch (_) { return a < b ? -1 : (a > b ? 1 : 0); }
      }
    }
  };

  // Chrome DevTools Custom Object Formatter reading `window.__QUARTZ_DEBUG_TYPES__`
  if (typeof window !== 'undefined') {
    window.devtoolsFormatters = window.devtoolsFormatters || [];
    window.devtoolsFormatters.push({
      header: function(obj) {
        if (!obj || typeof obj !== 'object' || !obj.__quartzType) return null;
        const meta = (window.__QUARTZ_DEBUG_TYPES__ || {})[obj.__quartzType];
        const tagIdx = obj.value && typeof obj.value.tag === 'number' ? obj.value.tag : -1;
        const label = meta && tagIdx >= 0 && meta.labels ? meta.labels[tagIdx] : obj.__quartzType;
        return ['div', { style: 'color:#38bdf8;font-family:monospace' },
          'Quartz<' + obj.__quartzType + '> :: ' + (label || JSON.stringify(obj.value))];
      },
      hasBody: function(obj) { return !!(obj && obj.__quartzType && obj.value); },
      body: function(obj) {
        const meta = (window.__QUARTZ_DEBUG_TYPES__ || {})[obj.__quartzType];
        const fields = meta && meta.kind === 'record' && Array.isArray(meta.labels) ? meta.labels : null;
        const rows = [];
        if (fields && Array.isArray(obj.value)) {
          fields.forEach(function(k, i) { rows.push(['div', {}, k + ': ' + JSON.stringify(obj.value[i])]); });
        } else {
          rows.push(['div', {}, JSON.stringify(obj.value, null, 2)]);
        }
        return ['div', { style: 'padding-left:12px;color:#e2e8f0;font-family:monospace' }].concat(rows);
      }
    });
  }

  function hydrateIslands() {
    const nodes = document.querySelectorAll('[data-q-island]');
    nodes.forEach(function(el) {
      if (el.__qHydrated) return;
      const name = el.getAttribute('data-q-island');
      const fn = window.__quartzIslands[name];
      if (typeof fn === 'function') {
        el.__qHydrated = true;
        let props = {};
        try { props = JSON.parse(el.getAttribute('data-q-props') || '{}'); } catch (_) {}
        fn(el, props);
      } else if (!el.__qOfflineBound) {
        el.__qOfflineBound = true;
        // Zero-server interactive tabs fallback when running statically
        el.querySelectorAll('[role="tab"]').forEach(function(tab) {
          tab.addEventListener('click', function() {
            const list = tab.closest('[role="tablist"]');
            if (!list) return;
            list.querySelectorAll('[role="tab"]').forEach(function(t) {
              t.setAttribute('aria-selected', t === tab ? 'true' : 'false');
            });
          });
        });
      }
    });
  }

  function hydrateExternalWidgets() {
    const nodes = document.querySelectorAll('[data-q-external]');
    nodes.forEach(function(el) {
      if (el.__qMountedExt) return;
      const gname = el.getAttribute('data-q-external');
      const g = window[gname];
      if (g) {
        el.__qMountedExt = true;
        let props = {};
        try { props = JSON.parse(el.getAttribute('data-q-props') || '{}'); } catch (_) {}
        if (typeof g.mount === 'function') g.mount(el, props);
        else if (typeof g === 'function') g(el, props);
      }
    });
  }

  // Full 60fps WebGL2 Scene3d Runner (zero Three.js required, optional ThreeJS handoff)
  function compileShader(gl, type, src) {
    const s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    return s;
  }

  function mat4Perspective(fovDeg, aspect, near, far) {
    const f = 1.0 / Math.tan((fovDeg * Math.PI / 180.0) / 2.0);
    const nf = 1.0 / (near - far);
    return new Float32Array([
      f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, (far + near) * nf, -1,
      0, 0, 2 * far * near * nf, 0
    ]);
  }

  function makeBoxGeom(w, h, d) {
    const hw = (w || 2) * 0.5, hh = (h || 2) * 0.5, hd = (d || 2) * 0.5;
    const p = [
      -hw,-hh,hd,  hw,-hh,hd,  hw,hh,hd, -hw,hh,hd,
      -hw,-hh,-hd,-hw,hh,-hd,  hw,hh,-hd, hw,-hh,-hd,
      -hw,hh,-hd, -hw,hh,hd,   hw,hh,hd,  hw,hh,-hd,
      -hw,-hh,-hd, hw,-hh,-hd, hw,-hh,hd,-hw,-hh,hd,
       hw,-hh,-hd, hw,hh,-hd,  hw,hh,hd,  hw,-hh,hd,
      -hw,-hh,-hd,-hw,-hh,hd, -hw,hh,hd, -hw,hh,-hd
    ];
    const idx = [
      0,1,2, 0,2,3, 4,5,6, 4,6,7, 8,9,10, 8,10,11,
      12,13,14, 12,14,15, 16,17,18, 16,18,19, 20,21,22, 20,22,23
    ];
    return { pos: new Float32Array(p), idx: new Uint16Array(idx) };
  }

  function makeSphereGeom(radius, segs) {
    const r = radius || 1.0, s = Math.max(8, segs || 18);
    const p = [], idx = [];
    for (let y = 0; y <= s; y++) {
      const v = y / s, phi = v * Math.PI;
      for (let x = 0; x <= s; x++) {
        const u = x / s, theta = u * Math.PI * 2;
        p.push(-r * Math.cos(theta) * Math.sin(phi), r * Math.cos(phi), r * Math.sin(theta) * Math.sin(phi));
      }
    }
    for (let y = 0; y < s; y++) {
      for (let x = 0; x < s; x++) {
        const a = y * (s + 1) + x, b = a + s + 1;
        idx.push(a, b, a + 1, b, b + 1, a + 1);
      }
    }
    return { pos: new Float32Array(p), idx: new Uint16Array(idx) };
  }

  function makeTorusGeom(radius, tube, radialSegs, tubularSegs) {
    const R = radius || 1.1, r = tube || 0.36;
    const jSeg = Math.max(8, radialSegs || 16), iSeg = Math.max(12, tubularSegs || 24);
    const p = [], idx = [];
    for (let j = 0; j <= jSeg; j++) {
      const v = (j / jSeg) * Math.PI * 2;
      for (let i = 0; i <= iSeg; i++) {
        const u = (i / iSeg) * Math.PI * 2;
        p.push((R + r * Math.cos(v)) * Math.cos(u), r * Math.sin(v), (R + r * Math.cos(v)) * Math.sin(u));
      }
    }
    for (let j = 1; j <= jSeg; j++) {
      for (let i = 1; i <= iSeg; i++) {
        const a = (iSeg + 1) * j + i - 1;
        const b = (iSeg + 1) * (j - 1) + i - 1;
        const c = (iSeg + 1) * (j - 1) + i;
        const d = (iSeg + 1) * j + i;
        idx.push(a, b, d, b, c, d);
      }
    }
    return { pos: new Float32Array(p), idx: new Uint16Array(idx) };
  }

  function makePlaneGeom(w, h) {
    const hw = (w || 3) * 0.5, hh = (h || 3) * 0.5;
    return {
      pos: new Float32Array([-hw, 0, -hh, hw, 0, -hh, hw, 0, hh, -hw, 0, hh]),
      idx: new Uint16Array([0, 1, 2, 0, 2, 3])
    };
  }

  function geomForSpec(g) {
    if (!g) return makeBoxGeom(1.5, 1.5, 1.5);
    if (g.kind === 'sphere') return makeSphereGeom(g.radius || 1.0, g.segments || 18);
    if (g.kind === 'torus') return makeTorusGeom(g.radius || 1.0, g.tube || 0.35, 14, 24);
    if (g.kind === 'plane') return makePlaneGeom(g.w || 3.0, g.h || 3.0);
    return makeBoxGeom(g.w || 1.4, g.h || 1.4, g.d || 1.4);
  }

  function hexToRgb01(hex) {
    const h = (hex || '#38bdf8').replace('#', '');
    if (h.length !== 6) return [0.22, 0.74, 0.97];
    return [
      parseInt(h.slice(0,2), 16) / 255,
      parseInt(h.slice(2,4), 16) / 255,
      parseInt(h.slice(4,6), 16) / 255
    ];
  }

  function buildMeshProgram(gl, vsSrc, fsSrc) {
    const prog = gl.createProgram();
    gl.attachShader(prog, compileShader(gl, gl.VERTEX_SHADER, vsSrc));
    gl.attachShader(prog, compileShader(gl, gl.FRAGMENT_SHADER, fsSrc));
    gl.linkProgram(prog);
    return prog;
  }

  function mountScene3d(canvas, spec) {
    if (spec.renderer === 'threejs' && window.THREE) {
      try {
        const renderer = new window.THREE.WebGLRenderer({ canvas: canvas, antialias: true });
        renderer.setSize(canvas.width, canvas.height, false);
        const scene = new window.THREE.Scene();
        scene.background = new window.THREE.Color(spec.background || '#020617');
        const camSpec = spec.camera || {};
        const camera = new window.THREE.PerspectiveCamera(camSpec.fov || 45, canvas.width / Math.max(1, canvas.height), camSpec.near || 0.1, camSpec.far || 100);
        const cp = camSpec.pos || [0, 0, 5];
        camera.position.set(cp[0], cp[1], cp[2]);
        scene.add(new window.THREE.AmbientLight(0xffffff, 0.6));
        const dl = new window.THREE.DirectionalLight(0xffffff, 0.9);
        dl.position.set(3, 5, 4);
        scene.add(dl);
        const meshItems = (spec.meshes || []).map(function(m) {
          const g = m.geometry || {};
          const geom = g.kind === 'sphere' ? new window.THREE.SphereGeometry(g.radius || 1, 24, 24)
            : g.kind === 'torus' ? new window.THREE.TorusGeometry(g.radius || 1, g.tube || 0.35, 16, 36)
            : g.kind === 'plane' ? new window.THREE.PlaneGeometry(g.w || 3, g.h || 3)
            : new window.THREE.BoxGeometry(g.w || 1.4, g.h || 1.4, g.d || 1.4);
          const mat = new window.THREE.MeshStandardMaterial({ color: (m.material && m.material.color) || '#38bdf8' });
          const tm = new window.THREE.Mesh(geom, mat);
          const mp = m.pos || [0, 0, 0];
          tm.position.set(mp[0], mp[1], mp[2]);
          scene.add(tm);
          return { mesh: tm, spin: m.spin || [0.4, 0.8, 0.1] };
        });
        function tickThree() {
          meshItems.forEach(function(it) {
            it.mesh.rotation.x += 0.016 * it.spin[0];
            it.mesh.rotation.y += 0.016 * it.spin[1];
          });
          renderer.render(scene, camera);
          requestAnimationFrame(tickThree);
        }
        requestAnimationFrame(tickThree);
        return;
      } catch (_) {}
    }
    const gl = canvas.getContext('webgl2') || canvas.getContext('webgl');
    if (!gl) return;
    const defaultVs = 'attribute vec3 aPos; uniform float uT; uniform mat4 uProj; varying vec3 vCol; uniform vec3 uCol; uniform vec3 uPos; uniform vec3 uSpin; void main(){ float ax=uT*uSpin.x, ay=uT*uSpin.y; vec3 p=aPos; float cy=cos(ay),sy=sin(ay); p=vec3(p.x*cy-p.z*sy, p.y, p.x*sy+p.z*cy); float cx=cos(ax),sx=sin(ax); p=vec3(p.x, p.y*cx-p.z*sx, p.y*sx+p.z*cx); p += uPos; gl_Position = uProj * vec4(p, 1.0); vCol = uCol * (0.65 + 0.35 * normalize(aPos).y); }';
    const defaultFs = 'precision mediump float; varying vec3 vCol; void main(){ gl_FragColor = vec4(vCol, 1.0); }';
    const defaultProg = buildMeshProgram(gl, defaultVs, defaultFs);
    const compiledMeshes = (spec.meshes || []).map(function(m) {
      const geom = geomForSpec(m.geometry);
      const vb = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, vb);
      gl.bufferData(gl.ARRAY_BUFFER, geom.pos, gl.STATIC_DRAW);
      const ib = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, geom.idx, gl.STATIC_DRAW);
      let prog = defaultProg;
      if (m.material && m.material.kind === 'shader' && m.material.vertex && m.material.fragment) {
        const custom = buildMeshProgram(gl, m.material.vertex, m.material.fragment);
        if (gl.getProgramParameter(custom, gl.LINK_STATUS)) prog = custom;
      }
      return { m: m, geom: geom, vb: vb, ib: ib, prog: prog };
    });
    gl.enable(gl.DEPTH_TEST);
    const bg = hexToRgb01(spec.background || '#020617');
    const tStart = performance.now();
    function frame(now) {
      const t = (now - tStart) / 1000.0;
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(bg[0], bg[1], bg[2], 1.0);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      const cam = spec.camera || {};
      const proj = mat4Perspective(cam.fov || 45, canvas.width / Math.max(1, canvas.height), cam.near || 0.1, cam.far || 100);
      const cp = cam.pos || [0, 0, 5];
      compiledMeshes.forEach(function(item) {
        const prog = item.prog;
        gl.useProgram(prog);
        gl.bindBuffer(gl.ARRAY_BUFFER, item.vb);
        const loc = gl.getAttribLocation(prog, 'aPos');
        if (loc >= 0) { gl.enableVertexAttribArray(loc); gl.vertexAttribPointer(loc, 3, gl.FLOAT, false, 0, 0); }
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, item.ib);
        const uT = gl.getUniformLocation(prog, 'uT'); if (uT) gl.uniform1f(uT, t);
        const uProj = gl.getUniformLocation(prog, 'uProj'); if (uProj) gl.uniformMatrix4fv(uProj, false, proj);
        const mc = hexToRgb01((item.m.material && item.m.material.color) || '#38bdf8');
        const uCol = gl.getUniformLocation(prog, 'uCol'); if (uCol) gl.uniform3f(uCol, mc[0], mc[1], mc[2]);
        const mp = item.m.pos || [0, 0, 0];
        const uPos = gl.getUniformLocation(prog, 'uPos'); if (uPos) gl.uniform3f(uPos, mp[0] - cp[0], mp[1] - cp[1], mp[2] - cp[2]);
        const ms = item.m.spin || [0.4, 0.8, 0.1];
        const uSpin = gl.getUniformLocation(prog, 'uSpin'); if (uSpin) gl.uniform3f(uSpin, ms[0], ms[1], ms[2]);
        gl.drawElements(gl.TRIANGLES, item.geom.idx.length, gl.UNSIGNED_SHORT, 0);
      });
      requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }

  async function mountWebGpuScene(canvas, spec) {
    if (!navigator.gpu) return false;
    try {
      const adapter = await navigator.gpu.requestAdapter();
      if (!adapter) return false;
      const device = await adapter.requestDevice();
      const ctx = canvas.getContext('webgpu');
      if (!ctx) return false;
      const fmt = navigator.gpu.getPreferredCanvasFormat();
      ctx.configure({ device, format: fmt, alphaMode: 'opaque' });
      const wgslCode = `
        struct Uniforms {
          proj : mat4x4<f32>,
          pos  : vec4<f32>,
          spin : vec4<f32>,
          col  : vec4<f32>,
        };
        @group(0) @binding(0) var<uniform> u : Uniforms;
        struct VsOut {
          @builtin(position) clipPos : vec4<f32>,
          @location(0) vCol : vec3<f32>,
        };
        @vertex fn vs_main(@location(0) aPos : vec3<f32>) -> VsOut {
          let t = u.pos.w;
          let ax = t * u.spin.x;
          let ay = t * u.spin.y;
          let cy = cos(ay); let sy = sin(ay);
          var p = vec3<f32>(aPos.x * cy - aPos.z * sy, aPos.y, aPos.x * sy + aPos.z * cy);
          let cx = cos(ax); let sx = sin(ax);
          p = vec3<f32>(p.x, p.y * cx - p.z * sx, p.y * sx + p.z * cx) + u.pos.xyz;
          var out : VsOut;
          out.clipPos = u.proj * vec4<f32>(p, 1.0);
          let shade = 0.65 + 0.35 * normalize(aPos + vec3<f32>(0.0001, 0.0001, 0.0001)).y;
          out.vCol = u.col.xyz * shade;
          return out;
        }
        @fragment fn fs_main(in : VsOut) -> @location(0) vec4<f32> {
          return vec4<f32>(in.vCol, 1.0);
        }
      `;
      const shaderModule = device.createShaderModule({ code: wgslCode });
      const pipeline = device.createRenderPipeline({
        layout: 'auto',
        vertex: {
          module: shaderModule,
          entryPoint: 'vs_main',
          buffers: [{
            arrayStride: 12,
            attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }]
          }]
        },
        fragment: {
          module: shaderModule,
          entryPoint: 'fs_main',
          targets: [{ format: fmt }]
        },
        primitive: { topology: 'triangle-list', cullMode: 'none' },
        depthStencil: { format: 'depth24plus', depthWriteEnabled: true, depthCompare: 'less' }
      });
      const depthTex = device.createTexture({
        size: [Math.max(1, canvas.width), Math.max(1, canvas.height)],
        format: 'depth24plus',
        usage: GPUTextureUsage.RENDER_ATTACHMENT
      });
      const gpuMeshes = (spec.meshes || []).map(function(m) {
        const geom = geomForSpec(m.geometry);
        const vb = device.createBuffer({
          size: Math.max(16, (geom.pos.byteLength + 3) & ~3),
          usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST
        });
        device.queue.writeBuffer(vb, 0, geom.pos);
        const paddedIdxLen = (geom.idx.length + 1) & ~1;
        const paddedIdx = new Uint16Array(paddedIdxLen);
        paddedIdx.set(geom.idx);
        const ib = device.createBuffer({
          size: Math.max(4, paddedIdx.byteLength),
          usage: GPUBufferUsage.INDEX | GPUBufferUsage.COPY_DST
        });
        device.queue.writeBuffer(ib, 0, paddedIdx);
        const ub = device.createBuffer({
          size: 128,
          usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
        });
        const bgGroup = device.createBindGroup({
          layout: pipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: ub } }]
        });
        return { m: m, vb: vb, ib: ib, ub: ub, bgGroup: bgGroup, idxCount: geom.idx.length };
      });
      const bg = hexToRgb01(spec.background || '#020617');
      const tStart = performance.now();
      const uData = new Float32Array(32);
      function frame(now) {
        const t = (now - tStart) / 1000.0;
        const cam = spec.camera || {};
        const proj = mat4Perspective(cam.fov || 45, canvas.width / Math.max(1, canvas.height), cam.near || 0.1, cam.far || 100);
        const cp = cam.pos || [0, 0, 5];
        const enc = device.createCommandEncoder();
        const pass = enc.beginRenderPass({
          colorAttachments: [{
            view: ctx.getCurrentTexture().createView(),
            clearValue: { r: bg[0], g: bg[1], b: bg[2], a: 1.0 },
            loadOp: 'clear',
            storeOp: 'store'
          }],
          depthStencilAttachment: {
            view: depthTex.createView(),
            depthClearValue: 1.0,
            depthLoadOp: 'clear',
            depthStoreOp: ' discard' === '' ? 'store' : 'store'
          }
        });
        pass.setPipeline(pipeline);
        gpuMeshes.forEach(function(item) {
          uData.set(proj, 0);
          const mp = item.m.pos || [0, 0, 0];
          uData[16] = mp[0] - cp[0]; uData[17] = mp[1] - cp[1]; uData[18] = mp[2] - cp[2]; uData[19] = t;
          const ms = item.m.spin || [0.4, 0.8, 0.1];
          uData[20] = ms[0]; uData[21] = ms[1]; uData[22] = ms[2]; uData[23] = 0.0;
          const mc = hexToRgb01((item.m.material && item.m.material.color) || '#38bdf8');
          uData[24] = mc[0]; uData[25] = mc[1]; uData[26] = mc[2]; uData[27] = 1.0;
          device.queue.writeBuffer(item.ub, 0, uData);
          pass.setBindGroup(0, item.bgGroup);
          pass.setVertexBuffer(0, item.vb);
          pass.setIndexBuffer(item.ib, 'uint16');
          pass.drawIndexed(item.idxCount, 1, 0, 0, 0);
        });
        pass.end();
        device.queue.submit([enc.finish()]);
        requestAnimationFrame(frame);
      }
      requestAnimationFrame(frame);
      return true;
    } catch (_) { return false; }
  }

  function hydrateScene3d() {
    document.querySelectorAll('canvas[data-q-scene3d]').forEach(function(c) {
      if (c.__qMounted3d) return;
      c.__qMounted3d = true;
      try {
        const spec = JSON.parse(c.getAttribute('data-q-scene3d'));
        if (spec.renderer === 'webgpu') {
          mountWebGpuScene(c, spec).then(function(ok) {
            if (!ok) mountScene3d(c, spec);
          });
        } else {
          mountScene3d(c, spec);
        }
      } catch (e) { console.error('[Quartz Scene3d]', e); }
    });
  }

  function hydratePlugs() {
    document.querySelectorAll('[data-q-plug="1"]').forEach(function(el) {
      if (el.__qPlugMounted) return;
      el.__qPlugMounted = true;
      const pkg = el.getAttribute('data-q-pkg');
      const ver = el.getAttribute('data-q-ver') || 'latest';
      const expName = el.getAttribute('data-q-export') || 'default';
      const fw = el.getAttribute('data-q-framework') || 'vanilla';
      const cdn = (el.getAttribute('data-q-cdn') || 'https://esm.sh').replace(/\/+$/, '');
      const cssList = (el.getAttribute('data-q-css') || '').split(',').filter(Boolean);
      cssList.forEach(function(href) {
        if (!document.querySelector('link[href="' + href + '"]')) {
          const l = document.createElement('link');
          l.rel = 'stylesheet'; l.href = href;
          document.head.appendChild(l);
        }
      });
      let props = {};
      try { props = JSON.parse(el.getAttribute('data-q-props') || '{}'); } catch (_) {}
      const emit = function(payload) {
        const hId = el.getAttribute('data-q-input') || el.getAttribute('data-q-change');
        if (hId) sendEvent(hId, typeof payload === 'string' ? payload : JSON.stringify(payload), false);
      };
      const modUrl = (pkg.startsWith('http://') || pkg.startsWith('https://') || pkg.startsWith('/'))
        ? pkg : (cdn + '/' + pkg + '@' + ver);
      import(modUrl).then(async function(mod) {
        const target = mod[expName] || mod.default || mod;
        if (fw === 'react') {
          const React = await import(cdn + '/react@18');
          const ReactDOM = await import(cdn + '/react-dom@18/client');
          const createRoot = ReactDOM.createRoot || (ReactDOM.default && ReactDOM.default.createRoot);
          if (typeof createRoot === 'function') {
            el.innerHTML = '';
            createRoot(el).render((React.createElement || React.default.createElement)(target, Object.assign({}, props, { onQuartzEvent: emit })));
          }
        } else if (fw === 'webcomponent') {
          Object.assign(el, props);
        } else if (typeof target === 'function') {
          el.innerHTML = '';
          target(el, props, emit);
        } else if (target && typeof target.mount === 'function') {
          el.innerHTML = '';
          target.mount(el, props, emit);
        }
      }).catch(function(err) {
        console.warn('[Quartz Npm_plug] failed to load ' + modUrl, err);
      });
    });
  }

  function hydrateIntl() {
    document.querySelectorAll('[data-q-intl-date]').forEach(function(el) {
      if (el.__qIntlDone) return;
      el.__qIntlDone = true;
      const epoch = Number(el.getAttribute('data-q-intl-date') || '0');
      const styleFmt = el.getAttribute('data-q-intl-style') || 'medium';
      if (!Number.isNaN(epoch) && window.Quartz && window.Quartz.intl) {
        el.textContent = window.Quartz.intl.formatDate(epoch, { dateStyle: styleFmt });
      }
    });
    document.querySelectorAll('[data-q-intl-num]').forEach(function(el) {
      if (el.__qIntlDone) return;
      el.__qIntlDone = true;
      const num = Number(el.getAttribute('data-q-intl-num') || '0');
      const cur = el.getAttribute('data-q-intl-currency') || '';
      if (!Number.isNaN(num) && window.Quartz && window.Quartz.intl) {
        el.textContent = window.Quartz.intl.formatNumber(num, cur || null);
      }
    });
  }

  function hydrateHeadlessInteractions() {
    document.querySelectorAll('[data-q-combobox]').forEach(function(wrap) {
      if (wrap.__qComboBound) return;
      wrap.__qComboBound = true;
      const inp = wrap.querySelector('input');
      const list = wrap.querySelector('[role="listbox"]');
      if (!inp || !list) return;
      inp.addEventListener('focus', function() { list.removeAttribute('hidden'); inp.setAttribute('aria-expanded', 'true'); });
      inp.addEventListener('input', function() {
        const q = (inp.value || '').toLowerCase();
        list.removeAttribute('hidden');
        list.querySelectorAll('[role="option"]').forEach(function(opt) {
          opt.hidden = q !== '' && !opt.textContent.toLowerCase().includes(q);
        });
      });
      list.addEventListener('click', function(e) {
        const opt = e.target.closest('[role="option"]');
        if (opt) {
          inp.value = opt.textContent.trim();
          list.querySelectorAll('[role="option"]').forEach(function(o) { o.setAttribute('aria-selected', o === opt ? 'true' : 'false'); });
          list.setAttribute('hidden', '');
          inp.setAttribute('aria-expanded', 'false');
        }
      });
    });
    document.querySelectorAll('[data-q-tooltip-wrap]').forEach(function(wrap) {
      if (wrap.__qTipBound) return;
      wrap.__qTipBound = true;
      const tip = wrap.querySelector('[role="tooltip"]');
      if (!tip) return;
      wrap.addEventListener('mouseenter', function() { tip.setAttribute('aria-hidden', 'false'); });
      wrap.addEventListener('mouseleave', function() { tip.setAttribute('aria-hidden', 'true'); });
      wrap.addEventListener('focusin', function() { tip.setAttribute('aria-hidden', 'false'); });
      wrap.addEventListener('focusout', function() { tip.setAttribute('aria-hidden', 'true'); });
    });
  }

  function hydrateAll() {
    hydrateIslands();
    hydrateExternalWidgets();
    hydratePlugs();
    hydrateIntl();
    hydrateHeadlessInteractions();
    hydrateScene3d();
  }

  window.addEventListener('popstate', async function() {
    try {
      const res = await fetch('/__quartz_nav?path=' + encodeURIComponent(window.location.pathname + window.location.search));
      await applyPayload(res, true);
    } catch (_) {}
  });

  // Hover Prefetching for <a data-q-prefetch="1">
  const prefetched = new Set();
  document.addEventListener('mouseover', function(e) {
    const a = e.target.closest('a[data-q-prefetch="1"]');
    if (a) {
      const href = a.getAttribute('href');
      if (href && !prefetched.has(href)) {
        prefetched.add(href);
        fetch(href, { method: 'HEAD' }).catch(function() {});
      }
    }
  });

  // WAI-ARIA Roving Tabindex & Focus Trap keyboard behavior for Headless primitives
  document.addEventListener('keydown', function(e) {
    const roving = e.target.closest('[data-q-roving]');
    if (roving) {
      const dir = roving.getAttribute('data-q-roving');
      const items = Array.from(roving.querySelectorAll('[role="tab"],[role="menuitem"],[role="option"],button,a'));
      const idx = items.indexOf(e.target);
      if (idx >= 0) {
        let next = -1;
        if ((dir === 'horizontal' && e.key === 'ArrowRight') || (dir === 'vertical' && e.key === 'ArrowDown'))
          next = (idx + 1) % items.length;
        else if ((dir === 'horizontal' && e.key === 'ArrowLeft') || (dir === 'vertical' && e.key === 'ArrowUp'))
          next = (idx - 1 + items.length) % items.length;
        else if (e.key === 'Home') next = 0;
        else if (e.key === 'End') next = items.length - 1;
        if (next >= 0) {
          e.preventDefault();
          items[idx].setAttribute('tabindex', '-1');
          items[next].setAttribute('tabindex', '0');
          items[next].focus();
        }
      }
    }
    const t = e.target.closest('[data-q-keydown]');
    if (t) sendEvent(t.getAttribute('data-q-keydown'), e.key || '', false);
  });

  let lastMoveSent = 0;
  document.addEventListener('mousemove', function(e) {
    const cards = document.querySelectorAll('[data-q-spotlight="1"]');
    for (let i = 0; i < cards.length; i++) {
      const rect = cards[i].getBoundingClientRect();
      cards[i].style.setProperty('--qx', (e.clientX - rect.left) + 'px');
      cards[i].style.setProperty('--qy', (e.clientY - rect.top) + 'px');
    }
    const mm = e.target.closest('[data-q-mousemove]');
    if (mm) {
      const now = performance.now();
      if (now - lastMoveSent > 90) {
        lastMoveSent = now;
        const r = mm.getBoundingClientRect();
        sendEvent(mm.getAttribute('data-q-mousemove'), Math.round(e.clientX - r.left) + ',' + Math.round(e.clientY - r.top), false);
      }
    }
    // Magnetic cursor pull physics for [data-q-magnetic="1"]
    if (!window.matchMedia || !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      document.querySelectorAll('[data-q-magnetic="1"]').forEach(function(mag) {
        const r = mag.getBoundingClientRect();
        const cx = r.left + r.width * 0.5, cy = r.top + r.height * 0.5;
        const dx = e.clientX - cx, dy = e.clientY - cy;
        const dist = Math.hypot(dx, dy);
        if (dist < Math.max(r.width, r.height) * 1.15) {
          mag.style.setProperty('--mx', (dx * 0.22).toFixed(1) + 'px');
          mag.style.setProperty('--my', (dy * 0.22).toFixed(1) + 'px');
        } else {
          mag.style.setProperty('--mx', '0px');
          mag.style.setProperty('--my', '0px');
        }
      });
    }
  });

  // Pointer drag-and-drop gestures for [data-q-draggable="1"]
  let dragTarget = null, dragStartX = 0, dragStartY = 0;
  document.addEventListener('pointerdown', function(e) {
    const dg = e.target.closest('[data-q-draggable="1"]');
    if (dg) {
      dragTarget = dg;
      dragStartX = e.clientX;
      dragStartY = e.clientY;
      dg.style.cursor = 'grabbing';
    }
  });
  document.addEventListener('pointermove', function(e) {
    if (!dragTarget) return;
    const dx = e.clientX - dragStartX, dy = e.clientY - dragStartY;
    dragTarget.style.transform = 'translate3d(' + dx.toFixed(1) + 'px,' + dy.toFixed(1) + 'px,0)';
  });
  document.addEventListener('pointerup', function(e) {
    if (!dragTarget) return;
    const cb = dragTarget.getAttribute('data-q-drag');
    const dx = Math.round(e.clientX - dragStartX), dy = Math.round(e.clientY - dragStartY);
    dragTarget.style.cursor = 'grab';
    if (cb) sendEvent(cb, dx + ',' + dy, false);
    dragTarget = null;
  });

  document.addEventListener('click', function(e) {
    const t = e.target.closest('[data-q-click]');
    if (t) {
      const isNav = t.tagName === 'A';
      if (isNav) e.preventDefault();
      sendEvent(t.getAttribute('data-q-click'), '', isNav);
    }
  });

  document.addEventListener('input', function(e) {
    const t = e.target.closest('[data-q-input]');
    if (t) sendEvent(t.getAttribute('data-q-input'), t.value || '', false);
  });

  document.addEventListener('change', function(e) {
    const tc = e.target.closest('[data-q-check]');
    if (tc) {
      sendEvent(tc.getAttribute('data-q-check'), tc.checked ? 'true' : 'false', false);
      return;
    }
    const ch = e.target.closest('[data-q-change]');
    if (ch) sendEvent(ch.getAttribute('data-q-change'), ch.value || '', false);
  });

  document.addEventListener('submit', function(e) {
    const f = e.target.closest('[data-q-submit]');
    if (f) {
      e.preventDefault();
      const fd = new FormData(f);
      const pairs = [];
      fd.forEach(function(v, k) { pairs.push(encodeURIComponent(k) + '=' + encodeURIComponent(v)); });
      sendEvent(f.getAttribute('data-q-submit'), pairs.join('&'), false);
    }
  });

  // Focus trap & Escape dismiss for dialog[open] / [data-q-popover]
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') {
      document.querySelectorAll('dialog[open]').forEach(function(d) {
        if (typeof d.close === 'function') d.close(); else d.removeAttribute('open');
      });
      document.querySelectorAll('[data-q-popover]').forEach(function(p) {
        if (typeof p.hidePopover === 'function') { try { p.hidePopover(); } catch (_) {} }
      });
    } else if (e.key === 'Tab') {
      const trap = e.target.closest('[data-q-trap-focus="1"]');
      if (trap) {
        const focusables = Array.from(trap.querySelectorAll('button:not([disabled]),a[href],input:not([disabled]),select:not([disabled]),[tabindex]:not([tabindex="-1"])'));
        if (focusables.length > 0) {
          const first = focusables[0], last = focusables[focusables.length - 1];
          if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
          else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
        }
      }
    }
  });

  // Live HMR WebSocket listener with exponential backoff reconnect
  if (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1') {
    let wsDelay = 500;
    function connectHmrWs() {
      try {
        const proto = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const ws = new WebSocket(proto + '//' + window.location.host + '/__quartz_ws');
        ws.onopen = function() { wsDelay = 500; };
        ws.onmessage = function(ev) {
          if (ev.data === 'hmr_reload') window.location.reload();
        };
        ws.onclose = function() {
          setTimeout(connectHmrWs, wsDelay);
          wsDelay = Math.min(8000, wsDelay * 2);
        };
      } catch (_) {}
    }
    connectHmrWs();
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', hydrateAll);
  else hydrateAll();
})();
|}
