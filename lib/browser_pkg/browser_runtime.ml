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
    }
  };

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

  function makeBoxGeom() {
    const p = [
      -1,-1,1,  1,-1,1,  1,1,1, -1,1,1,
      -1,-1,-1,-1,1,-1, 1,1,-1, 1,-1,-1,
      -1,1,-1, -1,1,1,  1,1,1,  1,1,-1,
      -1,-1,-1, 1,-1,-1, 1,-1,1,-1,-1,1,
       1,-1,-1, 1,1,-1,  1,1,1,  1,-1,1,
      -1,-1,-1,-1,-1,1, -1,1,1, -1,1,-1
    ];
    const idx = [
      0,1,2, 0,2,3, 4,5,6, 4,6,7, 8,9,10, 8,10,11,
      12,13,14, 12,14,15, 16,17,18, 16,18,19, 20,21,22, 20,22,23
    ];
    return { pos: new Float32Array(p), idx: new Uint16Array(idx) };
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

  function mountScene3d(canvas, spec) {
    const gl = canvas.getContext('webgl2') || canvas.getContext('webgl');
    if (!gl) return;
    const vs = 'attribute vec3 aPos; uniform float uT; uniform mat4 uProj; varying vec3 vCol; uniform vec3 uCol; uniform vec3 uPos; uniform vec3 uSpin; void main(){ float ax=uT*uSpin.x, ay=uT*uSpin.y; vec3 p=aPos; float cy=cos(ay),sy=sin(ay); p=vec3(p.x*cy-p.z*sy, p.y, p.x*sy+p.z*cy); float cx=cos(ax),sx=sin(ax); p=vec3(p.x, p.y*cx-p.z*sx, p.y*sx+p.z*cx); p += uPos; gl_Position = uProj * vec4(p, 1.0); vCol = uCol * (0.65 + 0.35 * normalize(aPos).y); }';
    const fs = 'precision mediump float; varying vec3 vCol; void main(){ gl_FragColor = vec4(vCol, 1.0); }';
    const prog = gl.createProgram();
    gl.attachShader(prog, compileShader(gl, gl.VERTEX_SHADER, vs));
    gl.attachShader(prog, compileShader(gl, gl.FRAGMENT_SHADER, fs));
    gl.linkProgram(prog);
    gl.useProgram(prog);
    const geom = makeBoxGeom();
    const vb = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, vb);
    gl.bufferData(gl.ARRAY_BUFFER, geom.pos, gl.STATIC_DRAW);
    const loc = gl.getAttribLocation(prog, 'aPos');
    gl.enableVertexAttribArray(loc);
    gl.vertexAttribPointer(loc, 3, gl.FLOAT, false, 0, 0);
    const ib = gl.createBuffer();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, geom.idx, gl.STATIC_DRAW);
    gl.enable(gl.DEPTH_TEST);
    const bg = hexToRgb01(spec.background || '#020617');
    const uT = gl.getUniformLocation(prog, 'uT');
    const uProj = gl.getUniformLocation(prog, 'uProj');
    const uCol = gl.getUniformLocation(prog, 'uCol');
    const uPos = gl.getUniformLocation(prog, 'uPos');
    const uSpin = gl.getUniformLocation(prog, 'uSpin');
    const tStart = performance.now();
    function frame(now) {
      const t = (now - tStart) / 1000.0;
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(bg[0], bg[1], bg[2], 1.0);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      const cam = spec.camera || {};
      const proj = mat4Perspective(cam.fov || 45, canvas.width / Math.max(1, canvas.height), cam.near || 0.1, cam.far || 100);
      gl.uniformMatrix4fv(uProj, false, proj);
      gl.uniform1f(uT, t);
      (spec.meshes || []).forEach(function(m) {
        const mc = hexToRgb01((m.material && m.material.color) || '#38bdf8');
        const mp = m.pos || [0, 0, 0];
        const cp = cam.pos || [0, 0, 5];
        const ms = m.spin || [0.4, 0.8, 0.1];
        gl.uniform3f(uCol, mc[0], mc[1], mc[2]);
        gl.uniform3f(uPos, mp[0] - cp[0], mp[1] - cp[1], mp[2] - cp[2]);
        gl.uniform3f(uSpin, ms[0], ms[1], ms[2]);
        gl.drawElements(gl.TRIANGLES, geom.idx.length, gl.UNSIGNED_SHORT, 0);
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
      const bg = hexToRgb01(spec.background || '#020617');
      function frame() {
        const enc = device.createCommandEncoder();
        const pass = enc.beginRenderPass({
          colorAttachments: [{
            view: ctx.getCurrentTexture().createView(),
            clearValue: { r: bg[0], g: bg[1], b: bg[2], a: 1.0 },
            loadOp: 'clear',
            storeOp: 'store'
          }]
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

  function hydrateAll() {
    hydrateIslands();
    hydrateExternalWidgets();
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
