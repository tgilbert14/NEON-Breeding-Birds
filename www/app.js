/* =========================================================================
   app.js — count-up stat counters + celebratory confetti
   ========================================================================= */

// When a tab becomes visible, nudge a resize so widgets that rendered while the
// tab was hidden (Leaflet maps, plotly charts) re-fit to their real size — the
// classic "0-sized widget in a hidden bootstrap tab" fix.
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 60);
});

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay (no reliance on a
  // custom Shiny message, which doesn't always register in time).
  if (typeof smtLoadDone === "function") smtLoadDone();
  const target = parseFloat(el.getAttribute("data-target")) || 0;
  const suffix = el.dataset.suffix || "";          // e.g. "d", "m", "g"
  const isFloat = !Number.isInteger(target);
  const fmt = (v) => (isFloat ? v.toFixed(1) : Math.round(v).toLocaleString()) + suffix;
  // reduced-motion: snap to the final value, no animation
  if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    el.textContent = fmt(target); return;
  }
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    el.textContent = fmt(target * eased);
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = fmt(target);
  }
  requestAnimationFrame(tick);
}

function runCounters() {
  document.querySelectorAll(".count-up").forEach(animateCount);
}

// Re-run whenever Shiny injects fresh stat cards.
const heroObserver = new MutationObserver(() => runCounters());
document.addEventListener("DOMContentLoaded", function () {
  const host = document.body;
  heroObserver.observe(host, { childList: true, subtree: true });
  runCounters();
});

// ---- confetti on legendary / epic finds ----------------------------------
function rodentConfetti(big) {
  if (typeof confetti !== "function") return;
  // Field Guide palette (rust / goldfinch / singing-green / calling-blue / ink).
  const colors = ["#c1502e", "#e8a317", "#1a7f37", "#2f7fb5", "#2b2722"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
  mascotCheer(big);
}

// ---- mascot celebration: a little bird hops up + fades on a legendary/epic find
function mascotCheer(big) {
  try {
    if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;
    var src = document.querySelector("#loadOverlay .mascot");
    if (!src) return;
    var wrap = document.createElement("div");
    wrap.className = "mascot-cheer";
    wrap.appendChild(src.cloneNode(true));
    document.body.appendChild(wrap);
    setTimeout(function () { if (wrap.parentNode) wrap.parentNode.removeChild(wrap); }, 1700);
  } catch (e) {}
}

// ---- first-visit: the splash mascot waves hello once (localStorage-gated) ----
document.addEventListener("DOMContentLoaded", function () {
  try {
    if (localStorage.getItem("smtMascotSeen") === "1") return;
    var g = document.querySelector(".splash-guide");
    if (g) {
      g.classList.add("wave");
      localStorage.setItem("smtMascotSeen", "1");
      setTimeout(function () { g.classList.remove("wave"); }, 3300);
    }
  } catch (e) {}
});

// ---- loading overlay (opaque, indeterminate) -----------------------------
// A site load is one synchronous blocking call whose duration we can't know,
// so we show an INDETERMINATE animated bar (no fake %) on an OPAQUE backdrop —
// it just spins until the server signals it's done. No number to "stall" at,
// and you don't see half-rendered data through it.
var smtSafetyTimer = null;
function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  // Raise the overlay IMMEDIATELY, synchronously, on the click. A site load is
  // 1–3s of BLOCKING work on the worker (decompress + clean + leaderboard + the
  // Overview tab's plotly renders). A server-sent "show" message can't paint
  // until that block ends — by then it's too late — so the only honest feedback
  // is to show it client-side right now. (Loads are never truly instant, so the
  // old 250ms defer just hid the feedback during exactly the freeze it's for.)
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  ov.style.display = "flex";
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }  // tactile "got it"
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {  // safety net so it can never stick
    var note = document.querySelector(".load-note");
    if (note) note.textContent = "Still working — a large site or a slow NEON Portal can take a bit. You can close this and try again.";
    setTimeout(smtLoadDone, 5000);
  }, 90000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

// (The site report card is now a server-side PDF streamed by a Shiny
//  downloadHandler — output$reportPdf, via the hero downloadLink — so the old
//  browser-print path (smtPrintReport) has been removed.)

// ---- dismiss any open info popover (click-outside + Esc) -----------------
// bslib/Bootstrap popovers don't close on an outside click by default, so make
// every "ⓘ" popover dismissible the way users expect.
function smtClosePopovers() {
  document.querySelectorAll(".popover").forEach(function (pop) {
    var trig = pop.id ? document.querySelector('[aria-describedby="' + pop.id + '"]') : null;
    if (trig && window.bootstrap && bootstrap.Popover) {
      var inst = bootstrap.Popover.getInstance(trig);
      if (inst) { inst.hide(); return; }
    }
    pop.remove(); // fallback: just remove the floating popover
  });
}
document.addEventListener("click", function (e) {
  if (e.target.closest(".popover") || e.target.closest(".info-dot") ||
      e.target.closest("bslib-popover")) return;        // clicking inside/trigger -> leave it
  if (document.querySelector(".popover")) smtClosePopovers();
});
document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") smtClosePopovers();
});

// ---- Shiny custom message handlers ---------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("countUp", function () {
      // small delay so the freshly-rendered DOM is in place
      setTimeout(runCounters, 60);
    });
    Shiny.addCustomMessageHandler("confetti", function (msg) {
      rodentConfetti(msg && msg.big);
    });
    Shiny.addCustomMessageHandler("loadDone", function () { smtLoadDone(); });
    // server-triggered overlay (e.g. a click on the national picker map, which
    // has no inline onclick to call smtLoadStart directly)
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) {
      smtLoadStart(msg && msg.label);
    });
    // A Leaflet map that initialised inside a hidden tab/container (the Plot-map
    // tab, or the picker map re-shown after "change site") can paint blank until
    // it recomputes its size. Dispatching 'resize' makes every Leaflet map
    // invalidateSize. The server kicks this after re-showing the splash.
    Shiny.addCustomMessageHandler("kickMaps", function () {
      // Dispatch 'resize' across several frames so Leaflet re-fits the SETTLED
      // width after "change site" re-shows the splash, instead of painting
      // half-width off a still-collapsing container.
      var kick = function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} };
      requestAnimationFrame(kick);
      [80, 250, 500, 900].forEach(function (t) { setTimeout(kick, t); });
    });
  }
});

// Re-fit any Leaflet map the moment its tab becomes visible (hidden-init blank fix).
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 60);
});
