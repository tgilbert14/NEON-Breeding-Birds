import crypto from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const read = (relative) => fs.readFileSync(path.join(root, relative), "utf8");
const docs = read("docs/index.html");
const global = read("global.R");
const ui = read("ui.R");
const posterCss = read("www/poster.css");
const birdCss = read("www/bird.css");
const appJs = read("www/app.js");
const provenance = read("docs/ART-PROVENANCE.md");

function assert(condition, message) {
  if (!condition) throw new Error(message);
}
function includes(source, needle, label) {
  assert(source.includes(needle), `${label}: missing ${JSON.stringify(needle)}`);
}
function excludes(source, needle, label) {
  assert(!source.includes(needle), `${label}: retired token remains: ${JSON.stringify(needle)}`);
}
function count(source, expression) {
  return (source.match(expression) || []).length;
}
function tagCount(source, tagName) {
  return count(source, new RegExp(`<${tagName}\\b[^>]*>`, "gi"));
}
function tagsWithClass(source, tagName, className) {
  const tags = source.match(new RegExp(`<${tagName}\\b[^>]*>`, "gi")) || [];
  return tags.filter((tag) => {
    const attr = tag.match(/\bclass\s*=\s*["']([^"']*)["']/i);
    return attr && attr[1].split(/\s+/).includes(className);
  });
}
function balancedTags(source, tagName) {
  const opened = tagCount(source, tagName);
  const closed = count(source, new RegExp(`</${tagName}>`, "gi"));
  assert(opened === closed, `docs/index.html: unbalanced <${tagName}> tags (${opened} open, ${closed} close)`);
}

// Pages: exactly the compact Suite Living Poster V1 invitation.
includes(docs, 'data-release-marker="breeding-birds-poster-v1"', "docs/index.html");
assert(tagCount(docs, "main") === 1, "docs/index.html: expected exactly one main landmark");
assert(tagCount(docs, "h1") === 1, "docs/index.html: expected exactly one h1");
assert(tagsWithClass(docs, "main", "poster").length === 1, "docs/index.html: expected exactly one poster main");
assert(tagsWithClass(docs, "a", "button").length === 1, "docs/index.html: expected exactly one contextual CTA");
assert(tagsWithClass(docs, "a", "driver-route").length === 1, "docs/index.html: expected exactly one Driver route");
assert(tagsWithClass(docs, "div", "poster-actions").length === 1, "docs/index.html: expected exactly one poster action group");
assert(tagsWithClass(docs, "figure", "poster-art").length === 1, "docs/index.html: expected exactly one dominant artwork");
assert(tagCount(docs, "figcaption") === 0, "docs/index.html: illustration badge must stay absent");
assert(tagsWithClass(docs, "details", "honesty").length === 1, "docs/index.html: expected exactly one compact honesty disclosure");
for (const tag of ["main", "header", "div", "h1", "p", "a", "figure", "picture", "figcaption", "footer", "details", "summary", "nav"]) {
  balancedTags(docs, tag);
}

includes(docs, "Who’s singing", "docs/index.html");
includes(docs, "where?", "docs/index.html");
includes(docs, "Follow the birds NEON hears and sees across 47 breeding-season field sites.", "docs/index.html");
includes(docs, "Choose a field site", "docs/index.html");
excludes(docs, "Generated editorial illustration · not field documentation or measured data", "docs/index.html");
excludes(docs, "art-note", "docs/index.html");
includes(docs, "Public NEON <strong>DP1.10003.001</strong>", "docs/index.html");
includes(docs, "47 bundled field sites", "docs/index.html");
includes(docs, "detection index—not population size", "docs/index.html");
includes(docs, "What the counts can—and cannot—say", "docs/index.html");
includes(docs, "og-image-v2.png", "docs/index.html");
includes(docs, 'content="1200"', "docs/index.html");
includes(docs, 'content="630"', "docs/index.html");
assert(count(docs, /https:\/\/tgilbert14\.github\.io\/NEON-Driver-Cascade\//g) === 1,
  "docs/index.html: Driver Cascade must appear exactly once");
assert(!/<(?:video|canvas)\b/i.test(docs), "docs/index.html: dominant poster art must be static");
assert(!/<script\b[^>]*\bsrc\s*=\s*["']https?:/i.test(docs), "docs/index.html: remote script asset found");
assert(!/<img\b[^>]*\bsrc\s*=\s*["']https?:/i.test(docs), "docs/index.html: remote image asset found");
for (const link of docs.match(/<link\b[^>]*>/gi) || []) {
  if (/\brel\s*=\s*["']stylesheet["']/i.test(link)) {
    assert(!/\bhref\s*=\s*["']https?:/i.test(link), "docs/index.html: remote stylesheet asset found");
  }
}
for (const retired of ["constel-sec", "cascade-band", "series-grid", "metric-band", "methods-block", "truth-card", "boundary-panel", "second-bridge", "flybird"]) {
  excludes(docs, retired, "docs/index.html");
}
for (const stale of ["46 breeding-season", "46-site", "46 sites", "All 46", "19 sites"]) {
  excludes(docs, stale, "docs/index.html");
}
includes(docs, ".poster-art { order: 1", "docs/index.html mobile CSS");
includes(docs, ".poster-copy { order: 2", "docs/index.html mobile CSS");
includes(docs, "@media (prefers-reduced-motion: reduce)", "docs/index.html");
includes(docs, "@media (forced-colors: active)", "docs/index.html");

// In-app first-run surface: same invitation and one focus-moving action.
includes(global, 'identical(as.integer(RELEASE_STAMP$schema_version), 3L)', "global.R release stamp");
includes(global, 'stamp_hash("payload_sha256")', "global.R release stamp");
includes(global, 'stamp_hash("manifest_contract_sha256")', "global.R release stamp");
includes(global, '"neon-breeding-birds-release-instance-v3"', "global.R release stamp");
includes(ui, 'content = "breeding-birds-release-2026-v1"', "ui.R");
includes(ui, 'name = "ddl-release-instance", content = RELEASE_STAMP_ID', "ui.R");
includes(ui, "bird_poster <- function()", "ui.R");
includes(ui, 'class = "brd-poster"', "ui.R");
includes(ui, 'aria-label` = "Who’s singing where?"', "ui.R");
includes(ui, "Follow the birds NEON hears and sees across 47 breeding-season field sites.", "ui.R");
includes(ui, 'class = "brd-poster-cta", href = "#site-picker-start"', "ui.R");
includes(ui, "target.focus({preventScroll:true})", "ui.R");
includes(ui, 'id = "site-picker-start", class = "picker-start", tabindex = "-1"', "ui.R");
includes(ui, 'class = "app-skip", href = "#appMain"', "ui.R");
includes(ui, 'tags$main(id = "appMain", class = "app-main", tabindex = "-1"', "ui.R");
includes(ui, 'asset_url("poster.css")', "ui.R");
includes(ui, 'asset_url("vendor/html-to-image/html-to-image.js")', "ui.R");
excludes(ui, "Generated editorial illustration · not field documentation or measured data", "ui.R");
excludes(ui, "tags$figcaption", "ui.R");
includes(ui, 'width = "1536", height = "1024"', "ui.R");
includes(ui, 'role = "dialog"', "ui.R loading overlay");
includes(ui, 'aria-modal` = "true"', "ui.R loading overlay");
includes(ui, 'aria-live` = "polite"', "ui.R loading overlay");
includes(ui, 'aria-busy` = "false"', "ui.R loading overlay");
includes(ui, 'aria-hidden` = "true"', "ui.R loading overlay");
includes(ui, 'aria-labelledby` = "loadTitle"', "ui.R loading overlay");
includes(ui, 'aria-label` = "How it works"', "ui.R top bar");
includes(ui, '"Temperature units"', "ui.R top bar");
includes(ui, 'aria-label` = "Color theme"', "ui.R top bar");
assert(count(ui, /https:\/\/tgilbert14\.github\.io\/NEON-Driver-Cascade\//g) === 1,
  "ui.R: Driver Cascade must appear exactly once");
for (const stale of ["46 breeding-season", "46-site", "46 sites", "All 46", "19 sites"]) {
  excludes(ui, stale, "ui.R");
}
for (const retired of ["canvas-confetti", "sweetalert2", "fonts.googleapis.com", "fonts.gstatic.com", "cdn.jsdelivr.net", "MASCOT_CRITTER", "splash-guide"]) {
  excludes(ui, retired, "ui.R");
}

// Responsive/focus/loading implementation contracts.
includes(posterCss, "min-height: 52px", "www/poster.css CTA");
includes(posterCss, ".brd-poster-suite-link", "www/poster.css Driver route");
includes(posterCss, "min-height: 44px", "www/poster.css touch targets");
includes(posterCss, "@media (max-width: 700px)", "www/poster.css");
includes(posterCss, "order: 1", "www/poster.css artwork-first mobile order");
includes(posterCss, "order: 2", "www/poster.css copy-second mobile order");
includes(posterCss, "@media (prefers-reduced-motion: reduce)", "www/poster.css");
includes(posterCss, "@media (forced-colors: active)", "www/poster.css");
excludes(posterCss, "figcaption", "www/poster.css");
includes(birdCss, '.load-overlay[aria-hidden="true"]', "www/bird.css");
includes(birdCss, '.load-overlay[aria-hidden="false"]', "www/bird.css");
includes(birdCss, ":focus-visible", "www/bird.css");
includes(birdCss, "min-height:44px", "www/bird.css touch targets");
for (const retired of [".mascot", "mascotBob", ".splash-guide", ".mascot-cheer"]) {
  excludes(birdCss, retired, "www/bird.css");
}
includes(appJs, 'setAttribute("aria-hidden", "false")', "www/app.js loading start");
includes(appJs, 'setAttribute("aria-hidden", "true")', "www/app.js loading finish");
includes(appJs, 'setAttribute("aria-busy", "true")', "www/app.js loading start");
includes(appJs, 'setAttribute("aria-busy", "false")', "www/app.js loading finish");
includes(appJs, "smtLoadReturnFocus", "www/app.js focus return");
includes(appJs, "installLocalToast", "www/app.js local export feedback");
for (const retired of ["rodentConfetti", "mascotCheer", "canvas-confetti", 'addCustomMessageHandler("confetti"']) {
  excludes(appJs, retired, "www/app.js");
}

function sha256(relative) {
  return crypto.createHash("sha256").update(fs.readFileSync(path.join(root, relative))).digest("hex");
}
function pngSize(relative) {
  const data = fs.readFileSync(path.join(root, relative));
  assert(data.length >= 24 && data.subarray(1, 4).toString("ascii") === "PNG", `${relative}: not a PNG`);
  return [data.readUInt32BE(16), data.readUInt32BE(20)];
}
function webpSize(relative) {
  const data = fs.readFileSync(path.join(root, relative));
  assert(data.length >= 30 && data.subarray(0, 4).toString("ascii") === "RIFF" && data.subarray(8, 12).toString("ascii") === "WEBP", `${relative}: not a WebP`);
  let offset = 12;
  while (offset + 8 <= data.length) {
    const kind = data.subarray(offset, offset + 4).toString("ascii");
    const length = data.readUInt32LE(offset + 4);
    const payload = offset + 8;
    if (kind === "VP8X" && length >= 10) {
      return [data.readUIntLE(payload + 4, 3) + 1, data.readUIntLE(payload + 7, 3) + 1];
    }
    if (kind === "VP8 " && length >= 10 && data[payload + 3] === 0x9d && data[payload + 4] === 0x01 && data[payload + 5] === 0x2a) {
      return [data.readUInt16LE(payload + 6) & 0x3fff, data.readUInt16LE(payload + 8) & 0x3fff];
    }
    if (kind === "VP8L" && length >= 5 && data[payload] === 0x2f) {
      const bits = data.readUInt32LE(payload + 1);
      return [(bits & 0x3fff) + 1, ((bits >>> 14) & 0x3fff) + 1];
    }
    offset = payload + length + (length % 2);
  }
  throw new Error(`${relative}: WebP dimensions not found`);
}
function assertDimensions(relative, expected, reader) {
  const actual = reader(relative);
  assert(actual[0] === expected[0] && actual[1] === expected[1],
    `${relative}: expected ${expected.join("x")}, found ${actual.join("x")}`);
}

const artFiles = [
  ["birds-living-poster-v1.png", [1536, 1024], pngSize],
  ["birds-living-poster-v1.webp", [1536, 1024], webpSize],
  ["birds-living-poster-v1-840.webp", [840, 560], webpSize]
];
assert(fs.existsSync(path.join(root, "www/vendor/html-to-image/LICENSE")), "vendored html-to-image license is missing");
assert(sha256("www/vendor/html-to-image/html-to-image.js") === "a90b42909d80964269ef6d5f3d1e4a5a7e2a4c263a5d2a76a9e7151901343262",
  "vendored html-to-image bytes differ from the reviewed suite copy");
for (const [name, dimensions, reader] of artFiles) {
  const docsPath = `docs/assets/${name}`;
  const appPath = `www/assets/${name}`;
  assert(fs.existsSync(path.join(root, docsPath)), `${docsPath}: missing`);
  assert(fs.existsSync(path.join(root, appPath)), `${appPath}: missing`);
  assert(sha256(docsPath) === sha256(appPath), `${name}: Pages and app bytes differ`);
  assert(provenance.includes(`\`${docsPath}\` | \`${sha256(docsPath)}\``),
    `${docsPath}: provenance SHA-256 is missing or stale`);
  assertDimensions(docsPath, dimensions, reader);
  assertDimensions(appPath, dimensions, reader);
}
assertDimensions("docs/og-image-v2.png", [1200, 630], pngSize);
assert(provenance.includes(`\`docs/og-image-v2.png\` | \`${sha256("docs/og-image-v2.png")}\``),
  "docs/og-image-v2.png: provenance SHA-256 is missing or stale");
includes(provenance, "Generated: 2026-07-28 with OpenAI ImageGen.", "docs/ART-PROVENANCE.md");
includes(provenance, "not a photograph, field record, measured datum", "docs/ART-PROVENANCE.md");
const fullWebpBytes = fs.statSync(path.join(root, "docs/assets/birds-living-poster-v1.webp")).size;
const compactWebpBytes = fs.statSync(path.join(root, "docs/assets/birds-living-poster-v1-840.webp")).size;
const socialBytes = fs.statSync(path.join(root, "docs/og-image-v2.png")).size;
assert(fullWebpBytes <= 700 * 1024, `full WebP exceeds 700 KiB (${fullWebpBytes} bytes)`);
assert(compactWebpBytes <= 220 * 1024, `840 WebP exceeds 220 KiB (${compactWebpBytes} bytes)`);
assert(socialBytes >= 100 * 1024 && socialBytes <= 2500 * 1024,
  `social image fails 100–2500 KiB sanity budget (${socialBytes} bytes)`);

console.log("OK: Pages/app Living Poster structure, local assets, responsive order, accessibility hooks, and release scope passed.");
