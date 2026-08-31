import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const files = ["www/app.js", "www/pincards.js"];
const expected = new Set(["countUp", "loadDone", "smtLoadStart", "kickMaps"]);
const registrations = [];

for (const relative of files) {
  const source = fs.readFileSync(path.join(root, relative), "utf8");
  const calls = [...source.matchAll(/(?:Shiny|window\.Shiny)\.addCustomMessageHandler\s*\(/g)];
  const parsed = [...source.matchAll(
    /(?:Shiny|window\.Shiny)\.addCustomMessageHandler\s*\(\s*["']([^"']+)["']\s*,\s*function\s*\(([^)]*)\)/g
  )];

  if (calls.length !== parsed.length) {
    throw new Error(`${relative}: found ${calls.length} registrations but could parse only ${parsed.length}; use a named message plus a traditional one-argument function callback`);
  }

  for (const match of parsed) {
    const params = match[2].split(",").map((value) => value.trim()).filter(Boolean);
    if (params.length !== 1) {
      throw new Error(`${relative}: handler ${JSON.stringify(match[1])} must accept exactly one payload argument; found ${params.length}`);
    }
    registrations.push({ file: relative, name: match[1], parameter: params[0] });
  }
}

const counts = new Map();
for (const registration of registrations) {
  counts.set(registration.name, (counts.get(registration.name) || 0) + 1);
}

for (const name of expected) {
  if (counts.get(name) !== 1) {
    throw new Error(`expected exactly one ${JSON.stringify(name)} handler; found ${counts.get(name) || 0}`);
  }
}
for (const [name, count] of counts) {
  if (!expected.has(name)) throw new Error(`unexpected Shiny custom-message handler: ${JSON.stringify(name)}`);
  if (count !== 1) throw new Error(`duplicate Shiny custom-message handler ${JSON.stringify(name)}: ${count}`);
}

console.log(`OK: ${registrations.length} Shiny custom-message handlers each accept exactly one payload argument.`);
