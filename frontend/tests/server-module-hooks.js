import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL, fileURLToPath } from 'node:url';
import { registerHooks } from 'node:module';

const frontendRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const srcRoot = path.join(frontendRoot, 'src');

function resolvedFileUrl(basePath) {
  const candidates = [basePath, `${basePath}.ts`, `${basePath}.js`];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return pathToFileURL(candidate).href;
    }
  }
  return null;
}

registerHooks({
  resolve(specifier, context, nextResolve) {
    if (specifier.startsWith('$lib/')) {
      const resolved = resolvedFileUrl(path.join(srcRoot, 'lib', specifier.slice('$lib/'.length)));
      if (resolved) {
        return { url: resolved, shortCircuit: true };
      }
    }

    if ((specifier.startsWith('./') || specifier.startsWith('../')) && context.parentURL?.startsWith('file:')) {
      const resolved = resolvedFileUrl(path.resolve(path.dirname(fileURLToPath(context.parentURL)), specifier));
      if (resolved) {
        return { url: resolved, shortCircuit: true };
      }
    }

    return nextResolve(specifier, context);
  }
});
