import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';

const projectRoot = new URL('..', import.meta.url).pathname;
const serverRoot = path.join(projectRoot, 'src', 'lib', 'server');
const routesRoot = path.join(projectRoot, 'src', 'routes');

async function walkFiles(root, predicate = () => true) {
  const result = [];

  async function visit(directory) {
    const entries = await fs.readdir(directory, { withFileTypes: true });
    for (const entry of entries) {
      const filePath = path.join(directory, entry.name);
      if (entry.isDirectory()) {
        await visit(filePath);
        continue;
      }
      if (predicate(filePath)) {
        result.push(filePath);
      }
    }
  }

  await visit(root);
  return result.sort();
}

test('video-library remains a re-export-only compatibility facade', async () => {
  const filePath = path.join(serverRoot, 'video-library.ts');
  const source = await fs.readFile(filePath, 'utf-8');
  const remainder = source.replace(/export\s*\{[\s\S]*?\}\s*from\s*['"][^'"]+['"];\s*/g, '').trim();
  assert.equal(remainder, '');
});

test('media domain modules do not import the video-library facade', async () => {
  const files = await walkFiles(serverRoot, (filePath) =>
    path.basename(filePath).startsWith('media-') && filePath.endsWith('.ts')
  );
  const offenders = [];

  for (const filePath of files) {
    const source = await fs.readFile(filePath, 'utf-8');
    if (/from\s*['"](?:\.\/video-library|\$lib\/server\/video-library)['"]/.test(source)) {
      offenders.push(path.relative(projectRoot, filePath));
    }
  }

  assert.deepEqual(offenders, []);
});

test('routes use video-library facade instead of media domain modules', async () => {
  const files = await walkFiles(routesRoot, (filePath) => filePath.endsWith('.ts') || filePath.endsWith('.svelte'));
  const offenders = [];

  for (const filePath of files) {
    const source = await fs.readFile(filePath, 'utf-8');
    if (/from\s*['"]\$lib\/server\/media-[^'"]+['"]/.test(source)) {
      offenders.push(path.relative(projectRoot, filePath));
    }
  }

  assert.deepEqual(offenders, []);
});

test('production media workflows do not import direct catalog writes', async () => {
  const files = await walkFiles(serverRoot, (filePath) => filePath.endsWith('.ts'));
  const offenders = [];

  for (const filePath of files) {
    if (path.basename(filePath) === 'media-catalog.ts') {
      continue;
    }
    const source = await fs.readFile(filePath, 'utf-8');
    if (/import\s*\{[^}]*\bwriteMediaCatalog\b[^}]*\}\s*from\s*['"]\.\/media-catalog['"]/.test(source)) {
      offenders.push(path.relative(projectRoot, filePath));
    }
  }

  assert.deepEqual(offenders, []);
});

test('media read models use the no-write video library read path', async () => {
  const filePath = path.join(serverRoot, 'media-read-models.ts');
  const source = await fs.readFile(filePath, 'utf-8');

  assert.match(source, /import\s*\{[^}]*\breadVideoLibrary\b[^}]*\}\s*from\s*['"]\.\/media-bootstrap-service['"]/);
  assert.doesNotMatch(source, /from\s*['"]\.\/media-catalog['"]/);
  assert.doesNotMatch(source, /\bgetVideoLibrary(?:WithRepairs)?\s*\(/);
});
