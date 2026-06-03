import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';

delete process.env.DATA_DIR;
delete process.env.MEDIA_ROOT;
delete process.env.SOURCE_ROOT;
delete process.env.POSTER_ROOT;

test('server path defaults resolve under data/live', async () => {
  const {
    resolveAppStateBootstrapDir,
    resolveCatalogBootstrapDir,
    resolveDataDir,
    resolveMediaRoot,
    resolvePosterRoot,
    resolveSourceRoot
  } = await import('../src/lib/server/paths.ts');

  assert.equal(resolveDataDir(), path.resolve(process.cwd(), '../data/live'));
  assert.equal(resolveCatalogBootstrapDir(), path.resolve(process.cwd(), '../data/live/bootstrap/catalog'));
  assert.equal(resolveAppStateBootstrapDir(), path.resolve(process.cwd(), '../data/live/bootstrap/app-state'));
  assert.equal(resolveMediaRoot(), path.resolve(process.cwd(), '../data/live/media/video-moves'));
  assert.equal(resolveSourceRoot(), path.resolve(process.cwd(), '../data/live/media/video-sources'));
  assert.equal(resolvePosterRoot(), path.resolve(process.cwd(), '../data/live/media/video-posters'));
});
