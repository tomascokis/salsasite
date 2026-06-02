import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';

import {
  cleanupMediaTestEnvironment,
  exists,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-catalog-empty');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('media catalog repository returns defaults when JSON seed is missing', async () => {
  const { readMediaCatalog } = await import('../src/lib/server/media-catalog.ts');
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');

  assert.deepEqual(await readMediaCatalog(), {
    videoAssets: [],
    moveVideoLinks: [],
    derivedClips: []
  });
  assert.equal(await exists(path.join(env.dataDir, 'video-library.backup-before-sqlite.json')), false);

  const meta = getAppDatabase().prepare('SELECT value FROM app_state_meta WHERE key = ?').get('media_catalog_sqlite_v1');
  assert.equal(meta.value, 'complete');
});
