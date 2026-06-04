import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import { cleanupMediaTestEnvironment, setupMediaTestEnvironment } from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('positions-server');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

test('custom positions are persisted and returned as reusable options', async () => {
  const { getAppDatabase } = await import('../src/lib/server/app-state.ts');
  const { getPositionOptions, savePositionOption } = await import('../src/lib/server/positions.ts');

  const created = await savePositionOption({ label: 'Open Break' });
  assert.deepEqual(created, { id: 'open-break', label: 'Open Break', source: 'custom' });

  assert.deepEqual(await getPositionOptions([]), [created]);

  const updated = await savePositionOption({ label: 'Open break' });
  assert.deepEqual(updated, { id: 'open-break', label: 'Open break', source: 'custom' });
  assert.deepEqual(await getPositionOptions([]), [updated]);

  const actions = getAppDatabase()
    .prepare('SELECT type, entity_type, entity_id FROM actions ORDER BY created_at')
    .all()
    .map((row) => ({ ...row }));
  assert.deepEqual(actions, [
    { type: 'position.create', entity_type: 'position', entity_id: 'open-break' },
    { type: 'position.update', entity_type: 'position', entity_id: 'open-break' }
  ]);
});
