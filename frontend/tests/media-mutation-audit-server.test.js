import './server-module-hooks.js';

import test from 'node:test';
import assert from 'node:assert/strict';

import {
  cleanupMediaTestEnvironment,
  setupMediaTestEnvironment
} from './media-server-fixtures.js';

const env = await setupMediaTestEnvironment('media-mutation-audit');

test.after(async () => {
  await cleanupMediaTestEnvironment(env.tempRoot);
});

function historyEntry(entries, type) {
  return entries.find((entry) => entry.type === type);
}

test('media catalog mutations record non-undoable audit history actions', async () => {
  const {
    createSourceAsset,
    listMediaManagerJobs,
    queueClipRender,
    saveSourceClips,
    setClipKeyVideo,
    updateSourceAsset
  } = await import('../src/lib/server/video-library.ts');
  const { listHistory, undoAction } = await import('../src/lib/server/history.ts');
  const { GET } = await import('../src/routes/api/history/+server.ts');

  const created = await createSourceAsset({
    originalFilename: 'source.mp4',
    displayName: 'Source Clip',
    dancers: ['A'],
    timing: 'on1',
    contentType: 'music',
    environment: 'class',
    originType: 'self-recorded',
    recordDate: null,
    classWorkshop: null,
    tags: [],
    notes: null,
    fileBuffer: Buffer.from('source-video-bytes')
  });
  assert.equal(created.reusedExisting, false);

  await updateSourceAsset({
    assetId: created.asset.id,
    displayName: 'Updated Source',
    dancers: ['A', 'B'],
    timing: 'on2',
    contentType: 'counts',
    environment: 'social',
    originType: 'self-recorded',
    sourceUrl: null,
    createdAt: created.asset.createdAt,
    recordDate: '2026-06-02',
    classWorkshop: null,
    tags: ['low quality'],
    notes: 'Updated notes'
  });

  const savedClips = await saveSourceClips({
    sourceAssetId: created.asset.id,
    clips: [
      {
        moveId: 'RT000001',
        moveDisplayId: 'RT000001',
        isKeyVideo: true,
        label: null,
        descriptorLabel: null,
        startPositionId: null,
        endPositionId: null,
        timingGroupId: null,
        manuallyNamed: false,
        startMs: 0,
        endMs: 3000,
        actionStartMs: 500,
        actionEndMs: 2500,
        cropRect: null,
        countMarkers: [],
        countOverlayPlacement: 'top-left',
        countTimingPreset: 'on2-default'
      }
    ]
  });
  assert.equal(savedClips.length, 1);

  await setClipKeyVideo({ clipId: savedClips[0].id, isKeyVideo: false });

  const renderClipId = 'missing-render-clip';
  await queueClipRender(renderClipId);

  const entries = listHistory(50);
  const response = await GET({
    url: new URL('http://test.local/api/history?limit=50')
  });
  const payload = await response.json();
  assert.equal(response.status, 200);
  assert.equal(JSON.stringify(payload.entries).includes('"before"'), false);
  assert.equal(JSON.stringify(payload.entries).includes('"after"'), false);
  assert.ok(payload.entries.some((entry) => entry.type === 'media.source.create'));
  assert.ok(payload.entries.some((entry) => entry.type === 'media.source.update'));

  const createEntry = historyEntry(entries, 'media.source.create');
  const updateEntry = historyEntry(entries, 'media.source.update');
  const saveEntry = historyEntry(entries, 'media.clips.save');
  const keyEntry = historyEntry(entries, 'media.clip.key.update');
  const queueEntry = historyEntry(entries, 'media.clip.render.queue');

  for (const entry of [createEntry, updateEntry, saveEntry, keyEntry, queueEntry]) {
    assert.ok(entry);
    assert.equal(entry.canUndo, false);
    assert.equal(entry.undoUnavailableReason, 'This action type is not undoable yet');
    assert.equal(JSON.stringify(entry).includes('"before"'), false);
    assert.equal(JSON.stringify(entry).includes('"after"'), false);
    await assert.rejects(() => undoAction(entry.id), /This action type is not undoable yet/);
  }

  assert.equal(createEntry.entityType, 'media:source');
  assert.equal(updateEntry.entityId, created.asset.id);
  assert.equal(saveEntry.entityType, 'media:source');
  assert.equal(keyEntry.entityType, 'media:clip');
  assert.equal(queueEntry.entityId, renderClipId);

  const renderJob = listMediaManagerJobs(20).find((job) => job.type === 'clip.render' && job.targetId === renderClipId);
  assert.ok(renderJob);
});
