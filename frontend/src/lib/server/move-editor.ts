import fs from 'node:fs/promises';
import path from 'node:path';
import type { MoveRecord } from '$lib/types';
import { moveDisplayId, normalizeMoveDisplayId } from '$lib/move-id';
import { resolveDataDir } from './paths';

export type MoveRelationshipInput = {
  parentIds?: string[];
  childIds?: string[];
  relatedMoveIds?: string[];
};

export type EditableMoveInput = MoveRelationshipInput & {
  id?: string;
  displayId?: string | null;
  slug?: string;
  name?: string | null;
  topic?: string | null;
  level?: string | null;
  type?: string | null;
  category?: string | null;
  group?: string | null;
  baseMove?: string | null;
  components?: string | null;
  positions?: string | null;
  seeAlso?: string | null;
  tags?: string | null;
  description?: string | null;
  source?: string | null;
  comments?: string | null;
  reviewFlag?: boolean;
  reviewNotes?: string | null;
};

export type MoveDraft = {
  draftId: string;
  move: MoveRecord;
  createdAt: string;
  updatedAt: string;
};

type MovePatch = Partial<EditableMoveInput> & MoveRelationshipInput;

type MoveEditStore = {
  version: 1;
  overrides: Record<string, MovePatch>;
  createdMoves: MoveRecord[];
  drafts: MoveDraft[];
};

const STORE_FILENAME = 'move-edits.json';
const emptyStore = (): MoveEditStore => ({
  version: 1,
  overrides: {},
  createdMoves: [],
  drafts: []
});

function storePath() {
  return path.join(resolveDataDir(), STORE_FILENAME);
}

function nowIso() {
  return new Date().toISOString();
}

function normalizeOptionalText(value: unknown) {
  const text = String(value ?? '').trim();
  return text || null;
}

function normalizeMoveId(value: unknown) {
  return String(value ?? '').trim().toUpperCase();
}

function normalizeIdList(values: unknown) {
  const rawValues = Array.isArray(values) ? values : String(values ?? '').split(/[;,+]/);
  const seen = new Set<string>();
  const result: string[] = [];

  for (const value of rawValues) {
    const normalized = normalizeMoveId(value);
    if (!normalized || seen.has(normalized)) {
      continue;
    }
    seen.add(normalized);
    result.push(normalized);
  }

  return result;
}

function slugFromId(id: string) {
  return id.replace(/\//g, '_');
}

function serializeIds(ids: string[]) {
  return ids.length ? ids.join(' + ') : null;
}

async function readStore() {
  try {
    const contents = await fs.readFile(storePath(), 'utf-8');
    const parsed = JSON.parse(contents) as Partial<MoveEditStore>;
    return {
      ...emptyStore(),
      ...parsed,
      overrides: parsed.overrides ?? {},
      createdMoves: parsed.createdMoves ?? [],
      drafts: parsed.drafts ?? []
    };
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return emptyStore();
    }
    throw error;
  }
}

async function writeStore(store: MoveEditStore) {
  const dataDir = resolveDataDir();
  const targetPath = storePath();
  const temporaryPath = path.join(dataDir, `.${STORE_FILENAME}.${process.pid}.${Date.now()}.tmp`);
  await fs.mkdir(dataDir, { recursive: true });
  await fs.writeFile(temporaryPath, `${JSON.stringify(store, null, 2)}\n`);
  await fs.rename(temporaryPath, targetPath);
}

function duplicateMoveIdError(id: string, label: string) {
  return new Error(`ID ${id} is already used by ${label}.`);
}

function collisionLabel(move: Pick<MoveRecord, 'id' | 'name'> | MoveDraft['move']) {
  return move.name ?? move.id;
}

function findDisplayIdCollision(
  displayId: string,
  allMoves: MoveRecord[],
  drafts: MoveDraft[],
  options: { excludeMoveId?: string | null; excludeDraftId?: string | null } = {}
) {
  const normalizedDisplayId = normalizeMoveDisplayId(displayId);
  if (!normalizedDisplayId) {
    return null;
  }

  const publishedMove = allMoves.find(
    (move) => move.id !== options.excludeMoveId && moveDisplayId(move) === normalizedDisplayId
  );
  if (publishedMove) {
    return publishedMove;
  }

  return (
    drafts.find(
      (draft) => draft.draftId !== options.excludeDraftId && moveDisplayId(draft.move) === normalizedDisplayId
    ) ?? null
  );
}

function movePatchFromInput(input: EditableMoveInput): MovePatch {
  return {
    slug: input.slug ? slugFromId(String(input.slug)) : undefined,
    displayId: normalizeOptionalText(input.displayId),
    name: normalizeOptionalText(input.name),
    topic: normalizeOptionalText(input.topic),
    level: normalizeOptionalText(input.level),
    type: normalizeOptionalText(input.type),
    category: normalizeOptionalText(input.category),
    group: normalizeOptionalText(input.group),
    baseMove: normalizeOptionalText(input.baseMove),
    positions: normalizeOptionalText(input.positions),
    seeAlso: normalizeOptionalText(input.seeAlso),
    tags: normalizeOptionalText(input.tags),
    description: normalizeOptionalText(input.description),
    source: normalizeOptionalText(input.source),
    comments: normalizeOptionalText(input.comments),
    reviewFlag: Boolean(input.reviewFlag),
    reviewNotes: normalizeOptionalText(input.reviewNotes),
    parentIds: normalizeIdList(input.parentIds),
    childIds: normalizeIdList(input.childIds),
    relatedMoveIds: normalizeIdList(input.relatedMoveIds)
  };
}

function applyPatch(move: MoveRecord, patch: MovePatch | undefined): MoveRecord {
  if (!patch) {
    return move;
  }

  const parentIds = normalizeIdList(patch.parentIds ?? move.parentIds ?? move.components);
  const childIds = normalizeIdList(patch.childIds ?? move.childIds);
  const relatedMoveIds = normalizeIdList(patch.relatedMoveIds ?? move.relatedMoveIds);

  return {
    ...move,
    ...patch,
    id: move.id,
    slug: patch.slug ?? move.slug,
    parentIds,
    childIds,
    relatedMoveIds,
    components: serializeIds(parentIds)
  };
}

function emptyMove(id: string, input: EditableMoveInput = {}): MoveRecord {
  const normalizedId = normalizeMoveId(id);
  const parentIds = normalizeIdList(input.parentIds);

  return {
    id: normalizedId,
    displayId: normalizeOptionalText(input.displayId) ?? normalizedId,
    slug: input.slug ? slugFromId(String(input.slug)) : slugFromId(normalizedId),
    name: normalizeOptionalText(input.name) ?? normalizedId,
    topic: normalizeOptionalText(input.topic),
    level: normalizeOptionalText(input.level),
    type: normalizeOptionalText(input.type),
    category: normalizeOptionalText(input.category),
    group: normalizeOptionalText(input.group),
    baseMove: normalizeOptionalText(input.baseMove),
    components: serializeIds(parentIds),
    parentIds,
    childIds: normalizeIdList(input.childIds),
    relatedMoveIds: normalizeIdList(input.relatedMoveIds),
    positions: normalizeOptionalText(input.positions),
    seeAlso: normalizeOptionalText(input.seeAlso),
    tags: normalizeOptionalText(input.tags),
    description: normalizeOptionalText(input.description),
    source: normalizeOptionalText(input.source),
    comments: normalizeOptionalText(input.comments),
    reviewFlag: Boolean(input.reviewFlag),
    reviewNotes: normalizeOptionalText(input.reviewNotes),
    moveOrder: null,
    topicCol: null,
    topicOrder: null,
    familyOrder: null,
    valid: true,
    errors: null,
    hasLocalVideo: false,
    videoFiles: [],
    videoLinks: []
  };
}

export function normalizeMoveConnections(moves: MoveRecord[]) {
  const byId = new Map(moves.map((move) => [move.id, move]));
  const parentSets = new Map<string, Set<string>>();
  const childSets = new Map<string, Set<string>>();
  const relatedSets = new Map<string, Set<string>>();

  const ensureSet = (map: Map<string, Set<string>>, id: string) => {
    const existing = map.get(id);
    if (existing) {
      return existing;
    }
    const created = new Set<string>();
    map.set(id, created);
    return created;
  };

  for (const move of moves) {
    const moveId = move.id;
    const parentIds = normalizeIdList(move.parentIds?.length ? move.parentIds : move.components);
    const childIds = normalizeIdList(move.childIds);
    const relatedMoveIds = normalizeIdList(move.relatedMoveIds);

    for (const parentId of parentIds) {
      if (parentId === moveId) continue;
      ensureSet(parentSets, moveId).add(parentId);
      ensureSet(childSets, parentId).add(moveId);
    }

    for (const childId of childIds) {
      if (childId === moveId) continue;
      ensureSet(childSets, moveId).add(childId);
      ensureSet(parentSets, childId).add(moveId);
    }

    for (const relatedId of relatedMoveIds) {
      if (relatedId === moveId) continue;
      ensureSet(relatedSets, moveId).add(relatedId);
      ensureSet(relatedSets, relatedId).add(moveId);
    }
  }

  return moves.map((move) => {
    const parentIds = [...(parentSets.get(move.id) ?? new Set<string>())].filter((id) => byId.has(id)).sort();
    const childIds = [...(childSets.get(move.id) ?? new Set<string>())].filter((id) => byId.has(id)).sort();
    const relatedMoveIds = [...(relatedSets.get(move.id) ?? new Set<string>())].filter((id) => byId.has(id)).sort();

    return {
      ...move,
      parentIds,
      childIds,
      relatedMoveIds,
      components: serializeIds(parentIds)
    };
  });
}

export async function applyMoveEditStore(baseMoves: MoveRecord[]) {
  const store = await readStore();
  const createdById = new Map(store.createdMoves.map((move) => [move.id, move]));
  const baseById = new Map(baseMoves.map((move) => [move.id, move]));
  const combined = [...baseMoves];

  for (const createdMove of createdById.values()) {
    if (!baseById.has(createdMove.id)) {
      combined.push(createdMove);
    }
  }

  return normalizeMoveConnections(combined.map((move) => applyPatch(move, store.overrides[move.id])));
}

export async function listMoveDrafts() {
  const store = await readStore();
  return store.drafts;
}

export async function listCreatedMoveIds() {
  const store = await readStore();
  return store.createdMoves.map((move) => move.id);
}

export async function deleteMoveDraft(draftId: string) {
  const store = await readStore();
  const existingDraft = store.drafts.find((draft) => draft.draftId === draftId);
  if (!existingDraft) {
    throw new Error('Draft not found.');
  }

  store.drafts = store.drafts.filter((draft) => draft.draftId !== draftId);
  await writeStore(store);
  return existingDraft;
}

export async function saveMoveDraft(allMoves: MoveRecord[], input: EditableMoveInput & { draftId?: string }) {
  const store = await readStore();
  const id = normalizeMoveId(input.id);
  if (!id) {
    throw new Error('Move id is required.');
  }

  const existingDraft = input.draftId ? store.drafts.find((draft) => draft.draftId === input.draftId) ?? null : null;
  const duplicateMove = allMoves.find((move) => move.id === id);
  if (duplicateMove) {
    throw duplicateMoveIdError(id, collisionLabel(duplicateMove));
  }

  const duplicateDraftId = store.drafts.find((draft) => draft.draftId !== existingDraft?.draftId && draft.move.id === id);
  if (duplicateDraftId) {
    throw duplicateMoveIdError(id, collisionLabel(duplicateDraftId.move));
  }

  const nextDisplayId = normalizeMoveDisplayId(input.displayId) ?? id;
  const displayIdCollision = findDisplayIdCollision(nextDisplayId, allMoves, store.drafts, {
    excludeDraftId: existingDraft?.draftId ?? null
  });
  if (displayIdCollision) {
    throw duplicateMoveIdError(nextDisplayId, collisionLabel('move' in displayIdCollision ? displayIdCollision.move : displayIdCollision));
  }

  const createdAt = existingDraft?.createdAt ?? nowIso();
  const move = emptyMove(id, input);
  const draft: MoveDraft = {
    draftId: existingDraft?.draftId ?? `draft-${createdAt.replace(/[^0-9]/g, '')}`,
    move,
    createdAt,
    updatedAt: nowIso()
  };

  store.drafts = existingDraft
    ? store.drafts.map((entry) => (entry.draftId === draft.draftId ? draft : entry))
    : [draft, ...store.drafts];
  await writeStore(store);
  return draft;
}

export async function savePublishedMove(allMoves: MoveRecord[], moveId: string, input: EditableMoveInput) {
  const normalizedId = normalizeMoveId(moveId);
  const store = await readStore();
  const existing = allMoves.find((move) => move.id === normalizedId);
  if (!existing) {
    throw new Error('Move not found.');
  }

  const nextDisplayId = normalizeMoveDisplayId(input.displayId) ?? moveDisplayId(existing);
  const displayIdCollision = findDisplayIdCollision(nextDisplayId, allMoves, store.drafts, {
    excludeMoveId: normalizedId
  });
  if (displayIdCollision) {
    throw duplicateMoveIdError(nextDisplayId, collisionLabel('move' in displayIdCollision ? displayIdCollision.move : displayIdCollision));
  }

  const patch = movePatchFromInput(input);
  const updatedMoves = normalizeMoveConnections(
    allMoves.map((move) => (move.id === normalizedId ? applyPatch(move, patch) : move))
  );

  for (const move of updatedMoves) {
    store.overrides[move.id] = {
      ...(store.overrides[move.id] ?? {}),
      parentIds: move.parentIds,
      childIds: move.childIds,
      relatedMoveIds: move.relatedMoveIds,
      components: move.components
    };
  }

  store.overrides[normalizedId] = {
    ...(store.overrides[normalizedId] ?? {}),
    ...patch,
    parentIds: updatedMoves.find((move) => move.id === normalizedId)?.parentIds ?? [],
    childIds: updatedMoves.find((move) => move.id === normalizedId)?.childIds ?? [],
    relatedMoveIds: updatedMoves.find((move) => move.id === normalizedId)?.relatedMoveIds ?? []
  };

  await writeStore(store);
  return updatedMoves.find((move) => move.id === normalizedId) ?? existing;
}

export async function publishMoveDraft(allMoves: MoveRecord[], draftId: string, input?: EditableMoveInput) {
  const store = await readStore();
  const draft = store.drafts.find((entry) => entry.draftId === draftId);
  if (!draft) {
    throw new Error('Draft not found.');
  }

  const move = input ? emptyMove(input.id ?? draft.move.id, input) : draft.move;
  if (allMoves.some((entry) => entry.id === move.id)) {
    throw new Error('A published move already uses this id.');
  }

  const displayIdCollision = findDisplayIdCollision(moveDisplayId(move), allMoves, store.drafts, {
    excludeDraftId: draftId
  });
  if (displayIdCollision) {
    throw duplicateMoveIdError(moveDisplayId(move), collisionLabel('move' in displayIdCollision ? displayIdCollision.move : displayIdCollision));
  }

  store.createdMoves = [move, ...store.createdMoves.filter((entry) => entry.id !== move.id)];
  store.drafts = store.drafts.filter((entry) => entry.draftId !== draftId);

  const updatedMoves = normalizeMoveConnections([...allMoves, move]);
  for (const entry of updatedMoves) {
    store.overrides[entry.id] = {
      ...(store.overrides[entry.id] ?? {}),
      parentIds: entry.parentIds,
      childIds: entry.childIds,
      relatedMoveIds: entry.relatedMoveIds,
      components: entry.components
    };
  }

  await writeStore(store);
  return updatedMoves.find((entry) => entry.id === move.id) ?? move;
}
