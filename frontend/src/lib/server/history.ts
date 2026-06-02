import type { MetadataKind } from '$lib/types';
import {
  getAppDatabase,
  listActions,
  markActionUndone,
  recordAction,
  runInTransaction,
  type ActionRecord
} from './app-state';
import { restoreDancerState } from './dancers';
import { restoreMetadataEntryState } from './metadata';
import { restoreMoveEditStoreState } from './move-editor';

type ActionRow = {
  id: string;
  type: string;
  label: string;
  entity_type: string;
  entity_id: string;
  status: string;
  created_at: string;
  actor: string | null;
  before_json: string;
  after_json: string;
  undone_by_action_id: string | null;
  undo_of_action_id: string | null;
};

export type HistoryEntry = ActionRecord & {
  canUndo: boolean;
  undoUnavailableReason: string | null;
};

const UNDOABLE_MOVE_TYPES = new Set(['moveDraft.create', 'moveDraft.update', 'moveDraft.delete', 'move.update']);

function parseActionRow(row: ActionRow): ActionRecord {
  return {
    id: row.id,
    type: row.type,
    label: row.label,
    entityType: row.entity_type,
    entityId: row.entity_id,
    status: row.status === 'undone' ? 'undone' : 'active',
    createdAt: row.created_at,
    actor: row.actor,
    before: JSON.parse(row.before_json),
    after: JSON.parse(row.after_json),
    undoneByActionId: row.undone_by_action_id,
    undoOfActionId: row.undo_of_action_id
  };
}

function actionUndoReason(action: ActionRecord) {
  if (action.status === 'undone') {
    return 'Already undone';
  }

  if (action.undoOfActionId) {
    return 'Undo actions cannot be undone';
  }

  if (action.entityType === 'metadata:topic' || action.entityType === 'metadata:family') {
    return null;
  }

  if (action.entityType === 'dancer') {
    return null;
  }

  if ((action.entityType === 'moveDraft' || action.entityType === 'move') && UNDOABLE_MOVE_TYPES.has(action.type)) {
    return null;
  }

  if (action.type === 'moveDraft.publish') {
    return 'Published drafts may include media relinks; media undo is not part of this slice';
  }

  return 'This action type is not undoable yet';
}

function toHistoryEntry(action: ActionRecord): HistoryEntry {
  const undoUnavailableReason = actionUndoReason(action);
  return {
    ...action,
    canUndo: undoUnavailableReason === null,
    undoUnavailableReason
  };
}

function loadAction(id: string) {
  const row = getAppDatabase()
    .prepare(
      `
        SELECT
          id, type, label, entity_type, entity_id, status, created_at, actor,
          before_json, after_json, undone_by_action_id, undo_of_action_id
        FROM actions
        WHERE id = ?
      `
    )
    .get(id) as ActionRow | undefined;

  return row ? parseActionRow(row) : null;
}

export function listHistory(limit = 100) {
  return listActions(limit).map(toHistoryEntry);
}

export function undoAction(id: string) {
  const action = loadAction(id);
  if (!action) {
    throw new Error('Action not found.');
  }

  const undoUnavailableReason = actionUndoReason(action);
  if (undoUnavailableReason) {
    throw new Error(undoUnavailableReason);
  }

  let undoActionId = '';
  runInTransaction((db) => {
    if (action.entityType === 'metadata:topic' || action.entityType === 'metadata:family') {
      restoreMetadataEntryState(
        db,
        action.entityType.replace('metadata:', '') as MetadataKind,
        action.entityId,
        action.before as Parameters<typeof restoreMetadataEntryState>[3]
      );
    } else if (action.entityType === 'dancer') {
      restoreDancerState(db, action.before as Parameters<typeof restoreDancerState>[1]);
    } else if (action.entityType === 'moveDraft' || action.entityType === 'move') {
      restoreMoveEditStoreState(db, action.before as Parameters<typeof restoreMoveEditStoreState>[1]);
    }

    const undo = recordAction(db, {
      type: `${action.type}.undo`,
      label: `Undid: ${action.label}`,
      entityType: action.entityType,
      entityId: action.entityId,
      before: action.after,
      after: action.before,
      undoOfActionId: action.id
    });
    undoActionId = undo.id;
    markActionUndone(db, action.id, undo.id);
  });

  return {
    action: toHistoryEntry({
      ...action,
      status: 'undone',
      undoneByActionId: undoActionId
    })
  };
}

