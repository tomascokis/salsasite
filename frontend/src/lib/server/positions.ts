import fs from 'node:fs/promises';
import path from 'node:path';
import type { MoveRecord, PositionOption } from '$lib/types';
import { derivePositionOptions } from '$lib/video-library-utils';
import { resolveDataDir } from './paths';

type PositionStore = {
  version: 1;
  positions: Array<{ id?: string; label?: string; name?: string } | string>;
};

const STORE_FILENAME = 'positions.json';

function storePath() {
  return path.join(resolveDataDir(), STORE_FILENAME);
}

async function readStore(): Promise<PositionStore> {
  try {
    const contents = await fs.readFile(storePath(), 'utf-8');
    const parsed = JSON.parse(contents) as Partial<PositionStore>;
    return {
      version: 1,
      positions: Array.isArray(parsed.positions) ? parsed.positions : []
    };
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return { version: 1, positions: [] };
    }
    throw error;
  }
}

export async function getPositionOptions(moves: MoveRecord[]): Promise<PositionOption[]> {
  const store = await readStore();
  return derivePositionOptions(moves, store.positions) as PositionOption[];
}

export function positionLabelById(options: PositionOption[]) {
  return new Map(options.map((option) => [option.id, option.label]));
}
