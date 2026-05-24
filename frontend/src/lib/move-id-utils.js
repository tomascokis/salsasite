export function generatedMoveIdStem(value) {
  const normalized = String(value ?? '')
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .toUpperCase();
  const tokens = normalized.match(/[A-Z0-9]+/g) ?? [];
  return tokens.join('').slice(0, 18) || 'MOVE';
}

export function draftMoveIdFromName(name, existingIds = []) {
  const stem = generatedMoveIdStem(name);
  const usedIds = new Set(existingIds.map((id) => String(id ?? '').trim().toUpperCase()).filter(Boolean));
  let candidate = stem;
  let suffix = 2;

  while (usedIds.has(candidate)) {
    const suffixText = String(suffix);
    candidate = `${stem.slice(0, Math.max(1, 18 - suffixText.length))}${suffixText}`;
    suffix += 1;
  }

  return candidate;
}
