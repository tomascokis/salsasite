export function colorForProgress(kind: 'prep' | 'sequ' | 'succ', value: number | null) {
  const prepColors: Record<number, string> = {
    0: '#fff5ee',
    1: '#ffdead',
    2: '#ffd700',
    3: '#ffa07a',
    4: '#7fff00'
  };

  const sequColors: Record<number, string> = {
    0: '#ffffff',
    1: '#ee2c2c',
    2: '#ffd700',
    3: '#2f4f4f',
    4: '#7fff00',
    5: '#66cd00'
  };

  const succColors: Record<number, string> = {
    0: '#ffffff',
    1: '#ff4500',
    2: '#ffd700',
    3: '#bcee68',
    4: '#66cd00'
  };

  if (value === null || Number.isNaN(value)) return '#ffffff';
  if (kind === 'prep') return prepColors[value] ?? '#ffffff';
  if (kind === 'sequ') return sequColors[value] ?? '#ffffff';
  return succColors[value] ?? '#ffffff';
}
