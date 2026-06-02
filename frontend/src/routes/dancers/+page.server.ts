import { getMoves, getRawMoveReference } from '$lib/server/data';
import { getDancerProfiles } from '$lib/server/dancers';
import { readVideoLibrary } from '$lib/server/video-library';

export async function load() {
  const [moves, rawReferences] = await Promise.all([getMoves(), getRawMoveReference()]);
  const library = await readVideoLibrary();
  const dancers = await getDancerProfiles(moves, rawReferences, library);

  return {
    dancers
  };
}
