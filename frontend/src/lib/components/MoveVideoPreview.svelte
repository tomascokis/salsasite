<script lang="ts">
  export let filePath = '';
  export let posterFile: string | null = null;
  export let label = 'Move video preview';

  function mediaUrl(file: string) {
    return `/media/${encodeURIComponent(file)}`;
  }

  function posterUrl(file: string) {
    return `/posters/${encodeURIComponent(file)}`;
  }
</script>

<div class="move-editor-video-preview">
  <video
    controls
    preload="metadata"
    poster={posterFile ? posterUrl(posterFile) : undefined}
    src={mediaUrl(filePath)}
    aria-label={label}
  ></video>
</div>
