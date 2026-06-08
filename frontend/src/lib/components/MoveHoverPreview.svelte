<script lang="ts">
  export let id = '';
  export let name = '';
  export let videoFile: string | null = null;
  export let left = 0;
  export let top = 0;

  function mediaUrl(file: string) {
    return `/media/${encodeURIComponent(file)}`;
  }
</script>

<div
  class="relationship-node-preview"
  style={`left: ${left}px; top: ${top}px;`}
  aria-hidden="true"
>
  {#if videoFile}
    <video
      class="relationship-node-preview-video"
      src={mediaUrl(videoFile)}
      autoplay
      muted
      loop
      playsinline
      preload="metadata"
    ></video>
  {:else}
    <div class="relationship-node-preview-empty">No video</div>
  {/if}
  <div class="relationship-node-preview-body">
    <strong>{name}</strong>
    <span>{id}</span>
  </div>
</div>
