export interface MoveRecord {
  id: string;
  displayId?: string | null;
  slug: string;
  name: string | null;
  topic: string | null;
  level: string | null;
  type: string | null;
  category: string | null;
  group: string | null;
  baseMove: string | null;
  components: string | null;
  parentIds: string[];
  childIds: string[];
  relatedMoveIds: string[];
  positions: string | null;
  seeAlso: string | null;
  tags: string | null;
  description: string | null;
  source: string | null;
  comments: string | null;
  reviewFlag?: boolean;
  reviewNotes?: string | null;
  moveOrder: number | null;
  topicCol: number | null;
  topicOrder: number | null;
  familyOrder: number | null;
  valid: boolean;
  errors: string | null;
  hasLocalVideo: boolean;
  videoFiles: string[];
  videoLinks: string[];
  previewVideoFile?: string | null;
}

export type MetadataKind = 'topic' | 'family';

export interface MetadataEntry {
  id: string;
  slug: string;
  name: string;
  description: string | null;
  createdAt: string;
  updatedAt: string;
  source: 'derived' | 'custom';
  moveCount: number;
}

export interface SiteMetadata {
  topics: MetadataEntry[];
  families: MetadataEntry[];
}

export type DancerRole = 'lead' | 'follow' | 'unknown';
export type DancerLevel = 'world-class' | 'pro' | 'semi-pro' | 'amateur' | 'unknown';

export interface DancerRecord {
  id: string;
  slug: string;
  fullName: string;
  displayName: string;
  instagramHandle: string | null;
  role: DancerRole;
  level: DancerLevel;
  region: string | null;
  source: 'derived' | 'custom';
  createdAt: string;
  updatedAt: string;
}

export interface DancerDanceEntry {
  id: string;
  displayName: string;
  href: string;
  posterFile: string | null;
  meta: string;
}

export interface DancerMoveEntry {
  id: string;
  slug: string;
  name: string | null;
  topic: string | null;
}

export interface DancerProfile extends DancerRecord {
  dances: DancerDanceEntry[];
  moves: DancerMoveEntry[];
}

export type VideoAssetKind = 'move' | 'source';
export type VideoTiming = 'on1' | 'on2' | 'other';
export type VideoContentType = 'music' | 'counts' | 'other';
export type VideoEnvironment = 'social' | 'class';
export type VideoOriginType = 'self-recorded' | 'download';
export type DerivedClipStatus = 'pending' | 'rendering' | 'ready' | 'failed';
export type CountOverlayPlacement = 'top-left' | 'top-right' | 'bottom-left' | 'bottom-right';
export type CountTimingPreset = 'on2-default' | 'on2-all' | 'on1-default' | 'on1-all';

export interface ClipCropRect {
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface ClipCountMarker {
  id: string;
  count: string;
  ms: number;
  clear: boolean;
}

export interface VideoAsset {
  id: string;
  kind: VideoAssetKind;
  filePath: string;
  displayName: string;
  originalFilename: string;
  dancers: string[];
  timing: VideoTiming;
  contentType: VideoContentType;
  environment: VideoEnvironment;
  originType: VideoOriginType;
  sourceUrl: string | null;
  recordDate: string | null;
  classWorkshop: string | null;
  tags: string[];
  notes: string | null;
  createdAt: string;
}

export interface MoveVideoLink {
  id: string;
  moveId: string;
  assetId: string;
  order: number;
  createdAt: string;
}

export interface DerivedClip {
  id: string;
  sourceAssetId: string;
  moveId: string;
  moveDisplayId?: string | null;
  isKeyVideo: boolean;
  label: string | null;
  descriptorLabel: string | null;
  startPositionId: string | null;
  endPositionId: string | null;
  timingGroupId: string | null;
  manuallyNamed: boolean;
  startMs: number;
  endMs: number;
  actionStartMs: number | null;
  actionEndMs: number | null;
  cropRect: ClipCropRect | null;
  countMarkers: ClipCountMarker[];
  countOverlayPlacement: CountOverlayPlacement;
  countTimingPreset: CountTimingPreset;
  outputAssetId: string | null;
  lowResOutputFilePath: string | null;
  lowResPaddedOutputFilePath: string | null;
  publishedAssetId: string | null;
  publishedLowResFilePath: string | null;
  publishedLowResPaddedFilePath: string | null;
  publishedAt: string | null;
  status: DerivedClipStatus;
  error: string | null;
  createdAt: string;
  updatedAt: string;
}

export interface VideoLibrary {
  videoAssets: VideoAsset[];
  moveVideoLinks: MoveVideoLink[];
  derivedClips: DerivedClip[];
}

export interface MoveVideoEntry {
  assetId: string;
  filePath: string;
  lowResFilePath: string | null;
  lowResPaddedFilePath: string | null;
  displayName: string;
  posterFile: string | null;
  isDerived: boolean;
  kind: VideoAssetKind;
  dancers: string[];
  timing: VideoTiming;
  contentType: VideoContentType;
  environment: VideoEnvironment;
  recordDate: string | null;
  classWorkshop: string | null;
  tags: string[];
  notes: string | null;
  sourceAssetId: string | null;
  sourceDisplayName: string | null;
  sourceOriginalFilename: string | null;
  sourceDancers: string[];
  sourceRecordDate: string | null;
  sourceClassWorkshop: string | null;
  sourceTags: string[];
  sourceNotes: string | null;
  timingLabel: string;
  contentTypeLabel: string;
  environmentLabel: string;
  clipId: string | null;
  clipStartMs: number | null;
  clipActionStartMs: number | null;
  isKeyVideo: boolean;
  countMarkers: ClipCountMarker[];
  countOverlayPlacement: CountOverlayPlacement;
  moveId: string;
  moveDisplayId?: string | null;
  descriptorLabel: string | null;
  startPositionId: string | null;
  startPositionLabel: string | null;
  endPositionId: string | null;
  endPositionLabel: string | null;
  timingGroupId: string | null;
}

export interface PositionOption {
  id: string;
  label: string;
  source: 'derived' | 'custom';
}

export interface RelationshipDiagramNode {
  id: string;
  label: string;
  name: string;
  slug: string | null;
  hasVideo: boolean;
  previewVideoFile: string | null;
  isCurrent: boolean;
  isSummary?: boolean;
}

export interface RelationshipDiagramEdge {
  from: string;
  to: string;
  type?: 'parent-child' | 'related';
}

export interface RelationshipDiagramMeta {
  nodeCount: number;
  edgeCount: number;
  isLarge: boolean;
  hasDiagram: boolean;
}

export interface RelationshipDiagram {
  nodes: RelationshipDiagramNode[];
  edges: RelationshipDiagramEdge[];
  meta: RelationshipDiagramMeta;
}

export interface RawMoveReferenceRecord {
  level: number | string | null;
  id: string;
  name: string | null;
  topic: string | null;
  topic_order: number | string | null;
  components: string | null;
  positions: string | null;
  parent_move: string | null;
  added_move: string | null;
  see_also: string | null;
  tags: string | null;
  type: string | null;
  source: string | null;
  comments: string | null;
  topic_old: string | null;
  family: string | null;
  blend: string | null;
  blended_from: string | null;
  unnamed_19: string | null;
  description: string | null;
  unnamed_21: string | null;
  doc_flags: string | null;
  learned_from: string | null;
  alternative_names: string | null;
  video_links_1: string | null;
  video_links_2: string | null;
  video_links_3: string | null;
  video_links_4: string | null;
  dancers: string | null;
  add: string | null;
  in_tracking: string | null;
  notes: string | null;
}

export interface LayoutEntry {
  id: string | null;
  slug: string | null;
  name: string | null;
  entryType: string | null;
  group: string | null;
  level: string | null;
  type: string | null;
  layoutOrder: number | null;
  levelOrder: number | null;
  valid: boolean;
}

export interface LayoutColumn {
  column: number;
  entries: LayoutEntry[];
}

export interface ProgressEntry {
  id: string;
  preparation: string | null;
  sequencing: string | null;
  success: string | null;
  statNumPrep: number | null;
  statNumSequ: number | null;
  statNumSucc: number | null;
}

export interface ProgressSummary {
  totalMoves: number;
  preppedCount: number;
  goodSequencingCount: number;
  goodSuccessCount: number;
}

export interface ProgressSnapshot {
  date: string;
  summary: ProgressSummary;
  entries: ProgressEntry[];
}

export interface SearchIndexEntry {
  id: string;
  slug: string;
  title: string | null;
  topic: string | null;
  text: string;
}

export interface SiteManifest {
  generatedAt: string;
  source: {
    rawMoveReference: string;
    moves: string;
    layout: string;
    progress: string;
    localVideoDirectory: string | null;
    visualReference: string;
  };
  counts: {
    moveRows: number;
    validMoves: number;
    layoutRows: number;
    layoutColumns: number;
    progressRows: number;
    progressSnapshots: number;
    trackableMoves: number;
    movesWithLocalVideo: number;
  };
  routes: {
    home: '/';
    moveDetail: '/moves/[slug]';
    progress: '/progress';
    progressEditor: '/progress/editor';
  };
}

export interface ProgressViewEntryTitle {
  kind: 'title';
  name: string;
  group: string | null;
  layoutOrder: number | null;
}

export interface ProgressViewEntryMove {
  kind: 'move';
  id: string;
  slug: string;
  name: string;
  level: string | null;
  group: string | null;
  type: string | null;
  prep: number | null;
  sequ: number | null;
  succ: number | null;
  layoutOrder: number | null;
}

export type ProgressViewEntry = ProgressViewEntryTitle | ProgressViewEntryMove;

export interface ProgressViewColumn {
  column: number;
  entries: ProgressViewEntry[];
}

export interface ProgressView {
  date: string;
  label: string;
  summary: ProgressSummary;
  columns: ProgressViewColumn[];
}
