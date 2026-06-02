
// this file is generated — do not edit it


declare module "svelte/elements" {
	export interface HTMLAttributes<T> {
		'data-sveltekit-keepfocus'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-noscroll'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-preload-code'?:
			| true
			| ''
			| 'eager'
			| 'viewport'
			| 'hover'
			| 'tap'
			| 'off'
			| undefined
			| null;
		'data-sveltekit-preload-data'?: true | '' | 'hover' | 'tap' | 'off' | undefined | null;
		'data-sveltekit-reload'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-replacestate'?: true | '' | 'off' | undefined | null;
	}
}

export {};


declare module "$app/types" {
	type MatcherParam<M> = M extends (param : string) => param is (infer U extends string) ? U : string;

	export interface AppTypes {
		RouteId(): "/" | "/api" | "/api/clips" | "/api/clips/[clipId]" | "/api/clips/[clipId]/key" | "/api/dancers" | "/api/history" | "/api/history/[id]" | "/api/history/[id]/undo" | "/api/media" | "/api/media/library" | "/api/metadata" | "/api/moves" | "/api/moves/create" | "/api/moves/[...id]" | "/api/posters" | "/api/posters/[...path]" | "/api/search" | "/api/upload" | "/api/upload/clips" | "/api/upload/library" | "/api/upload/publish" | "/api/upload/render" | "/api/upload/source" | "/api/upload/source/[id]" | "/api/upload/source/[id]/detect-fields" | "/dancers" | "/families" | "/families/[slug]" | "/media" | "/media/edit" | "/media/edit/[id]" | "/media/[...path]" | "/moves" | "/moves/create" | "/moves/create/metadata" | "/moves/[slug]" | "/moves/[slug]/edit" | "/posters" | "/posters/[...path]" | "/progress" | "/progress/editor" | "/settings" | "/settings/history" | "/settings/pickers" | "/topics" | "/topics/[slug]" | "/upload";
		RouteParams(): {
			"/api/clips/[clipId]": { clipId: string };
			"/api/clips/[clipId]/key": { clipId: string };
			"/api/history/[id]": { id: string };
			"/api/history/[id]/undo": { id: string };
			"/api/moves/[...id]": { id: string };
			"/api/posters/[...path]": { path: string };
			"/api/upload/source/[id]": { id: string };
			"/api/upload/source/[id]/detect-fields": { id: string };
			"/families/[slug]": { slug: string };
			"/media/edit/[id]": { id: string };
			"/media/[...path]": { path: string };
			"/moves/[slug]": { slug: string };
			"/moves/[slug]/edit": { slug: string };
			"/posters/[...path]": { path: string };
			"/topics/[slug]": { slug: string }
		};
		LayoutParams(): {
			"/": { clipId?: string; id?: string; path?: string; slug?: string };
			"/api": { clipId?: string; id?: string; path?: string };
			"/api/clips": { clipId?: string };
			"/api/clips/[clipId]": { clipId: string };
			"/api/clips/[clipId]/key": { clipId: string };
			"/api/dancers": Record<string, never>;
			"/api/history": { id?: string };
			"/api/history/[id]": { id: string };
			"/api/history/[id]/undo": { id: string };
			"/api/media": Record<string, never>;
			"/api/media/library": Record<string, never>;
			"/api/metadata": Record<string, never>;
			"/api/moves": { id?: string };
			"/api/moves/create": Record<string, never>;
			"/api/moves/[...id]": { id: string };
			"/api/posters": { path?: string };
			"/api/posters/[...path]": { path: string };
			"/api/search": Record<string, never>;
			"/api/upload": { id?: string };
			"/api/upload/clips": Record<string, never>;
			"/api/upload/library": Record<string, never>;
			"/api/upload/publish": Record<string, never>;
			"/api/upload/render": Record<string, never>;
			"/api/upload/source": { id?: string };
			"/api/upload/source/[id]": { id: string };
			"/api/upload/source/[id]/detect-fields": { id: string };
			"/dancers": Record<string, never>;
			"/families": { slug?: string };
			"/families/[slug]": { slug: string };
			"/media": { id?: string; path?: string };
			"/media/edit": { id?: string };
			"/media/edit/[id]": { id: string };
			"/media/[...path]": { path: string };
			"/moves": { slug?: string };
			"/moves/create": Record<string, never>;
			"/moves/create/metadata": Record<string, never>;
			"/moves/[slug]": { slug: string };
			"/moves/[slug]/edit": { slug: string };
			"/posters": { path?: string };
			"/posters/[...path]": { path: string };
			"/progress": Record<string, never>;
			"/progress/editor": Record<string, never>;
			"/settings": Record<string, never>;
			"/settings/history": Record<string, never>;
			"/settings/pickers": Record<string, never>;
			"/topics": { slug?: string };
			"/topics/[slug]": { slug: string };
			"/upload": Record<string, never>
		};
		Pathname(): "/" | `/api/clips/${string}/key` & {} | "/api/dancers" | "/api/history" | `/api/history/${string}/undo` & {} | "/api/media/library" | "/api/metadata" | "/api/moves/create" | `/api/moves/${string}` & {} | `/api/posters/${string}` & {} | "/api/search" | "/api/upload/clips" | "/api/upload/library" | "/api/upload/publish" | "/api/upload/render" | "/api/upload/source" | `/api/upload/source/${string}` & {} | `/api/upload/source/${string}/detect-fields` & {} | "/dancers" | `/families/${string}` & {} | "/media" | `/media/edit/${string}` & {} | `/media/${string}` & {} | "/moves/create" | "/moves/create/metadata" | `/moves/${string}` & {} | `/moves/${string}/edit` & {} | `/posters/${string}` & {} | "/progress" | "/progress/editor" | "/settings" | "/settings/history" | "/settings/pickers" | `/topics/${string}` & {} | "/upload";
		ResolvedPathname(): `${"" | `/${string}`}${ReturnType<AppTypes['Pathname']>}`;
		Asset(): string & {};
	}
}