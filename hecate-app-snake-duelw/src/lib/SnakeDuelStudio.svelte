<svelte:options customElement={{ tag: "snake-duel-studio", shadow: "none" }} />

<script lang="ts">
	import { onMount } from 'svelte';
	import { setApi } from './game/client.js';
	import SnakeDuelView from './components/SnakeDuelView.svelte';

	interface PluginApi {
		get: <T>(path: string) => Promise<T>;
		post: <T>(path: string, body: unknown) => Promise<T>;
		del: <T>(path: string) => Promise<T>;
	}

	let { api }: { api: PluginApi } = $props();

	onMount(() => {
		setApi(api);
	});
</script>

<div class="flex flex-col h-full">
	<SnakeDuelView />
</div>
