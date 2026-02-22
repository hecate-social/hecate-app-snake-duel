<script lang="ts">
	interface Props {
		onBack: () => void;
		onNewMatch: () => void;
	}

	let { onBack, onNewMatch }: Props = $props();
</script>

<div class="flex items-center justify-between px-4 py-2.5 bg-surface-800 border-b border-surface-600 shrink-0">
	<button
		onclick={onBack}
		class="text-surface-400 hover:text-surface-100 transition-colors text-xs flex items-center gap-1"
	>
		&larr; Back
	</button>

	<h2
		class="text-sm font-bold tracking-wide"
		style="background: linear-gradient(135deg, #0f3460, #e94560); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
	>
		Snake Duel
	</h2>

	<button
		onclick={onNewMatch}
		class="text-xs px-3 py-1.5 rounded-lg bg-surface-700 hover:bg-surface-600 text-surface-200 transition-colors border border-surface-500/50"
	>
		New Match
	</button>
</div>
