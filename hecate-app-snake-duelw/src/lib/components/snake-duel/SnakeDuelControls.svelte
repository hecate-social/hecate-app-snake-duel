<script lang="ts">
	import type { GameStatus } from '../../game/types.js';
	import { COLORS } from '../../game/constants.js';

	interface Props {
		p1AssholeFactor: number;
		p2AssholeFactor: number;
		tickMs: number;
		status: GameStatus;
	}

	let {
		p1AssholeFactor = $bindable(),
		p2AssholeFactor = $bindable(),
		tickMs = $bindable(),
		status
	}: Props = $props();

	const disabled = $derived(status === 'running' || status === 'countdown');

	function personalityLabel(af: number): string {
		if (af < 20) return 'Gentleman';
		if (af < 40) return 'Chill';
		if (af < 60) return 'Competitive';
		if (af < 80) return 'Aggressive';
		return 'Total Jerk';
	}
</script>

<div
	class="flex items-center justify-center gap-6 px-4 py-3 bg-surface-800 border-t border-surface-600 shrink-0 flex-wrap"
>
	<label class="flex items-center gap-2 text-[11px] text-surface-300">
		<span class="w-14">Speed</span>
		<input
			type="range"
			min="30"
			max="200"
			step="10"
			bind:value={tickMs}
			{disabled}
			class="w-24 accent-surface-400"
		/>
		<span class="text-surface-500 w-12 text-right tabular-nums">{tickMs}ms</span>
	</label>

	<label class="flex items-center gap-2 text-[11px] text-surface-300">
		<span class="w-14" style:color={COLORS.player1Head}>Blue AF</span>
		<input
			type="range"
			min="0"
			max="100"
			step="1"
			bind:value={p1AssholeFactor}
			{disabled}
			class="w-24 accent-blue-500"
		/>
		<span class="text-surface-500 w-20 text-right">{p1AssholeFactor} {personalityLabel(p1AssholeFactor)}</span>
	</label>

	<label class="flex items-center gap-2 text-[11px] text-surface-300">
		<span class="w-14" style:color={COLORS.player2Head}>Red AF</span>
		<input
			type="range"
			min="0"
			max="100"
			step="1"
			bind:value={p2AssholeFactor}
			{disabled}
			class="w-24 accent-red-500"
		/>
		<span class="text-surface-500 w-20 text-right">{p2AssholeFactor} {personalityLabel(p2AssholeFactor)}</span>
	</label>
</div>
