<script lang="ts">
	import type { Snake, GameEvent } from '../../game/types.js';
	import { COLORS } from '../../game/constants.js';

	interface Props {
		snake1: Snake;
		snake2: Snake;
	}

	let { snake1, snake2 }: Props = $props();

	function personalityLabel(af: number): string {
		if (af < 20) return 'Gentleman';
		if (af < 40) return 'Chill';
		if (af < 60) return 'Competitive';
		if (af < 80) return 'Aggressive';
		return 'Total Jerk';
	}

	function eventIcon(event: GameEvent): string {
		if (event.type === 'food') return '\u{1F34E}';
		if (event.type === 'win') return '\u{1F3C6}';
		if (event.type === 'poison-drop') return '\u{1F52E}';
		if (event.type === 'poison-eat') return '\u{2620}\uFE0F';
		if (event.type === 'collision') {
			if (event.value.includes('wall')) return '\u{1F9F1}';
			if (event.value.includes('Self')) return '\u{1F4A0}';
			if (event.value.includes('Head')) return '\u{1F4A5}';
			return '\u{1F480}';
		}
		if (event.type === 'turn') {
			if (event.value.includes('up')) return '\u2B06\uFE0F';
			if (event.value.includes('down')) return '\u2B07\uFE0F';
			if (event.value.includes('left')) return '\u2B05\uFE0F';
			if (event.value.includes('right')) return '\u27A1\uFE0F';
		}
		return '\u2022';
	}

	function eventTrail(snake: Snake, count: number): string {
		return snake.events.slice(0, count).map(eventIcon).join(' ');
	}
</script>

<div class="flex items-center justify-between gap-4 px-4 py-2 shrink-0 select-none">
	<!-- Player 1 (Blue) -->
	<div class="flex items-center gap-3 flex-1 min-w-0">
		<div class="w-2.5 h-2.5 rounded-full shrink-0" style:background-color={COLORS.player1Head}></div>
		<span class="text-xs font-semibold" style:color={COLORS.player1Head}>Blue</span>
		<span class="text-2xl font-bold tabular-nums" style:color={COLORS.player1Head}>{snake1.score}</span>
		<span class="text-[10px] text-surface-500 hidden sm:inline">
			{personalityLabel(snake1.assholeFactor)}
		</span>
		<span class="text-xs hidden md:inline" title={snake1.events[0]?.value ?? ''}>
			{eventTrail(snake1, 5)}
		</span>
	</div>

	<!-- VS -->
	<span class="text-[10px] text-surface-500 font-semibold tracking-wider shrink-0">VS</span>

	<!-- Player 2 (Red) -->
	<div class="flex items-center gap-3 flex-1 min-w-0 justify-end">
		<span class="text-xs hidden md:inline" title={snake2.events[0]?.value ?? ''}>
			{eventTrail(snake2, 5)}
		</span>
		<span class="text-[10px] text-surface-500 hidden sm:inline">
			{personalityLabel(snake2.assholeFactor)}
		</span>
		<span class="text-2xl font-bold tabular-nums" style:color={COLORS.player2Head}>{snake2.score}</span>
		<span class="text-xs font-semibold" style:color={COLORS.player2Head}>Red</span>
		<div class="w-2.5 h-2.5 rounded-full shrink-0" style:background-color={COLORS.player2Head}></div>
	</div>
</div>
