<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { GRID_WIDTH, GRID_HEIGHT } from '../../game/constants.js';
	import { createGame } from '../../game/engine.js';
	import { startMatch as daemonStartMatch, connectMatchStream } from '../../game/client.js';
	import { drawFrame, drawCountdown, drawGameOver, drawIdle } from '../../game/renderer.js';
	import type { GameState } from '../../game/types.js';

	interface Props {
		p1AssholeFactor: number;
		p2AssholeFactor: number;
		tickMs: number;
		gameState: GameState;
	}

	let { p1AssholeFactor, p2AssholeFactor, tickMs, gameState = $bindable() }: Props = $props();

	let wrapper: HTMLDivElement;
	let canvas: HTMLCanvasElement;
	let ctx: CanvasRenderingContext2D | null = null;
	let resizeObserver: ResizeObserver | null = null;
	let disconnectStream: (() => void) | null = null;

	export function startMatch(): void {
		cleanup();

		// Start match on the daemon — it owns the game engine now
		daemonStartMatch(p1AssholeFactor, p2AssholeFactor, tickMs)
			.then(async (matchId) => {
				disconnectStream = await connectMatchStream(
					matchId,
					(state) => {
						gameState = state;
						render();
					},
					() => {
						// Stream finished normally
						render();
					},
					(err) => {
						console.warn('[snake-duel] stream error:', err);
					}
				);
			})
			.catch((err) => {
				console.error('[snake-duel] Failed to start match:', err);
			});
	}

	function cleanup(): void {
		if (disconnectStream) {
			disconnectStream();
			disconnectStream = null;
		}
	}

	function sizeCanvas(): void {
		if (!wrapper || !canvas) return;
		const rect = wrapper.getBoundingClientRect();
		const aspect = GRID_WIDTH / GRID_HEIGHT;
		const maxW = 720;
		const maxH = 576;
		let w = Math.min(rect.width, maxW);
		let h = w / aspect;
		if (h > Math.min(rect.height, maxH)) {
			h = Math.min(rect.height, maxH);
			w = h * aspect;
		}
		canvas.width = Math.floor(w);
		canvas.height = Math.floor(h);
		render();
	}

	function getEndReason(): string | undefined {
		if (gameState.status !== 'finished') return undefined;
		const s1Collision = gameState.snake1.events.find((e) => e.type === 'collision');
		const s2Collision = gameState.snake2.events.find((e) => e.type === 'collision');
		if (gameState.winner === 'player1' && s2Collision)
			return `Red: ${s2Collision.value}`;
		if (gameState.winner === 'player2' && s1Collision)
			return `Blue: ${s1Collision.value}`;
		if (s1Collision) return s1Collision.value;
		return s2Collision?.value;
	}

	function render(): void {
		if (!ctx) return;
		drawFrame(ctx, gameState);
		if (gameState.status === 'idle') {
			drawIdle(ctx);
		} else if (gameState.status === 'countdown') {
			drawCountdown(ctx, gameState.countdown);
		} else if (gameState.status === 'finished') {
			drawGameOver(ctx, gameState.winner, getEndReason());
		}
	}

	onMount(() => {
		ctx = canvas.getContext('2d');
		sizeCanvas();
		resizeObserver = new ResizeObserver(() => sizeCanvas());
		resizeObserver.observe(wrapper);
	});

	onDestroy(() => {
		cleanup();
		resizeObserver?.disconnect();
	});
</script>

<div bind:this={wrapper} class="flex-1 min-h-0 min-w-0 flex items-center justify-center">
	<canvas
		bind:this={canvas}
		class="rounded-lg border border-surface-600/30"
		style="image-rendering: pixelated;"
	></canvas>
</div>
