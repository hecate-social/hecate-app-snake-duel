<script lang="ts">
	import SnakeDuelLobby from './snake-duel/SnakeDuelLobby.svelte';
	import SnakeDuelHeader from './snake-duel/SnakeDuelHeader.svelte';
	import SnakeDuelCanvas from './snake-duel/SnakeDuelCanvas.svelte';
	import SnakeDuelScoreboard from './snake-duel/SnakeDuelScoreboard.svelte';
	import SnakeDuelControls from './snake-duel/SnakeDuelControls.svelte';
	import { createGame } from '../game/engine.js';
	import type { GameState } from '../game/types.js';

	let p1AssholeFactor = $state(30);
	let p2AssholeFactor = $state(70);
	let tickMs = $state(100);
	let gameState: GameState = $state(createGame(30, 70));
	let canvasRef: SnakeDuelCanvas;

	// "lobby" = dashboard with history/stats, "arena" = active match view
	let view: 'lobby' | 'arena' = $state('lobby');

	function handleStartMatch(): void {
		view = 'arena';
		// Wait for next tick so the canvas is mounted before starting
		requestAnimationFrame(() => {
			canvasRef?.startMatch();
		});
	}

	function handleBackToLobby(): void {
		view = 'lobby';
		// Reset game state so next arena entry is clean
		gameState = createGame(p1AssholeFactor, p2AssholeFactor);
	}

	function handleNewMatch(): void {
		canvasRef?.startMatch();
	}
</script>

{#if view === 'lobby'}
	<SnakeDuelLobby
		bind:p1AssholeFactor
		bind:p2AssholeFactor
		bind:tickMs
		onStartMatch={handleStartMatch}
	/>
{:else}
	<div class="flex flex-col h-full">
		<SnakeDuelHeader onBack={handleBackToLobby} onNewMatch={handleNewMatch} />
		<SnakeDuelScoreboard snake1={gameState.snake1} snake2={gameState.snake2} />

		<SnakeDuelCanvas
			bind:this={canvasRef}
			{p1AssholeFactor}
			{p2AssholeFactor}
			{tickMs}
			bind:gameState
		/>

		<SnakeDuelControls
			bind:p1AssholeFactor
			bind:p2AssholeFactor
			bind:tickMs
			status={gameState.status}
		/>
	</div>
{/if}
