<script lang="ts">
	import { onMount } from 'svelte';
	import { COLORS } from '../../game/constants.js';
	import {
		fetchHistory,
		fetchLeaderboard,
		type MatchResult,
		type LeaderboardData
	} from '../../game/client.js';

	interface Props {
		p1AssholeFactor: number;
		p2AssholeFactor: number;
		tickMs: number;
		onStartMatch: () => void;
	}

	let {
		p1AssholeFactor = $bindable(),
		p2AssholeFactor = $bindable(),
		tickMs = $bindable(),
		onStartMatch
	}: Props = $props();

	let history = $state<MatchResult[]>([]);
	let leaderboard = $state<LeaderboardData | null>(null);
	let loading = $state(true);

	onMount(async () => {
		await loadData();
	});

	async function loadData(): Promise<void> {
		loading = true;
		try {
			const [h, l] = await Promise.all([fetchHistory(10), fetchLeaderboard()]);
			history = h;
			leaderboard = l;
		} catch (e) {
			console.error('[snake-duel] Failed to load lobby data:', e);
		}
		loading = false;
	}

	function personalityLabel(af: number): string {
		if (af < 20) return 'Gentleman';
		if (af < 40) return 'Chill';
		if (af < 60) return 'Competitive';
		if (af < 80) return 'Aggressive';
		return 'Total Jerk';
	}

	function winnerLabel(w: string): string {
		if (w === 'player1') return 'Blue';
		if (w === 'player2') return 'Red';
		return 'Draw';
	}

	function winnerColor(w: string): string {
		if (w === 'player1') return COLORS.player1Head;
		if (w === 'player2') return COLORS.player2Head;
		return COLORS.food;
	}

	function formatDuration(startedAt: number, endedAt: number): string {
		const ms = endedAt - startedAt;
		if (ms < 1000) return `${ms}ms`;
		const secs = Math.round(ms / 100) / 10;
		return `${secs}s`;
	}

	function formatTimeAgo(ts: number): string {
		const diff = Date.now() - ts;
		if (diff < 60_000) return 'just now';
		if (diff < 3_600_000) return `${Math.floor(diff / 60_000)}m ago`;
		if (diff < 86_400_000) return `${Math.floor(diff / 3_600_000)}h ago`;
		return `${Math.floor(diff / 86_400_000)}d ago`;
	}

	const p1Wins = $derived(leaderboard?.stats.find((s) => s.winner === 'player1')?.wins ?? 0);
	const p2Wins = $derived(leaderboard?.stats.find((s) => s.winner === 'player2')?.wins ?? 0);
	const totalMatches = $derived(leaderboard?.total_matches ?? 0);
	const draws = $derived(leaderboard?.draws ?? 0);
</script>

<div class="flex flex-col h-full">
	<!-- Header -->
	<div
		class="flex items-center justify-center px-4 py-2.5 bg-surface-800 border-b border-surface-600 shrink-0"
	>
		<h2
			class="text-sm font-bold tracking-wide"
			style="background: linear-gradient(135deg, #0f3460, #e94560); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
		>
			Snake Duel
		</h2>
	</div>

	<!-- Body -->
	<div class="flex-1 overflow-y-auto">
		<div class="max-w-xl mx-auto px-4 py-6 flex flex-col gap-5">
			<!-- Start Match Card -->
			<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-5">
				<div class="flex items-center justify-between mb-4">
					<div>
						<h3 class="text-sm font-semibold text-surface-100">New Match</h3>
						<p class="text-[10px] text-surface-400 mt-0.5">
							Two AI snakes battle it out — configure and watch
						</p>
					</div>
					<button
						onclick={onStartMatch}
						class="px-4 py-2 rounded-lg text-xs font-semibold transition-all duration-200
							bg-gradient-to-r from-blue-600 to-red-600
							hover:from-blue-500 hover:to-red-500
							text-white shadow-lg shadow-blue-900/30
							hover:shadow-blue-800/40"
					>
						Start Match
					</button>
				</div>

				<!-- Settings -->
				<div class="flex flex-col gap-2.5">
					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-14">Speed</span>
						<input
							type="range"
							min="30"
							max="200"
							step="10"
							bind:value={tickMs}
							class="flex-1 accent-surface-400"
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
							class="flex-1 accent-blue-500"
						/>
						<span class="text-surface-500 w-28 text-right">
							{p1AssholeFactor}
							<span class="text-surface-600">{personalityLabel(p1AssholeFactor)}</span>
						</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-14" style:color={COLORS.player2Head}>Red AF</span>
						<input
							type="range"
							min="0"
							max="100"
							step="1"
							bind:value={p2AssholeFactor}
							class="flex-1 accent-red-500"
						/>
						<span class="text-surface-500 w-28 text-right">
							{p2AssholeFactor}
							<span class="text-surface-600">{personalityLabel(p2AssholeFactor)}</span>
						</span>
					</label>
				</div>
			</div>

			<!-- Stats Row -->
			{#if leaderboard && totalMatches > 0}
				<div class="grid grid-cols-4 gap-2">
					<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
						<div class="text-lg font-bold text-surface-100 tabular-nums">{totalMatches}</div>
						<div class="text-[9px] text-surface-500 uppercase tracking-wider">Matches</div>
					</div>
					<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
						<div class="text-lg font-bold tabular-nums" style:color={COLORS.player1Head}>
							{p1Wins}
						</div>
						<div class="text-[9px] uppercase tracking-wider" style:color={COLORS.player1}>
							Blue Wins
						</div>
					</div>
					<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
						<div class="text-lg font-bold tabular-nums" style:color={COLORS.player2Head}>
							{p2Wins}
						</div>
						<div class="text-[9px] uppercase tracking-wider" style:color={COLORS.player2}>
							Red Wins
						</div>
					</div>
					<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
						<div class="text-lg font-bold text-surface-100 tabular-nums">{draws}</div>
						<div class="text-[9px] text-surface-500 uppercase tracking-wider">Draws</div>
					</div>
				</div>
			{/if}

			<!-- Match History -->
			<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-4">
				<h3 class="text-xs font-semibold text-surface-300 mb-3 uppercase tracking-wider">
					Recent Matches
				</h3>

				{#if loading}
					<p class="text-[11px] text-surface-500 italic text-center py-4">Loading...</p>
				{:else if history.length === 0}
					<p class="text-[11px] text-surface-500 italic text-center py-4">
						No matches yet — start one!
					</p>
				{:else}
					<div class="flex flex-col gap-1.5">
						{#each history as match}
							<div
								class="flex items-center gap-3 px-3 py-2 rounded-lg bg-surface-900/50 text-[11px]"
							>
								<!-- Winner badge -->
								<span
									class="w-10 font-semibold text-center"
									style:color={winnerColor(match.winner)}
								>
									{winnerLabel(match.winner)}
								</span>

								<!-- Scores -->
								<span class="tabular-nums" style:color={COLORS.player1Head}
									>{match.score1}</span
								>
								<span class="text-surface-600">-</span>
								<span class="tabular-nums" style:color={COLORS.player2Head}
									>{match.score2}</span
								>

								<!-- AF configs -->
								<span class="text-surface-500 tabular-nums">
									AF {match.af1}/{match.af2}
								</span>

								<!-- Duration -->
								<span class="text-surface-600 tabular-nums">
									{formatDuration(match.started_at, match.ended_at)}
								</span>

								<!-- Time ago -->
								<span class="text-surface-600 ml-auto">
									{formatTimeAgo(match.ended_at)}
								</span>
							</div>
						{/each}
					</div>
				{/if}
			</div>
		</div>
	</div>
</div>
