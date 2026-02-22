// SSE client for streaming snake duel match state from hecate-daemon.
// Uses plugin API pattern — API calls go through the PluginApi injected
// by the host (hecate-web), and SSE streams use connectPluginSse.

import { connectPluginSse, type SseStream } from '../stream.js';
import type { GameState, GameEvent } from './types.js';

interface PluginApi {
	get: <T>(path: string) => Promise<T>;
	post: <T>(path: string, body: unknown) => Promise<T>;
}

let pluginApi: PluginApi | null = null;

export function setApi(api: PluginApi) {
	pluginApi = api;
}

interface StartMatchResponse {
	ok: boolean;
	match_id: string;
	af1: number;
	af2: number;
	tick_ms: number;
	status: string;
}

/** Start a new match on the daemon. Returns match_id. */
export async function startMatch(
	af1: number,
	af2: number,
	tickMs: number
): Promise<string> {
	const resp = await pluginApi!.post<StartMatchResponse>('/api/arcade/snake-duel/matches', {
		af1,
		af2,
		tick_ms: tickMs
	});
	return resp.match_id;
}

/** Connect to the SSE stream for a live match via plugin SSE.
 *  Returns a cleanup function that stops the stream and removes listeners.
 */
export async function connectMatchStream(
	matchId: string,
	onState: (state: GameState) => void,
	onDone?: () => void,
	onError?: (error: string) => void
): Promise<() => void> {
	const stream = connectPluginSse<Record<string, unknown>>(
		'snake-duel',
		`/api/arcade/snake-duel/matches/${matchId}/stream`,
		(data) => {
			try {
				const state = daemonToGameState(data);
				onState(state);
			} catch (e) {
				console.error('[snake-duel] Failed to parse SSE data:', e);
			}
		},
		onDone,
		onError
	);
	return () => stream.stop();
}

/** Convert daemon JSON map to client GameState type.
 * Daemon uses snake_case atoms; client uses camelCase strings.
 */
function daemonToGameState(data: Record<string, unknown>): GameState {
	return {
		snake1: daemonToSnake(data.snake1 as Record<string, unknown>),
		snake2: daemonToSnake(data.snake2 as Record<string, unknown>),
		food: data.food as [number, number],
		poisonApples: (data.poison_apples as Array<Record<string, unknown>>).map((p) => ({
			pos: p.pos as [number, number],
			owner: String(p.owner) as 'player1' | 'player2'
		})),
		walls: ((data.walls as Array<Record<string, unknown>>) ?? []).map((w) => ({
			pos: w.pos as [number, number],
			owner: String(w.owner) as 'player1' | 'player2',
			ttl: w.ttl as number
		})),
		status: String(data.status) as GameState['status'],
		winner:
			data.winner === 'none' || data.winner === null
				? null
				: (String(data.winner) as 'player1' | 'player2' | 'draw'),
		tick: data.tick as number,
		countdown: data.countdown as number
	};
}

function daemonToSnake(s: Record<string, unknown>): GameState['snake1'] {
	return {
		body: (s.body as number[][]).map(([x, y]) => [x, y] as [number, number]),
		direction: String(s.direction) as 'up' | 'down' | 'left' | 'right',
		score: s.score as number,
		assholeFactor: s.asshole_factor as number,
		events: ((s.events as Array<Record<string, unknown>>) || []).map((e) => ({
			type: mapEventType(String(e.type)),
			value: String(e.value),
			tick: e.tick as number
		}))
	};
}

function mapEventType(
	t: string
): GameEvent['type'] {
	const mapping: Record<string, string> = {
		food: 'food',
		turn: 'turn',
		collision: 'collision',
		win: 'win',
		poison_drop: 'poison-drop',
		poison_eat: 'poison-eat',
		wall_drop: 'wall-drop',
		wall_hit: 'wall-hit'
	};
	return (mapping[t] || t) as GameEvent['type'];
}

// --- Query endpoints (history, leaderboard) ---

export interface MatchResult {
	match_id: string;
	winner: string;
	af1: number;
	af2: number;
	tick_ms: number;
	score1: number;
	score2: number;
	ticks: number;
	started_at: number;
	ended_at: number;
}

export interface LeaderboardData {
	total_matches: number;
	draws: number;
	stats: Array<{
		winner: string;
		wins: number;
		avg_score: number;
	}>;
}

/** Fetch recent match history. */
export async function fetchHistory(limit = 20): Promise<MatchResult[]> {
	const resp = await pluginApi!.get<{ matches: MatchResult[] }>(
		`/api/arcade/snake-duel/history?limit=${limit}`
	);
	return resp.matches ?? [];
}

/** Fetch aggregate leaderboard stats. */
export async function fetchLeaderboard(): Promise<LeaderboardData> {
	return pluginApi!.get<LeaderboardData>('/api/arcade/snake-duel/leaderboard');
}
