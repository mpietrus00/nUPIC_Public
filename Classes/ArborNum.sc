// ============================================================
// ArborNum — Arborescent Numbers Shape Generators for nUPIC
// For Sonig release — Marcin Pietruszewski
//
// Conceptual sources:
//   Porphyry of Tyre — Arbor Porphyriana (binary division tree)
//   Nicolas Bourbaki — graph-theoretic arborescence
//   Ramon Llull — Ars Combinatoria (combinatorial wheels)
//   Galton-Watson — stochastic branching processes
//   Deleuze & Guattari — rhizome (anti-arborescent)
//   Bourbaki — three mother structures (algebraic, topological, order)
//
// Format compatible with MycAlpha:
//   Each generator returns an array of arcs
//   Each arc: [[t, f], [t, f], ...]
//   Frequencies in octave units (0=C1, 1=C2, ... 5=C6)
//   Use *octToFreq for Hz conversion
//
// Usage:
//   a = ArborNum.porphyry(dur: 10, depth: 4);
//   a.size  // -> 16 arcs (2^4 leaf paths)
//
//   b = ArborNum.galtonWatson(dur: 8, mean: 1.2, seed: 42);
//   c = ArborNum.wheel(dur: 6, n: 7);
//   d = ArborNum.rhizome(dur: 12, nPoints: 30);
//
//   // Combined with MycAlpha shapes:
//   arcs = ArborNum.porphyry ++ MycAlpha.cloud(0, 10, 2.5, 1.0);
// ============================================================

ArborNum {

	// ── Pitch mapping (shared with MycAlpha) ──
	*octToFreq { |oct|
		^32.703 * (2 ** oct)
	}

	*freqToOct { |freq|
		^(freq / 32.703).log2
	}

	// ── Turbulent noise (shared idiom) ──
	*noise { |x, seed = 0, scale = 1|
		var n;
		n = sin(x * 0.02 + seed) * 0.28
		+ sin(x * 0.05 + (seed * 1.7)) * 0.14
		+ sin(x * 0.11 + (seed * 2.3)) * 0.07
		+ sin(x * 0.23 + (seed * 3.1)) * 0.035
		+ sin(x * 0.47 + (seed * 4.7)) * 0.018;
		^(n * scale)
	}


	// ============================================================
	// I. PORPHYRIAN GENERATORS
	// ============================================================

	// ── Arbor Porphyriana ──
	// Binary division tree: genus → species through dichotomy
	// Produces a spreading fan of diverging arcs
	// Each arc traces one root-to-leaf path through the tree
	//
	// depth: number of binary divisions (n arcs = 2^depth)
	// freqCenter: center frequency in octaves (default 2.5 = ~185Hz)
	// freqSpan: total frequency span in octaves
	// wobble: turbulent deviation amount
	*porphyry { |dur = 10, depth = 4, freqCenter = 2.5, freqSpan = 4.0,
		nPoints = 40, wobble = 0.02, seed|
		var paths = List[];
		var recurse, arcs;

		seed = seed ?? { 1000.rand };

		recurse = { |level, center, halfSpan, pathSoFar|
			if(level >= depth) {
				paths.add(pathSoFar ++ [center]);
			} {
				var upper = center + (halfSpan * 0.5);
				var lower = center - (halfSpan * 0.5);
				recurse.(level + 1, upper, halfSpan * 0.5,
					pathSoFar ++ [center]);
				recurse.(level + 1, lower, halfSpan * 0.5,
					pathSoFar ++ [center]);
			};
		};

		recurse.(0, freqCenter, freqSpan * 0.5, []);

		// Convert paths to arcs with interpolation and wobble
		arcs = paths.collect { |path, pathIdx|
			var nLevels = path.size;
			var pts = Array.new(nPoints);
			nPoints.do { |i|
				var frac = i / (nPoints - 1).max(1);
				var t = frac * dur;
				// Interpolate through tree levels
				var levelPos = frac * (nLevels - 1);
				var levelIdx = levelPos.floor.asInteger.clip(0, nLevels - 2);
				var levelFrac = levelPos - levelIdx;
				var f = path[levelIdx].blend(path[levelIdx + 1], levelFrac);
				// Add wobble
				f = f + this.noise(t * 50 + (pathIdx * 7.3), seed + pathIdx, wobble);
				pts = pts.add([t, f.clip(0, 5)]);
			};
			pts
		};

		^arcs.asArray
	}

	// ── Scala Praedicamentalis ──
	// The ladder of being: stepwise ascent through categories
	// Each step = a plateau at a frequency level
	// steps: array of frequency levels in octaves, or number of steps
	// direction: \up, \down, or \updown
	*scala { |dur = 8, steps = 6, freqLow = 0.5, freqHigh = 4.5,
		direction = \up, dwellRatio = 0.6, wobble = 0.015, seed|
		var levels, arcs, arc, nSteps;

		seed = seed ?? { 1000.rand };

		// Generate step frequencies
		nSteps = steps.isKindOf(Array).if({ steps.size }, { steps });
		levels = steps.isKindOf(Array).if({ steps }, {
			nSteps.collect { |i|
				freqLow + (i * ((freqHigh - freqLow) / (nSteps - 1).max(1)))
			}
		});

		case
		{ direction == \down } { levels = levels.reverse }
		{ direction == \updown } { levels = levels ++ levels.reverse.drop(1) };

		// Build single arc with step-and-transition pattern
		arc = List[];
		levels.size.do { |i|
			var tStart = i * (dur / levels.size);
			var tEnd = (i + 1) * (dur / levels.size);
			var dwellEnd = tStart + ((tEnd - tStart) * dwellRatio);
			var freq = levels[i];
			var nextFreq = levels[(i + 1).min(levels.size - 1)];

			// Dwell on this level
			8.do { |j|
				var t = tStart + (j * (dwellEnd - tStart) / 7);
				var w = this.noise(t * 40 + (i * 3.7), seed + i, wobble);
				arc.add([t, (freq + w).clip(0, 5)]);
			};
			// Transition to next level
			if(i < (levels.size - 1)) {
				4.do { |j|
					var frac = (j + 1) / 5;
					var t = dwellEnd + (frac * (tEnd - dwellEnd));
					var f = freq.blend(nextFreq, frac);
					var w = this.noise(t * 40, seed + i + 100, wobble * 2);
					arc.add([t, (f + w).clip(0, 5)]);
				};
			};
		};

		^[arc.asArray]
	}


	// ============================================================
	// II. BOURBAKI GENERATORS
	// ============================================================

	// ── Arborescence ──
	// General directed rooted tree (Bourbaki graph theory)
	// branching: integer or array-per-level [3, 2, 4, ...]
	// More regular/formal than Xenakis's stochastic version
	*arborescence { |dur = 10, depth = 3, branching = 3, freqCenter = 2.5,
		freqSpan = 4.0, nPoints = 40, wobble = 0.02, seed|
		var paths = List[];
		var branchPerLevel, recurse, arcs;

		seed = seed ?? { 1000.rand };

		// Normalize branching to per-level array
		branchPerLevel = branching.isKindOf(Array).if(
			{ branching },
			{ depth.collect { branching } }
		);

		recurse = { |level, center, halfSpan, pathSoFar|
			if(level >= depth) {
				paths.add(pathSoFar ++ [center]);
			} {
				var b = branchPerLevel[level];
				var step = halfSpan * 2 / b.max(1);
				var base = center - halfSpan + (step * 0.5);
				b.do { |k|
					var childFreq = base + (k * step);
					var childSpan = step * 0.45;
					recurse.(level + 1, childFreq, childSpan,
						pathSoFar ++ [center]);
				};
			};
		};

		recurse.(0, freqCenter, freqSpan * 0.5, []);

		arcs = paths.collect { |path, pathIdx|
			var nLevels = path.size;
			var pts = Array.new(nPoints);
			nPoints.do { |i|
				var frac = i / (nPoints - 1).max(1);
				var t = frac * dur;
				var levelPos = frac * (nLevels - 1);
				var levelIdx = levelPos.floor.asInteger.clip(0, nLevels - 2);
				var levelFrac = levelPos - levelIdx;
				var f = path[levelIdx].blend(path[levelIdx + 1], levelFrac);
				f = f + this.noise(t * 50 + (pathIdx * 5.1), seed + pathIdx, wobble);
				pts = pts.add([t, f.clip(0, 5)]);
			};
			pts
		};

		^arcs.asArray
	}

	// ── Galton-Watson Process ──
	// Stochastic branching: bridge between Bourbaki and Xenakis
	// mean < 1: subcritical (dies out)
	// mean = 1: critical (edge of extinction)
	// mean > 1: supercritical (explosive growth)
	// maxPop: safety limit on total branches
	*galtonWatson { |dur = 10, depth = 5, mean = 1.0, freqCenter = 2.5,
		freqSpan = 4.0, nPoints = 40, wobble = 0.03, maxPop = 64, seed|
		var paths = List[];
		var rng, recurse, arcs;

		seed = seed ?? { 1000.rand };
		thisThread.randSeed = seed;

		recurse = { |level, center, halfSpan, pathSoFar|
			if((level >= depth) or: { paths.size >= maxPop }) {
				paths.add(pathSoFar ++ [center]);
			} {
				// Poisson-distributed offspring
				var nOffspring = 0;
				var threshold = exp(mean.neg);
				var p = 1.0;
				block { |break|
					loop {
						nOffspring = nOffspring + 1;
						p = p * 1.0.rand;
						if(p < threshold) { break.() };
					};
				};
				nOffspring = (nOffspring - 1).max(0);

				if(nOffspring == 0) {
					// Extinction at this node
					paths.add(pathSoFar ++ [center]);
				} {
					nOffspring = nOffspring.min(5); // cap branching
					if(nOffspring == 1) {
						// Single continuation — slight drift
						var drift = halfSpan * 0.3.rand2;
						recurse.(level + 1, center + drift,
							halfSpan * 0.8, pathSoFar ++ [center]);
					} {
						var step = halfSpan * 2 / nOffspring.max(1);
						var base = center - halfSpan + (step * 0.5);
						nOffspring.do { |k|
							var childFreq = base + (k * step)
							+ (step * 0.15.rand2);
							recurse.(level + 1, childFreq,
								step * 0.4, pathSoFar ++ [center]);
						};
					};
				};
			};
		};

		recurse.(0, freqCenter, freqSpan * 0.5, []);

		arcs = paths.collect { |path, pathIdx|
			var nLevels = path.size;
			var pts = Array.new(nPoints);
			nPoints.do { |i|
				var frac = i / (nPoints - 1).max(1);
				var t = frac * dur;
				var levelPos = frac * (nLevels - 1);
				var levelIdx = levelPos.floor.asInteger.clip(0, nLevels - 2);
				var levelFrac = levelPos - levelIdx;
				var f = path[levelIdx].blend(path[levelIdx + 1], levelFrac);
				f = f + this.noise(t * 50 + (pathIdx * 3.9), seed + pathIdx, wobble);
				pts = pts.add([t, f.clip(0, 5)]);
			};
			pts
		};

		^arcs.asArray
	}

	// ── Mother Structures ──
	// Bourbaki's three fundamental structure types

	// Algebraic: group operations on a pitch motif
	// motif: array of freq values in octaves
	// operations: array of symbols (\T, \I, \R, \RI)
	// T = transposition, I = inversion, R = retrograde, RI = retrograde-inversion
	*motherAlgebraic { |dur = 8, motif, interval = 0.5,
		operations, wobble = 0.01, seed|
		var arcs = List[];
		var ops, nOps, tPer;

		seed = seed ?? { 1000.rand };

		motif = motif ?? { [2.0, 2.3, 1.8, 2.7, 2.1] };

		ops = operations ?? {
			[\original, \T, \I, \R, \RI, \T, \T]
		};
		nOps = ops.size;
		tPer = dur / nOps;

		ops.do { |op, oi|
			var transformed, arc;
			var tOff = oi * tPer;
			var pivot = motif.mean;

			transformed = case
			{ op == \original } { motif }
			{ op == \T }  { motif + (interval * (oi + 1)) }
			{ op == \I }  { motif.collect { |f| pivot + (pivot - f) } }
			{ op == \R }  { motif.reverse }
			{ op == \RI } {
				motif.reverse.collect { |f| pivot + (pivot - f) }
			};

			arc = transformed.collect { |f, i|
				var t = tOff + (i * (tPer / transformed.size.max(1)));
				var w = this.noise(t * 40 + (oi * 5), seed + oi, wobble);
				[t, (f + w).clip(0, 5)]
			};

			arcs.add(arc);
		};

		^arcs.asArray
	}

	// Topological: continuous deformation of a contour
	// Preserves neighborhoods — stretches without tearing
	// source: array of [t, f] points or nil for generated contour
	// warp: deformation intensity (0 = identity, 1 = strong)
	// nVariations: number of deformed copies
	*motherTopological { |dur = 8, source, nVariations = 5,
		warp = 0.5, nPoints = 50, wobble = 0.01, seed|
		var arcs = List[];
		var baseCurve;

		seed = seed ?? { 1000.rand };

		// Generate base curve if none provided
		baseCurve = source ?? {
			nPoints.collect { |i|
				var t = i * (dur / (nPoints - 1).max(1));
				var f = 2.5 + (1.5 * sin(t * 0.5 + seed))
				+ this.noise(t * 30, seed, 0.3);
				[t, f.clip(0, 5)]
			}
		};

		// Original
		arcs.add(baseCurve);

		// Deformed variations
		nVariations.do { |vi|
			var deformed;
			var phase = seed + (vi * 17.3);
			var intensity = warp * ((vi + 1) / nVariations);

			deformed = baseCurve.collect { |pt, i|
				var t = pt[0];
				var f = pt[1];
				// Time warp: stretch/compress locally
				var tWarp = t + (intensity * 0.4 * dur
					* sin(t * 2.0 / dur * pi + phase));
				// Freq warp: bend contour
				var fWarp = f + (intensity * 1.5
					* sin(f * pi * 0.4 + phase + (vi * 2.1)));
				fWarp = fWarp + this.noise(
					t * 30 + (vi * 11), seed + vi + 200, wobble);
				[tWarp.clip(0, dur), fWarp.clip(0, 5)]
			};

			// Sort by time (warp may disorder)
			deformed = deformed.sort { |a, b| a[0] < b[0] };
			arcs.add(deformed);
		};

		^arcs.asArray
	}

	// Order: ascending/descending chains through a pitch lattice
	// Partial order structure — multiple maximal chains
	// latticeSize: number of distinct pitch levels
	// nChains: number of paths through the lattice
	*motherOrder { |dur = 8, latticeSize = 8, nChains = 5,
		freqLow = 0.5, freqHigh = 4.5, wobble = 0.01, seed|
		var arcs = List[];
		var levels, nPoints;

		seed = seed ?? { 1000.rand };
		thisThread.randSeed = seed;

		// Create lattice levels
		levels = latticeSize.collect { |i|
			freqLow + (i * ((freqHigh - freqLow) / (latticeSize - 1).max(1)))
		};

		nChains.do { |ci|
			var arc = List[];
			var nSteps = (latticeSize * 0.6).ceil.asInteger.rrand(latticeSize);
			var chain = List[];
			var current = levels.size.rand;

			// Generate a path through the lattice
			// Mostly ascending, with occasional retreats
			nSteps.do { |si|
				chain.add(levels[current]);
				if(0.75.coin) {
					// Ascend
					current = (current + 1.rrand(2)).min(levels.size - 1);
				} {
					// Descend slightly
					current = (current - 1).max(0);
				};
			};

			// Convert to timed arc
			chain.do { |f, i|
				var t = i * (dur / (chain.size - 1).max(1));
				var w = this.noise(t * 40 + (ci * 7), seed + ci, wobble);
				arc.add([t, (f + w).clip(0, 5)]);
			};

			arcs.add(arc.asArray);
		};

		^arcs.asArray
	}


	// ============================================================
	// III. LLULLIAN GENERATORS
	// ============================================================

	// ── Combinatorial Wheel ──
	// N frequencies on a circle, rotated against each other
	// Like Llull's concentric discs of divine attributes
	// Produces pairwise combinations over full rotation
	// n: number of elements on wheel
	// nRotations: how many full rotations of inner disc
	*wheel { |dur = 8, n = 7, freqLow = 0.8, freqHigh = 4.2,
		nRotations = 1, wobble = 0.015, seed|
		var arcs = List[];
		var elements, nSteps, tStep;

		seed = seed ?? { 1000.rand };

		// N frequencies equally spaced
		elements = n.collect { |i|
			freqLow + (i * ((freqHigh - freqLow) / (n - 1).max(1)))
		};

		nSteps = n * nRotations;
		tStep = dur / nSteps;

		// Outer disc: fixed sequence
		// Inner disc: rotates by 1 position per step
		// Two arcs: one for outer reading, one for inner reading
		2.do { |discIdx|
			var arc = List[];
			nSteps.do { |si|
				var t = si * tStep;
				var idx;
				if(discIdx == 0) {
					idx = si % n;  // outer: cycles through
				} {
					idx = (si * 2) % n;  // inner: rotates at different rate
				};
				4.do { |ptIdx|
					var tt = t + (ptIdx * tStep / 4);
					var w = this.noise(
						tt * 40 + (discIdx * 19), seed + discIdx, wobble);
					arc.add([tt, (elements[idx] + w).clip(0, 5)]);
				};
			};
			arcs.add(arc.asArray);
		};

		// Third arc: the combination (outer + inner mapped to third freq)
		{
			var arc = List[];
			nSteps.do { |si|
				var t = si * tStep;
				var outerIdx = si % n;
				var innerIdx = (si * 2) % n;
				var combinedFreq = (elements[outerIdx] + elements[innerIdx]) * 0.5;
				4.do { |ptIdx|
					var tt = t + (ptIdx * tStep / 4);
					var w = this.noise(tt * 40 + 37, seed + 99, wobble);
					arc.add([tt, (combinedFreq + w).clip(0, 5)]);
				};
			};
			arcs.add(arc.asArray);
		}.();

		^arcs.asArray
	}

	// ── Ars Combinatoria ──
	// Systematic enumeration of all combinations from a set
	// Like Llull's method: take N elements, produce all k-combinations
	// Each combination becomes a chord-arc (vertical cluster)
	// elements: array of frequencies, or number of elements
	// k: combination size
	*combinatoria { |dur = 10, elements = 5, k = 3, freqLow = 0.5,
		freqHigh = 4.5, chordDur = 0.3, wobble = 0.01, seed|
		var arcs = List[];
		var freqs, combos, nCombos, tStep;

		seed = seed ?? { 1000.rand };

		freqs = elements.isKindOf(Array).if({ elements }, {
			elements.collect { |i|
				freqLow + (i * ((freqHigh - freqLow) / (elements - 1).max(1)))
			}
		});

		// Generate all k-combinations
		combos = freqs.combinations(k);
		nCombos = combos.size;
		tStep = dur / nCombos;

		combos.do { |combo, ci|
			combo.do { |f, fi|
				var arc = List[];
				var tStart = ci * tStep;
				var tEnd = tStart + chordDur.min(tStep * 0.9);
				8.do { |i|
					var t = tStart + (i * (tEnd - tStart) / 7);
					var w = this.noise(
						t * 40 + (fi * 11) + (ci * 3),
						seed + ci + fi, wobble);
					arc.add([t, (f + w).clip(0, 5)]);
				};
				arcs.add(arc.asArray);
			};
		};

		^arcs.asArray
	}


	// ============================================================
	// IV. DELEUZIAN GENERATORS
	// ============================================================

	// ── Rhizome ──
	// Anti-arborescent: no root, no hierarchy
	// N points in time-freq space connected by random graph
	// Traversed by random walk — any point to any point
	// nPoints: number of nodes in the graph
	// density: edge probability (0-1)
	// nWalks: number of random walks through the graph
	*rhizome { |dur = 12, nPoints = 20, density = 0.3, nWalks = 4,
		freqLow = 0.3, freqHigh = 4.7, wobble = 0.02, seed|
		var arcs = List[];
		var nodes, edges, nEdges;

		seed = seed ?? { 1000.rand };
		thisThread.randSeed = seed;

		// Generate random nodes in [t, f] space
		nodes = nPoints.collect {
			[dur.rand, freqLow + ((freqHigh - freqLow).rand)]
		};

		// Generate random edges (undirected graph)
		edges = Array.fill(nPoints, { Array.fill(nPoints, { false }) });
		nPoints.do { |i|
			(i+1..nPoints-1).do { |j|
				if(density.coin) {
					edges[i][j] = true;
					edges[j][i] = true;
				};
			};
		};

		// Ensure connectivity: add edges for isolated nodes
		nPoints.do { |i|
			var hasEdge = false;
			nPoints.do { |j|
				if((j != i) and: { edges[i][j] }) { hasEdge = true };
			};
			if(hasEdge.not) {
				// Connect to nearest node
				var nearest = -1, nearestDist = inf;
				nPoints.do { |j|
					if(j != i) {
						var dist = ((nodes[i][0] - nodes[j][0]).squared
							+ (nodes[i][1] - nodes[j][1]).squared).sqrt;
						if(dist < nearestDist) {
							nearest = j;
							nearestDist = dist;
						};
					};
				};
				if(nearest >= 0) {
					edges[i][nearest] = true;
					edges[nearest][i] = true;
				};
			};
		};

		// Random walks through the graph
		nWalks.do { |wi|
			var arc = List[];
			var current = nPoints.rand;
			var stepsPerWalk = (nPoints * 1.5).ceil.asInteger;
			var visited = List[];

			stepsPerWalk.do { |si|
				var neighbors = List[];
				var next;
				var tStart, tEnd, fStart, fEnd;
				nPoints.do { |j|
					if(edges[current][j]) { neighbors.add(j) };
				};
				if(neighbors.size > 0) {
					next = neighbors.choose;
					// Interpolate between current and next node
					tStart = nodes[current][0];
					tEnd = nodes[next][0];
					fStart = nodes[current][1];
					fEnd = nodes[next][1];
					6.do { |ptIdx|
						var frac = ptIdx / 5;
						var t = tStart.blend(tEnd, frac);
						var f = fStart.blend(fEnd, frac);
						var w = this.noise(
							t * 30 + (wi * 13) + (si * 2.7),
							seed + wi, wobble);
						arc.add([t, (f + w).clip(0, 5)]);
					};
					current = next;
				};
			};

			// Sort by time for rendering
			arc = arc.sort { |a, b| a[0] < b[0] };
			arcs.add(arc.asArray);
		};

		^arcs.asArray
	}

	// ── Plateau ──
	// Deleuzian: intensive, non-hierarchical field
	// Dense horizontal band — no direction, no climax
	// A zone of intensity rather than a trajectory
	*plateau { |dur = 10, freqCenter = 2.5, freqBand = 0.8,
		nLayers = 8, nPoints = 60, seed|
		var arcs = List[];

		seed = seed ?? { 1000.rand };

		nLayers.do { |li|
			var arc = List[];
			var offset = (li - (nLayers * 0.5)) * (freqBand / nLayers);
			nPoints.do { |i|
				var t = i * (dur / (nPoints - 1).max(1));
				var f = freqCenter + offset
				+ this.noise(t * 60 + (li * 23.7), seed + li, 0.15)
				+ (0.05.rand2);
				arc.add([t, f.clip(0, 5)]);
			};
			arcs.add(arc.asArray);
		};

		^arcs.asArray
	}


	// ============================================================
	// V. HYBRID / STRUCTURAL GENERATORS
	// ============================================================

	// ── Subcritical Extinction ──
	// Galton-Watson tree that always dies out
	// Musical interest: the sound of a process ending
	*extinction { |dur = 6, depth = 6, freqCenter = 2.5,
		freqSpan = 3.0, seed|
		^this.galtonWatson(dur, depth, mean: 0.6,
			freqCenter: freqCenter, freqSpan: freqSpan,
			wobble: 0.04, seed: seed)
	}

	// ── Supercritical Explosion ──
	// Galton-Watson tree that proliferates wildly
	// Musical interest: explosive density increase
	*explosion { |dur = 8, depth = 4, freqCenter = 2.5,
		freqSpan = 4.0, maxPop = 48, seed|
		^this.galtonWatson(dur, depth, mean: 2.0,
			freqCenter: freqCenter, freqSpan: freqSpan,
			maxPop: maxPop, wobble: 0.02, seed: seed)
	}

	// ── Critical Process ──
	// Galton-Watson at the edge: mean offspring = 1
	// Sometimes dies, sometimes persists — each seed different
	*critical { |dur = 10, depth = 6, freqCenter = 2.5,
		freqSpan = 4.0, seed|
		^this.galtonWatson(dur, depth, mean: 1.0,
			freqCenter: freqCenter, freqSpan: freqSpan,
			wobble: 0.03, seed: seed)
	}

	// ── Arborescence with Rhizome ──
	// Tree structure that degenerates into rhizome at the leaves
	// Formal hierarchy dissolving into horizontal connections
	*dissolve { |dur = 12, treeDepth = 3, treeBranching = 3,
		rhizomeDensity = 0.4, freqCenter = 2.5, freqSpan = 4.0, seed|
		var treeArcs, rhizomeArcs, treeDur, rhizomeDur;

		seed = seed ?? { 1000.rand };
		treeDur = dur * 0.55;
		rhizomeDur = dur * 0.55;

		treeArcs = this.arborescence(treeDur, treeDepth, treeBranching,
			freqCenter, freqSpan, seed: seed);

		// Extract leaf positions from tree arcs
		{
			var leafFreqs = treeArcs.collect { |arc| arc.last[1] };
			var leafTime = treeDur * 0.9;

			// Generate rhizome from leaf positions
			rhizomeArcs = this.rhizome(
				dur: rhizomeDur,
				nPoints: leafFreqs.size.max(6),
				density: rhizomeDensity,
				nWalks: (leafFreqs.size * 0.5).ceil.asInteger.max(2),
				freqLow: leafFreqs.minItem.max(0.3),
				freqHigh: leafFreqs.maxItem.min(4.7),
				seed: seed + 500
			);

			// Offset rhizome in time
			rhizomeArcs = rhizomeArcs.collect { |arc|
				arc.collect { |pt|
					[pt[0] + (treeDur * 0.45), pt[1]]
				}
			};
		}.();

		^(treeArcs ++ rhizomeArcs)
	}


	// ============================================================
	// UTILITIES
	// ============================================================

	// Convert all arcs from octaves to Hz
	*arcsToHz { |arcs|
		^arcs.collect { |arc|
			arc.collect { |pt| [pt[0], this.octToFreq(pt[1])] }
		}
	}

	// Reverse arcs in time (retrograde)
	*retrograde { |arcs, dur|
		dur = dur ?? { arcs.flat.collect(_[0]).maxItem };
		^arcs.collect { |arc|
			arc.collect { |pt| [dur - pt[0], pt[1]] }.reverse
		}
	}

	// Invert arcs in frequency (mirror around axis)
	*invert { |arcs, axis = 2.5|
		^arcs.collect { |arc|
			arc.collect { |pt| [pt[0], (axis + (axis - pt[1])).clip(0, 5)] }
		}
	}

	// Transpose arcs by interval (in octaves)
	*transpose { |arcs, interval = 1.0|
		^arcs.collect { |arc|
			arc.collect { |pt| [pt[0], (pt[1] + interval).clip(0, 5)] }
		}
	}

	// Time-stretch arcs
	*stretch { |arcs, factor = 2.0|
		^arcs.collect { |arc|
			arc.collect { |pt| [pt[0] * factor, pt[1]] }
		}
	}

	// Concatenate multiple arc-sets with time offsets
	*concat { |...arcSetsAndDurs|
		// Takes pairs: [arcSet1, dur1, arcSet2, dur2, ...]
		var result = List[];
		var offset = 0;
		(arcSetsAndDurs.size / 2).do { |i|
			var arcs = arcSetsAndDurs[i * 2];
			var segDur = arcSetsAndDurs[(i * 2) + 1];
			arcs.do { |arc|
				result.add(arc.collect { |pt|
					[pt[0] + offset, pt[1]]
				});
			};
			offset = offset + segDur;
		};
		^result.asArray
	}

	// Thin arcs — keep only every nth point
	*thin { |arcs, n = 2|
		^arcs.collect { |arc|
			arc.select { |pt, i| (i % n) == 0 }
		}
	}

	// Densify — add interpolated points between existing ones
	*densify { |arcs, factor = 2|
		^arcs.collect { |arc|
			var result = List[];
			(arc.size - 1).do { |i|
				var p0 = arc[i], p1 = arc[i+1];
				result.add(p0);
				(factor - 1).do { |j|
					var frac = (j + 1) / factor;
					result.add([
						p0[0].blend(p1[0], frac),
						p0[1].blend(p1[1], frac)
					]);
				};
			};
			result.add(arc.last);
			result.asArray
		}
	}

	// Summary info
	*info { |arcs|
		var allPts = arcs.flat(1);
		var times = allPts.collect(_[0]);
		var freqs = allPts.collect(_[1]);
		"ArborNum: % arcs, % points total".format(
			arcs.size, allPts.size).postln;
		"  time: % – % s".format(
			times.minItem.round(0.01), times.maxItem.round(0.01)).postln;
		"  freq: % – % oct (% – % Hz)".format(
			freqs.minItem.round(0.01), freqs.maxItem.round(0.01),
			this.octToFreq(freqs.minItem).round(0.1),
			this.octToFreq(freqs.maxItem).round(0.1)).postln;
		^arcs
	}
}
