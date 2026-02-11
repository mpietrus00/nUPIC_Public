/*
===============================================================================
NUPICCloud - Usage Examples
===============================================================================

The NUPICCloud class encapsulates stochastic cloud generation with:
- Multiple distribution types
- Set operations (union, intersection, difference)
- Transformations
- Special generators (arborescence, sieve, screen, markov, etc.)
===============================================================================
*/

// ============================================================================
// SETUP
// ============================================================================

(
~app = NUPICApplication(oversampling: false);
~app.start;
)

// ============================================================================
// BASIC CLOUD GENERATION
// ============================================================================

// --- Simple uniform cloud ---
(
~cloud = NUPICCloud((
	density: 40,
	timeRange: [0, 1],
	freqRange: [200, 2000],
	distribution: \uniform
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Uniform cloud: % arcs".format(~cloud.size).postln;
)

// --- Gaussian distribution ---
(
~cloud = NUPICCloud((
	density: 60,
	freqRange: [300, 1500],
	distribution: \gaussian,
	morphology: \dot
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Gaussian cloud".postln;
)

// --- Cauchy distribution (heavy tails) ---
(
~cloud = NUPICCloud((
	density: 50,
	freqRange: [150, 3000],
	distribution: \cauchy,
	morphology: \ascending
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Cauchy cloud with ascending glissandi".postln;
)

// --- Exponential distribution ---
(
~cloud = NUPICCloud((
	density: 70,
	freqRange: [100, 4000],
	distribution: \exponential,
	morphology: \descending
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Exponential cloud with descending glissandi".postln;
)

// ============================================================================
// SET OPERATIONS
// ============================================================================

// --- Union of two clouds ---
(
~cloudA = NUPICCloud((
	density: 30,
	timeRange: [0, 0.5],
	freqRange: [100, 600],
	distribution: \uniform
));

~cloudB = NUPICCloud((
	density: 30,
	timeRange: [0.5, 1],
	freqRange: [800, 3000],
	distribution: \gaussian
));

~union = ~cloudA.union(~cloudB);

~app.data.clearAll;
~union.addToData(~app.data);
~app.gui.refresh;
"Union: low-left + high-right".postln;
)

// --- Intersection (keep only region) ---
(
~cloud = NUPICCloud((
	density: 100,
	freqRange: [100, 4000]
));

// Keep only center region
~filtered = ~cloud.intersection([0.3, 0.7, 400, 1500]);

~app.data.clearAll;
~filtered.addToData(~app.data);
~app.gui.refresh;
"Intersection: center region only".postln;
)

// --- Difference (remove region) ---
(
~cloud = NUPICCloud((
	density: 100,
	freqRange: [100, 4000]
));

// Remove center region (creates "hole")
~filtered = ~cloud.difference([0.3, 0.7, 400, 1500]);

~app.data.clearAll;
~filtered.addToData(~app.data);
~app.gui.refresh;
"Difference: center region removed".postln;
)

// --- Multiple operations ---
(
~base = NUPICCloud((density: 80, freqRange: [100, 4000]));

// Remove two regions
~result = ~base
	.difference([0.1, 0.3, 200, 800])
	.difference([0.6, 0.8, 1000, 2500]);

~app.data.clearAll;
~result.addToData(~app.data);
~app.gui.refresh;
"Cloud with two holes".postln;
)

// ============================================================================
// TRANSFORMATIONS
// ============================================================================

// --- Time shift ---
(
~cloud = NUPICCloud((
	density: 40,
	timeRange: [0, 0.4],
	freqRange: [300, 1200]
));

// Create shifted copy
~shifted = NUPICCloud((density: 0));
~shifted.arcs.addAll(~cloud.arcs.deepCopy);
~shifted.timeShift(0.5);

~app.data.clearAll;
~cloud.addToData(~app.data);
~shifted.addToData(~app.data);
~app.gui.refresh;
"Original + time-shifted copy".postln;
)

// --- Frequency shift ---
(
~cloud = NUPICCloud((
	density: 40,
	freqRange: [200, 600]
));

// Create octave-shifted copy
~high = NUPICCloud((density: 0));
~high.arcs.addAll(~cloud.arcs.deepCopy);
~high.freqShift(2.0);  // Octave up

~app.data.clearAll;
~cloud.addToData(~app.data);
~high.addToData(~app.data);
~app.gui.refresh;
"Original + octave up".postln;
)

// --- Time compression ---
(
~cloud = NUPICCloud((
	density: 60,
	freqRange: [200, 2000]
));

// Compress time toward center
~cloud.timeCompress(0.5, 0.5);  // factor, anchor

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Time compressed toward center".postln;
)

// ============================================================================
// SPECIAL GENERATORS
// ============================================================================

// --- Arborescence (divergent) ---
(
~tree = NUPICCloud.arborescence((
	rootX: 0.05,
	rootFreq: 500,
	generations: 5,
	branchFactor: 2,
	freqSpread: 1.5,
	mode: \diverge
));

~app.data.clearAll;
~tree.addToData(~app.data);
~app.gui.refresh;
"Divergent arborescence".postln;
)

// --- Arborescence (convergent) ---
(
~tree = NUPICCloud.arborescence((
	rootFreq: 600,
	generations: 4,
	branchFactor: 3,
	mode: \converge
));

~app.data.clearAll;
~tree.addToData(~app.data);
~app.gui.refresh;
"Convergent arborescence".postln;
)

// --- Von Neumann cumulative hierarchy ---
(
~vn = NUPICCloud.vonNeumann(6, 400);

~app.data.clearAll;
~vn.addToData(~app.data);
~app.gui.refresh;
"Von Neumann hierarchy".postln;
)

// --- Screen (2D probability grid) ---
(
~screen = NUPICCloud.screen((
	timeSlots: 40,
	pitchSlots: 30,
	probability: 0.15,
	freqMin: 100,
	freqMax: 3000
));

~app.data.clearAll;
~screen.addToData(~app.data);
~app.gui.refresh;
"Screen with 15% probability".postln;
)

// --- Screen with variable probability ---
(
~screen = NUPICCloud.screen((
	timeSlots: 50,
	pitchSlots: 35,
	probabilityFunc: { |t, p|
		// Higher probability at center pitch
		var distFromCenter = (p - 0.5).abs;
		0.25 * (1 - (distFromCenter * 2).squared)
	}
));

~app.data.clearAll;
~screen.addToData(~app.data);
~app.gui.refresh;
"Screen with center-weighted probability".postln;
)

// --- Sieve (pitch selection) ---
(
~sieve = NUPICCloud.sieve((
	density: 60,
	baseFreq: 100,
	modulo: 5,  // Every 5th semitone
	offset: 0,
	octaves: 5
));

~app.data.clearAll;
~sieve.addToData(~app.data);
~app.gui.refresh;
"Sieve: every 5th semitone".postln;
)

// --- Markov chain ---
(
~markov = NUPICCloud.markov((
	numGrains: 100,
	states: [200, 400, 600, 800, 1000, 1200]
));

~app.data.clearAll;
~markov.addToData(~app.data);
~app.gui.refresh;
"Markov chain".postln;
)

// --- Density envelope: crescendo ---
(
~cresc = NUPICCloud.densityEnvelope((
	totalDensity: 100,
	envelope: \crescendo,
	freqRange: [200, 2500]
));

~app.data.clearAll;
~cresc.addToData(~app.data);
~app.gui.refresh;
"Density crescendo".postln;
)

// --- Density envelope: pulsating ---
(
~pulse = NUPICCloud.densityEnvelope((
	totalDensity: 120,
	envelope: \pulsating,
	pulsations: 4,
	freqRange: [150, 3000]
));

~app.data.clearAll;
~pulse.addToData(~app.data);
~app.gui.refresh;
"Pulsating density".postln;
)

// ============================================================================
// COMPLEX COMPOSITIONS
// ============================================================================

// --- Layered composition ---
(
// Bass layer
~bass = NUPICCloud((
	density: 15,
	freqRange: [60, 200],
	durRange: [0.005, 0.01],
	morphology: \dot
));

// Mid layer
~mid = NUPICCloud((
	density: 50,
	freqRange: [300, 1200],
	distribution: \gaussian,
	morphology: \mixed
));

// High layer
~high = NUPICCloud((
	density: 25,
	freqRange: [1500, 4000],
	morphology: \descending
));

~app.data.clearAll;
~bass.addToData(~app.data);
~mid.addToData(~app.data);
~high.addToData(~app.data);
~app.gui.refresh;
"Three-layer composition".postln;
)

// --- Arborescence + clouds ---
(
~tree = NUPICCloud.arborescence((
	rootFreq: 400,
	generations: 4,
	branchFactor: 2,
	mode: \diverge
));

~cloud = NUPICCloud((
	density: 40,
	distribution: \gaussian,
	morphology: \dot
));

~app.data.clearAll;
~tree.addToData(~app.data);
~cloud.addToData(~app.data);
~app.gui.refresh;
"Arborescence with cloud overlay".postln;
)

// ============================================================================
// GENETIC ALGORITHMS
// ============================================================================

// --- Automatic evolution with default fitness ---
(
// Evolve for 20 generations
~evolved = NUPICCloud.evolve((
	populationSize: 12,
	generations: 20,
	mutationRate: 0.15,
	onGeneration: { |gen, pop, fits|
		"Gen %: best fitness = %".format(gen, fits.maxItem.round(0.01)).postln;
	}
));

~app.data.clearAll;
~evolved.addToData(~app.data);
~app.gui.refresh;
"Evolved cloud (% arcs)".format(~evolved.size).postln;
)

// --- Evolution with custom fitness function ---
(
// Fitness: prefer ascending glissandi and mid-frequencies
~customFitness = { |cloud|
	var ascendingScore = 0;
	var freqScore = 0;

	cloud.arcs.do { |arc|
		var startFreq = arc[0][\freq];
		var endFreq = arc[1][\freq];

		// Reward ascending motion
		if(endFreq > startFreq) {
			ascendingScore = ascendingScore + 1;
		};

		// Reward mid-frequencies (400-1200 Hz)
		if(startFreq.inRange(400, 1200)) {
			freqScore = freqScore + 1;
		};
	};

	if(cloud.size > 0) {
		((ascendingScore / cloud.size) + (freqScore / cloud.size)) / 2
	} { 0 }
};

~evolved = NUPICCloud.evolve((
	populationSize: 15,
	generations: 30,
	fitnessFunc: ~customFitness,
	onGeneration: { |gen, pop, fits|
		if(gen % 5 == 0) {
			"Gen %: best = %, avg = %".format(gen, fits.maxItem.round(0.01), fits.mean.round(0.01)).postln;
		}
	}
));

~app.data.clearAll;
~evolved.addToData(~app.data);
~app.gui.refresh;
"Evolved for ascending mid-frequency content".postln;
)

// --- Evolution from seed cloud ---
(
// Start with a specific cloud and evolve variations
~seed = NUPICCloud((
	density: 35,
	freqRange: [300, 1500],
	distribution: \gaussian,
	morphology: \ascending
));

~evolved = NUPICCloud.evolve((
	seedCloud: ~seed,
	populationSize: 10,
	generations: 15,
	mutationRate: 0.2,
	mutationStrength: 0.3
));

~app.data.clearAll;
~evolved.addToData(~app.data);
~app.gui.refresh;
"Evolved from Gaussian seed".postln;
)

// --- Crossover between two clouds ---
(
~parentA = NUPICCloud((
	density: 40,
	timeRange: [0, 1],
	freqRange: [100, 500],
	morphology: \dot
));

~parentB = NUPICCloud((
	density: 40,
	timeRange: [0, 1],
	freqRange: [1000, 3000],
	morphology: \descending
));

// Crossover at midpoint
~child = NUPICCloud.crossover(~parentA, ~parentB, 0.5);

~app.data.clearAll;
~child.addToData(~app.data);
~app.gui.refresh;
"Child: low dots (left) + high descending (right)".postln;
)

// --- Mutation exploration ---
(
~original = NUPICCloud((
	density: 30,
	freqRange: [400, 1200]
));

// Create mutated versions
~mutations = 4.collect { |i|
	var m = ~original.clone;
	m.mutate(0.3, 0.1 * (i + 1));  // Increasing mutation strength
	m
};

~app.data.clearAll;
~original.addToData(~app.data);
"Original cloud".postln;
)

// View mutations one by one
(
~mutations[0].arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"+ Mutation 1 (light)".postln;
)

(
~mutations[1].arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"+ Mutation 2 (medium)".postln;
)

// --- Interactive evolution workflow ---
(
// Generate initial population for user evaluation
~population = NUPICCloud.interactiveEvolve((
	populationSize: 6,
	seedCloud: NUPICCloud((density: 40, freqRange: [200, 2000]))
));

"Generated % candidates for evaluation".format(~population.size).postln;
"Use ~showCandidate.(index) to view each, then rate them".postln;
)

// Helper to show candidates
(
~showCandidate = { |index|
	~app.data.clearAll;
	~population[index].addToData(~app.data);
	~app.gui.refresh;
	"Showing candidate %".format(index).postln;
};
)

// View candidates (run each separately)
~showCandidate.(0);
~showCandidate.(1);
~showCandidate.(2);
~showCandidate.(3);
~showCandidate.(4);
~showCandidate.(5);

// After viewing, rate them (0-10 scale) and evolve
(
// Example ratings (replace with your own)
~ratings = [7, 3, 8, 5, 6, 4];

~population = NUPICCloud.evolveWithRatings(~population, ~ratings);
"Evolved to next generation based on ratings".postln;
)

// ============================================================================
// ECHELON CONSTRUCTIONS (Bourbaki)
// ============================================================================

// --- Echelon Schemes: E, E×E, P(E), P(E×E), E×P(E) ---
(
~cloud = NUPICCloud.echelonScheme((
	baseSet: [200, 400, 800]  // Base set E
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Echelon type signatures: E → E×E → P(E) → P(E×E) → E×P(E)".postln;
)

// --- Mother Structures: Algebraic | Order | Topological ---
(
~cloud = NUPICCloud.motherStructures((
	fundamental: 220
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Mother structures: three pillars of Bourbaki mathematics".postln;
)

// --- Echelon Scale Construction ---
(
// Build a scale from octave (2) and fifth (3) generators
~cloud = NUPICCloud.echelonScale((
	generators: [2, 3],    // Pythagorean generators
	maxDenom: 128,
	fundamental: 200
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
("Scale with" + ~cloud.metadata[\pitchesPerOctave] + "pitches per octave").postln;
)

// --- Spectral Echelon: harmonics → differences → sums → ring mod ---
(
~cloud = NUPICCloud.spectralEchelon((
	fundamental: 110,
	numHarmonics: 8
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Spectral echelon: building from harmonic series".postln;
)

// --- Echelon Morphisms: transposition and inversion ---
(
~cloud = NUPICCloud.echelonMorphism((
	baseFreqs: [200, 350, 500],
	transposition: 1.5,       // Up a fifth
	inversionCenter: 400      // Invert around 400 Hz
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Echelon morphisms: Original | Transposed | Inverted".postln;
)

// --- Recursive Echelon: P(P(P(E))) ---
(
~cloud = NUPICCloud.recursiveEchelon((
	baseSet: [150, 300, 600],
	levels: 5,
	maxPerLevel: 8
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
("Recursive echelon:" + ~cloud.metadata[\numLevels] + "levels").postln;
)

// --- Sieve-Filtered Echelon ---
(
~cloud = NUPICCloud.sieveEchelon((
	baseFreqs: [100, 200, 400, 800],
	modulo: 3,
	residue: 1
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
("Sieve echelon: kept" + ~cloud.metadata[\kept] + "of" + ~cloud.metadata[\total]).postln;
)

// ============================================================================
// ORDINAL CONSTRUCTIONS
// ============================================================================

// --- Basic Ordinal Spine with Branching ---
(
~cloud = NUPICCloud.ordinalSpine((
	numOrdinals: 12,
	baseFreq: 150,
	freqMult: 1.25
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Ordinal spine with branching".postln;
)

// --- Successor and Limit Ordinals: finite → ω → ω+n → ω·2 → ω² ---
(
~cloud = NUPICCloud.successorLimitOrdinals((
	numFinite: 8,
	fundamental: 120
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Successor and limit ordinals: finite → ω → ω+n → ω·2 → ω²".postln;
)

// --- Ordinal Arithmetic: demonstrating non-commutativity ---
(
~cloud = NUPICCloud.ordinalArithmetic((
	baseFreq: 200,
	omegaFreq: 600
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Ordinal arithmetic: 1+ω=ω | ω+1≠ω | ω·2".postln;
)

// --- Cantor Normal Form Spine ---
(
// Default: ω³ + ω²·2 + ω·3 + 5
~cloud = NUPICCloud.cantorNormalForm((
	spines: [
		[3, 1, 200],   // ω³ (exp=3, coef=1, freq=200)
		[2, 2, 400],   // ω²·2
		[1, 3, 700],   // ω·3
		[0, 5, 1100]   // 5
	]
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Cantor Normal Form: ω³ + ω²·2 + ω·3 + 5".postln;
)

// --- Transfinite Recursion Patterns ---
(
~cloud = NUPICCloud.transfiniteRecursion((
	baseFreq: 300,
	baseDur: 0.03,
	iterations: 12
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Transfinite recursion with limit consolidation".postln;
)

// --- Epsilon Numbers: self-similar fixed points ---
(
~cloud = NUPICCloud.epsilonNumbers((
	depth: 4,
	baseFreq: 150,
	width: 0.35
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Epsilon numbers: ω^ε₀ = ε₀".postln;
)

// --- Multi-Spine Ordinal Weave ---
(
~cloud = NUPICCloud.multiSpineWeave((
	numSpines: 4,
	numOrdinals: 16
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Multi-spine ordinal weave with cross-connections".postln;
)

// --- Ordinal Collapse ---
(
~cloud = NUPICCloud.ordinalCollapse((
	largeRange: [80, 4000],
	collapsedRange: [300, 600],
	numPoints: 20
));

~app.data.clearAll;
~cloud.addToData(~app.data);
~app.gui.refresh;
"Ordinal collapse: wide range → narrow band".postln;
)

// ============================================================================
// COMBINING ECHELON AND ORDINAL WITH OTHER OPERATIONS
// ============================================================================

// --- Echelon + Transformation ---
(
~echelon = NUPICCloud.spectralEchelon((fundamental: 150));
~echelon.freqCompress(0.5);  // Compress frequency range
~echelon.timeCompress(0.6);  // Compress time

~app.data.clearAll;
~echelon.addToData(~app.data);
~app.gui.refresh;
"Spectral echelon with compression transforms".postln;
)

// --- Ordinal Spine + Union with Stochastic Cloud ---
(
~spine = NUPICCloud.ordinalSpine((numOrdinals: 8));

~stochastic = NUPICCloud((
	density: 30,
	freqRange: [500, 2000],
	distribution: \gaussian
));

~combined = ~spine.union(~stochastic);

~app.data.clearAll;
~combined.addToData(~app.data);
~app.gui.refresh;
"Ordinal spine + Gaussian cloud union".postln;
)

// --- Echelon Evolution ---
(
// Use echelon scheme as seed for evolution
~seed = NUPICCloud.echelonScheme((baseSet: [200, 400, 600]));

~evolved = NUPICCloud.evolve((
	seedCloud: ~seed,
	populationSize: 8,
	generations: 10,
	mutationRate: 0.2,
	mutationStrength: 0.3
));

~app.data.clearAll;
~evolved.addToData(~app.data);
~app.gui.refresh;
"Evolved echelon structure".postln;
)

// --- Multi-layered Construction ---
(
// Layer 1: Mother structures base
~base = NUPICCloud.motherStructures((fundamental: 200));

// Layer 2: Ordinal spine overlay
~spine = NUPICCloud.ordinalSpine((numOrdinals: 6, baseFreq: 800));
~spine.timeShift(0.1);

// Layer 3: Epsilon decoration
~epsilon = NUPICCloud.epsilonNumbers((depth: 3, baseFreq: 1500, width: 0.2));
~epsilon.timeShift(0.7);

// Combine all layers
~combined = ~base.union(~spine);
~combined = ~combined.union(~epsilon);

~app.data.clearAll;
~combined.addToData(~app.data);
~app.gui.refresh;
"Multi-layered: Mother structures + Ordinal spine + Epsilon".postln;
)

// ============================================================================
// CLEANUP
// ============================================================================

(
~app.data.clearAll;
~app.gui.refresh;
)

(
~app.stop;
)
