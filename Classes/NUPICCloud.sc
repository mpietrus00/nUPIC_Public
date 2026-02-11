/*
NUPICCloud - Stochastic Cloud Generator for nUPIC
Based on Xenakis's approach to granular/stochastic composition

Concepts from:
- Xenakis: Formalized Music, stochastic processes, arborescences
- Bourbaki: Mother structures, echelon constructions
- Badiou: Multiplicity from void, set-theoretic ontology
*/

NUPICCloud {
	var <params;
	var <arcs;
	var <metadata;

	// Distribution types
	classvar <distributions;

	*initClass {
		distributions = [\uniform, \gaussian, \cauchy, \exponential];
	}

	*new { |params|
		^super.new.init(params)
	}

	init { |inParams|
		params = IdentityDictionary.newFrom([
			\density, 50,
			\timeRange, [0, 1],
			\freqRange, [100, 3000],
			\durRange, [0.01, 0.04],
			\distribution, \uniform,
			\glissando, 1.0,  // 1.0 = no gliss, >1 = tendency up, <1 = tendency down
			\morphology, \mixed,  // \dot, \ascending, \descending, \mixed
		]);

		if(inParams.notNil) {
			inParams.keysValuesDo { |k, v| params[k] = v };
		};

		arcs = List[];
		metadata = IdentityDictionary[];
		this.generate;
	}

	// ========== Generation ==========

	generate {
		var density = params[\density];
		var timeRange = params[\timeRange];
		var freqRange = params[\freqRange];
		var durRange = params[\durRange];
		var distribution = params[\distribution];
		var glissando = params[\glissando];
		var morphology = params[\morphology];

		arcs.clear;

		density.do {
			var x, freq, dur, endFreq;

			// Time position based on distribution
			x = this.prSampleDistribution(distribution, timeRange[0], timeRange[1]);

			// Frequency based on distribution (exponential mapping)
			freq = this.prSampleFrequency(distribution, freqRange[0], freqRange[1]);

			// Duration
			dur = rrand(durRange[0], durRange[1]);

			// End frequency based on morphology
			endFreq = this.prCalculateEndFreq(freq, morphology, glissando);

			arcs.add([
				(x: x, freq: freq),
				(x: (x + dur).min(1), freq: endFreq.clip(50, 6000))
			]);
		};

		metadata[\generatedAt] = Date.getDate.asString;
		metadata[\arcCount] = arcs.size;
	}

	prSampleDistribution { |dist, min, max|
		var center = (min + max) / 2;
		var spread = (max - min) / 2;

		^switch(dist,
			\uniform, { rrand(min, max) },
			\gaussian, { (center + (spread * 0.gauss(1) * 0.4)).clip(min, max) },
			\cauchy, { this.prCauchy(center, spread * 0.3).clip(min, max) },
			\exponential, { min + ((max - min) * (1 - (1.0.rand.log.neg / 3)).clip(0, 1)) },
			{ rrand(min, max) }  // default
		)
	}

	prSampleFrequency { |dist, fMin, fMax|
		// Frequencies should be sampled in exponential space
		var center = sqrt(fMin * fMax);
		var ratio = sqrt(fMax / fMin);

		^switch(dist,
			\uniform, { exprand(fMin, fMax) },
			\gaussian, { (center * (ratio ** (0.gauss(1) * 0.5))).clip(fMin, fMax) },
			\cauchy, { (center * (ratio ** (this.prCauchy(0, 0.3)))).clip(fMin, fMax) },
			\exponential, { fMin * ((fMax/fMin) ** rrand(0.0, 1.0).squared) },
			{ exprand(fMin, fMax) }
		)
	}

	prCauchy { |location, scale|
		var u = rrand(0.001, 0.999);
		^(location + (scale * tan(pi * (u - 0.5))))
	}

	prCalculateEndFreq { |startFreq, morphology, glissando|
		^switch(morphology,
			\dot, { startFreq },
			\ascending, { startFreq * rrand(1.1, glissando.max(1.1)) },
			\descending, { startFreq * rrand(glissando.min(0.9), 0.9) },
			\mixed, {
				var morph = [\dot, \ascending, \descending].choose;
				this.prCalculateEndFreq(startFreq, morph, glissando)
			},
			{ startFreq * rrand(1/glissando, glissando) }
		)
	}

	// ========== Set Operations ==========

	union { |otherCloud|
		var result = NUPICCloud((density: 0));
		result.arcs.clear;
		result.arcs.addAll(this.arcs);
		result.arcs.addAll(otherCloud.arcs);
		^result
	}

	intersection { |region|
		// region = [xMin, xMax, freqMin, freqMax]
		var result = NUPICCloud((density: 0));
		result.arcs.clear;
		result.arcs.addAll(
			this.arcs.select { |arc|
				var x = arc[0][\x];
				var freq = arc[0][\freq];
				this.prInRegion(x, freq, region)
			}
		);
		^result
	}

	difference { |region|
		// Remove events in region
		var result = NUPICCloud((density: 0));
		result.arcs.clear;
		result.arcs.addAll(
			this.arcs.reject { |arc|
				var x = arc[0][\x];
				var freq = arc[0][\freq];
				this.prInRegion(x, freq, region)
			}
		);
		^result
	}

	prInRegion { |x, freq, region|
		^(x >= region[0]) and: { x <= region[1] } and:
		{ freq >= region[2] } and: { freq <= region[3] }
	}

	// ========== Transformations ==========

	timeShift { |delta|
		arcs = arcs.collect { |arc|
			arc.collect { |pt|
				(x: (pt[\x] + delta).clip(0, 1), freq: pt[\freq])
			}
		};
	}

	freqShift { |ratio|
		arcs = arcs.collect { |arc|
			arc.collect { |pt|
				(x: pt[\x], freq: (pt[\freq] * ratio).clip(50, 6000))
			}
		};
	}

	timeCompress { |factor, anchor = 0|
		arcs = arcs.collect { |arc|
			arc.collect { |pt|
				var newX = anchor + ((pt[\x] - anchor) * factor);
				(x: newX.clip(0, 1), freq: pt[\freq])
			}
		};
	}

	freqCompress { |factor|
		var centerFreq = this.prGetCenterFreq;
		arcs = arcs.collect { |arc|
			arc.collect { |pt|
				var logDist = (pt[\freq] / centerFreq).log;
				var newFreq = centerFreq * exp(logDist * factor);
				(x: pt[\x], freq: newFreq.clip(50, 6000))
			}
		};
	}

	prGetCenterFreq {
		var allFreqs = arcs.flatten.collect { |pt| pt[\freq] };
		^allFreqs.mean
	}

	// ========== Arborescence Generation ==========

	*arborescence { |params|
		var cloud = NUPICCloud((density: 0));
		var rootX = params[\rootX] ?? 0.05;
		var rootFreq = params[\rootFreq] ?? 500;
		var generations = params[\generations] ?? 5;
		var branchFactor = params[\branchFactor] ?? 2;
		var freqSpread = params[\freqSpread] ?? 1.4;
		var branchProb = params[\branchProb] ?? 0.9;
		var mode = params[\mode] ?? \diverge;

		var currentPoints, targetFreq, targetX;

		cloud.arcs.clear;

		if(mode == \diverge) {
			currentPoints = [(x: rootX, freq: rootFreq)];

			generations.do { |gen|
				var nextPoints = List[];
				var timeSlice = rootX + ((gen + 1) / generations * (0.9 - rootX));

				currentPoints.do { |pt|
					var numChildren = if(branchProb.coin) {
						(branchFactor + rrand(-1, 1)).round.max(1).min(4)
					} { 0 };

					numChildren.do {
						var childFreq = (pt[\freq] * rrand(1/freqSpread, freqSpread)).clip(60, 5000);
						var childX = (timeSlice + rrand(-0.03, 0.03)).clip(0, 1);

						cloud.arcs.add([
							(x: pt[\x], freq: pt[\freq]),
							(x: childX, freq: childFreq)
						]);

						nextPoints.add((x: childX, freq: childFreq));
					};
				};

				currentPoints = nextPoints;
			};
		} {
			// Converge mode
			targetFreq = rootFreq;
			targetX = 0.95;
			currentPoints = (branchFactor ** generations).asInteger.min(50).collect {
				(x: rrand(0.02, 0.15), freq: exprand(100, 3000))
			};

			generations.do { |gen|
				var nextPoints = List[];
				var timeSlice = 0.15 + ((gen + 1) / generations * 0.75);
				var blend = (gen + 1) / generations * 0.4;

				currentPoints.clump(branchFactor.max(2)).do { |group|
					var avgFreq = group.collect(_[\freq]).mean;
					var blendedFreq = avgFreq.blend(targetFreq, blend);
					var childX = (timeSlice + rrand(-0.02, 0.02)).clip(0, 1);

					group.do { |pt|
						cloud.arcs.add([
							(x: pt[\x], freq: pt[\freq]),
							(x: childX, freq: blendedFreq.clip(60, 5000))
						]);
					};

					nextPoints.add((x: childX, freq: blendedFreq));
				};

				currentPoints = nextPoints;
			};

			currentPoints.do { |pt|
				cloud.arcs.add([
					(x: pt[\x], freq: pt[\freq]),
					(x: targetX, freq: targetFreq)
				]);
			};
		};

		^cloud
	}

	// Von Neumann cumulative hierarchy
	*vonNeumann { |levels = 6, baseFreq = 400|
		var cloud = NUPICCloud((density: 0));
		cloud.arcs.clear;

		levels.do { |level|
			var numElements = (2 ** level).asInteger.min(64);
			var timeStart = level / levels;
			var timeEnd = (level + 1) / levels;
			var freqSpread = 1.2 ** level;

			numElements.do {
				var x = rrand(timeStart, timeEnd);
				var freq = baseFreq * rrand(1/freqSpread, freqSpread);
				var dur = rrand(0.005, 0.02);

				cloud.arcs.add([
					(x: x, freq: freq.clip(50, 5000)),
					(x: (x + dur).min(1), freq: freq.clip(50, 5000))
				]);
			};
		};

		^cloud
	}

	// ========== Screen (2D Grid) ==========

	*screen { |params|
		var cloud = NUPICCloud((density: 0));
		var timeSlots = params[\timeSlots] ?? 40;
		var pitchSlots = params[\pitchSlots] ?? 30;
		var probability = params[\probability] ?? 0.15;
		var freqMin = params[\freqMin] ?? 100;
		var freqMax = params[\freqMax] ?? 3000;
		var probabilityFunc = params[\probabilityFunc];  // Optional function(t, p) -> probability

		cloud.arcs.clear;

		timeSlots.do { |t|
			pitchSlots.do { |p|
				var prob = if(probabilityFunc.notNil) {
					probabilityFunc.value(t / timeSlots, p / pitchSlots)
				} { probability };

				if(prob.coin) {
					var x = t / timeSlots;
					var freq = p.linexp(0, pitchSlots - 1, freqMin, freqMax);
					var dur = 0.8 / timeSlots;

					cloud.arcs.add([
						(x: x, freq: freq),
						(x: x + dur, freq: freq)
					]);
				};
			};
		};

		^cloud
	}

	// ========== Sieve (Pitch Selection) ==========

	*sieve { |params|
		var cloud = NUPICCloud((density: 0));
		var density = params[\density] ?? 60;
		var baseFreq = params[\baseFreq] ?? 100;
		var modulo = params[\modulo] ?? 5;
		var offset = params[\offset] ?? 0;
		var octaves = params[\octaves] ?? 5;

		var sieveFreqs;

		cloud.arcs.clear;

		// Generate valid frequencies from sieve
		sieveFreqs = (0..(octaves * 12)).select { |semitone|
			(semitone - offset) % modulo == 0
		}.collect { |semitone|
			baseFreq * (2 ** (semitone / 12))
		};

		density.do {
			var x = rrand(0, 1);
			var freq = sieveFreqs.choose;
			var dur = rrand(0.01, 0.04);

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: (x + dur).min(1), freq: freq)
			]);
		};

		^cloud
	}

	// ========== Markov Chain ==========

	*markov { |params|
		var cloud = NUPICCloud((density: 0));
		var numGrains = params[\numGrains] ?? 100;
		var states = params[\states] ?? [200, 400, 600, 800, 1000, 1200];
		var transitionMatrix = params[\transitionMatrix];
		var currentState, nextState;

		cloud.arcs.clear;

		// Default transition matrix if not provided
		if(transitionMatrix.isNil) {
			transitionMatrix = states.size.collect { |i|
				var probs = Array.fill(states.size, { 1.0 / states.size });
				// Bias toward neighboring states
				if(i > 0) { probs[i-1] = probs[i-1] + 0.2 };
				if(i < (states.size - 1)) { probs[i+1] = probs[i+1] + 0.2 };
				probs = probs.normalizeSum;
				probs
			};
		};

		currentState = states.size.rand;

		numGrains.do { |i|
			var x = i / numGrains;
			var freq = states[currentState];
			var dur = rrand(0.005, 0.015);

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: (x + dur).min(1), freq: freq)
			]);

			// Transition to next state
			nextState = cloud.prMarkovNext(currentState, transitionMatrix);
			currentState = nextState;
		};

		^cloud
	}

	prMarkovNext { |currentState, matrix|
		var probs = matrix[currentState];
		var cumProbs = probs.integrate;
		var r = rrand(0.0, 1.0);
		^cumProbs.detectIndex { |p| r <= p }
	}

	// ========== Density Envelopes ==========

	*densityEnvelope { |params|
		var cloud = NUPICCloud((density: 0));
		var totalDensity = params[\totalDensity] ?? 100;
		var timeSlots = params[\timeSlots] ?? 20;
		var envelope = params[\envelope] ?? \crescendo;  // \crescendo, \decrescendo, \pulsating
		var freqRange = params[\freqRange] ?? [100, 3000];
		var pulsations = params[\pulsations] ?? 4;

		var grainsPerSlot;

		cloud.arcs.clear;

		grainsPerSlot = switch(envelope,
			\crescendo, {
				timeSlots.collect { |i| ((i + 1) / timeSlots).squared * (totalDensity / timeSlots * 2) }.round.asInteger
			},
			\decrescendo, {
				timeSlots.collect { |i| (1 - (i / timeSlots)).squared * (totalDensity / timeSlots * 2) }.round.asInteger
			},
			\pulsating, {
				timeSlots.collect { |i|
					var phase = i / timeSlots * 2pi * pulsations;
					((sin(phase) + 1) / 2 * (totalDensity / timeSlots * 2)).round.asInteger + 1
				}
			},
			{ Array.fill(timeSlots, { (totalDensity / timeSlots).round.asInteger }) }
		);

		timeSlots.do { |slot|
			var slotStart = slot / timeSlots;
			var slotEnd = (slot + 1) / timeSlots;

			grainsPerSlot[slot].do {
				var x = rrand(slotStart, slotEnd);
				var freq = exprand(freqRange[0], freqRange[1]);
				var dur = rrand(0.005, 0.03);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: (x + dur).min(1), freq: freq * rrand(0.95, 1.05))
				]);
			};
		};

		^cloud
	}

	// ========== Genetic Algorithms ==========

	// Crossover: combine two parent clouds
	*crossover { |parentA, parentB, crossoverPoint = 0.5|
		var child = NUPICCloud((density: 0));
		child.arcs.clear;

		// Take arcs from parentA before crossover point
		parentA.arcs.do { |arc|
			if(arc[0][\x] < crossoverPoint) {
				child.arcs.add(arc.deepCopy);
			};
		};

		// Take arcs from parentB after crossover point
		parentB.arcs.do { |arc|
			if(arc[0][\x] >= crossoverPoint) {
				child.arcs.add(arc.deepCopy);
			};
		};

		^child
	}

	// Mutation: randomly modify arcs
	mutate { |mutationRate = 0.1, mutationStrength = 0.2|
		arcs = arcs.collect { |arc|
			if(mutationRate.coin) {
				// Mutate this arc
				arc.collect { |pt|
					var newX = pt[\x] + (mutationStrength.rand2 * 0.1);
					var freqMutate = 2 ** (mutationStrength.rand2);  // Exponential mutation
					var newFreq = pt[\freq] * freqMutate;
					(x: newX.clip(0, 1), freq: newFreq.clip(50, 6000))
				}
			} {
				arc
			}
		};
	}

	// Clone this cloud
	clone {
		var copy = NUPICCloud((density: 0));
		copy.arcs.clear;
		copy.arcs.addAll(this.arcs.deepCopy);
		^copy
	}

	// Evolve a population of clouds
	*evolve { |params|
		var populationSize = params[\populationSize] ?? 10;
		var generations = params[\generations] ?? 20;
		var mutationRate = params[\mutationRate] ?? 0.15;
		var mutationStrength = params[\mutationStrength] ?? 0.2;
		var eliteCount = params[\eliteCount] ?? 2;
		var seedCloud = params[\seedCloud];
		var fitnessFunc = params[\fitnessFunc];  // Function(cloud) -> fitness score
		var onGeneration = params[\onGeneration];  // Callback(generation, population, fitnesses)

		var population, fitnesses, newPopulation, sorted;

		// Initialize population
		if(seedCloud.notNil) {
			// Create variations of seed cloud
			population = populationSize.collect { |i|
				var cloud = seedCloud.clone;
				if(i > 0) {
					cloud.mutate(0.3, 0.3);  // Initial diversity
				};
				cloud
			};
		} {
			// Random initial population
			population = populationSize.collect {
				NUPICCloud((
					density: rrand(20, 60),
					freqRange: [exprand(80, 300), exprand(1000, 4000)],
					distribution: [\uniform, \gaussian, \cauchy].choose,
					morphology: [\dot, \ascending, \descending, \mixed].choose
				))
			};
		};

		// Default fitness function: prefer medium density, wide frequency range
		if(fitnessFunc.isNil) {
			fitnessFunc = { |cloud|
				var densityScore, freqRangeScore;
				var allFreqs = cloud.arcs.flatten.collect { |pt| pt[\freq] };

				if(allFreqs.size > 0) {
					var fMin = allFreqs.minItem;
					var fMax = allFreqs.maxItem;
					var range = (fMax / fMin).log2;  // Octaves

					// Prefer 30-50 arcs
					densityScore = 1 - ((cloud.size - 40).abs / 40).clip(0, 1);
					// Prefer 3-5 octave range
					freqRangeScore = 1 - ((range - 4).abs / 4).clip(0, 1);

					(densityScore + freqRangeScore) / 2
				} { 0 }
			};
		};

		// Evolution loop
		generations.do { |gen|
			// Evaluate fitness
			fitnesses = population.collect { |cloud| fitnessFunc.(cloud) };

			// Callback for monitoring
			if(onGeneration.notNil) {
				onGeneration.value(gen, population, fitnesses);
			};

			// Sort by fitness (best first)
			sorted = population.collect { |c, i| [c, fitnesses[i]] }
				.sort { |a, b| a[1] > b[1] };

			population = sorted.collect(_[0]);
			fitnesses = sorted.collect(_[1]);

			// Create new population
			newPopulation = List[];

			// Keep elite (best individuals)
			eliteCount.min(populationSize).do { |i|
				newPopulation.add(population[i].clone);
			};

			// Fill rest with crossover + mutation
			while { newPopulation.size < populationSize } {
				var parentA, parentB, child;

				// Tournament selection
				parentA = this.prTournamentSelect(population, fitnesses, 3);
				parentB = this.prTournamentSelect(population, fitnesses, 3);

				// Crossover
				child = NUPICCloud.crossover(parentA, parentB, rrand(0.3, 0.7));

				// Mutation
				child.mutate(mutationRate, mutationStrength);

				newPopulation.add(child);
			};

			population = newPopulation.asArray;
		};

		// Return best individual
		fitnesses = population.collect { |cloud| fitnessFunc.(cloud) };
		^population[fitnesses.maxIndex]
	}

	*prTournamentSelect { |population, fitnesses, tournamentSize|
		var indices = (0..population.size-1).scramble.keep(tournamentSize);
		var bestIdx = indices[0];
		var bestFitness = fitnesses[bestIdx];

		indices.do { |idx|
			if(fitnesses[idx] > bestFitness) {
				bestIdx = idx;
				bestFitness = fitnesses[idx];
			};
		};

		^population[bestIdx]
	}

	// Interactive evolution: user rates clouds
	*interactiveEvolve { |params, app|
		var populationSize = params[\populationSize] ?? 6;
		var initialCloud = params[\seedCloud];
		var population;

		// Create initial population
		if(initialCloud.notNil) {
			population = populationSize.collect { |i|
				var cloud = initialCloud.clone;
				if(i > 0) { cloud.mutate(0.3, 0.3) };
				cloud
			};
		} {
			population = populationSize.collect {
				NUPICCloud((
					density: rrand(25, 50),
					freqRange: [exprand(100, 400), exprand(1500, 4000)]
				))
			};
		};

		// Return population for user to evaluate
		// User calls evolveWithRatings(population, ratings) to continue
		^population
	}

	*evolveWithRatings { |population, ratings, mutationRate = 0.15|
		var newPopulation = List[];
		var populationSize = population.size;

		// Normalize ratings
		var totalRating = ratings.sum.max(0.001);
		var probs = ratings / totalRating;

		// Keep best
		var bestIdx = ratings.maxIndex;
		newPopulation.add(population[bestIdx].clone);

		// Create children based on ratings
		while { newPopulation.size < populationSize } {
			var parentA, parentB, child;

			// Weighted random selection based on ratings
			parentA = this.prWeightedSelect(population, probs);
			parentB = this.prWeightedSelect(population, probs);

			child = NUPICCloud.crossover(parentA, parentB, rrand(0.3, 0.7));
			child.mutate(mutationRate, 0.2);

			newPopulation.add(child);
		};

		^newPopulation.asArray
	}

	*prWeightedSelect { |items, probs|
		var cumProbs = probs.integrate;
		var r = 1.0.rand;
		var idx = cumProbs.detectIndex { |p| r <= p } ?? 0;
		^items[idx]
	}

	// ========== Output ==========

	addToData { |data|
		arcs.do { |arc|
			data.addArc(arc);
		};
	}

	size { ^arcs.size }

	printOn { |stream|
		stream << "NUPICCloud(" << arcs.size << " arcs)";
	}

	// ================================================================
	// ECHELON CONSTRUCTIONS (Bourbaki)
	// ================================================================

	// Echelon Schemes: E, E×E, P(E), P(E×E), E×P(E)
	*echelonScheme { |params|
		var cloud = NUPICCloud((density: 0));
		var baseSet = params[\baseSet] ?? [200, 400, 800];
		var echelonTypes = Dictionary[];

		cloud.arcs.clear;

		// Type 0: Base elements E
		echelonTypes[\E] = baseSet.collect { |f| (freq: f, type: \E, depth: 0) };

		// Type 1: Product E × E
		echelonTypes[\ExE] = [];
		baseSet.do { |a|
			baseSet.do { |b|
				var newFreq = sqrt(a * b);
				echelonTypes[\ExE] = echelonTypes[\ExE].add(
					(freq: newFreq, type: \ExE, parents: [a, b], depth: 1)
				);
			};
		};

		// Type 2: Power set P(E)
		echelonTypes[\PE] = [];
		(2 ** baseSet.size).asInteger.do { |i|
			var subset = [];
			baseSet.size.do { |j|
				if(i.asBinaryDigits(baseSet.size)[j] == 1) {
					subset = subset.add(baseSet[j]);
				};
			};
			if(subset.size > 0) {
				var avgFreq = subset.sum / subset.size;
				echelonTypes[\PE] = echelonTypes[\PE].add(
					(freq: avgFreq, type: \PE, subset: subset, depth: 1)
				);
			};
		};

		// Type 3: P(E × E)
		echelonTypes[\PExE] = [];
		echelonTypes[\ExE].do { |elem|
			if(0.5.coin) {
				var variation = elem[\freq] * [0.75, 1.0, 1.5].choose;
				echelonTypes[\PExE] = echelonTypes[\PExE].add(
					(freq: variation, type: \PExE, source: elem, depth: 2)
				);
			};
		};

		// Type 4: E × P(E)
		echelonTypes[\ExPE] = [];
		baseSet.do { |e|
			echelonTypes[\PE].do { |p|
				var combined = sqrt(e * p[\freq]);
				echelonTypes[\ExPE] = echelonTypes[\ExPE].add(
					(freq: combined, type: \ExPE, base: e, powerset: p, depth: 2)
				);
			};
		};

		// Visualize: each type gets a time region
		[\E, \ExE, \PE, \PExE, \ExPE].do { |type, typeIdx|
			var timeBase = typeIdx / 5;
			var elements = echelonTypes[type];

			elements.do { |elem, i|
				var x = timeBase + (i / elements.size.max(1) * 0.15) + 0.02;
				var freq = elem[\freq].clip(80, 4000);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.01, freq: freq)
				]);

				// Connect to parents
				if(elem[\parents].notNil) {
					elem[\parents].do { |parentFreq|
						var parentIdx = baseSet.indexOf(parentFreq);
						if(parentIdx.notNil) {
							var parentX = 0 + (parentIdx / baseSet.size * 0.15) + 0.02;
							cloud.arcs.add([
								(x: parentX, freq: parentFreq),
								(x: x, freq: freq)
							]);
						};
					};
				};
			};
		};

		cloud.metadata[\type] = \echelonScheme;
		^cloud
	}

	// Mother Structures: Algebraic, Order, Topological
	*motherStructures { |params|
		var cloud = NUPICCloud((density: 0));
		var fundamental = params[\fundamental] ?? 220;

		cloud.arcs.clear;

		// ALGEBRAIC STRUCTURE (left third)
		{
			var groupElements = [1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2];
			var baseX = 0.02;

			groupElements.do { |ratio, i|
				var freq = fundamental * ratio;
				var x = baseX + (i * 0.035);
				var inverted = fundamental * fundamental / freq;

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.02, freq: freq)
				]);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.025, freq: inverted.clip(80, 4000))
				]);
			};
		}.value;

		// ORDER STRUCTURE (middle third)
		{
			var latticeBase = 0.35;
			var lattice = [
				[1, 0, 0], [5/4, -0.03, 1], [3/2, 0.03, 1],
				[15/8, 0, 2], [2, 0, 3]
			];

			lattice.do { |node|
				var ratio = node[0];
				var xOff = node[1];
				var level = node[2];
				var freq = fundamental * ratio;
				var x = latticeBase + xOff + (level * 0.06);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.015, freq: freq)
				]);
			};

			[[0,1], [0,2], [1,3], [2,3], [3,4]].do { |edge|
				var from = lattice[edge[0]];
				var to = lattice[edge[1]];
				var x1 = latticeBase + from[1] + (from[2] * 0.06);
				var x2 = latticeBase + to[1] + (to[2] * 0.06);

				cloud.arcs.add([
					(x: x1, freq: fundamental * from[0]),
					(x: x2, freq: fundamental * to[0])
				]);
			};
		}.value;

		// TOPOLOGICAL STRUCTURE (right third)
		{
			var topoBase = 0.68;
			var centers = [300, 600, 1200];
			var radii = [50, 80, 150];

			centers.do { |center, i|
				var x = topoBase + (i * 0.1);
				var radius = radii[i];

				cloud.arcs.add([
					(x: x, freq: center),
					(x: x + 0.02, freq: center)
				]);

				8.do {
					var neighborFreq = center + radius.rand2;
					var neighborX = x + rrand(-0.03, 0.03);
					var dur = rrand(0.005, 0.015);

					cloud.arcs.add([
						(x: neighborX, freq: neighborFreq.clip(80, 4000)),
						(x: neighborX + dur, freq: (neighborFreq * rrand(0.98, 1.02)).clip(80, 4000))
					]);

					if(0.3.coin) {
						cloud.arcs.add([
							(x: x, freq: center),
							(x: neighborX, freq: neighborFreq.clip(80, 4000))
						]);
					};
				};
			};
		}.value;

		cloud.metadata[\type] = \motherStructures;
		^cloud
	}

	// Scale Construction via Echelon
	*echelonScale { |params|
		var cloud = NUPICCloud((density: 0));
		var baseRatios = params[\generators] ?? [2, 3];
		var maxDenom = params[\maxDenom] ?? 128;
		var fundamental = params[\fundamental] ?? 200;
		var ratios = Set[];
		var scale, level1, level2;

		cloud.arcs.clear;
		ratios.add(1);

		// Echelon level 1
		baseRatios.do { |a|
			baseRatios.do { |b|
				var ratio = a / b;
				while { ratio >= 2 } { ratio = ratio / 2 };
				while { ratio < 1 } { ratio = ratio * 2 };
				ratios.add(ratio.round(0.0001));
			};
		};

		// Echelon level 2
		level1 = ratios.asArray;
		level1.do { |a|
			level1.do { |b|
				var ratio = a * b;
				while { ratio >= 2 } { ratio = ratio / 2 };
				while { ratio < 1 } { ratio = ratio * 2 };
				if(ratio.asFraction[1] <= maxDenom) {
					ratios.add(ratio.round(0.0001));
				};
			};
		};

		// Echelon level 3
		level2 = ratios.asArray;
		level2.do { |a|
			baseRatios.do { |b|
				[a * b, a / b].do { |ratio|
					while { ratio >= 2 } { ratio = ratio / 2 };
					while { ratio < 1 } { ratio = ratio * 2 };
					if(ratio.asFraction[1] <= maxDenom) {
						ratios.add(ratio.round(0.0001));
					};
				};
			};
		};

		scale = ratios.asArray.sort;

		// Display scale across time with octave equivalents
		3.do { |octave|
			var octaveMult = 2 ** octave;
			var timeOffset = octave * 0.3 + 0.05;

			scale.do { |ratio, i|
				var freq = fundamental * ratio * octaveMult;
				var x = timeOffset + (i / scale.size * 0.25);

				if(freq > 80 and: { freq < 4000 }) {
					cloud.arcs.add([
						(x: x, freq: freq),
						(x: x + 0.01, freq: freq)
					]);

					if(octave > 0) {
						var prevFreq = fundamental * ratio * (2 ** (octave - 1));
						var prevX = (octave - 1) * 0.3 + 0.05 + (i / scale.size * 0.25);
						if(prevFreq > 80) {
							cloud.arcs.add([
								(x: prevX, freq: prevFreq),
								(x: x, freq: freq)
							]);
						};
					};
				};
			};
		};

		cloud.metadata[\type] = \echelonScale;
		cloud.metadata[\pitchesPerOctave] = scale.size;
		^cloud
	}

	// Spectral Echelon: harmonics -> difference -> sum -> ring mod
	*spectralEchelon { |params|
		var cloud = NUPICCloud((density: 0));
		var fundamental = params[\fundamental] ?? 110;
		var numHarmonics = params[\numHarmonics] ?? 8;
		var harmonics = (1..numHarmonics).collect { |n| fundamental * n };
		var level0, level1, level2;

		cloud.arcs.clear;

		// Level 0: Base harmonics
		level0 = harmonics.collect { |f, i|
			var x = 0.02 + (i * 0.025);
			cloud.arcs.add([
				(x: x, freq: f),
				(x: x + 0.015, freq: f)
			]);
			(freq: f, x: x)
		};

		// Level 1: Difference tones
		level1 = List[];
		harmonics.do { |a, i|
			harmonics.do { |b, j|
				if(i > j) {
					var diff = (a - b).abs;
					if(diff > 50 and: { diff < 3000 }) {
						var x = 0.25 + ((i + j) / (numHarmonics * 2) * 0.2) + rrand(-0.02, 0.02);

						cloud.arcs.add([
							(x: x, freq: diff),
							(x: x + 0.01, freq: diff)
						]);

						cloud.arcs.add([
							(x: level0[i][\x], freq: a),
							(x: x, freq: diff)
						]);
						cloud.arcs.add([
							(x: level0[j][\x], freq: b),
							(x: x, freq: diff)
						]);

						level1.add((freq: diff, x: x, parents: [a, b]));
					};
				};
			};
		};

		// Level 2: Summation tones
		level2 = List[];
		harmonics[0..3].do { |a, i|
			harmonics[0..3].do { |b, j|
				if(i != j) {
					var sum = a + b;
					if(sum < 3000 and: { 0.5.coin }) {
						var x = 0.55 + rrand(0, 0.15);

						cloud.arcs.add([
							(x: x, freq: sum),
							(x: x + 0.01, freq: sum)
						]);
						cloud.arcs.add([
							(x: level0[i][\x], freq: a),
							(x: x, freq: sum)
						]);

						level2.add((freq: sum, x: x));
					};
				};
			};
		};

		// Level 3: Ring modulation
		level1.do { |diff|
			level2.do { |sum|
				if(0.3.coin) {
					var ring = sqrt(diff[\freq] * sum[\freq]);
					if(ring > 80 and: { ring < 3000 }) {
						var x = 0.78 + rrand(0, 0.15);

						cloud.arcs.add([
							(x: x, freq: ring),
							(x: x + 0.012, freq: ring)
						]);
						cloud.arcs.add([
							(x: diff[\x], freq: diff[\freq]),
							(x: x, freq: ring)
						]);
					};
				};
			};
		};

		cloud.metadata[\type] = \spectralEchelon;
		^cloud
	}

	// Echelon with Morphisms (transposition, inversion)
	*echelonMorphism { |params|
		var cloud = NUPICCloud((density: 0));
		var baseFreqs = params[\baseFreqs] ?? [200, 350, 500];
		var transpositionRatio = params[\transposition] ?? 1.5;
		var inversionCenter = params[\inversionCenter] ?? 400;

		var buildEchelon = { |freqs, startX|
			var localArcs = List[];
			var level0 = freqs.collect { |f, i|
				var x = startX + (i * 0.03);
				localArcs.add([
					(x: x, freq: f),
					(x: x + 0.01, freq: f)
				]);
				(freq: f, x: x)
			};

			freqs.do { |a, i|
				freqs.do { |b, j|
					if(i < j) {
						var prod = sqrt(a * b);
						var x = startX + 0.1 + ((i + j) * 0.02);

						localArcs.add([
							(x: level0[i][\x], freq: a),
							(x: x, freq: prod)
						]);
						localArcs.add([
							(x: level0[j][\x], freq: b),
							(x: x, freq: prod)
						]);
						localArcs.add([
							(x: x, freq: prod),
							(x: x + 0.01, freq: prod)
						]);
					};
				};
			};

			localArcs
		};

		cloud.arcs.clear;

		// Original
		cloud.arcs.addAll(buildEchelon.(baseFreqs, 0.02));

		// Transposed
		cloud.arcs.addAll(buildEchelon.(baseFreqs.collect(_ * transpositionRatio), 0.35));

		// Inverted
		cloud.arcs.addAll(buildEchelon.(baseFreqs.collect { |f| inversionCenter * inversionCenter / f }, 0.68));

		// Morphism arrows
		baseFreqs.size.do { |i|
			var origX = 0.02 + (i * 0.03);
			var transX = 0.35 + (i * 0.03);
			var origF = baseFreqs[i];
			var transF = origF * transpositionRatio;

			5.do { |j|
				var t = (j + 0.5) / 5;
				var x1 = origX.blend(transX, t);
				var x2 = origX.blend(transX, t + 0.15);
				var f1 = origF.blend(transF, t);
				var f2 = origF.blend(transF, t + 0.15);

				if(f1 > 80 and: { f2 < 4000 }) {
					cloud.arcs.add([
						(x: x1, freq: f1),
						(x: x2.min(transX), freq: f2.min(transF))
					]);
				};
			};
		};

		cloud.metadata[\type] = \echelonMorphism;
		^cloud
	}

	// Recursive Echelon: P(P(P(E)))
	*recursiveEchelon { |params|
		var cloud = NUPICCloud((density: 0));
		var baseE = params[\baseSet] ?? [150, 300, 600];
		var numLevels = params[\levels] ?? 5;
		var maxPerLevel = params[\maxPerLevel] ?? 8;
		var levels = List[];

		cloud.arcs.clear;

		// Level 0: E
		levels.add(baseE.collect { |f| (freq: f, level: 0) });

		// Build levels through powerset operation
		(numLevels - 1).do { |levelNum|
			var prevLevel = levels[levelNum];
			var nextLevel = List[];

			maxPerLevel.do {
				var numParents = rrand(1, prevLevel.size.min(3));
				var parents = prevLevel.scramble.keep(numParents);
				var newFreq = parents.collect(_[\freq]).product ** (1 / numParents);
				newFreq = newFreq * rrand(0.9, 1.1);

				if(newFreq > 80 and: { newFreq < 4000 }) {
					nextLevel.add((
						freq: newFreq,
						level: levelNum + 1,
						parents: parents
					));
				};
			};

			levels.add(nextLevel.asArray);
		};

		// Visualize
		levels.do { |level, levelNum|
			var timeBase = levelNum / levels.size * 0.85 + 0.05;

			level.do { |elem, i|
				var x = timeBase + (i / level.size.max(1) * 0.12);
				var freq = elem[\freq];

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.008, freq: freq)
				]);

				elem[\x] = x;

				if(elem[\parents].notNil) {
					elem[\parents].do { |parent|
						if(parent[\x].notNil) {
							cloud.arcs.add([
								(x: parent[\x], freq: parent[\freq]),
								(x: x, freq: freq)
							]);
						};
					};
				};
			};
		};

		cloud.metadata[\type] = \recursiveEchelon;
		cloud.metadata[\numLevels] = levels.size;
		^cloud
	}

	// Echelon with Sieve Filtering
	*sieveEchelon { |params|
		var cloud = NUPICCloud((density: 0));
		var baseFreqs = params[\baseFreqs] ?? [100, 200, 400, 800];
		var sieveModulo = params[\modulo] ?? 3;
		var sieveResidue = params[\residue] ?? 1;
		var elements = List[];
		var filtered, unfiltered;

		var sieve = { |n, modulo, residue| (n % modulo) == residue };

		cloud.arcs.clear;

		// Build echelon
		baseFreqs.do { |a, i|
			baseFreqs.do { |b, j|
				if(i <= j) {
					var newFreq = sqrt(a * b);
					elements.add((freq: newFreq, idx: elements.size, parents: [i, j]));
				};
			};
		};

		elements.copy.do { |elem|
			baseFreqs.do { |base|
				var combined = sqrt(elem[\freq] * base);
				elements.add((freq: combined, idx: elements.size, parentElem: elem));
			};
		};

		// Apply sieve
		filtered = elements.select { |elem| sieve.(elem[\idx], sieveModulo, sieveResidue) };
		unfiltered = elements.reject { |elem| sieve.(elem[\idx], sieveModulo, sieveResidue) };

		// Draw filtered (prominent)
		filtered.do { |elem, i|
			var x = 0.05 + (i / filtered.size * 0.6);
			var freq = elem[\freq].clip(80, 4000);

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: x + 0.03, freq: freq)
			]);
		};

		// Draw unfiltered (faint)
		unfiltered.do { |elem, i|
			var x = 0.7 + (i / unfiltered.size.max(1) * 0.25);
			var freq = elem[\freq].clip(80, 4000);

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: x + 0.005, freq: freq * 1.01)
			]);
		};

		cloud.metadata[\type] = \sieveEchelon;
		cloud.metadata[\kept] = filtered.size;
		cloud.metadata[\total] = elements.size;
		^cloud
	}

	// ================================================================
	// ORDINAL CONSTRUCTIONS
	// ================================================================

	// Basic Ordinal Spine with Branching
	*ordinalSpine { |params|
		var cloud = NUPICCloud((density: 0));
		var numOrdinals = params[\numOrdinals] ?? 12;
		var baseFreq = params[\baseFreq] ?? 150;
		var freqMult = params[\freqMult] ?? 1.25;
		var spineFreqs = Array.geom(numOrdinals, baseFreq, freqMult);

		cloud.arcs.clear;

		// Main trunk
		(numOrdinals - 1).do { |i|
			var x1 = i / numOrdinals;
			var x2 = (i + 1) / numOrdinals;

			cloud.arcs.add([
				(x: x1, freq: spineFreqs[i]),
				(x: x2, freq: spineFreqs[i + 1])
			]);
		};

		// Branches at each ordinal
		numOrdinals.do { |i|
			var x = i / numOrdinals;
			var baseF = spineFreqs[i];
			var numBranches = (i + 1).min(5);

			numBranches.do {
				var branchFreq = baseF * rrand(0.5, 2.0);
				var branchX = x + rrand(0.02, 0.06);

				cloud.arcs.add([
					(x: x, freq: baseF),
					(x: branchX.min(1), freq: branchFreq.clip(80, 4500))
				]);

				if(0.3.coin) {
					var subFreq = branchFreq * rrand(0.7, 1.4);
					cloud.arcs.add([
						(x: branchX, freq: branchFreq),
						(x: (branchX + rrand(0.01, 0.03)).min(1), freq: subFreq.clip(80, 4500))
					]);
				};
			};
		};

		cloud.metadata[\type] = \ordinalSpine;
		^cloud
	}

	// Successor and Limit Ordinals: finite → ω → ω+n → ω·2 → ω²
	*successorLimitOrdinals { |params|
		var cloud = NUPICCloud((density: 0));
		var numFinite = params[\numFinite] ?? 8;
		var fundamental = params[\fundamental] ?? 120;
		var finitePositions, omegaPos, omega2Pos;

		cloud.arcs.clear;

		// Finite ordinals (successors)
		finitePositions = numFinite.collect { |n|
			var x = n / numFinite * 0.25 + 0.02;
			var freq = fundamental * (n + 1);

			cloud.arcs.add([
				(x: x, freq: freq.clip(80, 4000)),
				(x: x + 0.02, freq: freq.clip(80, 4000))
			]);

			if(n > 0) {
				var prevX = (n - 1) / numFinite * 0.25 + 0.02;
				var prevFreq = fundamental * n;
				cloud.arcs.add([
					(x: prevX + 0.02, freq: prevFreq),
					(x: x, freq: freq.clip(80, 4000))
				]);
			};

			(x: x, freq: freq, ordinal: n)
		};

		// First limit ordinal: ω
		{
			var omegaX = 0.35;
			var omegaFreq = 800;

			finitePositions.do { |pos, i|
				var intensity = (i / numFinite) ** 2;
				if(intensity > 0.1) {
					cloud.arcs.add([
						(x: pos[\x] + 0.02, freq: pos[\freq].clip(80, 4000)),
						(x: omegaX, freq: omegaFreq)
					]);
				};
			};

			cloud.arcs.add([
				(x: omegaX, freq: omegaFreq),
				(x: omegaX + 0.04, freq: omegaFreq)
			]);

			omegaPos = (x: omegaX, freq: omegaFreq);
		}.value;

		// ω + n successors
		4.do { |n|
			var x = 0.42 + (n * 0.04);
			var freq = 800 + (n + 1) * 80;

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: x + 0.015, freq: freq)
			]);

			if(n == 0) {
				cloud.arcs.add([
					(x: omegaPos[\x] + 0.04, freq: omegaPos[\freq]),
					(x: x, freq: freq)
				]);
			} {
				var prevX = 0.42 + ((n - 1) * 0.04);
				var prevFreq = 800 + n * 80;
				cloud.arcs.add([
					(x: prevX + 0.015, freq: prevFreq),
					(x: x, freq: freq)
				]);
			};
		};

		// ω · 2
		{
			var omega2X = 0.62;
			var omega2Freq = 1400;

			4.do { |n|
				var srcX = 0.42 + (n * 0.04);
				var srcFreq = 800 + (n + 1) * 80;

				cloud.arcs.add([
					(x: srcX + 0.015, freq: srcFreq),
					(x: omega2X, freq: omega2Freq)
				]);
			};

			cloud.arcs.add([
				(x: omega2X, freq: omega2Freq),
				(x: omega2X + 0.04, freq: omega2Freq)
			]);

			omega2Pos = (x: omega2X, freq: omega2Freq);
		}.value;

		// ω²
		{
			var omegaSqX = 0.85;
			var omegaSqFreq = 2200;

			cloud.arcs.add([
				(x: omega2Pos[\x] + 0.04, freq: omega2Pos[\freq]),
				(x: omegaSqX, freq: omegaSqFreq)
			]);

			cloud.arcs.add([
				(x: omegaSqX, freq: omegaSqFreq),
				(x: omegaSqX + 0.06, freq: omegaSqFreq)
			]);
		}.value;

		cloud.metadata[\type] = \successorLimitOrdinals;
		^cloud
	}

	// Ordinal Arithmetic: 1+ω=ω, ω+1≠ω, ω·2
	*ordinalArithmetic { |params|
		var cloud = NUPICCloud((density: 0));
		var baseFreq = params[\baseFreq] ?? 200;
		var omegaFreq = params[\omegaFreq] ?? 600;

		cloud.arcs.clear;

		// 1 + ω = ω
		{
			var startX = 0.02;

			cloud.arcs.add([
				(x: startX, freq: baseFreq),
				(x: startX + 0.02, freq: baseFreq)
			]);

			8.do { |i|
				var x = startX + 0.05 + (i * 0.02);
				var freq = baseFreq + (i * 30);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.01, freq: freq)
				]);
			};

			cloud.arcs.add([
				(x: startX + 0.23, freq: omegaFreq),
				(x: startX + 0.28, freq: omegaFreq)
			]);

			cloud.arcs.add([
				(x: startX + 0.02, freq: baseFreq),
				(x: startX + 0.23, freq: omegaFreq)
			]);
		}.value;

		// ω + 1 ≠ ω
		{
			var startX = 0.38;

			8.do { |i|
				var x = startX + (i * 0.02);
				var freq = baseFreq + (i * 30);

				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.01, freq: freq)
				]);
			};

			cloud.arcs.add([
				(x: startX + 0.18, freq: omegaFreq),
				(x: startX + 0.22, freq: omegaFreq)
			]);

			cloud.arcs.add([
				(x: startX + 0.26, freq: omegaFreq + 100),
				(x: startX + 0.30, freq: omegaFreq + 100)
			]);

			cloud.arcs.add([
				(x: startX + 0.22, freq: omegaFreq),
				(x: startX + 0.26, freq: omegaFreq + 100)
			]);
		}.value;

		// ω · 2
		{
			var startX = 0.72;

			4.do { |i|
				var x = startX + (i * 0.015);
				var freq = 300 + (i * 40);
				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.008, freq: freq)
				]);
			};
			cloud.arcs.add([
				(x: startX + 0.07, freq: 500),
				(x: startX + 0.09, freq: 500)
			]);

			4.do { |i|
				var x = startX + 0.11 + (i * 0.015);
				var freq = 550 + (i * 40);
				cloud.arcs.add([
					(x: x, freq: freq),
					(x: x + 0.008, freq: freq)
				]);
			};
			cloud.arcs.add([
				(x: startX + 0.18, freq: 750),
				(x: startX + 0.20, freq: 750)
			]);

			cloud.arcs.add([
				(x: startX + 0.09, freq: 500),
				(x: startX + 0.11, freq: 550)
			]);
		}.value;

		cloud.metadata[\type] = \ordinalArithmetic;
		^cloud
	}

	// Cantor Normal Form Spine
	*cantorNormalForm { |params|
		var cloud = NUPICCloud((density: 0));
		var spines = params[\spines] ?? [
			[3, 1, 200],  // ω³
			[2, 2, 400],  // ω²·2
			[1, 3, 700],  // ω·3
			[0, 5, 1100]  // 5
		];

		cloud.arcs.clear;

		spines.do { |spine, spineIdx|
			var exp = spine[0];
			var coef = spine[1];
			var baseFreq = spine[2];
			var spineX = spineIdx / spines.size * 0.85 + 0.05;
			var spineWidth = 0.18;
			var numPoints = (exp + 1) * 3;
			var freqRange = baseFreq * (1.5 ** exp);

			numPoints.do { |i|
				var t = i / numPoints;
				var x = spineX + (t * spineWidth);
				var freq = baseFreq + (freqRange * (t ** (exp + 1)));

				if(i > 0) {
					var prevT = (i - 1) / numPoints;
					var prevX = spineX + (prevT * spineWidth);
					var prevFreq = baseFreq + (freqRange * (prevT ** (exp + 1)));

					cloud.arcs.add([
						(x: prevX, freq: prevFreq.clip(80, 4000)),
						(x: x, freq: freq.clip(80, 4000))
					]);
				};
			};

			coef.do {
				var branchX = spineX + (spineWidth * rrand(0.3, 0.9));
				var branchT = (branchX - spineX) / spineWidth;
				var branchBaseFreq = baseFreq + (freqRange * (branchT ** (exp + 1)));
				var branchFreq = branchBaseFreq * rrand(0.6, 1.6);

				cloud.arcs.add([
					(x: branchX, freq: branchBaseFreq.clip(80, 4000)),
					(x: branchX + rrand(0.02, 0.04), freq: branchFreq.clip(80, 4000))
				]);
			};
		};

		// Connect spines
		(spines.size - 1).do { |i|
			var spine1 = spines[i];
			var spine2 = spines[i + 1];
			var x1 = (i + 1) / spines.size * 0.85 + 0.05;
			var x2 = x1;
			var freq1 = spine1[2] + (spine1[2] * (1.5 ** spine1[0]));
			var freq2 = spine2[2];

			cloud.arcs.add([
				(x: x1, freq: freq1.clip(80, 4000)),
				(x: x2 + 0.02, freq: freq2.clip(80, 4000))
			]);
		};

		cloud.metadata[\type] = \cantorNormalForm;
		^cloud
	}

	// Transfinite Recursion Patterns
	*transfiniteRecursion { |params|
		var cloud = NUPICCloud((density: 0));
		var baseFreq = params[\baseFreq] ?? 300;
		var baseDur = params[\baseDur] ?? 0.03;
		var numIterations = params[\iterations] ?? 12;
		var levels = List[];

		cloud.arcs.clear;

		// F(0): base case
		levels.add([
			(x: 0.02, freq: baseFreq, dur: baseDur)
		]);
		cloud.arcs.add([
			(x: 0.02, freq: baseFreq),
			(x: 0.02 + baseDur, freq: baseFreq)
		]);

		// F(n+1): successor rule
		numIterations.do { |n|
			var prevLevel = levels[n];
			var nextLevel = List[];
			var timeOffset = (n + 1) * 0.07 + 0.05;
			var limitFreq, limitX;

			prevLevel.do { |elem|
				var newX = timeOffset + (elem[\x] - prevLevel[0][\x]) * 0.8;
				var harmFreq;

				nextLevel.add((x: newX, freq: elem[\freq], dur: elem[\dur] * 0.9));

				harmFreq = elem[\freq] * (n + 2) / (n + 1);
				if(harmFreq < 4000) {
					nextLevel.add((x: newX + 0.01, freq: harmFreq, dur: elem[\dur] * 0.7));
				};
			};

			nextLevel = nextLevel.keep(8);
			levels.add(nextLevel.asArray);

			nextLevel.do { |elem|
				cloud.arcs.add([
					(x: elem[\x], freq: elem[\freq]),
					(x: elem[\x] + elem[\dur], freq: elem[\freq])
				]);
			};

			// Limit consolidation every 4th step
			if((n + 1) % 4 == 0) {
				limitFreq = nextLevel.collect(_[\freq]).sum / nextLevel.size;
				limitX = timeOffset + 0.04;

				nextLevel.do { |elem|
					cloud.arcs.add([
						(x: elem[\x] + elem[\dur], freq: elem[\freq]),
						(x: limitX, freq: limitFreq)
					]);
				};

				cloud.arcs.add([
					(x: limitX, freq: limitFreq),
					(x: limitX + 0.025, freq: limitFreq)
				]);
			};
		};

		cloud.metadata[\type] = \transfiniteRecursion;
		^cloud
	}

	// Epsilon Numbers: self-similar fixed points
	*epsilonNumbers { |params|
		var cloud = NUPICCloud((density: 0));
		var maxDepth = params[\depth] ?? 4;
		var baseFreq = params[\baseFreq] ?? 150;
		var width = params[\width] ?? 0.35;

		var epsilon0 = { |depth, x, freq, w, cloudRef|
			if(depth > 0 and: { freq < 4000 } and: { freq > 80 }) {
				cloudRef.arcs.add([
					(x: x, freq: freq),
					(x: x + w, freq: freq * 1.3)
				]);

				3.do { |i|
					var branchX = x + (w * (i + 1) / 4);
					var branchFreq = freq * (1 + (i * 0.15));
					var branchWidth = w * 0.4;

					epsilon0.(depth - 1, branchX, branchFreq, branchWidth, cloudRef);
				};
			};
		};

		cloud.arcs.clear;

		// Build ε₀
		epsilon0.(maxDepth, 0.02, baseFreq, width, cloud);

		// Second ε₀ at higher register (ε₁ analogy)
		epsilon0.(maxDepth - 1, 0.55, 800, 0.25, cloud);

		// Connect them
		cloud.arcs.add([
			(x: 0.37, freq: baseFreq * (1.3 ** 3)),
			(x: 0.55, freq: 800)
		]);

		cloud.metadata[\type] = \epsilonNumbers;
		^cloud
	}

	// Multi-Spine Ordinal Weave
	*multiSpineWeave { |params|
		var cloud = NUPICCloud((density: 0));
		var numSpines = params[\numSpines] ?? 4;
		var numOrdinals = params[\numOrdinals] ?? 16;
		var spineColors = params[\spineColors] ?? [
			(base: 150, mult: 1.2, phase: 0),
			(base: 220, mult: 1.18, phase: 0.1),
			(base: 330, mult: 1.15, phase: 0.2),
			(base: 500, mult: 1.12, phase: 0.05)
		];
		var spinePositions;

		cloud.arcs.clear;
		spinePositions = numSpines.collect { List[] };

		// Build each spine
		numSpines.do { |s|
			var color = spineColors[s];
			var prevX, prevFreq;

			numOrdinals.do { |i|
				var x = (i / numOrdinals * 0.85) + color[\phase].fold(0, 0.1) + 0.02;
				var freq = color[\base] * (color[\mult] ** i);

				x = x + (sin(i * 1.5 + (s * 2)) * 0.02);
				freq = freq * (1 + (sin(i * 0.8 + (s * 3)) * 0.1));
				freq = freq.clip(80, 4000);

				spinePositions[s].add((x: x, freq: freq, ordinal: i));

				if(i > 0) {
					cloud.arcs.add([
						(x: prevX, freq: prevFreq),
						(x: x, freq: freq)
					]);
				};

				prevX = x;
				prevFreq = freq;
			};
		};

		// Cross-spine connections
		numOrdinals.do { |i|
			if(i % 4 == 0 and: { i > 0 }) {
				(numSpines - 1).do { |s|
					var pos1 = spinePositions[s][i];
					var pos2 = spinePositions[s + 1][i];

					cloud.arcs.add([
						(x: pos1[\x], freq: pos1[\freq]),
						(x: pos2[\x], freq: pos2[\freq])
					]);
				};
			};
		};

		// Limit convergence
		{
			var limitX = 0.92;
			var limitFreq = spineColors.collect(_[\base]).sum / numSpines * (1.15 ** numOrdinals);
			limitFreq = limitFreq.clip(80, 4000);

			numSpines.do { |s|
				var lastPos = spinePositions[s].last;
				cloud.arcs.add([
					(x: lastPos[\x], freq: lastPos[\freq]),
					(x: limitX, freq: limitFreq)
				]);
			};

			cloud.arcs.add([
				(x: limitX, freq: limitFreq),
				(x: limitX + 0.05, freq: limitFreq)
			]);
		}.value;

		cloud.metadata[\type] = \multiSpineWeave;
		^cloud
	}

	// Ordinal Collapse
	*ordinalCollapse { |params|
		var cloud = NUPICCloud((density: 0));
		var largeRange = params[\largeRange] ?? [80, 4000];
		var collapsedRange = params[\collapsedRange] ?? [300, 600];
		var numPoints = params[\numPoints] ?? 20;
		var uncollapsed = List[];
		var collapsed = List[];

		cloud.arcs.clear;

		// Phase 1: Expansive (uncollapsed)
		numPoints.do { |i|
			var x = 0.02 + (i / numPoints * 0.35);
			var freq = exprand(largeRange[0], largeRange[1]);

			uncollapsed.add((x: x, freq: freq));

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: x + 0.01, freq: freq)
			]);
		};

		uncollapsed.scramble.clump(2).do { |pair|
			if(pair.size == 2) {
				cloud.arcs.add([
					(x: pair[0][\x], freq: pair[0][\freq]),
					(x: pair[1][\x], freq: pair[1][\freq])
				]);
			};
		};

		// Phase 2: Collapse
		uncollapsed.do { |pt, i|
			var newX = 0.45 + (i / numPoints * 0.25);
			var normalized = pt[\freq].explin(largeRange[0], largeRange[1], 0, 1);
			var newFreq = normalized.linexp(0, 1, collapsedRange[0], collapsedRange[1]);

			collapsed.add((x: newX, freq: newFreq, original: pt));

			cloud.arcs.add([
				(x: newX, freq: newFreq),
				(x: newX + 0.008, freq: newFreq)
			]);

			cloud.arcs.add([
				(x: pt[\x] + 0.01, freq: pt[\freq]),
				(x: newX, freq: newFreq)
			]);
		};

		// Phase 3: Post-collapse ordered
		collapsed.sort { |a, b| a[\freq] < b[\freq] };
		collapsed.do { |pt, i|
			var x = 0.75 + (i / numPoints * 0.2);
			var freq = pt[\freq];

			cloud.arcs.add([
				(x: x, freq: freq),
				(x: x + 0.012, freq: freq)
			]);

			if(i > 0) {
				var prevPt = collapsed[i - 1];
				var prevX = 0.75 + ((i - 1) / numPoints * 0.2);
				cloud.arcs.add([
					(x: prevX + 0.012, freq: prevPt[\freq]),
					(x: x, freq: freq)
				]);
			};
		};

		cloud.metadata[\type] = \ordinalCollapse;
		^cloud
	}
}
