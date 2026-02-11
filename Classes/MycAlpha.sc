// ============================================================
// MycAlpha — Mycènes Alpha UPIC Score Shape Generator
// Generates [time, freq] point arrays for each segment
// after Iannis Xenakis (1978)
//
// Usage:
//   a = MycAlpha.segment(1);  // -> array of arcs for segment 1
//   a[0]  // -> first arc: [[t, f], [t, f], ...]
//
//   // All 13 segments:
//   MycAlpha.segments.do { |seg, i| ... };
//
//   // Individual shapes:
//   MycAlpha.cloud(tStart, tEnd, freqCenter, freqHeight, seed);
//   MycAlpha.mountain(tStart, tEnd, freqBase, freqHeight, seed);
// ============================================================

MycAlpha {

	classvar <segData;

	*initClass {
		// segment: [duration(s), startTime(s), type]
		segData = IdentityDictionary[
			1  -> [17,  0,   \sustained],
			2  -> [38,  17,  \glissando],
			3  -> [58,  55,  \sustained],
			4  -> [5,   113, \sustained],
			5  -> [55,  118, \glissando],
			6  -> [60,  173, \sustained],
			7  -> [24,  233, \sustained],
			8  -> [59,  257, \glissando],
			9  -> [60,  316, \sustained],
			10 -> [60,  376, \sustained],
			11 -> [59,  436, \glissando],
			12 -> [20,  495, \sustained],
			13 -> [61,  515, \sustained],
		];
	}

	// ── Pitch mapping ──
	// Score axis: C1=0, C2=1, C3=2, A3(440)=2.75, C4=3, C5=4, C6=5
	// Returns Hz from octave position (0–5)
	*octToFreq { |oct|
		// C1 = 32.7 Hz, each octave doubles
		^32.703 * (2 ** oct)
	}

	// Inverse
	*freqToOct { |freq|
		^(freq / 32.703).log2
	}

	// ── Turbulent noise ──
	// Layered sine noise for organic contours
	// Returns deviation in octaves
	*noise { |x, seed = 0, scale = 1|
		var n;
		n = sin(x * 0.02 + seed) * 0.28
		+ sin(x * 0.05 + (seed * 1.7)) * 0.14
		+ sin(x * 0.11 + (seed * 2.3)) * 0.07
		+ sin(x * 0.23 + (seed * 3.1)) * 0.035
		+ sin(x * 0.47 + (seed * 4.7)) * 0.018;
		^(n * scale)
	}

	// ── Wavy arc generator ──
	// Single horizontal arc with wobble, returns [[t,f], ...]
	*wavyArc { |tStart, tEnd, freq, wobbleAmt = 0.02, nPoints = 40, seed|
		var pts, step, phase, wFreq;
		seed = seed ?? { 1000.rand };
		phase = seed;
		wFreq = 0.025 + (0.035.rand);
		step = (tEnd - tStart) / nPoints;
		pts = (0..nPoints).collect { |i|
			var t = tStart + (i * step);
			var w = sin(t * wFreq * 100 + phase) * wobbleAmt;
			w = w + (wobbleAmt * 0.4 * (1.0.rand2));
			[t, freq + w]
		};
		^pts
	}

	// ── Fill between two contour functions ──
	// topFn, botFn: functions of time returning freq (octaves)
	// Returns array of wavy arcs filling the region
	*fillBetween { |tStart, tEnd, topFn, botFn, spacing = 0.03, nPointsPerArc = 40, seed|
		var arcs, fTop, fBot, fMin, fMax, f, nSamples;
		seed = seed ?? { 10000.rand };
		// Sample to find freq range
		nSamples = 50;
		fMin = inf; fMax = neg(inf);
		nSamples.do { |i|
			var t = tStart + ((tEnd - tStart) * i / nSamples);
			fTop = topFn.value(t);
			fBot = botFn.value(t);
			fMin = min(fMin, min(fTop, fBot));
			fMax = max(fMax, max(fTop, fBot));
		};
		// Generate horizontal arcs at each freq level
		arcs = List[];
		f = fMin;
		while { f <= fMax } {
			var arc, pts;
			pts = List[];
			nPointsPerArc.do { |i|
				var t = tStart + ((tEnd - tStart) * i / (nPointsPerArc - 1));
				var ft = topFn.value(t);
				var fb = botFn.value(t);
				var yMin = min(ft, fb);
				var yMax = max(ft, fb);
				if(f >= yMin and: { f <= yMax }) {
					var wobble = sin(t * (25 + (35.rand)) + seed) * spacing * 0.3;
					wobble = wobble + (spacing * 0.15 * 1.0.rand2);
					pts.add([t, f + wobble]);
				};
			};
			// Break into contiguous segments
			if(pts.size > 1) {
				var seg = List[];
				pts.do { |pt, i|
					if(i == 0) {
						seg.add(pt);
					} {
						var dt = pt[0] - pts[i-1][0];
						var step = (tEnd - tStart) / (nPointsPerArc - 1);
						if(dt > (step * 1.8)) {
							// Gap — save current segment, start new
							if(seg.size > 1) { arcs.add(seg.asArray) };
							seg = List[];
						};
						seg.add(pt);
					};
				};
				if(seg.size > 1) { arcs.add(seg.asArray) };
			};
			f = f + spacing + (spacing * 0.3).rand2;
		};
		^arcs.asArray
	}

	// ── CLOUD MASS ──
	// Turbulent blob at given center, returns array of arcs
	*cloud { |tStart, tEnd, freqCenter = 2.5, freqHeight = 0.8, seed|
		var s1, s2, topFn, botFn;
		seed = seed ?? { 10000.rand };
		s1 = seed; s2 = seed + 50;
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqCenter
			} {
				var env = sin(norm * pi);
				var turb = MycAlpha.noise(t * 100, s1, 0.6);
				freqCenter + (env * freqHeight * 0.5) + turb
			}
		};
		botFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqCenter
			} {
				var env = sin(norm * pi);
				var turb = MycAlpha.noise(t * 100, s2, 0.6);
				freqCenter - (env * freqHeight * 0.5) + turb
			}
		};
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.025)
	}

	// ── MOUNTAIN MASS ──
	// Rises from baseline with turbulent top contour
	*mountain { |tStart, tEnd, freqBase = 0.0, freqHeight = 1.5, seed|
		var s, topFn, botFn;
		seed = seed ?? { 10000.rand };
		s = seed;
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				var env = sin(norm * pi);
				var turb = MycAlpha.noise(t * 100, s, 0.5);
				var turb2 = MycAlpha.noise(t * 130, s + 20, 0.2).abs;
				freqBase + (env * freqHeight) + turb + turb2
			}
		};
		botFn = { |t| freqBase };
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.025)
	}

	// ── COLUMN MASS ──
	// Stepped rectangular mass (castle-like)
	*column { |tStart, tEnd, freqBase = 0.0, freqHeight = 2.5, nSteps = 4, seed|
		var s, stepW, topFn, botFn;
		seed = seed ?? { 10000.rand };
		s = seed;
		stepW = (tEnd - tStart) / nSteps;
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				var si = ((t - tStart) / stepW).floor;
				var stepH = freqHeight * (0.5 + (sin(si * 2.7 + s) * 0.35) + (sin(si * 4.1 + (s * 1.3)) * 0.15));
				var turb = MycAlpha.noise(t * 100, s, 0.12);
				freqBase + stepH + turb
			}
		};
		botFn = { |t| freqBase };
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.025)
	}

	// ── LOBE (biological form) ──
	// Blob with protruding lobes — returns arcs filling the shape
	*lobe { |tCenter, freqCenter, radiusT = 1.0, radiusF = 0.5, nLobes = 4, seed|
		var s, contour, tMin, tMax, fMin, fMax, topLookup, botLookup;
		seed = seed ?? { 10000.rand };
		s = seed;
		// Build polar contour
		contour = (0..100).collect { |i|
			var a = i / 100 * 2pi;
			var r = 1.0;
			nLobes.do { |l|
				var la = (l / nLobes) * 2pi + s;
				var d = ((a - la) % 2pi + 2pi % 2pi - pi).abs;
				if(d < 0.8) { r = r + ((0.8 - d) * (0.25 + (sin(l * 3 + s) * 0.15))) };
			};
			r = r + (sin(a * 5 + s) * 0.08) + (sin(a * 11 + (s * 2)) * 0.04);
			[tCenter + (cos(a) * radiusT * r), freqCenter + (sin(a) * radiusF * r)]
		};
		// Find bounding box
		tMin = contour.collect(_[0]).minItem;
		tMax = contour.collect(_[0]).maxItem;
		fMin = contour.collect(_[1]).minItem;
		fMax = contour.collect(_[1]).maxItem;
		// Fill with horizontal arcs via ray casting
		^this.fillPolygon(contour, 0.025)
	}

	// ── Fill polygon with horizontal arcs ──
	*fillPolygon { |contour, spacing = 0.025|
		var arcs, fMin, fMax, f;
		fMin = contour.collect(_[1]).minItem;
		fMax = contour.collect(_[1]).maxItem;
		arcs = List[];
		f = fMin;
		while { f <= fMax } {
			var intersections = List[];
			contour.size.do { |i|
				var p1 = contour[i];
				var p2 = contour[(i + 1) % contour.size];
				if((p1[1] <= f and: { p2[1] > f }) or: { p2[1] <= f and: { p1[1] > f } }) {
					var ix = p1[0] + ((f - p1[1]) / (p2[1] - p1[1]) * (p2[0] - p1[0]));
					intersections.add(ix);
				};
			};
			intersections = intersections.sort;
			// Pair up intersections
			(intersections.size / 2).floor.do { |i|
				var tA = intersections[i * 2];
				var tB = intersections[(i * 2) + 1];
				if((tB - tA) > 0.01) {
					arcs.add(this.wavyArc(tA, tB, f, spacing * 0.3, 20));
				};
			};
			f = f + spacing + (spacing * 0.3).rand2;
		};
		^arcs.asArray
	}

	// ── GLISSANDO LINE ──
	// Single oblique arc with wobble
	*glissando { |tStart, tEnd, freqStart, freqEnd, wobble = 0.03, nPoints = 30|
		var pts;
		pts = (0..nPoints).collect { |i|
			var norm = i / nPoints;
			var t = tStart.blend(tEnd, norm);
			var f = freqStart.blend(freqEnd, norm);
			f = f + (wobble * 1.0.rand2);
			[t, f]
		};
		^[pts]  // Wrap in array for consistency with other shape methods
	}

	// ── STEM (thick vertical line) ──
	*stem { |tCenter, freqStart, freqEnd, width = 0.5, seed|
		var topFn, botFn, tS, tE;
		seed = seed ?? { 10000.rand };
		tS = tCenter - (width / 2);
		tE = tCenter + (width / 2);
		topFn = { |t|
			var norm = (t - tS) / width;
			var fNorm = norm.clip(0, 1);
			freqStart.blend(freqEnd, fNorm) + MycAlpha.noise(t * 200, seed, 0.05)
		};
		botFn = { |t|
			var norm = (t - tS) / width;
			var fNorm = norm.clip(0, 1);
			freqStart.blend(freqEnd, fNorm) - MycAlpha.noise(t * 200, seed + 50, 0.05)
		};
		^this.fillBetween(tS, tE, topFn, botFn, 0.02, 15)
	}

	// ── TRIANGULAR WEDGE ──
	// Ascending triangle (seg 6 golden section marker)
	*wedge { |tStart, tEnd, freqBase = 0.0, freqPeak = 4.5, seed|
		var topFn, botFn;
		seed = seed ?? { 10000.rand };
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				freqBase + (norm * (freqPeak - freqBase)) + MycAlpha.noise(t * 100, seed, 0.15)
			}
		};
		botFn = { |t| freqBase };
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.025)
	}

	// ── FUNNEL ──
	// Lines converge at top then fan downward (seg 5)
	*funnel { |tStart, tEnd, freqApex = 4.0, freqSpread = 2.5, nLines = 20|
		var arcs;
		arcs = nLines.collect { |i|
			var norm = i / nLines;
			var fEnd = freqApex - (freqSpread * (0.15 + (norm * 0.85)));
			var nPts = 22;
			(0..nPts).collect { |j|
				var p = j / nPts;
				var t = tStart.blend(tStart + ((tEnd - tStart) * (0.02 + (norm * 0.25))), p);
				var f = freqApex.blend(fEnd, p);
				f = f + (sin(p * pi * (1 + (3.0.rand))) * (0.02 + (p * 0.15)));
				f = f + (0.01 + (p * 0.05)).rand2;
				[t, f]
			}
		};
		^arcs
	}

	// ── DIAMOND / HOURGLASS ──
	// Two triangular masses meeting at a point (X or bowtie shape)
	// Visible in Mycenae-Alpha seg 1 stacked forms
	*diamond { |tStart, tEnd, freqCenter = 2.5, freqSpread = 1.5, pinchPoint = 0.5, seed|
		var topFn, botFn, tMid;
		seed = seed ?? { 10000.rand };
		tMid = tStart + ((tEnd - tStart) * pinchPoint);
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			var env;
			if(norm < 0 or: { norm > 1 }) {
				freqCenter
			} {
				env = if(norm < pinchPoint) {
					1.0 - (norm / pinchPoint)
				} {
					(norm - pinchPoint) / (1.0 - pinchPoint)
				};
				freqCenter + (env * freqSpread) + MycAlpha.noise(t * 100, seed, 0.08)
			}
		};
		botFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			var env;
			if(norm < 0 or: { norm > 1 }) {
				freqCenter
			} {
				env = if(norm < pinchPoint) {
					1.0 - (norm / pinchPoint)
				} {
					(norm - pinchPoint) / (1.0 - pinchPoint)
				};
				freqCenter - (env * freqSpread) + MycAlpha.noise(t * 100, seed + 50, 0.08)
			}
		};
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.025)
	}

	// ── SWARM / DISPERSAL ──
	// Dense chaotic lines that scatter outward (like birds dispersing)
	// Visible in seg 1 rightward dispersal, seg 2
	*swarm { |tStart, tEnd, freqCenter = 3.0, freqSpread = 1.5, nLines = 25, disperseDir = 1, seed|
		var arcs;
		seed = seed ?? { 10000.rand };
		arcs = nLines.collect { |i|
			var norm = i / nLines;
			var fStart = freqCenter + (freqSpread * (norm - 0.5) * 2);
			var fEnd = fStart + (disperseDir * freqSpread * (0.3 + norm) * 1.0.rand2);
			var tEndVar = tEnd + ((tEnd - tStart) * 0.3.rand2);
			var wobble = 0.02 + (norm * 0.08);
			var nPts = 20 + 15.rand;
			thisThread.randSeed = seed + i;
			(0..nPts).collect { |j|
				var p = j / nPts;
				var t = tStart.blend(tEndVar, p);
				var f = fStart.blend(fEnd, p);
				// Add increasing chaos toward end
				f = f + (wobble * p * 3 * 1.0.rand2);
				f = f + (sin(p * pi * (2 + 4.rand)) * 0.05);
				[t, f]
			}
		};
		^arcs
	}

	// ── TENDRIL / VINE ──
	// Curling organic lines that branch and spiral at ends
	// Visible in Mycenae-Alpha seg 8 organic forms
	*tendril { |tStart, tEnd, freqStart, freqEnd, curlAmount = 0.5, nCurls = 3, seed|
		var pts, nPts;
		seed = seed ?? { 10000.rand };
		nPts = 35;
		pts = (0..nPts).collect { |i|
			var norm = i / nPts;
			var t = tStart.blend(tEnd, norm);
			var f = freqStart.blend(freqEnd, norm);
			// Add curling that increases toward the end
			var curlPhase = norm * nCurls * 2pi + seed;
			var curlEnv = norm * norm * curlAmount; // Grows quadratically
			f = f + (sin(curlPhase) * curlEnv);
			t = t + (cos(curlPhase) * curlEnv * 0.3 * (tEnd - tStart));
			[t, f]
		};
		^[pts]
	}

	// ── TREE / BRANCHING ──
	// Vertical stem with organic bulbous growths branching off
	// Visible in Mycenae-Alpha seg 8
	*tree { |tCenter, freqBase = 0.0, freqTop = 4.0, trunkWidth = 0.8, nBranches = 4, seed|
		var arcs = List[];
		var trunkArcs;
		seed = seed ?? { 10000.rand };
		// Main trunk (stem)
		trunkArcs = this.stem(tCenter, freqBase, freqTop, trunkWidth, seed);
		arcs.addAll(trunkArcs);
		// Branches with lobed ends
		nBranches.do { |b|
			var branchNorm = (b + 1) / (nBranches + 1);
			var branchFreq = freqBase.blend(freqTop, branchNorm);
			var branchDir = [-1, 1].choose;
			var branchLen = 1.0 + 2.0.rand;
			var branchEnd = tCenter + (branchDir * branchLen);
			var branchEndFreq = branchFreq + (0.8.rand2);
			// Branch line
			arcs.addAll(this.tendril(
				tCenter + (branchDir * trunkWidth * 0.3),
				branchEnd,
				branchFreq,
				branchEndFreq,
				0.3 + 0.4.rand,
				2 + 2.rand,
				seed + b
			));
			// Bulbous node at branch end
			if(0.7.coin) {
				arcs.addAll(this.lobe(
					branchEnd,
					branchEndFreq,
					0.5 + 1.0.rand,
					0.2 + 0.3.rand,
					3 + 3.rand,
					seed + b + 100
				));
			};
		};
		// Top crown (larger lobe)
		arcs.addAll(this.lobe(tCenter, freqTop + 0.2.rand, 1.5 + 1.0.rand, 0.4 + 0.3.rand, 4 + 2.rand, seed + 200));
		^arcs.asArray
	}

	// ── CASCADE / WATERFALL ──
	// Dense parallel descending lines (like water falling)
	// Visible in eua'on (1980) left side
	*cascade { |tStart, tEnd, freqTop = 4.5, freqBottom = 0.5, nLines = 30, density = 1.0, seed|
		var arcs;
		seed = seed ?? { 10000.rand };
		arcs = nLines.collect { |i|
			var norm = i / nLines;
			var tOffset = (tEnd - tStart) * 0.02 * norm.rand2;
			var fTop = freqTop + (0.2.rand2);
			var fBot = freqBottom + (0.3.rand2);
			var nPts = (20 * density).asInteger + 10.rand;
			thisThread.randSeed = seed + i;
			(0..nPts).collect { |j|
				var p = j / nPts;
				var t = (tStart + tOffset).blend(tEnd + tOffset, p);
				var f = fTop.blend(fBot, p);
				// Small horizontal wobble
				f = f + (0.03 * 1.0.rand2);
				t = t + ((tEnd - tStart) * 0.01 * 1.0.rand2);
				[t, f]
			}
		};
		^arcs
	}

	// ── BOULDER ──
	// Large irregular rounded rock-like mass with internal striations
	// Visible throughout Mycenae-Alpha, particularly segs 11, 13
	*boulder { |tStart, tEnd, freqBase = 0.0, freqHeight = 2.5, irregularity = 0.4, seed|
		var topFn, botFn, s1, s2;
		seed = seed ?? { 10000.rand };
		s1 = seed; s2 = seed + 77;
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				// Irregular rocky top contour
				var base = sin(norm * pi);
				var irreg = MycAlpha.noise(t * 50, s1, irregularity);
				var irreg2 = MycAlpha.noise(t * 120, s1 + 30, irregularity * 0.5);
				var bump = sin(norm * pi * (3 + (sin(s1) * 2).abs)) * irregularity * 0.3;
				freqBase + (base * freqHeight) + irreg + irreg2 + bump
			}
		};
		botFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				// Slightly irregular bottom
				var irreg = MycAlpha.noise(t * 80, s2, irregularity * 0.15);
				freqBase + irreg
			}
		};
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.02)
	}

	// ── RIBBON / WAVE BAND ──
	// Thick undulating band that flows horizontally
	// Common texture in UPIC scores
	*ribbon { |tStart, tEnd, freqCenter = 2.5, thickness = 0.4, waveAmp = 0.3, waveFreq = 2.0, seed|
		var topFn, botFn;
		seed = seed ?? { 10000.rand };
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			var wave = sin(norm * 2pi * waveFreq + seed) * waveAmp;
			var turb = MycAlpha.noise(t * 80, seed, 0.1);
			freqCenter + (thickness / 2) + wave + turb
		};
		botFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			var wave = sin(norm * 2pi * waveFreq + seed) * waveAmp;
			var turb = MycAlpha.noise(t * 80, seed + 50, 0.1);
			freqCenter - (thickness / 2) + wave + turb
		};
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.02)
	}

	// ── STALACTITE / ICICLE ──
	// Downward-pointing narrow mass (drip shape)
	// Inverse of wedge
	*stalactite { |tStart, tEnd, freqTop = 4.0, freqTip = 1.0, tipPos = 0.5, seed|
		var topFn, botFn, tTip;
		seed = seed ?? { 10000.rand };
		tTip = tStart + ((tEnd - tStart) * tipPos);
		topFn = { |t|
			var turb = MycAlpha.noise(t * 100, seed, 0.1);
			freqTop + turb
		};
		botFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqTop
			} {
				var env = if(norm < tipPos) {
					norm / tipPos
				} {
					1.0 - ((norm - tipPos) / (1.0 - tipPos))
				};
				var depth = freqTop - freqTip;
				var turb = MycAlpha.noise(t * 100, seed + 50, 0.08);
				freqTop - (env * depth) + turb
			}
		};
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.02)
	}

	// ── ARCHIPELAGO ──
	// Scattered small fragment islands
	// Common sparse texture in UPIC scores
	*archipelago { |tStart, tEnd, freqMin = 0.5, freqMax = 4.5, nIslands = 8, sizeRange = #[0.3, 1.2], seed|
		var arcs = List[];
		seed = seed ?? { 10000.rand };
		thisThread.randSeed = seed;
		nIslands.do { |i|
			var tS = tStart + ((tEnd - tStart) * (i / nIslands)) + ((tEnd - tStart) * 0.08.rand2);
			var dur = sizeRange[0] + ((sizeRange[1] - sizeRange[0]) * 1.0.rand);
			var tE = tS + dur;
			var fCenter = freqMin + ((freqMax - freqMin) * 1.0.rand);
			var fHeight = 0.15 + (0.25.rand);
			var shapeType = 3.rand;
			// Call each shape with correct argument order
			case
			{ shapeType == 0 } { arcs.addAll(this.cloud(tS, tE, fCenter, fHeight, seed + i)) }
			{ shapeType == 1 } { arcs.addAll(this.mountain(tS, tE, fCenter - (fHeight/2), fHeight, seed + i)) }
			{ shapeType == 2 } { arcs.addAll(this.boulder(tS, tE, fCenter - (fHeight/2), fHeight, 0.3, seed + i)) };
		};
		^arcs.asArray
	}

	// ── SPIRE / TOWER ──
	// Tall thin triangular tower (like seg 6 golden section marker)
	// Denser and more vertical than wedge
	*spire { |tStart, tEnd, freqBase = 0.0, freqPeak = 5.0, asymmetry = 0.7, seed|
		var topFn, botFn, tPeak;
		seed = seed ?? { 10000.rand };
		tPeak = tStart + ((tEnd - tStart) * asymmetry);
		topFn = { |t|
			var norm = (t - tStart) / (tEnd - tStart);
			if(norm < 0 or: { norm > 1 }) {
				freqBase
			} {
				var env = if(norm < asymmetry) {
					norm / asymmetry
				} {
					1.0 - ((norm - asymmetry) / (1.0 - asymmetry))
				};
				var turb = MycAlpha.noise(t * 150, seed, 0.12);
				freqBase + (env * (freqPeak - freqBase)) + turb
			}
		};
		botFn = { |t| freqBase };
		^this.fillBetween(tStart, tEnd, topFn, botFn, 0.018)
	}

	// ── VORTEX / SPIRAL ──
	// Lines converging to a point in a spiral pattern
	// Visible in seg 5 funnel variant
	*vortex { |tCenter, freqCenter = 2.5, radius = 2.0, nSpirals = 15, rotations = 1.5, seed|
		var arcs;
		seed = seed ?? { 10000.rand };
		arcs = nSpirals.collect { |i|
			var startAngle = (i / nSpirals) * 2pi;
			var nPts = 30;
			(0..nPts).collect { |j|
				var norm = j / nPts;
				var angle = startAngle + (norm * rotations * 2pi);
				var r = radius * (1.0 - norm); // Spiral inward
				var t = tCenter + (cos(angle) * r);
				var f = freqCenter + (sin(angle) * r * 0.5);
				// Add slight wobble
				f = f + (0.02 * 1.0.rand2);
				[t, f]
			}
		};
		^arcs
	}

	// ── STRATA / LAYERED BANDS ──
	// Horizontal layered bands with gaps (geological strata)
	*strata { |tStart, tEnd, freqBase = 0.0, freqTop = 4.0, nLayers = 5, gapRatio = 0.3, seed|
		var arcs = List[];
		var layerHeight, f;
		seed = seed ?? { 10000.rand };
		layerHeight = (freqTop - freqBase) / nLayers;
		nLayers.do { |i|
			var fBot = freqBase + (i * layerHeight) + (layerHeight * gapRatio * 0.5);
			var fTop = freqBase + ((i + 1) * layerHeight) - (layerHeight * gapRatio * 0.5);
			var turb1 = seed + i;
			var turb2 = seed + i + 50;
			var topFn = { |t| fTop + MycAlpha.noise(t * 60, turb1, 0.08) };
			var botFn = { |t| fBot + MycAlpha.noise(t * 60, turb2, 0.08) };
			arcs.addAll(this.fillBetween(tStart, tEnd, topFn, botFn, 0.02));
		};
		^arcs.asArray
	}

	// ── EXPLOSION / BURST ──
	// Radiating lines from a point (impact or explosion)
	*burst { |tCenter, freqCenter = 2.5, radius = 3.0, nRays = 20, seed|
		var arcs;
		seed = seed ?? { 10000.rand };
		arcs = nRays.collect { |i|
			var angle = (i / nRays) * 2pi + (0.1.rand2);
			var tEnd = tCenter + (cos(angle) * radius);
			var fEnd = freqCenter + (sin(angle) * radius * 0.6);
			var nPts = 15 + 10.rand;
			var wobble = 0.02 + 0.03.rand;
			(0..nPts).collect { |j|
				var norm = j / nPts;
				var t = tCenter.blend(tEnd, norm);
				var f = freqCenter.blend(fEnd, norm);
				f = f + (wobble * 1.0.rand2);
				[t, f]
			}
		};
		^arcs
	}

	// ============================================================
	// SEGMENT GENERATORS — after the actual score
	// All times relative to segment start (0 to duration)
	// All freqs in octaves (0=C1 to 5=C6)
	// ============================================================

	*segment { |num|
		var data = segData[num];
		var dur = data[0];
		var arcs;
		arcs = this.perform(("seg" ++ num).asSymbol, dur);
		^arcs
	}

	*segments {
		^(1..13).collect { |i| this.segment(i) }
	}

	// ── Seg 1: Cloud masses at multiple registers + dispersal ──
	*seg1 { |dur|
		var arcs = List[];
		// Cloud blobs at different octaves
		[
			[4.8, 0.6, dur * 0.08],
			[4.2, 0.8, dur * 0.10],
			[3.5, 0.65, dur * 0.07],
			[2.7, 1.0, dur * 0.12],
			[1.7, 1.1, dur * 0.11],
			[0.7, 0.65, dur * 0.09],
		].do { |spec|
			var fCenter = spec[0], fH = spec[1], tW = spec[2];
			var tS = dur * 0.05.rrand(0.15);
			arcs.addAll(this.cloud(tS, tS + tW, fCenter + 0.2.rand2, fH * 0.7.rrand(1.3)));
		};
		// Rightward dispersal — dissolving wavy lines
		15.do { |f|
			var norm = f / 18;
			var y0 = 4.5 + 0.4.rand2;
			var y1 = 4.5 + (0.3 + (norm * 1.1)).rand2;
			var tS = dur * 0.2;
			var tE = tS + (dur * (0.08 + (norm * 0.3)));
			arcs.add(this.glissando(tS, tE, y0, y1, 0.02 + (norm * 0.12), 18));
		};
		// Small fragments far right
		3.do {
			arcs.addAll(this.cloud(dur * 0.82.rrand(0.94), dur * 0.86.rrand(0.98), 3.0.rrand(5.0), 0.1.rrand(0.2)));
		};
		^arcs.asArray
	}

	// ── Seg 2: Converging lines fan outward ──
	*seg2 { |dur|
		var arcs = List[];
		var ox = dur * 0.06, oy = 4.0.rrand(4.4);
		24.do { |a|
			var norm = a / 24;
			var tE = ox + (dur * (0.3 + 0.5.rand));
			var fE = oy + (2.5 * (0.1 + norm)).rand2;
			arcs.add(this.glissando(ox + 0.5.rand, tE, oy + 0.15.rand2, fE, 0.02 + (norm * 0.03)));
		};
		^arcs.asArray
	}

	// ── Seg 3: Thin sustained baseline ──
	*seg3 { |dur|
		var arcs = List[];
		arcs.addAll(this.fillBetween(0, dur * 0.85,
			{ |t| 0.06 }, { |t| -0.03 }, 0.015));
		// Sparse fragments
		3.do {
			var tS = dur * 0.35.rrand(0.75);
			arcs.addAll(this.cloud(tS, tS + 1.0.rrand(2.5), 0.4.rrand(1.2), 0.08.rrand(0.15)));
		};
		^arcs.asArray
	}

	// ── Seg 4: Brief dense cloud patches ──
	*seg4 { |dur|
		var arcs = List[];
		4.do {
			arcs.addAll(this.cloud(
				dur * 0.2.rrand(0.8), dur * 0.25.rrand(0.85),
				0.5.rrand(4.5), 0.2.rrand(0.5)
			));
		};
		^arcs.asArray
	}

	// ── Seg 5: Dramatic downward funnel ──
	*seg5 { |dur|
		var arcs = List[];
		arcs.addAll(this.funnel(0, dur * 0.5, 4.0.rrand(4.3), 2.5, 22));
		// Dense cluster at bottom of funnel
		arcs.addAll(this.cloud(dur * 0.15, dur * 0.35, 2.0, 0.4));
		^arcs.asArray
	}

	// ── Seg 6: Baseline + ascending triangular wedge (golden section) ──
	*seg6 { |dur|
		var arcs = List[];
		// Low baseline
		arcs.addAll(this.fillBetween(0, dur * 0.65,
			{ |t| 0.05 }, { |t| -0.03 }, 0.015));
		// Small mountain masses
		2.do { |m|
			arcs.addAll(this.mountain(dur * (0.03 + (m * 0.15) + 0.04.rand),
				dur * (0.09 + (m * 0.15) + 0.1.rand), 0.0, 1.0.rrand(1.5)));
		};
		// THE ascending wedge
		arcs.addAll(this.wedge(dur * 0.70, dur * 0.94, 0.0, 4.5));
		^arcs.asArray
	}

	// ── Seg 7: Discontinuous small masses ──
	*seg7 { |dur|
		var arcs = List[];
		var ng = 5.rrand(7);
		ng.do { |g|
			var tS = (g / ng) * dur * 0.8 + (dur * 0.04.rand);
			var tE = tS + (dur * 0.02.rrand(0.06));
			arcs.addAll(this.mountain(tS, tE, 0.0, 0.25.rrand(0.6)));
		};
		^arcs.asArray
	}

	// ── Seg 8: Biological lobed forms ──
	*seg8 { |dur|
		var arcs = List[];
		var nStems;
		// Small baseline masses
		6.do { |m|
			arcs.addAll(this.mountain(dur * (0.02 + (m * 0.07) + 0.03.rand),
				dur * (0.05 + (m * 0.07) + 0.06.rand), 0.0, 0.2.rrand(0.4)));
		};
		// Vertical stems with lobed nodes
		nStems = 3.rrand(4);
		nStems.do { |s|
			var tC = dur * (0.25 + (s * 0.15) + 0.05.rand);
			var fBot = 0.3;
			var fTop = 3.5.rrand(5.0);
			var nNodes;
			arcs.addAll(this.stem(tC, fBot, fTop, 0.5.rrand(1.0)));
			// Lobed nodes along stem
			nNodes = 2.rrand(4);
			nNodes.do { |n|
				var nt = 0.15.rrand(0.85);
				var nf = fBot.blend(fTop, nt);
				arcs.addAll(this.lobe(tC + 0.3.rand2, nf, 1.0.rrand(2.5), 0.3.rrand(0.6), 3.rrand(6)));
			};
			// Top node — larger
			arcs.addAll(this.lobe(tC + 0.3.rand2, fTop + 0.1.rrand(0.4), 1.5.rrand(3.0), 0.4.rrand(0.7), 4.rrand(6)));
		};
		// Columnar mass at right
		arcs.addAll(this.column(dur * 0.7, dur * 0.82, 0.0, 2.0, 4));
		^arcs.asArray
	}

	// ── Seg 9: Turbulent mountains + columns ──
	*seg9 { |dur|
		var arcs = List[];
		// Multiple turbulent mountains in first half
		var nPeaks = 4.rrand(6);
		nPeaks.do { |p|
			var tS = dur * (0.02 + (p / nPeaks * 0.48));
			var tE = tS + (dur * 0.06.rrand(0.12));
			arcs.addAll(this.mountain(tS, tE, 0.0, 0.75.rrand(1.8)));
		};
		// Glissando connecting to column section
		arcs.add(this.glissando(dur * 0.52, dur * 0.62, 2.5, 1.5, 0.03));
		// Tall columns in right half
		arcs.addAll(this.column(dur * 0.65, dur * 0.75, 0.0, 2.75, 3.rrand(4)));
		arcs.addAll(this.column(dur * 0.78, dur * 0.86, 0.0, 3.5, 4));
		arcs.addAll(this.column(dur * 0.88, dur * 0.94, 0.0, 2.25, 3));
		^arcs.asArray
	}

	// ── Seg 10: Columnar castle-like masses ──
	*seg10 { |dur|
		var arcs = List[];
		var nCols = 3.rrand(5);
		nCols.do { |c|
			var tS = dur * (0.05 + (c * 0.2) + 0.05.rand);
			var tE = tS + (dur * 0.06.rrand(0.14));
			arcs.addAll(this.column(tS, tE, 0.0, 1.5.rrand(3.5), 3.rrand(5)));
		};
		// Low baseline
		arcs.addAll(this.fillBetween(0, dur * 0.9,
			{ |t| 0.03 }, { |t| -0.01 }, 0.015));
		^arcs.asArray
	}

	// ── Seg 11: Rock-formation masses ──
	*seg11 { |dur|
		var arcs = List[];
		// Large rock formation left
		arcs.addAll(this.mountain(dur * 0.02, dur * 0.28, 0.0, 2.0.rrand(3.0)));
		// Lobed mass upper
		arcs.addAll(this.lobe(dur * 0.15, 3.5, 2.0, 0.5, 5));
		// Connecting glissandi
		5.do {
			arcs.add(this.glissando(
				dur * 0.15.rrand(0.25), dur * 0.30.rrand(0.45),
				2.0.rrand(4.0), 1.0.rrand(4.0), 0.03));
		};
		// Low baseline masses
		4.do { |m|
			arcs.addAll(this.mountain(dur * (0.3 + (m * 0.1) + 0.04.rand),
				dur * (0.34 + (m * 0.1) + 0.08.rand), 0.0, 0.15.rrand(0.3)));
		};
		^arcs.asArray
	}

	// ── Seg 12: Small sparse mass ──
	*seg12 { |dur|
		var arcs = List[];
		arcs.addAll(this.fillBetween(dur * 0.1, dur * 0.5,
			{ |t| 0.15 }, { |t| 0.0 }, 0.02));
		arcs.add(this.glissando(dur * 0.15, dur * 0.4, 2.0, 2.2, 0.02));
		^arcs.asArray
	}

	// ── Seg 13: Tower-like rock formations ──
	*seg13 { |dur|
		var arcs = List[];
		// Two tower formations
		arcs.addAll(this.mountain(dur * 0.15, dur * 0.35, 0.0, 2.5.rrand(3.5)));
		arcs.addAll(this.mountain(dur * 0.45, dur * 0.62, 0.0, 2.8.rrand(3.8)));
		// Small baseline masses
		5.do { |m|
			arcs.addAll(this.mountain(dur * (0.02 + (m * 0.12) + 0.04.rand),
				dur * (0.05 + (m * 0.12) + 0.07.rand), 0.0, 0.15.rrand(0.3)));
		};
		^arcs.asArray
	}

	// ============================================================
	// UTILITIES
	// ============================================================

	// Convert all arcs in a segment from octaves to Hz
	*segmentHz { |num|
		^this.segment(num).collect { |arc|
			arc.collect { |pt| [pt[0], this.octToFreq(pt[1])] }
		}
	}

	// Get segment with absolute time (offset by start time)
	*segmentAbs { |num|
		var data = segData[num];
		var offset = data[1];
		^this.segment(num).collect { |arc|
			arc.collect { |pt| [pt[0] + offset, pt[1]] }
		}
	}

	// Full piece — all segments concatenated with absolute times, in Hz
	*fullScore {
		^(1..13).collect { |i|
			this.segmentAbs(i).collect { |arc|
				arc.collect { |pt| [pt[0], this.octToFreq(pt[1])] }
			}
		}.flat
	}

	// Plot a segment (requires SC IDE)
	*plot { |num|
		var arcs = this.segment(num);
		var data = segData[num];
		arcs.do { |arc|
			var ts = arc.collect(_[0]);
			var fs = arc.collect(_[1]);
			// Could use Plotter or your own visualization
			[ts, fs].postln;
		};
		("Segment" + num + ":" + arcs.size + "arcs," + data[0] + "s").postln;
	}
}
