/*
 * nUPIC Generative Algorithms
 * ===========================
 * Algorithmic generators for arcs, amplitude envelopes, and spatial curves.
 * Based on techniques from Xenakis and stochastic music composition.
 *
 * Usage: Execute this file to load generators, then use with NUPICApplication.
 *
 * GENERATORS INCLUDED:
 *   1. Brownian Motion (random walk)
 *   2. Xenakis Dynamic Stochastic Synthesis (GENDYN-style)
 *   3. Xenakis Arborescences (branching structures)
 *   4. Lévy Flights (heavy-tailed random walks)
 *   5. Ornstein-Uhlenbeck Process (mean-reverting)
 *   6. Stochastic Bridges (constrained endpoints)
 */

(
// ============================================================================
// GENERATOR NAMESPACE
// ============================================================================

~nUPICGenerators = IdentityDictionary.new;

// Frequency constants
// NOTE: X coordinates are NORMALIZED (0-1 range, where 0=start, 1=end of timeline)
// Y coordinates are calculated from frequency and not stored separately
~nUPICGenerators[\freqMin] = 20;
~nUPICGenerators[\freqMax] = 7500;


// ============================================================================
// 1. BROWNIAN MOTION
// ============================================================================
// Classic random walk - each step is a random increment from previous position.
// Creates organic, wandering frequency trajectories.
//
// Parameters:
//   startX      - Starting X position (normalized 0-1, where 0=start, 1=end)
//   startFreq   - Starting frequency in Hz
//   duration    - Length of arc (normalized 0-1, e.g., 0.3 = 30% of timeline)
//   stepSize    - Size of frequency steps (ratio, e.g., 0.05 = ±5%)
//   numPoints   - Number of points in arc
//   barrier     - Optional frequency barriers [min, max] to bounce off
//   drift       - Optional drift term (positive = upward tendency)

~nUPICGenerators[\brownianArc] = { |startX = 0.1, startFreq = 440, duration = 0.3, stepSize = 0.03, numPoints = 50, barrier = nil, drift = 0|
    var arc = List.new;
    var freq = startFreq;
    var fMin = barrier !? { barrier[0] } ?? { ~nUPICGenerators[\freqMin] };
    var fMax = barrier !? { barrier[1] } ?? { ~nUPICGenerators[\freqMax] };

    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = startX + (t * duration);
        var step = (1.0.rand2 * stepSize) + drift;

        // Apply step as ratio
        freq = freq * (1 + step);

        // Reflect off barriers
        if(freq < fMin) { freq = fMin * (fMin / freq).min(1.5) };
        if(freq > fMax) { freq = fMax * (fMax / freq).max(0.67) };
        freq = freq.clip(fMin, fMax);

        arc.add((x: x, y: 0, freq: freq));  // y is ignored, freq used for drawing
    };

    arc;
};

// Generate multiple Brownian arcs
~nUPICGenerators[\brownianCloud] = { |centerX = 0.3, centerFreq = 800, spread = 0.4, arcCount = 10, duration = 0.2, stepSize = 0.02|
    var arcs = List.new;

    arcCount.do { |i|
        var offsetX = spread.rand2 * 0.5;
        var offsetFreq = centerFreq * (0.5 + 1.0.rand);  // ±1 octave
        var arcDuration = duration * (0.5 + 0.5.rand);
        var arc = ~nUPICGenerators[\brownianArc].value(
            (centerX + offsetX).clip(0, 1),
            offsetFreq,
            arcDuration.min(1 - centerX - offsetX),  // Don't go past end
            stepSize * (0.5 + 1.0.rand)
        );
        arcs.add(arc);
    };

    arcs;
};


// ============================================================================
// 2. XENAKIS DYNAMIC STOCHASTIC SYNTHESIS (GENDYN-Style)
// ============================================================================
// Based on Xenakis's GENDYN algorithm - breakpoints walk randomly in both
// time and amplitude/frequency dimensions. Creates complex, organic curves.
//
// Parameters:
//   startX       - Starting X position (normalized 0-1)
//   centerFreq   - Center frequency around which to vary (Hz)
//   duration     - Total duration (normalized 0-1)
//   numBreakpoints - Number of control points
//   freqStep     - Step size for frequency random walk (ratio)
//   freqBarrier  - Maximum deviation from center (ratio)
//   timeStep     - Step size for time random walk (ratio of segment)
//   timeBarrier  - Maximum time deviation (ratio)
//   distribution - Random distribution (\uniform, \gaussian, \cauchy)

~nUPICGenerators[\gendynArc] = { |startX = 0.1, centerFreq = 600, duration = 0.4, numBreakpoints = 12,
                                   freqStep = 0.1, freqBarrier = 0.5, timeStep = 0.15, timeBarrier = 0.3,
                                   distribution = \gaussian|
    var arc = List.new;
    var breakpoints = List.new;
    var fMin = ~nUPICGenerators[\freqMin];
    var fMax = ~nUPICGenerators[\freqMax];
    var numPoints = 60;
    var randDist;

    // Random distribution function
    randDist = switch(distribution,
        \uniform, { { 1.0.rand2 } },
        \gaussian, { { (1.0.rand + 1.0.rand + 1.0.rand - 1.5) * 0.67 } },  // Approx Gaussian
        \cauchy, { { tan((0.5.rand - 0.25) * pi) * 0.2 } },  // Cauchy-like
        { { 1.0.rand2 } }  // Default uniform
    );

    // Initialize breakpoints with random deviations
    numBreakpoints.do { |i|
        var t = i / (numBreakpoints - 1);
        var baseTime = t;
        var timeDeviation = randDist.value * timeStep;
        var freqDeviation = randDist.value * freqStep;

        breakpoints.add((
            time: (baseTime + timeDeviation).clip(0, 1),
            freqRatio: 1 + (freqDeviation * freqBarrier)
        ));
    };

    // Sort breakpoints by time
    breakpoints = breakpoints.sort { |a, b| a.time < b.time };

    // Ensure first and last are at boundaries
    breakpoints[0].time = 0;
    breakpoints[breakpoints.size - 1].time = 1;

    // Interpolate between breakpoints to create smooth arc
    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = startX + (t * duration);
        var freq;
        var prevBP, nextBP;
        var localT;

        // Find surrounding breakpoints
        breakpoints.do { |bp, j|
            if(bp.time <= t) {
                prevBP = bp;
                nextBP = breakpoints[j + 1] ?? bp;
            };
        };

        // Interpolate
        localT = if(nextBP.time > prevBP.time) {
            (t - prevBP.time) / (nextBP.time - prevBP.time)
        } { 0 };

        freq = centerFreq * prevBP.freqRatio.blend(nextBP.freqRatio, localT);
        freq = freq.clip(fMin, fMax);

        arc.add((x: x, y: 0, freq: freq));
    };

    arc;
};

// Generate evolving GENDYN waveform over time (multiple generations)
~nUPICGenerators[\gendynEvolution] = { |startX = 0.05, centerFreq = 500, duration = 0.5,
                                         generations = 5, freqStep = 0.08, freqBarrier = 0.6|
    var arcs = List.new;
    var segmentDuration = duration / generations;

    generations.do { |gen|
        var genX = startX + (gen * segmentDuration);
        // Each generation has slightly different parameters
        var arc = ~nUPICGenerators[\gendynArc].value(
            genX,
            centerFreq * (0.8 + (0.4 * gen / generations)),  // Rising center
            segmentDuration,
            8 + (gen * 2),  // More breakpoints over time
            freqStep * (1 + (gen * 0.1)),
            freqBarrier
        );
        arcs.add(arc);
    };

    arcs;
};


// ============================================================================
// 3. XENAKIS ARBORESCENCES
// ============================================================================
// Branching tree-like structures inspired by Mycènes Alpha (1978).
// A root arc spawns child arcs, which spawn grandchildren, creating
// organic branching patterns in the frequency-time plane.
//
// Parameters:
//   rootX        - Root starting X position (normalized 0-1)
//   rootFreq     - Root starting frequency (Hz)
//   branchCount  - Number of main branches from root
//   spread       - Horizontal spread (normalized 0-1)
//   depth        - Recursion depth (1-4)
//   duration     - Average arc duration (normalized 0-1)
//   angleSpread  - Angular spread of branches (radians)

~nUPICGenerators[\arborescence] = { |rootX = 0.2, rootFreq = 1000, branchCount = 5,
                                      spread = 0.3, depth = 2, duration = 0.1, angleSpread = 0.8|
    var arcs = List.new;
    var fMin = ~nUPICGenerators[\freqMin];
    var fMax = ~nUPICGenerators[\freqMax];
    var generateBranch;

    // Recursive branch generator (only uses X and freq, Y is ignored)
    generateBranch = { |startX, startFreq, length, currentDepth, branchId|
        var arc = List.new;
        var numPoints = 20;
        var freqChange = (0.5.rand2) * (startFreq * 0.5);
        var endFreq = (startFreq + freqChange).clip(fMin, fMax);
        var endX = (startX + length).clip(0, 1);

        // Create arc points
        numPoints.do { |i|
            var t = i / (numPoints - 1);
            var x = startX + (t * (endX - startX));
            var freq = startFreq.blend(endFreq, t);
            // Add slight frequency wobble for organic feel
            freq = freq * (1 + (sin(t * pi) * 0.05.rand2));
            arc.add((x: x, y: 0, freq: freq.clip(fMin, fMax)));
        };

        arcs.add(arc);

        // Generate sub-branches
        if(currentDepth < depth) {
            var subBranchCount = (2 + 2.rand);
            subBranchCount.do { |j|
                var branchPoint = 0.4 + (0.5.rand);
                var branchX, branchFreq, newLength;

                branchX = startX + (branchPoint * (endX - startX));
                branchFreq = startFreq.blend(endFreq, branchPoint);
                // Add some frequency variation for each branch
                branchFreq = branchFreq * (0.7 + 0.6.rand);
                newLength = length * (0.5 + 0.3.rand);

                generateBranch.value(branchX, branchFreq.clip(fMin, fMax), newLength,
                    currentDepth + 1, branchId ++ "_" ++ j);
            };
        };
    };

    // Generate main branches from root
    branchCount.do { |i|
        var freqOffset = ((i / (branchCount - 1).max(1)) - 0.5) * rootFreq;
        var branchFreq = (rootFreq + freqOffset).clip(fMin, fMax);
        var length = (duration * 0.3) + (duration * 0.4.rand);

        generateBranch.value(rootX, branchFreq, length, 0, i.asString);
    };

    arcs;
};


// ============================================================================
// 4. LÉVY FLIGHTS
// ============================================================================
// Random walk with occasional large jumps (heavy-tailed distribution).
// Creates more dramatic, punctuated frequency contours than Brownian motion.
//
// Parameters:
//   startX     - Starting X position (normalized 0-1)
//   startFreq  - Starting frequency (Hz)
//   duration   - Arc duration (normalized 0-1)
//   numPoints  - Number of points
//   alpha      - Lévy stability parameter (1.0 = Cauchy, 2.0 = Gaussian)
//   scale      - Scale of jumps

~nUPICGenerators[\levyFlightArc] = { |startX = 0.1, startFreq = 500, duration = 0.3,
                                       numPoints = 40, alpha = 1.5, scale = 0.1|
    var arc = List.new;
    var freq = startFreq;
    var fMin = ~nUPICGenerators[\freqMin];
    var fMax = ~nUPICGenerators[\freqMax];

    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = startX + (t * duration);
        var step, u1, u2, gaussian;

        // Generate Lévy-like step using Box-Muller
        u1 = 1.0.rand.max(0.0001);
        u2 = 1.0.rand;
        gaussian = ((-2 * log(u1)).abs.sqrt) * cos(2 * pi * u2);

        // Occasional large jumps for heavy-tailed behavior
        step = if(0.1.coin) {
            gaussian * (1 + 2.rand) * scale  // Large jump
        } {
            gaussian * scale * 0.3  // Normal step
        };

        // Clip step to prevent extreme values (keep freq positive)
        step = step.clip(-0.8, 0.8);

        // Apply as frequency ratio
        freq = freq * (1 + step);

        // Simple clipping - keep within bounds
        freq = freq.clip(fMin, fMax);

        arc.add((x: x, y: 0, freq: freq));
    };

    arc;
};


// ============================================================================
// 5. ORNSTEIN-UHLENBECK PROCESS
// ============================================================================
// Mean-reverting random walk - tends to return to center frequency.
// Good for creating arcs that wander but stay near a target pitch.
//
// Parameters:
//   startX       - Starting X position (normalized 0-1)
//   centerFreq   - Mean frequency to revert towards (Hz)
//   startFreq    - Starting frequency (can differ from center)
//   duration     - Arc duration (normalized 0-1)
//   theta        - Mean reversion rate (higher = faster return)
//   sigma        - Volatility (noise level)
//   numPoints    - Number of points

~nUPICGenerators[\ornsteinUhlenbeckArc] = { |startX = 0.1, centerFreq = 600, startFreq = nil,
                                              duration = 0.3, theta = 0.1, sigma = 0.05, numPoints = 50|
    var arc = List.new;
    var freq = startFreq ?? centerFreq;
    var fMin = ~nUPICGenerators[\freqMin];
    var fMax = ~nUPICGenerators[\freqMax];
    var dt = 1 / numPoints;
    var logFreq, logCenter;

    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = startX + (t * duration);
        var noise = 1.0.rand2 * sigma * dt.sqrt;

        logFreq = log(freq);
        logCenter = log(centerFreq);

        // OU process in log-frequency space
        logFreq = logFreq + (theta * (logCenter - logFreq) * dt) + noise;
        freq = exp(logFreq).clip(fMin, fMax);

        arc.add((x: x, y: 0, freq: freq));
    };

    arc;
};


// ============================================================================
// 6. STOCHASTIC BRIDGES
// ============================================================================
// Random walk constrained to hit specific endpoints.
// Useful for creating arcs that must start and end at certain frequencies.
//
// Parameters:
//   startX     - Starting X position (normalized 0-1)
//   startFreq  - Starting frequency (Hz, fixed)
//   endFreq    - Ending frequency (Hz, fixed)
//   duration   - Arc duration (normalized 0-1)
//   sigma      - Volatility of random walk
//   numPoints  - Number of points

~nUPICGenerators[\stochasticBridge] = { |startX = 0.1, startFreq = 300, endFreq = 1200,
                                          duration = 0.3, sigma = 0.08, numPoints = 40|
    var arc = List.new;
    var fMin = ~nUPICGenerators[\freqMin];
    var fMax = ~nUPICGenerators[\freqMax];
    var logStart = log(startFreq);
    var logEnd = log(endFreq);
    var logFreq = logStart;
    var bridge;

    // Generate Brownian bridge
    bridge = Array.fill(numPoints, { |i|
        var t = i / (numPoints - 1);
        var remaining = 1 - t;
        var noise = 1.0.rand2 * sigma;
        var correction;

        if(remaining > 0.01) {
            // Bridge correction to ensure we hit endpoint
            correction = (logEnd - logFreq) / (numPoints - i);
            logFreq = logFreq + correction + (noise * remaining.sqrt);
        } {
            logFreq = logEnd;
        };

        exp(logFreq);
    });

    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = startX + (t * duration);
        var freq = bridge[i].clip(fMin, fMax);

        arc.add((x: x, y: 0, freq: freq));
    };

    arc;
};


// ============================================================================
// AMPLITUDE ENVELOPE GENERATORS
// ============================================================================
// Generate amplitude envelopes using stochastic processes.

// Brownian amplitude envelope
~nUPICGenerators[\brownianAmplitude] = { |arcLength = 600, numPoints = 20, stepSize = 0.1|
    var env = List.new;
    var amp = 0.5 + 0.3.rand;  // Start around 0.5

    numPoints.do { |i|
        var x = (i / (numPoints - 1)) * arcLength;
        var step = 1.0.rand2 * stepSize;

        amp = (amp + step).clip(0.05, 1.0);
        env.add((x: x, amp: amp));
    };

    // Ensure fade in/out
    env[0].amp = 0;
    env[env.size - 1].amp = 0;

    env;
};

// GENDYN-style amplitude envelope
~nUPICGenerators[\gendynAmplitude] = { |arcLength = 600, numBreakpoints = 8, step = 0.15, barrier = 0.4|
    var env = List.new;
    var amp = 0.5;

    numBreakpoints.do { |i|
        var x = (i / (numBreakpoints - 1)) * arcLength;
        var delta = (1.0.rand2 * step);

        amp = (amp + delta).clip(0.1, 0.9);
        env.add((x: x, amp: amp));
    };

    // Fade in/out
    env[0].amp = 0;
    env[env.size - 1].amp = 0;

    env;
};


// ============================================================================
// SPATIAL ENVELOPE GENERATORS
// ============================================================================
// Generate spatial/panning envelopes using stochastic processes.

// Brownian spatial movement
~nUPICGenerators[\brownianSpatial] = { |arcLength = 600, numPoints = 15, numChannels = 8, stepSize = 0.5|
    var env = List.new;
    var channel = numChannels.rand.asFloat;

    numPoints.do { |i|
        var x = (i / (numPoints - 1)) * arcLength;
        var step = 1.0.rand2 * stepSize;

        channel = (channel + step).wrap(0, numChannels - 0.01);
        env.add((x: x, channel: channel.floor));
    };

    env;
};

// Circular spatial movement
~nUPICGenerators[\circularSpatial] = { |arcLength = 600, numPoints = 20, numChannels = 8, rotations = 1|
    var env = List.new;

    numPoints.do { |i|
        var t = i / (numPoints - 1);
        var x = t * arcLength;
        var channel = (t * rotations * numChannels).mod(numChannels).floor;

        env.add((x: x, channel: channel));
    };

    env;
};

// Random jumps spatial
~nUPICGenerators[\jumpingSpatial] = { |arcLength = 600, numJumps = 6, numChannels = 8|
    var env = List.new;
    var channel = numChannels.rand;

    numJumps.do { |i|
        var x = (i / (numJumps - 1)) * arcLength;
        channel = numChannels.rand;
        env.add((x: x, channel: channel));
    };

    env;
};


// ============================================================================
// HELPER: ADD GENERATED ARCS TO NUPIC
// ============================================================================
// Convenience function to add generated arcs to the current NUPICData instance.

~nUPICGenerators[\addToData] = { |data, arcs, synthDef = nil, ampEnv = nil, spatialEnv = nil|
    var addedIndices = List.new;

    if(data.isNil) {
        "Error: No NUPICData instance provided".postln;
        ^nil;
    };

    // Handle single arc or list of arcs
    if(arcs[0].isKindOf(Event)) {
        // Single arc
        arcs = [arcs];
    };

    arcs.do { |arc|
        var idx = data.addArc(arc);
        if(idx.notNil) {
            addedIndices.add(idx);

            // Set synthdef if provided
            if(synthDef.notNil) {
                data.setSynthDefForArc(idx, synthDef);
            };

            // Set amplitude envelope if provided
            if(ampEnv.notNil) {
                data.setAmplitudeEnvelope(idx, ampEnv);
            };

            // Set spatial envelope if provided
            if(spatialEnv.notNil) {
                data.setSpatialEnvelope(idx, spatialEnv);
            };
        };
    };

    "Added % arcs (indices: %)".format(addedIndices.size, addedIndices).postln;
    addedIndices;
};


// ============================================================================
// COMPOSITION EXAMPLES
// ============================================================================

"nUPIC Generators loaded. Available generators:".postln;
"".postln;
"ARCS:".postln;
"  ~nUPICGenerators[\\brownianArc].(startX, startFreq, duration, stepSize, numPoints)".postln;
"  ~nUPICGenerators[\\brownianCloud].(centerX, centerFreq, spread, arcCount, duration, stepSize)".postln;
"  ~nUPICGenerators[\\gendynArc].(startX, centerFreq, duration, numBreakpoints, freqStep, ...)".postln;
"  ~nUPICGenerators[\\gendynEvolution].(startX, centerFreq, duration, generations, ...)".postln;
"  ~nUPICGenerators[\\arborescence].(rootX, rootFreq, branchCount, spread, depth, duration)".postln;
"  ~nUPICGenerators[\\levyFlightArc].(startX, startFreq, duration, numPoints, alpha, scale)".postln;
"  ~nUPICGenerators[\\ornsteinUhlenbeckArc].(startX, centerFreq, startFreq, duration, theta, sigma)".postln;
"  ~nUPICGenerators[\\stochasticBridge].(startX, startFreq, endFreq, duration, sigma)".postln;
"".postln;
"AMPLITUDE ENVELOPES:".postln;
"  ~nUPICGenerators[\\brownianAmplitude].(arcLength, numPoints, stepSize)".postln;
"  ~nUPICGenerators[\\gendynAmplitude].(arcLength, numBreakpoints, step, barrier)".postln;
"".postln;
"SPATIAL ENVELOPES:".postln;
"  ~nUPICGenerators[\\brownianSpatial].(arcLength, numPoints, numChannels, stepSize)".postln;
"  ~nUPICGenerators[\\circularSpatial].(arcLength, numPoints, numChannels, rotations)".postln;
"  ~nUPICGenerators[\\jumpingSpatial].(arcLength, numJumps, numChannels)".postln;
"".postln;
"HELPER:".postln;
"  ~nUPICGenerators[\\addToData].(data, arcs, synthDef, ampEnv, spatialEnv)".postln;
)


// ============================================================================
// USAGE EXAMPLES
// ============================================================================

/*

// After starting nUPIC app:
~app = NUPICApplication.new;
~app.start;

// Wait for app to start, then:
~data = ~app.data;
~gui = ~app.gui;

// Example 1: Single Brownian arc
(
var arc = ~nUPICGenerators[\brownianArc].(
    startX: 0.1,       // normalized position (0-1)
    startFreq: 440,    // Hz
    duration: 0.3,     // normalized length (0-1)
    stepSize: 0.04,    // frequency step ratio
    numPoints: 60
);
~nUPICGenerators[\addToData].(~data, arc, \upicWavetable);
~gui.refresh;
)

// Example 2: Brownian cloud
(
var arcs = ~nUPICGenerators[\brownianCloud].(
    centerX: 0.3,      // normalized center position (0-1)
    centerFreq: 600,   // Hz
    spread: 0.4,       // normalized spread (0-1)
    arcCount: 15,      // number of arcs
    duration: 0.2,     // normalized arc length (0-1)
    stepSize: 0.03     // frequency step ratio
);
~nUPICGenerators[\addToData].(~data, arcs, \upicWavetable8ch);
~gui.refresh;
)

// Example 3: GENDYN arc with custom amplitude envelope
(
var arc = ~nUPICGenerators[\gendynArc].(
    startX: 0.1,           // normalized position (0-1)
    centerFreq: 800,       // Hz
    duration: 0.4,         // normalized length (0-1)
    numBreakpoints: 15,    // control points
    freqStep: 0.12,        // frequency walk step
    freqBarrier: 0.6,      // max freq deviation ratio
    timeStep: 0.1,         // time walk step
    timeBarrier: 0.2,      // max time deviation
    distribution: \gaussian // \uniform, \gaussian, or \cauchy
);
var ampEnv = ~nUPICGenerators[\gendynAmplitude].(
    arcLength: 500,        // pixels (for envelope)
    numBreakpoints: 10,
    step: 0.2
);
~nUPICGenerators[\addToData].(~data, arc, \upicWavetable, ampEnv);
~gui.refresh;
)

// Example 4: Arborescence (branching tree structure)
(
var arcs = ~nUPICGenerators[\arborescence].(
    rootX: 0.2,        // normalized root position (0-1)
    rootFreq: 1000,    // Hz
    branchCount: 6,    // main branches from root
    spread: 0.3,       // normalized spread (0-1)
    depth: 3,          // recursion depth (1-4)
    duration: 0.1      // normalized branch length (0-1)
);
~nUPICGenerators[\addToData].(~data, arcs, \upicWavetable8ch);
~gui.refresh;
)

// Example 5: Lévy flight with spatial movement
(
var arc = ~nUPICGenerators[\levyFlightArc].(
    startX: 0.1,       // normalized position (0-1)
    startFreq: 400,    // Hz
    duration: 0.3,     // normalized length (0-1)
    numPoints: 50,
    alpha: 1.3,        // stability (1=Cauchy, 2=Gaussian)
    scale: 0.15        // jump magnitude
);
var spatialEnv = ~nUPICGenerators[\brownianSpatial].(
    arcLength: 500,    // pixels (for envelope)
    numPoints: 20,
    numChannels: 8,
    stepSize: 0.8
);
~nUPICGenerators[\addToData].(~data, arc, \upicWavetable8ch, nil, spatialEnv);
~gui.refresh;
)

// Example 6: Ornstein-Uhlenbeck (mean-reverting random walk)
(
var arc = ~nUPICGenerators[\ornsteinUhlenbeckArc].(
    startX: 0.1,       // normalized position (0-1)
    centerFreq: 600,   // Hz - frequency to revert towards
    startFreq: 400,    // Hz - starting frequency (nil = use centerFreq)
    duration: 0.4,     // normalized length (0-1)
    theta: 0.15,       // mean reversion rate (higher = faster return)
    sigma: 0.08,       // volatility/noise level
    numPoints: 60
);
~nUPICGenerators[\addToData].(~data, arc, \upicWavetable);
~gui.refresh;
)

// Example 7: Stochastic bridge (constrained start/end frequencies)
(
var arc = ~nUPICGenerators[\stochasticBridge].(
    startX: 0.1,       // normalized position (0-1)
    startFreq: 200,    // Hz - fixed start frequency
    endFreq: 2000,     // Hz - fixed end frequency
    duration: 0.3,     // normalized length (0-1)
    sigma: 0.1,        // volatility of random walk
    numPoints: 50
);
~nUPICGenerators[\addToData].(~data, arc);
~gui.refresh;
)

// Example 8: GENDYN evolution (multiple generations)
(
var arcs = ~nUPICGenerators[\gendynEvolution].(
    startX: 0.05,      // normalized position (0-1)
    centerFreq: 500,   // Hz
    duration: 0.5,     // normalized total length (0-1)
    generations: 5,    // number of evolving segments
    freqStep: 0.08,
    freqBarrier: 0.6
);
~nUPICGenerators[\addToData].(~data, arcs, \upicWavetable8ch);
~gui.refresh;
)

// Example 9: Full composition - layered textures
(
var lowArcs, midArcs, highArcs;
~data.clearAll;

// Layer 1: Low GENDYN evolution
lowArcs = ~nUPICGenerators[\gendynEvolution].(
    startX: 0.05, centerFreq: 150, duration: 0.5,
    generations: 4, freqStep: 0.06, freqBarrier: 0.4
);
~nUPICGenerators[\addToData].(~data, lowArcs, \upicWavetable8ch);

// Layer 2: Mid arborescence
midArcs = ~nUPICGenerators[\arborescence].(
    rootX: 0.3, rootFreq: 600, branchCount: 4,
    spread: 0.25, depth: 2, duration: 0.1
);
~nUPICGenerators[\addToData].(~data, midArcs, \upicWavetable8ch);

// Layer 3: High Brownian cloud
highArcs = ~nUPICGenerators[\brownianCloud].(
    centerX: 0.7, centerFreq: 2500, spread: 0.2,
    arcCount: 8, duration: 0.12, stepSize: 0.02
);
~nUPICGenerators[\addToData].(~data, highArcs, \upicWavetable8ch);

~gui.refresh;
"Composition complete!".postln;
)

*/
