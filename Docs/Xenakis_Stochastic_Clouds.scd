/*
===============================================================================
XENAKIS STOCHASTIC CLOUDS - Exploratory Examples for nUPIC
===============================================================================

Iannis Xenakis pioneered the use of probability theory and statistical
mechanics in music composition. His approach treated sound masses as
statistical entities rather than collections of individual notes.

Key concepts:
- Clouds as probability distributions in pitch-time space
- Grains ranging from impulses (dots) to short glissandi (arcs)
- Set operations (union, difference, intersection) on sound masses
- Stochastic control of density, register, duration

These examples explore how these concepts could be realized in nUPIC.
===============================================================================
*/

// ============================================================================
// SETUP
// ============================================================================

(
// Start nUPIC
~app = NUPICApplication(oversampling: true);
~app.start;
)

// ============================================================================
// PART 1: BASIC CLOUD GENERATION
// ============================================================================

/*
A cloud is defined by statistical parameters, not individual events.
The generator produces grains according to probability distributions.
*/

// --- 1.1 Uniform Distribution Cloud ---
// Events spread evenly across pitch and time ranges
(
var density = 40;        // grains per normalized time unit
var timeRange = [0, 1];  // full page width
var freqRange = [200, 2000];  // Hz
var durRange = [0.01, 0.05];  // grain duration (normalized x)

~arcs = density.collect {
    var startX = rrand(timeRange[0], timeRange[1]);
    var dur = rrand(durRange[0], durRange[1]);
    var freq1 = exprand(freqRange[0], freqRange[1]);
    var freq2 = exprand(freqRange[0], freqRange[1]);

    // Short arc (grain) from freq1 to freq2
    [
        (x: startX, freq: freq1),
        (x: (startX + dur).min(1), freq: freq2)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Uniform cloud: % grains".format(density).postln;
)

// --- 1.2 Gaussian Distribution Cloud ---
// Events clustered around center pitch, sparser at extremes
(
var density = 60;
var centerFreq = 800;     // Hz - center of distribution
var freqSpread = 400;     // Hz - standard deviation
var timeCenter = 0.5;
var timeSpread = 0.2;

~arcs = density.collect {
    var startX = (timeCenter + (freqSpread.rand2 / 2000)).clip(0, 1);
    var dur = rrand(0.01, 0.03);
    var freq = (centerFreq + (freqSpread * 0.gauss(1))).clip(50, 5000);

    // Dot-like grain (very short arc)
    [
        (x: startX, freq: freq),
        (x: (startX + dur).min(1), freq: freq * rrand(0.98, 1.02))
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Gaussian cloud centered at % Hz".format(centerFreq).postln;
)

// --- 1.3 Cauchy Distribution Cloud ---
// Heavy tails - occasional extreme outliers (Xenakis favored this)
(
var density = 50;
var centerFreq = 600;
var timeRange = [0.2, 0.8];

// Cauchy random number generator
~cauchy = { |location, scale|
    var u = rrand(0.001, 0.999);
    location + (scale * tan(pi * (u - 0.5)))
};

~arcs = density.collect {
    var startX = rrand(timeRange[0], timeRange[1]);
    var dur = rrand(0.005, 0.02);
    var freq = ~cauchy.(centerFreq, 150).clip(80, 4000);

    [
        (x: startX, freq: freq),
        (x: (startX + dur).min(1), freq: freq)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Cauchy cloud - note the outliers".postln;
)

// --- 1.4 Exponential Distribution Cloud ---
// Dense at low frequencies, sparse at high (natural acoustic distribution)
(
var density = 70;
var freqMin = 100;
var freqMax = 3000;

~arcs = density.collect {
    var startX = rrand(0, 1);
    var dur = rrand(0.01, 0.04);
    // Exponential favors lower frequencies
    var freq = freqMin * ((freqMax/freqMin) ** rrand(0.0, 1.0).squared);
    var freqEnd = freq * rrand(0.9, 1.1);

    [
        (x: startX, freq: freq),
        (x: (startX + dur).min(1), freq: freqEnd.clip(50, 5000))
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Exponential cloud - denser at low frequencies".postln;
)


// ============================================================================
// PART 2: GRAIN MORPHOLOGY
// ============================================================================

/*
Xenakis distinguished between:
- Points (impulses) - instantaneous events
- Glissandi - continuous pitch movement
- Sustained tones - constant pitch

In nUPIC, these map to arc shapes.
*/

// --- 2.1 Point Cloud (Impulses/Dots) ---
(
var density = 100;
var dotDuration = 0.003;  // Very short = dot-like

~arcs = density.collect {
    var x = rrand(0, 1);
    var freq = exprand(150, 3000);

    // Minimal duration creates dot/impulse
    [
        (x: x, freq: freq),
        (x: (x + dotDuration).min(1), freq: freq)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Point cloud: % impulses".format(density).postln;
)

// --- 2.2 Ascending Glissandi Cloud ---
(
var density = 30;

~arcs = density.collect {
    var startX = rrand(0, 0.8);
    var dur = rrand(0.05, 0.15);
    var freqStart = exprand(100, 1000);
    var interval = rrand(1.1, 2.0);  // ascending ratio

    [
        (x: startX, freq: freqStart),
        (x: (startX + dur).min(1), freq: freqStart * interval)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Ascending glissandi cloud".postln;
)

// --- 2.3 Descending Glissandi Cloud ---
(
var density = 30;

~arcs = density.collect {
    var startX = rrand(0, 0.8);
    var dur = rrand(0.05, 0.15);
    var freqStart = exprand(500, 3000);
    var interval = rrand(0.5, 0.9);  // descending ratio

    [
        (x: startX, freq: freqStart),
        (x: (startX + dur).min(1), freq: freqStart * interval)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Descending glissandi cloud".postln;
)

// --- 2.4 Mixed Morphology Cloud ---
(
var density = 50;

~arcs = density.collect {
    var startX = rrand(0, 0.9);
    var morphType = 3.rand;  // 0=dot, 1=ascending, 2=descending
    var freqStart = exprand(150, 2500);
    var dur, freqEnd;

    switch(morphType,
        0, {  // Dot
            dur = rrand(0.002, 0.01);
            freqEnd = freqStart;
        },
        1, {  // Ascending glissando
            dur = rrand(0.03, 0.12);
            freqEnd = freqStart * rrand(1.2, 3.0);
        },
        2, {  // Descending glissando
            dur = rrand(0.03, 0.12);
            freqEnd = freqStart * rrand(0.3, 0.8);
        }
    );

    [
        (x: startX, freq: freqStart),
        (x: (startX + dur).min(1), freq: freqEnd.clip(50, 5000))
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Mixed morphology cloud".postln;
)


// ============================================================================
// PART 3: DENSITY CONTROL
// ============================================================================

/*
Xenakis controlled cloud density as a function of time.
Density can increase, decrease, or follow complex envelopes.
*/

// --- 3.1 Density Crescendo ---
(
var totalGrains = 80;
var timeSlots = 20;
var grainsPerSlot;

// Exponentially increasing density
grainsPerSlot = timeSlots.collect { |i|
    ((i + 1) / timeSlots).squared * 8
}.round.asInteger;

~arcs = List[];
timeSlots.do { |slot|
    var slotStart = slot / timeSlots;
    var slotEnd = (slot + 1) / timeSlots;
    var numGrains = grainsPerSlot[slot];

    numGrains.do {
        var x = rrand(slotStart, slotEnd);
        var freq = exprand(200, 2000);
        var dur = rrand(0.005, 0.02);

        ~arcs.add([
            (x: x, freq: freq),
            (x: (x + dur).min(1), freq: freq * rrand(0.95, 1.05))
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Density crescendo: sparse to dense".postln;
)

// --- 3.2 Density Decrescendo ---
(
var timeSlots = 20;
var grainsPerSlot;

grainsPerSlot = timeSlots.collect { |i|
    (1 - (i / timeSlots)).squared * 10
}.round.asInteger;

~arcs = List[];
timeSlots.do { |slot|
    var slotStart = slot / timeSlots;
    var slotEnd = (slot + 1) / timeSlots;

    grainsPerSlot[slot].do {
        var x = rrand(slotStart, slotEnd);
        var freq = exprand(150, 2500);
        var dur = rrand(0.005, 0.03);

        ~arcs.add([
            (x: x, freq: freq),
            (x: (x + dur).min(1), freq: freq)
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Density decrescendo: dense to sparse".postln;
)

// --- 3.3 Pulsating Density ---
(
var timeSlots = 40;
var pulsations = 4;

~arcs = List[];
timeSlots.do { |slot|
    var slotStart = slot / timeSlots;
    var slotEnd = (slot + 1) / timeSlots;
    var phase = slot / timeSlots * 2pi * pulsations;
    var density = ((sin(phase) + 1) / 2 * 8).round.asInteger + 1;

    density.do {
        var x = rrand(slotStart, slotEnd);
        var freq = exprand(100, 3000);
        var dur = rrand(0.01, 0.03);

        ~arcs.add([
            (x: x, freq: freq),
            (x: (x + dur).min(1), freq: freq * rrand(0.9, 1.1))
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Pulsating density: % waves".format(pulsations).postln;
)


// ============================================================================
// PART 4: REGISTER CONTROL
// ============================================================================

/*
The pitch range (register) of clouds can evolve over time.
Xenakis often used expanding, contracting, or shifting registers.
*/

// --- 4.1 Expanding Register ---
(
var density = 60;
var centerFreq = 500;

~arcs = density.collect {
    var x = rrand(0, 1);
    var spread = x.linexp(0, 1, 1.05, 4);  // Spread increases with time
    var freq = centerFreq * rrand(1/spread, spread);
    var dur = rrand(0.01, 0.04);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq * rrand(0.95, 1.05))
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Expanding register: narrow to wide".postln;
)

// --- 4.2 Contracting Register ---
(
var density = 60;
var centerFreq = 600;

~arcs = density.collect {
    var x = rrand(0, 1);
    var spread = x.linexp(0, 1, 4, 1.05);  // Spread decreases with time
    var freq = centerFreq * rrand(1/spread, spread);
    var dur = rrand(0.01, 0.04);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Contracting register: wide to narrow".postln;
)

// --- 4.3 Ascending Register ---
(
var density = 70;

~arcs = density.collect {
    var x = rrand(0, 1);
    var baseFreq = x.linexp(0, 1, 100, 2000);  // Center rises with time
    var spread = 1.3;
    var freq = baseFreq * rrand(1/spread, spread);
    var dur = rrand(0.01, 0.03);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Ascending register cloud".postln;
)

// --- 4.4 Descending Register ---
(
var density = 70;

~arcs = density.collect {
    var x = rrand(0, 1);
    var baseFreq = x.linexp(0, 1, 3000, 150);  // Center falls with time
    var spread = 1.3;
    var freq = baseFreq * rrand(1/spread, spread);
    var dur = rrand(0.01, 0.03);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Descending register cloud".postln;
)

// --- 4.5 Crossing Registers (Two Clouds) ---
(
var densityEach = 35;

// Cloud A: ascending
~arcsA = densityEach.collect {
    var x = rrand(0, 1);
    var baseFreq = x.linexp(0, 1, 150, 2500);
    var freq = baseFreq * rrand(0.9, 1.1);
    var dur = rrand(0.01, 0.03);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]
};

// Cloud B: descending
~arcsB = densityEach.collect {
    var x = rrand(0, 1);
    var baseFreq = x.linexp(0, 1, 2500, 150);
    var freq = baseFreq * rrand(0.9, 1.1);
    var dur = rrand(0.01, 0.03);

    [
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]
};

~app.data.clearAll;
(~arcsA ++ ~arcsB).do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Crossing registers: two clouds intersecting".postln;
)


// ============================================================================
// PART 5: SET OPERATIONS ON CLOUDS
// ============================================================================

/*
Xenakis applied set theory to sound masses:
- Union (A + B): All events from both clouds
- Intersection (A n B): Only events in overlapping regions
- Difference (A - B): Events from A not in B's region

In pitch-time space, we define regions and filter events.
*/

// --- Helper: Check if point is in region ---
(
~inRegion = { |x, freq, region|
    // region = (xMin, xMax, freqMin, freqMax)
    (x >= region[0]) and: { x <= region[1] } and:
    { freq >= region[2] } and: { freq <= region[3] }
};
)

// --- 5.1 Union of Two Clouds ---
(
var densityA = 40, densityB = 40;

// Cloud A: left half, low register
~cloudA = densityA.collect {
    var x = rrand(0, 0.5);
    var freq = exprand(100, 600);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Cloud B: right half, high register
~cloudB = densityB.collect {
    var x = rrand(0.5, 1.0);
    var freq = exprand(800, 3000);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Union: simply combine both
~union = ~cloudA ++ ~cloudB;

~app.data.clearAll;
~union.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Union of two clouds: A (low-left) + B (high-right)".postln;
)

// --- 5.2 Intersection of Overlapping Clouds ---
(
var densityA = 60, densityB = 60;
var overlapRegion = [0.3, 0.7, 400, 1500];  // Where clouds might intersect

// Cloud A: left-leaning, wide register
~cloudA = densityA.collect {
    var x = rrand(0.1, 0.7);
    var freq = exprand(200, 2000);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Cloud B: right-leaning, wide register
~cloudB = densityB.collect {
    var x = rrand(0.3, 0.9);
    var freq = exprand(300, 1800);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Intersection: only events in overlap region from both clouds
~intersection = (~cloudA ++ ~cloudB).select { |arc|
    var x = arc[0][\x];
    var freq = arc[0][\freq];
    ~inRegion.(x, freq, overlapRegion)
};

~app.data.clearAll;
~intersection.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Intersection: only overlapping region".postln;
)

// --- 5.3 Difference (A - B) ---
(
var densityA = 80;
var excludeRegion = [0.4, 0.6, 500, 1500];  // Region to remove

// Cloud A: full page
~cloudA = densityA.collect {
    var x = rrand(0, 1);
    var freq = exprand(100, 3000);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Difference: remove events in exclude region
~difference = ~cloudA.reject { |arc|
    var x = arc[0][\x];
    var freq = arc[0][\freq];
    ~inRegion.(x, freq, excludeRegion)
};

~app.data.clearAll;
~difference.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Difference: cloud with rectangular hole".postln;
)

// --- 5.4 Multiple Exclusion Zones ---
(
var density = 120;
var excludeZones = [
    [0.1, 0.3, 200, 800],
    [0.5, 0.7, 600, 2000],
    [0.8, 0.95, 100, 500]
];

~cloud = density.collect {
    var x = rrand(0, 1);
    var freq = exprand(80, 3500);
    var dur = rrand(0.01, 0.025);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

// Remove all exclusion zones
~result = ~cloud.reject { |arc|
    var x = arc[0][\x];
    var freq = arc[0][\freq];
    excludeZones.any { |zone| ~inRegion.(x, freq, zone) }
};

~app.data.clearAll;
~result.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Cloud with multiple exclusion zones".postln;
)

// --- 5.5 Circular Exclusion (using distance) ---
(
var density = 100;
var holeCenter = [0.5, 800];  // x, freq
var holeRadius = 0.2;  // normalized radius

~cloud = density.collect {
    var x = rrand(0, 1);
    var freq = exprand(100, 3000);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

~result = ~cloud.reject { |arc|
    var x = arc[0][\x];
    var freq = arc[0][\freq];
    var yNorm = freq.explin(100, 3000, 0, 1);
    var dist = hypot(x - holeCenter[0], yNorm - holeCenter[1].explin(100, 3000, 0, 1));
    dist < holeRadius
};

~app.data.clearAll;
~result.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Cloud with circular hole".postln;
)


// ============================================================================
// PART 6: SCREENS (Xenakis's 2D Pitch-Time Spaces)
// ============================================================================

/*
Xenakis used "screens" - discrete grids in pitch-time space where
probability determines whether each cell is activated.
*/

// --- 6.1 Basic Screen ---
(
var timeSlots = 40;
var pitchSlots = 30;
var probability = 0.15;
var freqMin = 100, freqMax = 3000;

~arcs = List[];

timeSlots.do { |t|
    pitchSlots.do { |p|
        if(probability.coin) {
            var x = t / timeSlots;
            var freq = p.linexp(0, pitchSlots - 1, freqMin, freqMax);
            var dur = 0.8 / timeSlots;  // Fill most of cell

            ~arcs.add([
                (x: x, freq: freq),
                (x: x + dur, freq: freq)
            ]);
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Screen with % probability".format(probability).postln;
)

// --- 6.2 Variable Probability Screen ---
(
var timeSlots = 50;
var pitchSlots = 35;
var freqMin = 80, freqMax = 4000;

~arcs = List[];

timeSlots.do { |t|
    pitchSlots.do { |p|
        // Probability varies: higher at center of pitch range
        var pitchCenter = pitchSlots / 2;
        var distFromCenter = (p - pitchCenter).abs / pitchCenter;
        var probability = 0.25 * (1 - distFromCenter.squared);

        if(probability.coin) {
            var x = t / timeSlots;
            var freq = p.linexp(0, pitchSlots - 1, freqMin, freqMax);
            var dur = 0.7 / timeSlots;

            ~arcs.add([
                (x: x, freq: freq),
                (x: x + dur, freq: freq)
            ]);
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Screen with center-weighted probability".postln;
)

// --- 6.3 Diagonal Screen ---
(
var timeSlots = 45;
var pitchSlots = 35;
var freqMin = 100, freqMax = 3500;
var bandWidth = 8;  // Slots from diagonal

~arcs = List[];

timeSlots.do { |t|
    pitchSlots.do { |p|
        // Probability based on distance from diagonal
        var expectedPitch = t.linlin(0, timeSlots, 0, pitchSlots);
        var distFromDiag = (p - expectedPitch).abs;
        var probability = if(distFromDiag < bandWidth) {
            0.4 * (1 - (distFromDiag / bandWidth))
        } { 0 };

        if(probability.coin) {
            var x = t / timeSlots;
            var freq = p.linexp(0, pitchSlots - 1, freqMin, freqMax);
            var dur = rrand(0.01, 0.03);

            ~arcs.add([
                (x: x, freq: freq),
                (x: (x + dur).min(1), freq: freq)
            ]);
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Diagonal screen (ascending band)".postln;
)


// ============================================================================
// PART 7: SIEVES (Xenakis's Pitch Selection)
// ============================================================================

/*
Xenakis developed sieve theory to generate pitch collections
using modular arithmetic. A sieve selects pitches at regular
intervals with offsets.
*/

// --- 7.1 Simple Sieve ---
(
var density = 60;
var baseFreq = 100;  // Reference frequency
var sieveModulo = 5; // Every 5th semitone
var sieveOffset = 0;

// Generate valid frequencies from sieve
~sieveFreqs = (0..60).select { |semitone|
    (semitone - sieveOffset) % sieveModulo == 0
}.collect { |semitone|
    baseFreq * (2 ** (semitone / 12))
};

~arcs = density.collect {
    var x = rrand(0, 1);
    var freq = ~sieveFreqs.choose;
    var dur = rrand(0.01, 0.04);

    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Simple sieve: every % semitones".format(sieveModulo).postln;
)

// --- 7.2 Union of Sieves ---
(
var density = 80;
var baseFreq = 100;

// Sieve A: every 3rd semitone starting from 0
// Sieve B: every 4th semitone starting from 1
// Union creates irregular but structured pattern

~sieveA = (0..60).select { |s| s % 3 == 0 };
~sieveB = (0..60).select { |s| (s - 1) % 4 == 0 };
~sieveUnion = (~sieveA ++ ~sieveB).asSet.asArray.sort;

~sieveFreqs = ~sieveUnion.collect { |semitone|
    baseFreq * (2 ** (semitone / 12))
};

~arcs = density.collect {
    var x = rrand(0, 1);
    var freq = ~sieveFreqs.choose;
    var dur = rrand(0.01, 0.03);

    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Union of two sieves".postln;
)

// --- 7.3 Intersection of Sieves ---
(
var density = 50;
var baseFreq = 80;

// Intersection: only pitches in BOTH sieves
~sieveA = (0..72).select { |s| s % 3 == 0 };
~sieveB = (0..72).select { |s| s % 5 == 0 };
~sieveIntersection = ~sieveA.select { |s| ~sieveB.includes(s) };

~sieveFreqs = ~sieveIntersection.collect { |semitone|
    baseFreq * (2 ** (semitone / 12))
};

~arcs = density.collect {
    var x = rrand(0, 1);
    var freq = ~sieveFreqs.choose;
    var dur = rrand(0.02, 0.05);

    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq)]
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Intersection of sieves (sparse)".postln;
)


// ============================================================================
// PART 8: MARKOV CHAINS
// ============================================================================

/*
Xenakis used Markov chains for pitch sequences where the next
pitch depends probabilistically on the current pitch.
*/

// --- 8.1 Simple Markov Chain ---
(
var numGrains = 100;
var states = [200, 400, 600, 800, 1000, 1200];  // Frequencies
var transitionMatrix = [
    // Each row: probability of transitioning to each state
    [0.1, 0.4, 0.3, 0.1, 0.1, 0.0],  // From 200 Hz
    [0.2, 0.1, 0.4, 0.2, 0.1, 0.0],  // From 400 Hz
    [0.1, 0.2, 0.2, 0.3, 0.1, 0.1],  // From 600 Hz
    [0.0, 0.1, 0.3, 0.2, 0.3, 0.1],  // From 800 Hz
    [0.0, 0.1, 0.1, 0.3, 0.2, 0.3],  // From 1000 Hz
    [0.0, 0.0, 0.1, 0.2, 0.4, 0.3],  // From 1200 Hz
];

~markovNext = { |currentState|
    var probs = transitionMatrix[currentState];
    var cumProbs = probs.integrate;
    var r = rrand(0.0, 1.0);
    cumProbs.detectIndex { |p| r <= p }
};

~arcs = List[];
~currentState = states.size.rand;

numGrains.do { |i|
    var x = i / numGrains;
    var freq = states[~currentState];
    var dur = rrand(0.005, 0.015);

    ~arcs.add([
        (x: x, freq: freq),
        (x: (x + dur).min(1), freq: freq)
    ]);

    ~currentState = ~markovNext.(~currentState);
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Markov chain: sequential grains with state transitions".postln;
)


// ============================================================================
// PART 9: COMPLEX COMPOSITIONS
// ============================================================================

// --- 9.1 Layered Clouds with Different Characters ---
(
// Layer 1: Sparse bass dots
~bass = 20.collect {
    var x = rrand(0, 1);
    var freq = exprand(60, 200);
    [(x: x, freq: freq), (x: x + 0.005, freq: freq)]
};

// Layer 2: Dense mid-range texture
~mid = 80.collect {
    var x = rrand(0, 1);
    var freq = exprand(300, 1200);
    var dur = rrand(0.01, 0.03);
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: freq * rrand(0.98, 1.02))]
};

// Layer 3: Sparse high glissandi
~high = 25.collect {
    var x = rrand(0, 0.85);
    var freq = exprand(1500, 4000);
    var dur = rrand(0.05, 0.12);
    var direction = [-1, 1].choose;
    var interval = rrand(1.2, 1.8) ** direction;
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: (freq * interval).clip(100, 5000))]
};

~app.data.clearAll;
(~bass ++ ~mid ++ ~high).do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Three-layer composition: bass dots, mid texture, high glissandi".postln;
)

// --- 9.2 Temporal Evolution ---
(
// A composition that evolves through distinct phases
var totalDensity = 150;

~arcs = List[];

// Phase 1 (0-0.25): Low register, sparse, descending
30.do {
    var x = rrand(0, 0.25);
    var freq = exprand(100, 500);
    var dur = rrand(0.02, 0.06);
    ~arcs.add([(x: x, freq: freq), (x: (x + dur).min(0.25), freq: freq * rrand(0.7, 0.95))]);
};

// Phase 2 (0.25-0.5): Middle register, dense, static
50.do {
    var x = rrand(0.25, 0.5);
    var freq = exprand(400, 1200);
    var dur = rrand(0.005, 0.02);
    ~arcs.add([(x: x, freq: freq), (x: (x + dur).min(0.5), freq: freq)]);
};

// Phase 3 (0.5-0.75): Wide register, medium density, mixed motion
40.do {
    var x = rrand(0.5, 0.75);
    var freq = exprand(150, 3000);
    var dur = rrand(0.01, 0.04);
    var motion = rrand(0.8, 1.25);
    ~arcs.add([(x: x, freq: freq), (x: (x + dur).min(0.75), freq: (freq * motion).clip(100, 4000))]);
};

// Phase 4 (0.75-1.0): High register, sparse, ascending
30.do {
    var x = rrand(0.75, 0.97);
    var freq = exprand(800, 2500);
    var dur = rrand(0.02, 0.06);
    ~arcs.add([(x: x, freq: freq), (x: (x + dur).min(1), freq: freq * rrand(1.1, 1.5))]);
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Four-phase composition with distinct characters".postln;
)

// --- 9.3 Stochastic Counterpoint ---
(
// Two "voices" with stochastic but distinct behaviors

// Voice A: Ascending tendency, longer grains
~voiceA = 45.collect {
    var x = rrand(0, 0.9);
    var baseFreq = x.linexp(0, 1, 200, 1500);
    var freq = baseFreq * rrand(0.85, 1.15);
    var dur = rrand(0.03, 0.08);
    var endFreq = freq * rrand(1.0, 1.3);  // Tends upward
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: endFreq.clip(100, 4000))]
};

// Voice B: Descending tendency, shorter grains
~voiceB = 45.collect {
    var x = rrand(0, 0.9);
    var baseFreq = x.linexp(0, 1, 2000, 400);
    var freq = baseFreq * rrand(0.85, 1.15);
    var dur = rrand(0.01, 0.04);
    var endFreq = freq * rrand(0.75, 1.0);  // Tends downward
    [(x: x, freq: freq), (x: (x + dur).min(1), freq: endFreq.clip(100, 4000))]
};

~app.data.clearAll;
(~voiceA ++ ~voiceB).do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Stochastic counterpoint: ascending vs descending voices".postln;
)

// --- 9.4 Fibonacci Time Structure ---
(
// Events placed according to Fibonacci ratios
var fib = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89];
var fibSum = fib.sum;
var fibPositions = fib.integrate / fibSum;

~arcs = List[];

fibPositions.do { |pos, i|
    var density = (15 - i).max(3);  // More events at earlier positions

    density.do {
        var x = pos + rrand(-0.02, 0.02);
        var freq = exprand(150, 2500);
        var dur = rrand(0.01, 0.04);

        ~arcs.add([
            (x: x.clip(0, 1), freq: freq),
            (x: (x + dur).clip(0, 1), freq: freq * rrand(0.9, 1.1))
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Fibonacci time structure".postln;
)


// ============================================================================
// PART 10: INTERACTIVE EXPLORATION
// ============================================================================

/*
Functions for live exploration and modification
*/

// --- 10.1 Cloud Generator Function ---
(
~makeCloud = { |params|
    var density = params[\density] ?? 50;
    var timeRange = params[\timeRange] ?? [0, 1];
    var freqRange = params[\freqRange] ?? [100, 3000];
    var durRange = params[\durRange] ?? [0.01, 0.04];
    var distribution = params[\distribution] ?? \uniform;
    var glissando = params[\glissando] ?? 1.0;  // 1.0 = no gliss, >1 = up, <1 = down

    density.collect {
        var x, freq, dur, endFreq;

        x = switch(distribution,
            \uniform, { rrand(timeRange[0], timeRange[1]) },
            \gaussian, { (timeRange.mean + (timeRange[1] - timeRange[0]) * 0.2 * 0.gauss(1)).clip(timeRange[0], timeRange[1]) },
            { rrand(timeRange[0], timeRange[1]) }
        );

        freq = exprand(freqRange[0], freqRange[1]);
        dur = rrand(durRange[0], durRange[1]);
        endFreq = freq * rrand(1/glissando, glissando);

        [(x: x, freq: freq), (x: (x + dur).min(1), freq: endFreq.clip(50, 6000))]
    }
};

"Cloud generator function ~makeCloud defined".postln;
)

// Example usage:
(
~cloud1 = ~makeCloud.((
    density: 60,
    timeRange: [0, 0.5],
    freqRange: [200, 1500],
    distribution: \gaussian,
    glissando: 1.3
));

~cloud2 = ~makeCloud.((
    density: 40,
    timeRange: [0.5, 1],
    freqRange: [400, 2500],
    durRange: [0.02, 0.06],
    glissando: 0.8
));

~app.data.clearAll;
(~cloud1 ++ ~cloud2).do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
)

// --- 10.2 Set Operation Functions ---
(
~cloudUnion = { |cloudA, cloudB|
    cloudA ++ cloudB
};

~cloudDifference = { |cloud, excludeRegion|
    // excludeRegion = [xMin, xMax, freqMin, freqMax]
    cloud.reject { |arc|
        var x = arc[0][\x];
        var freq = arc[0][\freq];
        (x >= excludeRegion[0]) and: { x <= excludeRegion[1] } and:
        { freq >= excludeRegion[2] } and: { freq <= excludeRegion[3] }
    }
};

~cloudIntersection = { |cloud, region|
    // Keep only events within region
    cloud.select { |arc|
        var x = arc[0][\x];
        var freq = arc[0][\freq];
        (x >= region[0]) and: { x <= region[1] } and:
        { freq >= region[2] } and: { freq <= region[3] }
    }
};

"Set operation functions defined: ~cloudUnion, ~cloudDifference, ~cloudIntersection".postln;
)

// Example:
(
~fullCloud = ~makeCloud.((density: 100, freqRange: [100, 4000]));
~filtered = ~cloudDifference.(~fullCloud, [0.3, 0.7, 400, 1500]);

~app.data.clearAll;
~filtered.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Cloud with center region removed".postln;
)


// ============================================================================
// PART 11: ARBORESCENCES - VON NEUMANN / BOURBAKI MODEL
// ============================================================================

/*
Connection to Set Theory and Badiou's Ontology:

The von Neumann cumulative hierarchy V builds all sets from the empty set:
  V₀ = ∅
  V₁ = {∅}
  V₂ = {∅, {∅}}
  V₃ = {∅, {∅}, {{∅}}, {∅, {∅}}}
  ...

This is an arborescence: a tree rooted at ∅, branching through ordinal levels.

Bourbaki's "mother structures" similarly branch from foundational axioms.

Badiou (Briefings on Existence): Being itself has this arborescent structure -
multiplicity emerging from the void through iterative construction.

Xenakis's arborescence parallels this: a musical point (like ∅) generates
proliferating lines through iterative random processes.
*/

// --- 11.1 Von Neumann Cumulative Hierarchy as Sound ---
(
// Each "level" Vα has 2^|Vα-1| elements
// We map this exponential growth to density and register expansion

var levels = 6;  // V₀ through V₅
var baseFreq = 400;

~arcs = List[];

levels.do { |level|
    var numElements = (2 ** level).asInteger.min(64);  // Cap for sanity
    var timeStart = level / levels;
    var timeEnd = (level + 1) / levels;
    var freqSpread = 1.2 ** level;  // Register expands each level

    numElements.do {
        var x = rrand(timeStart, timeEnd);
        var freq = baseFreq * rrand(1/freqSpread, freqSpread);
        var dur = rrand(0.005, 0.02);

        ~arcs.add([
            (x: x, freq: freq.clip(50, 5000)),
            (x: (x + dur).min(1), freq: freq.clip(50, 5000))
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Von Neumann hierarchy: exponential growth from void".postln;
)

// --- 11.2 True Arborescence: Branching from Root ---
(
// A single point branches into 2, each branches into 2, etc.
// This is the pure tree structure

var generations = 5;
var rootFreq = 500;
var rootX = 0.05;

~branches = List[];
~branches.add([(x: rootX, freq: rootFreq)]);  // Root "point"

~arcs = List[];

generations.do { |gen|
    var newBranches = List[];
    var timeOffset = (gen + 1) / (generations + 1);
    var branchSpread = 1.3 ** (gen + 1);  // Increasing divergence

    ~branches.do { |branch|
        var parentFreq = branch.last[\freq];
        var parentX = branch.last[\x];

        // Each branch splits into 2
        2.do { |i|
            var direction = if(i == 0) { 1.0 / branchSpread } { branchSpread };
            var childFreq = (parentFreq * direction * rrand(0.95, 1.05)).clip(80, 4000);
            var childX = timeOffset + rrand(-0.02, 0.02);

            // Arc from parent to child
            ~arcs.add([
                (x: parentX, freq: parentFreq),
                (x: childX, freq: childFreq)
            ]);

            newBranches.add([(x: childX, freq: childFreq)]);
        };
    };

    ~branches = newBranches;
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"True arborescence: binary branching tree".postln;
)

// --- 11.3 Stochastic Arborescence (Xenakis Style) ---
(
// Branches split with probability, not deterministically
// Number of children varies (1-3)
// Random walk perturbation at each generation

var generations = 6;
var rootFreq = 600;
var rootX = 0.02;
var branchProbability = 0.85;  // Probability of continued branching

~currentPoints = List[];
~currentPoints.add((x: rootX, freq: rootFreq));

~arcs = List[];

generations.do { |gen|
    var nextPoints = List[];
    var timeSlice = (gen + 1) / (generations + 1);

    ~currentPoints.do { |pt|
        var numChildren = if(branchProbability.coin) {
            [1, 2, 2, 3].choose  // Weighted toward 2
        } { 0 };

        numChildren.do {
            var freqWalk = rrand(0.7, 1.4);  // Random walk in freq
            var childFreq = (pt[\freq] * freqWalk).clip(80, 4500);
            var childX = timeSlice + rrand(-0.03, 0.03);
            var dur = rrand(0.01, 0.04);

            // Arc connecting parent to child region
            ~arcs.add([
                (x: pt[\x], freq: pt[\freq]),
                (x: childX.clip(0, 1), freq: childFreq)
            ]);

            nextPoints.add((x: childX, freq: childFreq));
        };
    };

    ~currentPoints = nextPoints;
    ("Generation %: % branches".format(gen + 1, nextPoints.size)).postln;
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Stochastic arborescence: probabilistic branching".postln;
)

// --- 11.4 Convergent Arborescence (Anti-Tree) ---
(
// Reverse of divergent: many points converge to one
// Like W.T. Tutte's "arborescence converging to root"

var generations = 5;
var targetFreq = 500;
var targetX = 0.95;

// Start with many scattered points
var numInitial = 32;
~currentPoints = numInitial.collect {
    (x: rrand(0.02, 0.15), freq: exprand(100, 3000))
};

~arcs = List[];

generations.do { |gen|
    var nextPoints = List[];
    var timeSlice = 0.15 + ((gen + 1) / generations * 0.75);
    var convergenceFactor = (gen + 1) / generations;

    // Group points into pairs/triples that merge
    ~currentPoints.clump(2).do { |group|
        var avgFreq = group.collect(_[\freq]).mean;
        var targetBlend = avgFreq.blend(targetFreq, convergenceFactor * 0.3);
        var childX = timeSlice + rrand(-0.02, 0.02);

        group.do { |pt|
            ~arcs.add([
                (x: pt[\x], freq: pt[\freq]),
                (x: childX.clip(0, 1), freq: targetBlend.clip(80, 4000))
            ]);
        };

        nextPoints.add((x: childX, freq: targetBlend));
    };

    ~currentPoints = nextPoints;
};

// Final convergence to target
~currentPoints.do { |pt|
    ~arcs.add([
        (x: pt[\x], freq: pt[\freq]),
        (x: targetX, freq: targetFreq)
    ]);
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Convergent arborescence: many to one".postln;
)

// --- 11.5 Echelon Construction (Bourbaki) ---
(
// Bourbaki built structures through "echelon" - recursive product/powerset
// We model: start with 2 base frequencies, take "products" (combinations)

var baseFreqs = [200, 800];  // Two "base sets"
var echelonLevels = 4;

~arcs = List[];
~currentLevel = baseFreqs.collect { |f| (freq: f, x: 0.05) };

// Draw initial points
~currentLevel.do { |pt|
    ~arcs.add([
        (x: pt[\x], freq: pt[\freq]),
        (x: pt[\x] + 0.02, freq: pt[\freq])
    ]);
};

echelonLevels.do { |level|
    var nextLevel = List[];
    var timeSlice = (level + 1) / echelonLevels * 0.8 + 0.1;

    // "Product": combine pairs from current level
    ~currentLevel.do { |a|
        ~currentLevel.do { |b|
            if(a != b) {
                // New frequency from combination
                var newFreq = sqrt(a[\freq] * b[\freq]) * rrand(0.9, 1.1);
                var newX = timeSlice + rrand(-0.03, 0.03);

                // Arcs from both parents
                ~arcs.add([
                    (x: a[\x], freq: a[\freq]),
                    (x: newX, freq: newFreq.clip(80, 4000))
                ]);
                ~arcs.add([
                    (x: b[\x], freq: b[\freq]),
                    (x: newX, freq: newFreq.clip(80, 4000))
                ]);

                nextLevel.add((freq: newFreq, x: newX));
            };
        };
    };

    // Also include "powerset" - variations of existing
    ~currentLevel.do { |pt|
        var variation = pt[\freq] * [0.5, 2.0].choose;
        var newX = timeSlice + rrand(-0.02, 0.02);

        ~arcs.add([
            (x: pt[\x], freq: pt[\freq]),
            (x: newX, freq: variation.clip(80, 4000))
        ]);
        nextLevel.add((freq: variation, x: newX));
    };

    ~currentLevel = nextLevel.scramble.keep(12);  // Limit growth
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Echelon construction: products and powersets".postln;
)

// --- 11.5b Echelon Schemes and Type Signatures ---
(
// Bourbaki's echelon scheme notation: E × E, P(E), P(E × E), P(P(E)), etc.
// Each level has a "type signature" describing how it was constructed
// We model this musically: frequencies carry their construction history

var baseSet = [200, 400, 800];  // Base set E (three pitches)
var echelonTypes = Dictionary[];  // Store elements by their type

// Type 0: Base elements E
echelonTypes[\E] = baseSet.collect { |f| (freq: f, type: \E, depth: 0) };

// Type 1: Product E × E (Cartesian product - all pairs)
echelonTypes[\ExE] = [];
baseSet.do { |a|
    baseSet.do { |b|
        // Pair becomes geometric mean
        var newFreq = sqrt(a * b);
        echelonTypes[\ExE] = echelonTypes[\ExE].add(
            (freq: newFreq, type: \ExE, parents: [a, b], depth: 1)
        );
    };
};

// Type 2: Power set P(E) (all subsets - represented by spectral combinations)
echelonTypes[\PE] = [];
(2 ** baseSet.size).asInteger.do { |i|
    var subset = [];
    baseSet.size.do { |j|
        if(i.asBinaryDigits(baseSet.size)[j] == 1) {
            subset = subset.add(baseSet[j]);
        };
    };
    if(subset.size > 0) {
        // Subset becomes average frequency weighted by position
        var avgFreq = subset.sum / subset.size;
        echelonTypes[\PE] = echelonTypes[\PE].add(
            (freq: avgFreq, type: \PE, subset: subset, depth: 1, cardinality: subset.size)
        );
    };
};

// Type 3: P(E × E) - Power set of product
echelonTypes[\PExE] = [];
echelonTypes[\ExE].do { |elem, i|
    // Include some elements from ExE in subsets
    if(0.5.coin) {
        var variation = elem[\freq] * [0.75, 1.0, 1.5].choose;
        echelonTypes[\PExE] = echelonTypes[\PExE].add(
            (freq: variation, type: \PExE, source: elem, depth: 2)
        );
    };
};

// Type 4: E × P(E) - Product of base with power set
echelonTypes[\ExPE] = [];
baseSet.do { |e|
    echelonTypes[\PE].do { |p|
        var combined = sqrt(e * p[\freq]);
        echelonTypes[\ExPE] = echelonTypes[\ExPE].add(
            (freq: combined, type: \ExPE, base: e, powerset: p, depth: 2)
        );
    };
};

~arcs = List[];

// Visualize: each type gets a time region, arcs show derivation
[\E, \ExE, \PE, \PExE, \ExPE].do { |type, typeIdx|
    var timeBase = typeIdx / 5;
    var elements = echelonTypes[type];
    var typeDepth = elements[0][\depth];

    elements.do { |elem, i|
        var x = timeBase + (i / elements.size.max(1) * 0.15) + 0.02;
        var freq = elem[\freq].clip(80, 4000);

        // Dot for each element
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.01, freq: freq)
        ]);

        // Connect to parents if they exist
        if(elem[\parents].notNil) {
            elem[\parents].do { |parentFreq|
                var parentX = 0 + (baseSet.indexOf(parentFreq) ? 0 / baseSet.size * 0.15) + 0.02;
                ~arcs.add([
                    (x: parentX, freq: parentFreq),
                    (x: x, freq: freq)
                ]);
            };
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Echelon type signatures: E, E×E, P(E), P(E×E), E×P(E)".postln;
)

// --- 11.5c Mother Structures (Bourbaki's Three Pillars) ---
(
// Bourbaki identified three "mother structures" from which all math derives:
// 1. Algebraic structures (groups, rings, fields) - operations
// 2. Order structures (lattices, orderings) - relations
// 3. Topological structures (neighborhoods, continuity) - proximity
//
// Musical interpretation:
// - Algebraic: intervals as group operations (transposition, inversion)
// - Order: pitch ordering, temporal sequence
// - Topological: spectral neighborhoods, timbral proximity

var fundamentalFreq = 220;

~arcs = List[];

// === ALGEBRAIC STRUCTURE (left third) ===
// Group operations on pitch: transposition by intervals
{
    var groupElements = [1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2];  // Just intonation ratios
    var baseX = 0.02;

    // The "group" - transpositions
    groupElements.do { |ratio, i|
        var freq = fundamentalFreq * ratio;
        var x = baseX + (i * 0.035);
        var inverted;

        // Base pitch
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.02, freq: freq)
        ]);

        // Show "operation" - inversion around fundamental
        inverted = fundamentalFreq * fundamentalFreq / freq;
        inverted = inverted.clip(80, 4000);
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.025, freq: inverted)
        ]);
    };

    "Algebraic: group operations (transposition, inversion)".postln;
}.value;

// === ORDER STRUCTURE (middle third) ===
// Lattice of pitch relationships
{
    var latticeBase = 0.35;
    var lattice = [
        // (freq ratio, x offset, y level)
        [1, 0, 0],       // Bottom: fundamental
        [5/4, -0.03, 1], // Major third
        [3/2, 0.03, 1],  // Fifth
        [15/8, 0, 2],    // Major seventh (meet of 5/4 and 3/2 in frequency lattice)
        [2, 0, 3]        // Octave: top of lattice
    ];

    // Draw lattice nodes
    lattice.do { |node|
        var ratio = node[0];
        var xOff = node[1];
        var level = node[2];
        var freq = fundamentalFreq * ratio;
        var x = latticeBase + xOff + (level * 0.06);

        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.015, freq: freq)
        ]);
    };

    // Lattice edges (partial order)
    [
        [0, 1], [0, 2],   // Fundamental to thirds
        [1, 3], [2, 3],   // Thirds to seventh
        [3, 4]            // Seventh to octave
    ].do { |edge|
        var from = lattice[edge[0]];
        var to = lattice[edge[1]];
        var x1 = latticeBase + from[1] + (from[2] * 0.06);
        var x2 = latticeBase + to[1] + (to[2] * 0.06);

        ~arcs.add([
            (x: x1, freq: fundamentalFreq * from[0]),
            (x: x2, freq: fundamentalFreq * to[0])
        ]);
    };

    "Order: lattice structure of pitch relations".postln;
}.value;

// === TOPOLOGICAL STRUCTURE (right third) ===
// Neighborhoods in frequency space
{
    var topoBase = 0.68;
    var centers = [300, 600, 1200];  // Three "open sets"
    var neighborhoodRadius = [50, 80, 150];  // Wider at higher frequencies

    centers.do { |center, i|
        var x = topoBase + (i * 0.1);
        var radius = neighborhoodRadius[i];

        // Center point (bold)
        ~arcs.add([
            (x: x, freq: center),
            (x: x + 0.02, freq: center)
        ]);

        // Neighborhood points (nearby frequencies)
        8.do {
            var neighborFreq = center + radius.rand2;
            var neighborX = x + rrand(-0.03, 0.03);
            var dur = rrand(0.005, 0.015);

            neighborFreq = neighborFreq.clip(80, 4000);

            // Small dot in neighborhood
            ~arcs.add([
                (x: neighborX, freq: neighborFreq),
                (x: neighborX + dur, freq: neighborFreq * rrand(0.98, 1.02))
            ]);

            // Connection to center (showing proximity)
            if(0.3.coin) {
                ~arcs.add([
                    (x: x, freq: center),
                    (x: neighborX, freq: neighborFreq)
                ]);
            };
        };
    };

    "Topological: neighborhoods in spectral space".postln;
}.value;

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Mother structures: Algebraic | Order | Topological".postln;
)

// --- 11.5d Echelon as Scale Construction ---
(
// Bourbaki's echelon construction can generate musical scales:
// Start with octave (2:1) and fifth (3:2), build through products

var baseRatios = [2, 3];  // Octave and fifth as generators
var maxDenom = 128;       // Limit complexity
var fundamental = 200;

~ratios = Set[];
~ratios.add(1);  // Unison

// Echelon level 1: simple products/quotients
baseRatios.do { |a|
    baseRatios.do { |b|
        var ratio = a / b;
        // Keep in one octave
        while { ratio >= 2 } { ratio = ratio / 2 };
        while { ratio < 1 } { ratio = ratio * 2 };
        ~ratios.add(ratio.round(0.0001));
    };
};

// Echelon level 2: products of level 1
~level1 = ~ratios.asArray;
~level1.do { |a|
    ~level1.do { |b|
        var ratio = a * b;
        while { ratio >= 2 } { ratio = ratio / 2 };
        while { ratio < 1 } { ratio = ratio * 2 };
        if(ratio.asFraction[1] <= maxDenom) {
            ~ratios.add(ratio.round(0.0001));
        };
    };
};

// Echelon level 3: more combinations
~level2 = ~ratios.asArray;
~level2.do { |a|
    baseRatios.do { |b|
        [a * b, a / b].do { |ratio|
            while { ratio >= 2 } { ratio = ratio / 2 };
            while { ratio < 1 } { ratio = ratio * 2 };
            if(ratio.asFraction[1] <= maxDenom) {
                ~ratios.add(ratio.round(0.0001));
            };
        };
    };
};

// Sort and create scale
~scale = ~ratios.asArray.sort;
("Echelon-generated scale with" + ~scale.size + "pitches per octave").postln;

~arcs = List[];

// Display scale across time, with octave equivalents stacked
3.do { |octave|
    var octaveMult = 2 ** octave;
    var timeOffset = octave * 0.3 + 0.05;

    ~scale.do { |ratio, i|
        var freq = fundamental * ratio * octaveMult;
        var x = timeOffset + (i / ~scale.size * 0.25);

        if(freq > 80 and: { freq < 4000 }) {
            // Scale tone
            ~arcs.add([
                (x: x, freq: freq),
                (x: x + 0.01, freq: freq)
            ]);

            // Connect octave equivalents
            if(octave > 0) {
                var prevFreq = fundamental * ratio * (2 ** (octave - 1));
                var prevX = (octave - 1) * 0.3 + 0.05 + (i / ~scale.size * 0.25);
                if(prevFreq > 80) {
                    ~arcs.add([
                        (x: prevX, freq: prevFreq),
                        (x: x, freq: freq)
                    ]);
                };
            };
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Echelon scale construction from octave/fifth generators".postln;
)

// --- 11.5e Spectral Echelon (Harmonic Series as Base) ---
(
// Use harmonic series as base set, build echelon from overtones
// This connects Bourbaki's construction to acoustic/spectral music

var fundamental = 110;
var numHarmonics = 8;
var harmonics = (1..numHarmonics).collect { |n| fundamental * n };

~arcs = List[];

// Level 0: Base harmonics (the "set E")
~level0 = harmonics.collect { |f, i|
    var x = 0.02 + (i * 0.025);
    ~arcs.add([
        (x: x, freq: f),
        (x: x + 0.015, freq: f)
    ]);
    (freq: f, x: x)
};

// Level 1: Difference tones (E × E -> difference)
~level1 = List[];
harmonics.do { |a, i|
    harmonics.do { |b, j|
        if(i > j) {
            var diff = (a - b).abs;
            if(diff > 50 and: { diff < 3000 }) {
                var x = 0.25 + ((i + j) / (numHarmonics * 2) * 0.2);
                x = x + rrand(-0.02, 0.02);

                ~arcs.add([
                    (x: x, freq: diff),
                    (x: x + 0.01, freq: diff)
                ]);

                // Show derivation from parents
                ~arcs.add([
                    (x: ~level0[i][\x], freq: a),
                    (x: x, freq: diff)
                ]);
                ~arcs.add([
                    (x: ~level0[j][\x], freq: b),
                    (x: x, freq: diff)
                ]);

                ~level1.add((freq: diff, x: x, parents: [a, b]));
            };
        };
    };
};

// Level 2: Summation tones (selected pairs)
~level2 = List[];
harmonics[0..3].do { |a, i|
    harmonics[0..3].do { |b, j|
        if(i != j) {
            var sum = a + b;
            if(sum < 3000 and: { 0.5.coin }) {
                var x = 0.55 + rrand(0, 0.15);

                ~arcs.add([
                    (x: x, freq: sum),
                    (x: x + 0.01, freq: sum)
                ]);

                // Derivation arcs
                ~arcs.add([
                    (x: ~level0[i][\x], freq: a),
                    (x: x, freq: sum)
                ]);

                ~level2.add((freq: sum, x: x));
            };
        };
    };
};

// Level 3: Ring modulation products (|a ± b| combined)
~level3 = List[];
~level1.do { |diff|
    ~level2.do { |sum|
        if(0.3.coin) {
            var ring = sqrt(diff[\freq] * sum[\freq]);
            if(ring > 80 and: { ring < 3000 }) {
                var x = 0.78 + rrand(0, 0.15);

                ~arcs.add([
                    (x: x, freq: ring),
                    (x: x + 0.012, freq: ring)
                ]);

                // Connect to sources
                ~arcs.add([
                    (x: diff[\x], freq: diff[\freq]),
                    (x: x, freq: ring)
                ]);
            };
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Spectral echelon: harmonics -> difference tones -> sums -> ring mod".postln;
)

// --- 11.5f Echelon Transport (Morphisms) ---
(
// In Bourbaki, structures have morphisms (structure-preserving maps)
// We create an echelon structure then "transport" it via morphisms:
// - Transposition (additive in log-frequency)
// - Scaling (multiplicative in time)
// - Reflection (inversion)

var buildEchelon = { |baseFreqs, startX, label|
    var arcs = List[];
    var level0, level1;

    // Level 0
    level0 = baseFreqs.collect { |f, i|
        var x = startX + (i * 0.03);
        arcs.add([
            (x: x, freq: f),
            (x: x + 0.01, freq: f)
        ]);
        (freq: f, x: x)
    };

    // Level 1: products
    baseFreqs.do { |a, i|
        baseFreqs.do { |b, j|
            if(i < j) {
                var prod = sqrt(a * b);
                var x = startX + 0.1 + ((i + j) * 0.02);

                arcs.add([
                    (x: level0[i][\x], freq: a),
                    (x: x, freq: prod)
                ]);
                arcs.add([
                    (x: level0[j][\x], freq: b),
                    (x: x, freq: prod)
                ]);
                arcs.add([
                    (x: x, freq: prod),
                    (x: x + 0.01, freq: prod)
                ]);
            };
        };
    };

    arcs
};

~arcs = List[];

// Original echelon
~original = buildEchelon.([200, 350, 500], 0.02, "original");
~arcs.addAll(~original);

// Morphism 1: Transposition (up a fifth = multiply by 3/2)
~transposed = buildEchelon.([200, 350, 500].collect(_ * 1.5), 0.35, "transposed");
~arcs.addAll(~transposed);

// Morphism 2: Inversion (around 400 Hz)
~inverted = buildEchelon.([200, 350, 500].collect { |f| 400 * 400 / f }, 0.68, "inverted");
~arcs.addAll(~inverted);

// Draw morphism arrows between corresponding elements
3.do { |i|
    var origX = 0.02 + (i * 0.03);
    var transX = 0.35 + (i * 0.03);
    var invX = 0.68 + (i * 0.03);

    var origF = [200, 350, 500][i];
    var transF = origF * 1.5;
    var invF = 400 * 400 / origF;

    // Dashed-style connecting arcs (small segments)
    5.do { |j|
        var t = (j + 0.5) / 5;
        var x1 = origX.blend(transX, t);
        var x2 = origX.blend(transX, t + 0.15);
        var f1 = origF.blend(transF, t);
        var f2 = origF.blend(transF, t + 0.15);

        if(f1 > 80 and: { f2 < 4000 }) {
            ~arcs.add([
                (x: x1, freq: f1),
                (x: x2.min(transX), freq: f2.min(transF))
            ]);
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Echelon morphisms: Original | Transposed (×1.5) | Inverted".postln;
)

// --- 11.5g Recursive Echelon Depth ---
(
// Deep echelon: P(P(P(E))) - iterated powerset
// Each level exponentially increases complexity
// We limit by sampling at each level

var baseE = [150, 300, 600];  // 3 elements
var maxPerLevel = 8;          // Prevent explosion

~levels = List[];
~arcs = List[];

// Level 0: E
~levels.add(baseE.collect { |f| (freq: f, level: 0) });

// Build levels through powerset operation
4.do { |levelNum|
    var prevLevel = ~levels[levelNum];
    var nextLevel = List[];

    // "Powerset" - create elements representing subsets
    // Each new element is derived from combinations of previous level
    maxPerLevel.do {
        var numParents = rrand(1, prevLevel.size.min(3));
        var parents = prevLevel.scramble.keep(numParents);
        var newFreq;

        // New frequency from parent combination
        newFreq = parents.collect(_[\freq]).product ** (1 / numParents);
        newFreq = newFreq * rrand(0.9, 1.1);  // Small variation

        if(newFreq > 80 and: { newFreq < 4000 }) {
            nextLevel.add((
                freq: newFreq,
                level: levelNum + 1,
                parents: parents
            ));
        };
    };

    ~levels.add(nextLevel.asArray);
};

// Visualize: time = level, connect to parents
~levels.do { |level, levelNum|
    var timeBase = levelNum / ~levels.size * 0.85 + 0.05;

    level.do { |elem, i|
        var x = timeBase + (i / level.size.max(1) * 0.12);
        var freq = elem[\freq];

        // Element dot
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.008, freq: freq)
        ]);

        // Store position for child connections
        elem[\x] = x;

        // Connect to parents
        if(elem[\parents].notNil) {
            elem[\parents].do { |parent|
                if(parent[\x].notNil) {
                    ~arcs.add([
                        (x: parent[\x], freq: parent[\freq]),
                        (x: x, freq: freq)
                    ]);
                };
            };
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
("Recursive echelon: E -> P(E) -> P(P(E)) -> ... (" ++ ~levels.size ++ " levels)").postln;
)

// --- 11.5h Echelon with Sieve Filtering ---
(
// Combine Xenakis sieves with Bourbaki echelon:
// Build echelon structure, then filter through sieve

var sieve = { |n, modulo, residue|
    (n % modulo) == residue
};

var baseFreqs = [100, 200, 400, 800];
var sieveModulo = 3;
var sieveResidue = 1;

~arcs = List[];
~elements = List[];

// Build echelon
baseFreqs.do { |a, i|
    baseFreqs.do { |b, j|
        if(i <= j) {
            var newFreq = sqrt(a * b);
            ~elements.add((freq: newFreq, idx: ~elements.size, parents: [i, j]));
        };
    };
};

// Add more echelon levels
~elements.copy.do { |elem, i|
    baseFreqs.do { |base|
        var combined = sqrt(elem[\freq] * base);
        ~elements.add((freq: combined, idx: ~elements.size, parentElem: elem));
    };
};

// Apply sieve: only keep elements where index passes sieve
~filtered = ~elements.select { |elem|
    sieve.(elem[\idx], sieveModulo, sieveResidue)
};

~unfiltered = ~elements.reject { |elem|
    sieve.(elem[\idx], sieveModulo, sieveResidue)
};

// Draw filtered elements (prominent)
~filtered.do { |elem, i|
    var x = 0.05 + (i / ~filtered.size * 0.6);
    var freq = elem[\freq].clip(80, 4000);

    ~arcs.add([
        (x: x, freq: freq),
        (x: x + 0.03, freq: freq)
    ]);
};

// Draw unfiltered (faint, short)
~unfiltered.do { |elem, i|
    var x = 0.7 + (i / ~unfiltered.size.max(1) * 0.25);
    var freq = elem[\freq].clip(80, 4000);

    ~arcs.add([
        (x: x, freq: freq),
        (x: x + 0.005, freq: freq * 1.01)  // Tiny dots
    ]);
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
("Sieve-filtered echelon: mod" + sieveModulo + "residue" + sieveResidue).postln;
("Kept" + ~filtered.size + "of" + ~elements.size + "elements").postln;
)

// --- 11.6 Ordinal Spine with Branching ---
(
// The ordinals form the "spine" of the cumulative hierarchy
// We create a central ascending line with branches at each ordinal level

var numOrdinals = 12;
var spineFreqs = Array.geom(numOrdinals, 150, 1.25);  // Geometric ascent

~arcs = List[];

// The ordinal spine (main trunk)
(numOrdinals - 1).do { |i|
    var x1 = i / numOrdinals;
    var x2 = (i + 1) / numOrdinals;

    ~arcs.add([
        (x: x1, freq: spineFreqs[i]),
        (x: x2, freq: spineFreqs[i + 1])
    ]);
};

// Branches at each ordinal
numOrdinals.do { |i|
    var x = i / numOrdinals;
    var baseFreq = spineFreqs[i];
    var numBranches = (i + 1).min(5);  // More branches at higher ordinals

    numBranches.do {
        var branchFreq = baseFreq * rrand(0.5, 2.0);
        var branchX = x + rrand(0.02, 0.06);
        var dur = rrand(0.01, 0.03);

        ~arcs.add([
            (x: x, freq: baseFreq),
            (x: branchX.min(1), freq: branchFreq.clip(80, 4500))
        ]);

        // Sometimes branch further
        if(0.3.coin) {
            var subFreq = branchFreq * rrand(0.7, 1.4);
            ~arcs.add([
                (x: branchX, freq: branchFreq),
                (x: (branchX + rrand(0.01, 0.03)).min(1), freq: subFreq.clip(80, 4500))
            ]);
        };
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Ordinal spine with branching".postln;
)

// --- 11.6b Successor and Limit Ordinals ---
(
// In set theory, ordinals come in two types:
// - Successor ordinals: α + 1 (have an immediate predecessor)
// - Limit ordinals: ω, ω·2, ω² (limits of sequences, no immediate predecessor)
//
// Musical interpretation:
// - Successors: discrete steps, clear articulation
// - Limits: continuous approach, accumulation points

var numFinite = 8;      // 0, 1, 2, ... 7 (finite ordinals, all successors except 0)
var fundamental = 120;

~arcs = List[];

// === FINITE ORDINALS (successors) ===
// Clear discrete steps
~finitePositions = numFinite.collect { |n|
    var x = n / numFinite * 0.25 + 0.02;
    var freq = fundamental * (n + 1);  // Harmonic series

    // Solid tone for each ordinal
    ~arcs.add([
        (x: x, freq: freq.clip(80, 4000)),
        (x: x + 0.02, freq: freq.clip(80, 4000))
    ]);

    // Successor arrow from previous
    if(n > 0) {
        var prevX = (n - 1) / numFinite * 0.25 + 0.02;
        var prevFreq = fundamental * n;
        ~arcs.add([
            (x: prevX + 0.02, freq: prevFreq),
            (x: x, freq: freq.clip(80, 4000))
        ]);
    };

    (x: x, freq: freq, ordinal: n)
};

// === FIRST LIMIT ORDINAL: ω ===
// The limit of all finite ordinals - represented as accumulation
{
    var omegaX = 0.35;
    var omegaFreq = 800;  // New register for limit

    // Convergence arcs from finite ordinals toward ω
    ~finitePositions.do { |pos, i|
        var intensity = (i / numFinite) ** 2;  // Later ordinals contribute more

        if(intensity > 0.1) {
            ~arcs.add([
                (x: pos[\x] + 0.02, freq: pos[\freq].clip(80, 4000)),
                (x: omegaX, freq: omegaFreq)
            ]);
        };
    };

    // ω itself - broader, sustained
    ~arcs.add([
        (x: omegaX, freq: omegaFreq),
        (x: omegaX + 0.04, freq: omegaFreq)
    ]);

    // Label point
    ~omegaPos = (x: omegaX, freq: omegaFreq);
}.value;

// === ω + n (successors of ω) ===
4.do { |n|
    var x = 0.42 + (n * 0.04);
    var freq = 800 + (n + 1) * 80;  // Steps above ω

    ~arcs.add([
        (x: x, freq: freq),
        (x: x + 0.015, freq: freq)
    ]);

    // Successor connection
    if(n == 0) {
        ~arcs.add([
            (x: ~omegaPos[\x] + 0.04, freq: ~omegaPos[\freq]),
            (x: x, freq: freq)
        ]);
    } {
        var prevX = 0.42 + ((n - 1) * 0.04);
        var prevFreq = 800 + n * 80;
        ~arcs.add([
            (x: prevX + 0.015, freq: prevFreq),
            (x: x, freq: freq)
        ]);
    };
};

// === ω · 2 (second limit ordinal) ===
{
    var omega2X = 0.62;
    var omega2Freq = 1400;

    // Convergence from ω + n
    4.do { |n|
        var srcX = 0.42 + (n * 0.04);
        var srcFreq = 800 + (n + 1) * 80;

        ~arcs.add([
            (x: srcX + 0.015, freq: srcFreq),
            (x: omega2X, freq: omega2Freq)
        ]);
    };

    ~arcs.add([
        (x: omega2X, freq: omega2Freq),
        (x: omega2X + 0.04, freq: omega2Freq)
    ]);

    ~omega2Pos = (x: omega2X, freq: omega2Freq);
}.value;

// === ω² (limit of ω·n) ===
{
    var omegaSqX = 0.85;
    var omegaSqFreq = 2200;

    // Implied convergence
    ~arcs.add([
        (x: ~omega2Pos[\x] + 0.04, freq: ~omega2Pos[\freq]),
        (x: omegaSqX, freq: omegaSqFreq)
    ]);

    // ω² - even broader
    ~arcs.add([
        (x: omegaSqX, freq: omegaSqFreq),
        (x: omegaSqX + 0.06, freq: omegaSqFreq)
    ]);
}.value;

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Successor and limit ordinals: finite → ω → ω+n → ω·2 → ω²".postln;
)

// --- 11.6c Ordinal Arithmetic as Musical Operations ---
(
// Ordinal arithmetic is non-commutative!
// 1 + ω = ω, but ω + 1 ≠ ω
// We demonstrate this musically

var baseFreq = 200;
var omegaFreq = 600;

~arcs = List[];

// === 1 + ω = ω ===
// The single "1" gets absorbed into the limit
{
    var startX = 0.02;

    // The "1" - a single tone
    ~arcs.add([
        (x: startX, freq: baseFreq),
        (x: startX + 0.02, freq: baseFreq)
    ]);

    // Sequence approaching ω
    8.do { |i|
        var x = startX + 0.05 + (i * 0.02);
        var freq = baseFreq + (i * 30);

        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.01, freq: freq)
        ]);
    };

    // ω absorbs the "1"
    ~arcs.add([
        (x: startX + 0.23, freq: omegaFreq),
        (x: startX + 0.28, freq: omegaFreq)
    ]);

    // Show absorption
    ~arcs.add([
        (x: startX + 0.02, freq: baseFreq),
        (x: startX + 0.23, freq: omegaFreq)
    ]);
}.value;

// === ω + 1 ≠ ω ===
// The "1" after ω remains distinct
{
    var startX = 0.38;

    // ω first
    8.do { |i|
        var x = startX + 0.05 + (i * 0.02);
        var freq = baseFreq + (i * 30);

        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.01, freq: freq)
        ]);
    };

    // ω limit point
    ~arcs.add([
        (x: startX + 0.18, freq: omegaFreq),
        (x: startX + 0.22, freq: omegaFreq)
    ]);

    // + 1: distinct successor AFTER ω
    ~arcs.add([
        (x: startX + 0.26, freq: omegaFreq + 100),
        (x: startX + 0.30, freq: omegaFreq + 100)
    ]);

    // Connection showing succession
    ~arcs.add([
        (x: startX + 0.22, freq: omegaFreq),
        (x: startX + 0.26, freq: omegaFreq + 100)
    ]);
}.value;

// === ω · 2 vs 2 · ω ===
// ω · 2 = ω + ω (two copies of ω)
// 2 · ω = ω (just ω itself!)
{
    var startX = 0.72;

    // ω · 2: two distinct limit sequences
    // First ω
    4.do { |i|
        var x = startX + (i * 0.015);
        var freq = 300 + (i * 40);
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.008, freq: freq)
        ]);
    };
    ~arcs.add([
        (x: startX + 0.07, freq: 500),
        (x: startX + 0.09, freq: 500)
    ]);

    // Second ω (after first)
    4.do { |i|
        var x = startX + 0.11 + (i * 0.015);
        var freq = 550 + (i * 40);
        ~arcs.add([
            (x: x, freq: freq),
            (x: x + 0.008, freq: freq)
        ]);
    };
    ~arcs.add([
        (x: startX + 0.18, freq: 750),
        (x: startX + 0.20, freq: 750)
    ]);

    // Connection between the two omegas
    ~arcs.add([
        (x: startX + 0.09, freq: 500),
        (x: startX + 0.11, freq: 550)
    ]);
}.value;

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Ordinal arithmetic: 1+ω=ω | ω+1≠ω | ω·2".postln;
)

// --- 11.6d Cantor Normal Form Spine ---
(
// Every ordinal can be written in Cantor Normal Form:
// α = ω^β₁·c₁ + ω^β₂·c₂ + ... (decreasing exponents)
//
// We create spines for different "digits" in this representation

var spines = [
    // (exponent, coefficient, baseFreq)
    [3, 1, 200],   // ω³
    [2, 2, 400],   // ω²·2
    [1, 3, 700],   // ω·3
    [0, 5, 1100]   // 5
];

~arcs = List[];

spines.do { |spine, spineIdx|
    var exp = spine[0];
    var coef = spine[1];
    var baseFreq = spine[2];
    var spineX = spineIdx / spines.size * 0.85 + 0.05;
    var spineWidth = 0.18;

    // Draw the spine
    var numPoints = (exp + 1) * 3;
    var freqRange = baseFreq * (1.5 ** exp);

    numPoints.do { |i|
        var t = i / numPoints;
        var x = spineX + (t * spineWidth);
        // Exponential shape for higher exponents
        var freq = baseFreq + (freqRange * (t ** (exp + 1)));

        if(i > 0) {
            var prevT = (i - 1) / numPoints;
            var prevX = spineX + (prevT * spineWidth);
            var prevFreq = baseFreq + (freqRange * (prevT ** (exp + 1)));

            ~arcs.add([
                (x: prevX, freq: prevFreq.clip(80, 4000)),
                (x: x, freq: freq.clip(80, 4000))
            ]);
        };
    };

    // Coefficient branches
    coef.do { |c|
        var branchX = spineX + (spineWidth * rrand(0.3, 0.9));
        var branchT = (branchX - spineX) / spineWidth;
        var branchBaseFreq = baseFreq + (freqRange * (branchT ** (exp + 1)));
        var branchFreq = branchBaseFreq * rrand(0.6, 1.6);

        ~arcs.add([
            (x: branchX, freq: branchBaseFreq.clip(80, 4000)),
            (x: branchX + rrand(0.02, 0.04), freq: branchFreq.clip(80, 4000))
        ]);
    };
};

// Connect spines (showing addition)
(spines.size - 1).do { |i|
    var spine1 = spines[i];
    var spine2 = spines[i + 1];
    var x1 = (i + 1) / spines.size * 0.85 + 0.05;
    var x2 = (i + 1) / spines.size * 0.85 + 0.05;
    var freq1 = spine1[2] + (spine1[2] * (1.5 ** spine1[0]));
    var freq2 = spine2[2];

    ~arcs.add([
        (x: x1, freq: freq1.clip(80, 4000)),
        (x: x2 + 0.02, freq: freq2.clip(80, 4000))
    ]);
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Cantor Normal Form: ω³ + ω²·2 + ω·3 + 5".postln;
)

// --- 11.6e Transfinite Recursion Patterns ---
(
// Transfinite recursion: define F(α) for all ordinals α
// F(0) = base case
// F(α+1) = successor rule using F(α)
// F(λ) = limit rule using {F(β) : β < λ}
//
// Musical: each ordinal level adds a transformation

var baseFreq = 300;
var baseDur = 0.03;

~arcs = List[];
~levels = List[];

// F(0): base case - single tone
~levels.add([
    (x: 0.02, freq: baseFreq, dur: baseDur)
]);
~arcs.add([
    (x: 0.02, freq: baseFreq),
    (x: 0.02 + baseDur, freq: baseFreq)
]);

// F(n+1): successor adds harmonic + time shift
12.do { |n|
    var prevLevel = ~levels[n];
    var nextLevel = List[];
    var timeOffset = (n + 1) * 0.07 + 0.05;
    var limitFreq, limitX;

    prevLevel.do { |elem|
        var newX = timeOffset + (elem[\x] - prevLevel[0][\x]) * 0.8;
        var harmFreq;

        // Copy with time shift
        nextLevel.add((x: newX, freq: elem[\freq], dur: elem[\dur] * 0.9));

        // Add harmonic
        harmFreq = elem[\freq] * (n + 2) / (n + 1);
        if(harmFreq < 4000) {
            nextLevel.add((x: newX + 0.01, freq: harmFreq, dur: elem[\dur] * 0.7));
        };
    };

    // Keep only first few to prevent explosion
    nextLevel = nextLevel.keep(8);
    ~levels.add(nextLevel.asArray);

    // Draw this level
    nextLevel.do { |elem|
        ~arcs.add([
            (x: elem[\x], freq: elem[\freq]),
            (x: elem[\x] + elem[\dur], freq: elem[\freq])
        ]);
    };

    // Limit behavior: every 4th step, consolidate (mimics limit ordinal)
    if((n + 1) % 4 == 0) {
        limitFreq = nextLevel.collect(_[\freq]).sum / nextLevel.size;
        limitX = timeOffset + 0.04;

        // Convergence arcs
        nextLevel.do { |elem|
            ~arcs.add([
                (x: elem[\x] + elem[\dur], freq: elem[\freq]),
                (x: limitX, freq: limitFreq)
            ]);
        };

        // Limit point
        ~arcs.add([
            (x: limitX, freq: limitFreq),
            (x: limitX + 0.025, freq: limitFreq)
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Transfinite recursion: successor steps with limit consolidation".postln;
)

// --- 11.6f Epsilon Numbers (Fixed Points) ---
(
// ε₀ is the first ordinal where ω^ε₀ = ε₀
// It's a fixed point of the exponentiation operation
// Musically: self-similar structures, fractal spines

var epsilon0 = { |depth, x, freq, width|
    var arcs = List[];

    if(depth > 0 and: { freq < 4000 } and: { freq > 80 }) {
        // Main spine segment
        arcs.add([
            (x: x, freq: freq),
            (x: x + width, freq: freq * 1.3)
        ]);

        // Self-similar branches (ω^ε = ε)
        3.do { |i|
            var branchX = x + (width * (i + 1) / 4);
            var branchFreq = freq * (1 + (i * 0.15));
            var branchWidth = width * 0.4;

            // Recursive call - same structure at smaller scale
            arcs = arcs ++ epsilon0.(depth - 1, branchX, branchFreq, branchWidth);
        };
    };

    arcs
};

~arcs = List[];

// Build ε₀ structure
~arcs = ~arcs ++ epsilon0.(4, 0.02, 150, 0.35);

// Add a second ε₀ at different register (ε₁ analogy)
~arcs = ~arcs ++ epsilon0.(3, 0.55, 800, 0.25);

// Connect them (tower of epsilons)
~arcs.add([
    (x: 0.37, freq: 150 * (1.3 ** 3)),
    (x: 0.55, freq: 800)
]);

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Epsilon numbers: self-similar fixed points of ordinal exponentiation".postln;
)

// --- 11.6g Multi-Spine Ordinal Weave ---
(
// Multiple ordinal spines that interweave and interact
// Like polyphonic voices each following ordinal progression

var numSpines = 4;
var spineColors = [
    (base: 150, mult: 1.2, phase: 0),
    (base: 220, mult: 1.18, phase: 0.1),
    (base: 330, mult: 1.15, phase: 0.2),
    (base: 500, mult: 1.12, phase: 0.05)
];
var numOrdinals = 16;

~arcs = List[];
~spinePositions = numSpines.collect { List[] };

// Build each spine
numSpines.do { |s|
    var color = spineColors[s];
    var prevX, prevFreq;

    numOrdinals.do { |i|
        var x = (i / numOrdinals * 0.85) + color[\phase].fold(0, 0.1) + 0.02;
        var freq = color[\base] * (color[\mult] ** i);

        // Wobble
        x = x + (sin(i * 1.5 + (s * 2)) * 0.02);
        freq = freq * (1 + (sin(i * 0.8 + (s * 3)) * 0.1));
        freq = freq.clip(80, 4000);

        ~spinePositions[s].add((x: x, freq: freq, ordinal: i));

        // Connect to previous in same spine
        if(i > 0) {
            ~arcs.add([
                (x: prevX, freq: prevFreq),
                (x: x, freq: freq)
            ]);
        };

        prevX = x;
        prevFreq = freq;
    };
};

// Cross-spine connections at "harmonic" ordinals (multiples of 4)
numOrdinals.do { |i|
    if(i % 4 == 0 and: { i > 0 }) {
        // Connect all spines at this ordinal
        (numSpines - 1).do { |s|
            var pos1 = ~spinePositions[s][i];
            var pos2 = ~spinePositions[s + 1][i];

            ~arcs.add([
                (x: pos1[\x], freq: pos1[\freq]),
                (x: pos2[\x], freq: pos2[\freq])
            ]);
        };
    };
};

// Limit ordinal convergence (at end)
{
    var limitX = 0.92;
    var limitFreq = spineColors.collect(_[\base]).sum / numSpines * (1.15 ** numOrdinals);
    limitFreq = limitFreq.clip(80, 4000);

    numSpines.do { |s|
        var lastPos = ~spinePositions[s].last;
        ~arcs.add([
            (x: lastPos[\x], freq: lastPos[\freq]),
            (x: limitX, freq: limitFreq)
        ]);
    };

    // Convergence point
    ~arcs.add([
        (x: limitX, freq: limitFreq),
        (x: limitX + 0.05, freq: limitFreq)
    ]);
}.value;

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Multi-spine ordinal weave with cross-connections".postln;
)

// --- 11.6h Ordinal Collapse ---
(
// "Ordinal collapse" - used in proof theory
// Large ordinals mapped/collapsed to smaller ones
// Musically: wide-ranging material compressed into a register

var largeOrdinalRange = [80, 4000];  // Full range
var collapsedRange = [300, 600];     // Target range

~arcs = List[];

// Phase 1: Expansive ordinal structure (uncollapsed)
~uncollapsed = List[];
20.do { |i|
    var x = 0.02 + (i / 20 * 0.35);
    var freq = exprand(largeOrdinalRange[0], largeOrdinalRange[1]);

    ~uncollapsed.add((x: x, freq: freq));

    ~arcs.add([
        (x: x, freq: freq),
        (x: x + 0.01, freq: freq)
    ]);
};

// Connect some for structure
~uncollapsed.scramble.clump(2).do { |pair|
    if(pair.size == 2) {
        ~arcs.add([
            (x: pair[0][\x], freq: pair[0][\freq]),
            (x: pair[1][\x], freq: pair[1][\freq])
        ]);
    };
};

// Phase 2: Collapse mapping
~collapsed = List[];
~uncollapsed.do { |pt, i|
    var newX = 0.45 + (i / 20 * 0.25);
    // Logarithmic collapse into target range
    var normalized = pt[\freq].explin(largeOrdinalRange[0], largeOrdinalRange[1], 0, 1);
    var newFreq = normalized.linexp(0, 1, collapsedRange[0], collapsedRange[1]);

    ~collapsed.add((x: newX, freq: newFreq, original: pt));

    ~arcs.add([
        (x: newX, freq: newFreq),
        (x: newX + 0.008, freq: newFreq)
    ]);

    // Collapse arc
    ~arcs.add([
        (x: pt[\x] + 0.01, freq: pt[\freq]),
        (x: newX, freq: newFreq)
    ]);
};

// Phase 3: Post-collapse structure (maintains relative ordering)
~collapsed.sort { |a, b| a[\freq] < b[\freq] };
~collapsed.do { |pt, i|
    var x = 0.75 + (i / 20 * 0.2);
    var freq = pt[\freq];

    ~arcs.add([
        (x: x, freq: freq),
        (x: x + 0.012, freq: freq)
    ]);

    // Connect in order
    if(i > 0) {
        var prevPt = ~collapsed[i - 1];
        var prevX = 0.75 + ((i - 1) / 20 * 0.2);
        ~arcs.add([
            (x: prevX + 0.012, freq: prevPt[\freq]),
            (x: x, freq: freq)
        ]);
    };
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Ordinal collapse: wide range compressed to narrow band".postln;
)

// --- 11.7 The Void Blossoms (Badiou's Ontology) ---
(
// "The One is not" - Being emerges from void (∅)
// A single silent point explodes into multiplicity

var phases = 5;
var voidX = 0.02;
var voidFreq = 400;  // The "void" - a single origin

~arcs = List[];

// Phase 1: The void (single dot)
~arcs.add([
    (x: voidX, freq: voidFreq),
    (x: voidX + 0.01, freq: voidFreq)
]);

// Each phase: exponential multiplication
~currentMultiple = [(x: voidX, freq: voidFreq)];

phases.do { |phase|
    var nextMultiple = List[];
    var phaseDensity = (3 ** phase).min(80);
    var phaseStart = (phase + 1) / (phases + 1);
    var phaseSpread = 1.5 ** (phase + 1);

    phaseDensity.do {
        var parent = ~currentMultiple.choose;
        var childFreq = voidFreq * rrand(1/phaseSpread, phaseSpread);
        var childX = phaseStart + rrand(-0.05, 0.08);

        ~arcs.add([
            (x: parent[\x], freq: parent[\freq]),
            (x: childX.clip(0, 1), freq: childFreq.clip(60, 5000))
        ]);

        nextMultiple.add((x: childX, freq: childFreq));
    };

    ~currentMultiple = ~currentMultiple ++ nextMultiple;
    ~currentMultiple = ~currentMultiple.scramble.keep(30);  // Limit memory
};

~app.data.clearAll;
~arcs.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"The void blossoms: multiplicity from nothing".postln;
)

// --- 11.8 Arborescence Generator Function ---
(
// Reusable function for generating arborescences

~makeArborescence = { |params|
    var rootX = params[\rootX] ?? 0.05;
    var rootFreq = params[\rootFreq] ?? 500;
    var generations = params[\generations] ?? 5;
    var branchFactor = params[\branchFactor] ?? 2;  // Avg children per node
    var freqSpread = params[\freqSpread] ?? 1.4;
    var branchProb = params[\branchProb] ?? 0.9;
    var mode = params[\mode] ?? \diverge;  // \diverge or \converge

    var arcs = List[];
    var currentPoints, targetFreq, targetX;

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

                    arcs.add([
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
                    arcs.add([
                        (x: pt[\x], freq: pt[\freq]),
                        (x: childX, freq: blendedFreq.clip(60, 5000))
                    ]);
                };

                nextPoints.add((x: childX, freq: blendedFreq));
            };

            currentPoints = nextPoints;
        };

        currentPoints.do { |pt|
            arcs.add([
                (x: pt[\x], freq: pt[\freq]),
                (x: targetX, freq: targetFreq)
            ]);
        };
    };

    arcs
};

"Arborescence generator ~makeArborescence defined".postln;
)

// Example usage:
(
// Divergent arborescence
~tree1 = ~makeArborescence.((
    rootX: 0.05,
    rootFreq: 300,
    generations: 5,
    branchFactor: 2,
    freqSpread: 1.5,
    mode: \diverge
));

~app.data.clearAll;
~tree1.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Divergent arborescence".postln;
)

(
// Convergent arborescence
~tree2 = ~makeArborescence.((
    rootFreq: 600,
    generations: 4,
    branchFactor: 3,
    mode: \converge
));

~app.data.clearAll;
~tree2.do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Convergent arborescence".postln;
)

(
// Combined: diverge then converge (diamond shape)
~diverge = ~makeArborescence.((
    rootX: 0.05, rootFreq: 500,
    generations: 3, branchFactor: 2,
    mode: \diverge
));

// Shift converge to start where diverge ends
~converge = ~makeArborescence.((
    rootFreq: 500,
    generations: 3, branchFactor: 2,
    mode: \converge
));

~app.data.clearAll;
(~diverge ++ ~converge).do { |arc| ~app.data.addArc(arc) };
~app.gui.refresh;
"Diamond: diverge then converge".postln;
)


// ============================================================================
// CLEANUP
// ============================================================================

// Clear all arcs
(
~app.data.clearAll;
~app.gui.refresh;
"Cleared".postln;
)

// Stop application
(
~app.stop;
)

/*
===============================================================================
FURTHER EXPLORATION

These examples provide a foundation for stochastic composition in nUPIC.
Possible extensions:

1. Implement these as NUPICCloud class methods
2. Add real-time parameter modulation
3. Create GUI controls for cloud parameters
4. Implement genetic algorithms for cloud evolution
5. Add spectral analysis to inform cloud generation
6. Create notation/score export for the generated structures
7. Implement forcing (Cohen) for "generic" extensions of musical situations
8. Explore limit ordinals as asymptotic musical structures

References:

Music & Composition:
- Xenakis, I. (1992). Formalized Music: Thought and Mathematics in Composition
- Xenakis, I. & Varga, B.A. (1996). Conversations with Iannis Xenakis
- Roads, C. (2001). Microsound
- Di Scipio, A. (various). Articles on stochastic synthesis

Mathematics & Philosophy:
- Bourbaki, N. (1948). L'architecture des mathématiques
- Badiou, A. (2006). Briefings on Existence: A Short Treatise on Transitory Ontology
- Badiou, A. (2005). Being and Event (L'Être et l'événement)
- Von Neumann, J. - The cumulative hierarchy of sets

Graph Theory:
- Tutte, W.T. - Arborescences diverging/converging
- Ore, O. - Theory of Graphs (directed trees)
===============================================================================
*/
