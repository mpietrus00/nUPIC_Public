/*
 * nUPIC API Reference (Class-Based System)
 * =========================================
 * Programmatic access to arcs, data, and transformations using the new Class system.
 *
 * The nUPIC application is now built on proper SuperCollider classes:
 *   - NUPICApplication: Main app controller
 *   - NUPICData: Arc/page/envelope data management
 *   - NUPICGUI: User interface
 *   - NUPICPlayback: Audio playback engine
 *   - NUPICWavetableEditor: Wavetable editing
 *   - NUPICAmplitudeEditor: Amplitude envelope editing
 *   - NUPICSpatialEditor: Spatial envelope editing
 */


// ============================================================================
// STARTING THE APPLICATION
// ============================================================================

// Standard mode (no extensions required)
~app = NUPICApplication.new;
~app.start;

// Oversampling mode (requires OversamplingOscillators extension)
~app = NUPICApplication(oversampling: true);
~app.start;

// Or in one line:
NUPICApplication(oversampling: true).start;

// Access core components (after start)
~app.data;      // NUPICData instance
~app.gui;       // NUPICGUI instance
~app.playback;  // NUPICPlayback instance

// Stop the application
~app.stop;

// Check if running
~app.isRunning;


// ============================================================================
// DATA ACCESS (NUPICData)
// ============================================================================
// Access arc data, pages, selections, and envelopes through the data object.

// Get data reference (after app.start)
~data = ~app.data;

// --- Arc Counts & Access ---

~data.arcCount;           // Number of arcs on current page
~data.arcs;               // List of all arcs (each arc is List of points)
~data.getArc(0);          // Get specific arc by index

// --- Selection ---

~data.selectedArcs;                    // Set of selected arc indices
~data.select(0);                       // Select arc by index
~data.selectAll;                       // Select all arcs
~data.deselectAll;                     // Deselect all
~data.deselect(0);                     // Deselect specific arc
~data.isSelected(0);                   // Check if arc is selected
~data.toggleSelect(0);                 // Toggle selection state
~data.selectedCount;                   // Number of selected arcs

// --- Adding & Removing Arcs ---

// Add an arc (List of points with x, y OR freq)
// x: normalized timeline position (0-1)
// y: normalized frequency position (0=low, 1=high) - OR provide freq in Hz
// Note: Call ~gui.refresh after adding to see the arc
(
var arc = List.new;
20.do { |i|
    var t = i / 19;
    arc.add((
        x: 0.1 + (t * 0.3),     // 10% to 40% of timeline
        y: 0,                    // Ignored when freq is provided
        freq: 440 * (2 ** t)    // One octave glissando (440 Hz to 880 Hz)
    ));
};
~data.addArc(arc);
~gui.refresh;  // Refresh to see the new arc
)

// Remove arc by index
~data.removeArc(0);

// Remove multiple arcs
~data.removeArcs([0, 2, 3]);

// Remove selected arcs
~data.removeArcs(~data.selectedArcs);

// Clear all arcs on current page
~data.clearAll;


// ============================================================================
// SYNTHDEF MANAGEMENT
// ============================================================================

// Get/set synthdef for specific arc
~data.getSynthDefForArc(0);
~data.setSynthDefForArc(0, \upicWavetable8ch);

// Set synthdef for all selected arcs
~data.setSynthDefForSelected(\upicWavetable8ch);

// Set default synthdef for new arcs
~data.defaultSynthDef = \upicWavetable;

// Available synthdefs:
//   Wavetable variants:
//     \upicWavetable      - Mono
//     \upicWavetable2ch   - Stereo
//     \upicWavetable3ch   - 3 channel
//     \upicWavetable4ch   - Quad
//     \upicWavetable8ch   - Octophonic
//     \upicWavetable12ch  - 12 channel
//     \upicWavetable15ch  - 15 channel
//     \upicWavetable24ch  - 24 channel
//
//   SawBL variants (band-limited sawtooth):
//     \upicSawBL          - Mono
//     \upicSawBL2ch       - Stereo
//     \upicSawBL8ch       - 8 channel


// ============================================================================
// WAVETABLE MANAGEMENT
// ============================================================================

// Get wavetable data for arc (Array of 2048 floats, -1 to 1)
~data.getWavetableForArc(0);

// Set custom wavetable for arc
(
var wt = Array.fill(2048, { |i| sin(2pi * i / 2048) });  // Sine wave
~data.setWavetableForArc(0, wt, Server.default);
)

// Set wavetable for all selected arcs
~data.setWavetableForSelected(wavetableData, Server.default);

// Get server buffer for arc's wavetable
~data.getWavetableBufferForArc(0);

// Ensure all arcs have wavetable buffers (called automatically before playback)
~data.ensureWavetableBuffers(Server.default);


// ============================================================================
// AMPLITUDE ENVELOPE
// ============================================================================
// Amplitude envelopes control volume over the arc's duration.
// Format: List of (x: xPosition, amp: amplitude) where amp is 0.0 to 1.0

// Get amplitude envelope for arc
~data.getAmplitudeEnvelope(0);

// Set amplitude envelope
(
var env = List[
    (x: 0, amp: 0.0),      // Start silent
    (x: 100, amp: 1.0),    // Attack
    (x: 400, amp: 0.7),    // Sustain
    (x: 600, amp: 0.0)     // Release
];
~data.setAmplitudeEnvelope(0, env);
)

// Set for all selected arcs
~data.setAmplitudeEnvelopeForSelected(envelope);


// ============================================================================
// SPATIAL ENVELOPE
// ============================================================================
// Spatial envelopes control channel panning over the arc's duration.
// Format: List of (x: xPosition, channel: channelNumber)
// Used with multichannel synthdefs (2ch, 8ch, etc.)

// Get spatial envelope for arc
~data.getSpatialEnvelope(0);

// Set spatial envelope (pan from channel 0 to channel 7)
(
var env = List[
    (x: 0, channel: 0),
    (x: 300, channel: 3),
    (x: 600, channel: 7)
];
~data.setSpatialEnvelope(0, env);
)

// Set for all selected arcs
~data.setSpatialEnvelopeForSelected(envelope);


// ============================================================================
// CHANNEL OFFSET
// ============================================================================
// Output channel offset determines which audio output the arc starts on.
// For mono synths: plays on this specific channel
// For multichannel: outputs start from this channel

~data.channelOffsets[0];           // Get offset for arc 0
~data.channelOffsets[0] = 4;       // Route arc 0 to start at channel 4


// ============================================================================
// PAGE MANAGEMENT
// ============================================================================
// Each page has independent arcs, envelopes, and settings.
// Default pages: _a, _b, _c, _d (up to 24 pages supported)

// Get current page info
~data.currentPageIndex;    // Current page index (0-based)
~data.currentPage;         // Current page data (Event)
~data.pageCount;           // Number of pages

// Switch pages
~data.switchPage(0);       // Switch to page _a
~data.switchPage(1);       // Switch to page _b

// Page data structure:
// Each page is an Event containing:
//   - label: String ("_a", "_b", etc.)
//   - arcs: List of arcs
//   - selectedArcs: Set of selected indices
//   - amplitudeEnvelopes: List
//   - spatialEnvelopes: List
//   - arcSynthDefs: List
//   - arcWavetables: List
//   - arcWavetableBuffers: List
//   - channelOffsets: List


// ============================================================================
// COPY & PASTE
// ============================================================================

// Copy selected arcs to clipboard
~data.copy;

// Paste from clipboard with offset (normalized 0-1 range)
~data.paste(0.05, 0.02);   // X offset: +5% of timeline, Y offset: +2%

// Or paste at default position (small offset from original)
~data.paste;


// ============================================================================
// UNDO / REDO
// ============================================================================

~data.undo;
~data.redo;
~data.canUndo;
~data.canRedo;


// ============================================================================
// TRANSFORMATIONS
// ============================================================================
// Transform selected arcs programmatically.

// Reverse selected arcs horizontally (in time)
~data.reverseSelected;

// Invert selected arcs vertically (in frequency)
~data.inverseSelected;

// Time scaling
~data.timeCompressSelected(0.5);   // Compress to 50% duration
~data.timeExpandSelected(2.0);     // Expand to 200% duration

// Frequency scaling
~data.freqCompressSelected(0.5);   // Compress frequency range by 50%
~data.freqExpandSelected(2.0);     // Expand frequency range by 200%

// Move selected arcs
~data.moveSelected(100, 1.0);      // Move right 100px, no freq change
~data.moveSelected(0, 1.5);        // No X move, multiply freq by 1.5
~data.moveSelected(-50, 0.5);      // Move left 50px, halve frequency


// ============================================================================
// GUI ACCESS (NUPICGUI)
// ============================================================================

~gui = ~app.gui;

// Refresh display
~gui.refresh;

// Check if window is open
~gui.isOpen;

// Close window
~gui.close;

// Open window
~gui.open;

// Access controls (buttons, menus, etc.)
~gui.controls[\playButton];
~gui.controls[\directionMenu];
~gui.controls[\synthDefMenu];
~gui.controls[\scaleMenu];
// etc.

// Get/set playback duration
~gui.playDuration;
~gui.playDuration = 30;


// ============================================================================
// PLAYBACK (NUPICPlayback)
// ============================================================================

~playback = ~app.playback;

// Play/stop
~playback.play;
~playback.stop;

// Set mode: \forward, \reverse, \palindrome, \loop, \drag
~playback.setMode(\forward);
~playback.setMode(\palindrome);

// Set duration
~playback.setDuration(30);  // 30 seconds

// Check if playing
~playback.isPlaying;


// ============================================================================
// CALLBACKS
// ============================================================================
// Set callbacks to respond to events.

// Data change callback
~data.onDataChanged = {
    "Data changed!".postln;
};

// Selection change callback
~data.onSelectionChanged = { |selected|
    "Selected: %".format(selected).postln;
};

// Page change callback
~data.onPageChanged = { |index|
    "Switched to page %".format(index).postln;
};

// GUI callbacks
~gui.onArcDrawn = { |arc|
    "Drew arc with % points".format(arc.size).postln;
};

~gui.onArcErased = { |indices|
    "Erased arcs: %".format(indices).postln;
};

~gui.onSelectionChanged = { |selected|
    "Selection: %".format(selected).postln;
};

// Playback callbacks
~playback.onPlaybackStop = {
    "Playback stopped".postln;
};


// ============================================================================
// EXAMPLE: PROGRAMMATIC COMPOSITION
// ============================================================================
// NOTE: Arc coordinates are NORMALIZED (0-1 range):
//   x: position in timeline (0 = start, 1 = end)
//   y: frequency position (0 = low/20Hz, 1 = high/7500Hz) - OR use 'freq' in Hz
// The system supports both formats - if 'freq' is provided, it will be used.

(
// Start fresh
~data.clearAll;

// Create a rising glissando arc using freq (Hz)
var arc1 = List.new;
30.do { |i|
    var t = i / 29;
    arc1.add((
        x: 0.1 + (t * 0.4),    // Start at 10%, span 40% of timeline
        y: 0,                   // Ignored when freq is provided
        freq: 200 * (4 ** t)   // 200 Hz to 3200 Hz glissando
    ));
};

~data.addArc(arc1);

// Create a falling glissando
var arc2 = List.new;
30.do { |i|
    var t = i / 29;
    arc2.add((
        x: 0.1 + (t * 0.4),    // Same time range as arc1
        y: 0,
        freq: 3200 * (0.25 ** t)  // 3200 Hz to 200 Hz
    ));
};

~data.addArc(arc2);

// Set to 8-channel spatial synth
~data.selectAll;
~data.setSynthDefForSelected(\upicWavetable8ch);

// Add spatial movement (envelope x values are relative to arc length in pixels)
// Note: spatial envelope x values use arc's pixel range, not normalized time
~data.setSpatialEnvelope(0, List[
    (x: 0, channel: 0),
    (x: 500, channel: 7)
]);

~data.setSpatialEnvelope(1, List[
    (x: 0, channel: 7),
    (x: 500, channel: 0)
]);

// Refresh display
~gui.refresh;

"Composition created!".postln;
)

// Play it
~playback.setDuration(10);
~playback.play;


// ============================================================================
// GENERATORS
// ============================================================================
// For generative/algorithmic composition, see:
//   Docs/API_nUPIC_Generators.scd
//
// Includes:
//   - Xenakis Arborescences
//   - Brownian Motion
//   - Dynamic Stochastic Synthesis (GENDYN-style)
//   - Stochastic Clouds
//   - And more...
