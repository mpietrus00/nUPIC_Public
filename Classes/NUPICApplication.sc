/*
 * NUPICApplication - Main application class for nUPIC
 *
 * Handles component creation, wiring, and lifecycle management.
 * Encapsulates all the setup that was previously in the loader script.
 */

NUPICApplication {
	// Core components
	var <data;
	var <gui;
	var <playback;

	// Playback mode names matching menu order: FWD, REV, PAL, LOOP, DRAG
	var playbackModes;

	// Server reference
	var <server;

	// Oscillator mode: true = use OversamplingOscillators extension, false = use standard SC oscillators
	var <useOversampling;

	// Multichannel mode: true = show all channel variants, false = only Mono and 2ch
	var <useMultichannel;

	*new { |server, oversampling = false, multichannel = false|
		^super.new.init(server, oversampling, multichannel)
	}

	init { |argServer, argOversampling, argMultichannel|
		server = argServer ?? { Server.default };
		useOversampling = argOversampling;
		useMultichannel = argMultichannel;
		playbackModes = [\forward, \reverse, \palindrome, \loop, \drag];
	}

	// Start the application - boots server if needed and opens GUI
	start {
		// Configure server options before boot
		server.options.numInputBusChannels = 2;
		server.options.numOutputBusChannels = 8;  // Support multi-channel
		server.options.numWireBufs = 128;  // More wire buffers for complex synths
		server.options.memSize = 65536;  // More memory for buffers (64MB)
		server.options.numBuffers = 2048;  // More buffers for wavetables

		server.waitForBoot {
			this.prCreateComponents;
			this.prConnectComponents;
			this.prLoadSynthDefs;
			this.prOpenGUI;  // Must open GUI before setting up callbacks that access controls
			this.prSetupCallbacks;
			this.prPrintHelp;
		};
	}

	// Create core components
	prCreateComponents {
		data = NUPICData.new;
		gui = NUPICGUI.new;
		playback = NUPICPlayback.new;
	}

	// Wire components together
	prConnectComponents {
		gui.data = data;
		gui.playback = playback;  // For paste-to-playback-page
		gui.useOversampling = useOversampling;
		gui.useMultichannel = useMultichannel;
		playback.data = data;
		playback.gui = gui;
	}

	// Load all synthdefs
	prLoadSynthDefs {
		playback.loadSynthDefs(useOversampling);
	}

	// Set up all callbacks
	prSetupCallbacks {
		this.prSetupPlaybackCallbacks;
		this.prSetupGUICallbacks;
		this.prSetupDataCallbacks;
	}

	prSetupPlaybackCallbacks {
		// Connect direction menu to playback engine
		gui.controls[\directionMenu].action = { |menu|
			var mode = playbackModes[menu.value];
			playback.setMode(mode);
			"Playback mode: %".format(mode).postln;
		};

		// Connect play button to playback engine
		gui.onPlaybackToggle = { |playing|
			if(playing) {
				var duration = gui.playDuration;
				var mode = playbackModes[gui.controls[\directionMenu].value];

				playback.setMode(mode);
				playback.setDuration(duration);
				playback.play;
				gui.setPlaying(true);

				if(mode == \drag) {
					"DRAG mode: Click and drag on canvas to scrub through time".postln;
				};
			} {
				playback.stop;
				gui.setPlaying(false);
			};
		};

		// Connect scrub callback for DRAG mode
		gui.onScrub = { |timeNorm|
			playback.scrubTo(timeNorm);
		};

		// Playback stop callback
		playback.onPlaybackStop = {
			{ gui.setPlaying(false) }.defer;
		};
	}

	prSetupGUICallbacks {
		gui.onArcDrawn = { |arc|
			"Arc drawn with % points".format(arc.size).postln;
		};

		gui.onArcErased = { |indices|
			"Erased arcs at indices: %".format(indices).postln;
		};

		gui.onSelectionChanged = { |selected|
			"Selection changed: % arcs selected".format(selected.size).postln;
		};
	}

	prSetupDataCallbacks {
		data.onDataChanged = {
			"Data changed - % arcs total".format(data.arcCount).postln;
			gui.refresh;
			// Update arcs list view if open
			if(gui.arcsListView.notNil) {
				gui.arcsListView.updateForDataChange;
			};
		};

		data.onSelectionChanged = { |selected|
			gui.refresh;
			gui.updateUIForSelection;
		};

		data.onPageChanged = { |index|
			"Switched to page %".format(index).postln;
		};
	}

	prOpenGUI {
		gui.open;
	}

	prPrintHelp {
		"".postln;
		"=== nUPIC Application Started ===".postln;
		if(useOversampling) {
			"Oscillator mode: OVERSAMPLING (OversamplingOscillators extension)".postln;
		} {
			"Oscillator mode: STANDARD (no extensions required)".postln;
		};
		if(useMultichannel) {
			"Channel mode: MULTICHANNEL (all channel variants available)".postln;
		} {
			"Channel mode: STEREO (Mono and 2ch only)".postln;
		};
		"".postln;
		"CONTROLS:".postln;
		"  Spacebar      - Play/Stop".postln;
		"  Direction     - FWD, REV, PAL (palindrome), LOOP".postln;
		"".postln;
		"DRAWING:".postln;
		"  Mouse drag    - Draw arcs".postln;
		"  _select       - Enter selection mode (marquee)".postln;
		"  _eraser       - Enter erase mode".postln;
		"".postln;
		"KEYBOARD:".postln;
		"  Cmd+Z         - Undo".postln;
		"  Cmd+Shift+Z   - Redo".postln;
		"  Cmd+A         - Select all".postln;
		"  Delete        - Remove selected".postln;
		"  Escape        - Deselect all".postln;
		"  Arrow keys    - Move selected (left/right time, up/down pitch)".postln;
		"".postln;
		"TRANSFORM:".postln;
		"  ><  <>        - Time compress/expand".postln;
		"  ^v  v^        - Freq compress/expand".postln;
		"  _rev          - Reverse".postln;
		"  _inv          - Invert (flip vertically)".postln;
		"  _cp _ps       - Copy/Paste".postln;
		"  _edit WT      - Edit wavetable (select arc first)".postln;
		"  _edit amps    - Edit amplitude envelope (select arc first)".postln;
		"  _edit spatial - Edit spatial envelope (select arc first)".postln;
	}

	// Stop the application
	stop {
		if(playback.notNil) {
			playback.stop;
		};
		if(gui.notNil) {
			gui.close;
		};
	}

	// Check if application is running
	isRunning {
		^(gui.notNil and: { gui.isOpen })
	}
}
