/*
 * NUPICWavetableEditor - Wavetable editing window for nUPIC
 *
 * Allows drawing and editing wavetables for selected arcs
 * Supports tabbed interface for multiple arcs
 */

NUPICWavetableEditor {
	// References
	var <>data;           // NUPICData reference
	var <>gui;            // NUPICGUI reference for refresh

	// Window elements
	var <window;
	var <drawView;
	var <tabView;
	var <controlView;
	var <tabButtons;

	// State
	var <arcIndices;
	var <activeTabIndex = 0;
	var <wavetableCache;
	var <wavetableBuffers;
	var <isDrawing = false;
	var lastX, lastY;

	// Constants
	var numSamples = 2048;
	var width = 480;
	var height = 200;
	var tabHeight = 25;
	var controlHeight = 35;

	// Class variables
	classvar <>current;
	classvar <>tablesPath;      // Path to TABLES directory
	classvar <>wavFiles;        // List of WAV files found in TABLES

	*new { |dataRef, guiRef|
		^super.new.init(dataRef, guiRef)
	}

	init { |dataRef, guiRef|
		data = dataRef;
		gui = guiRef;
		wavetableCache = IdentityDictionary.new;
		wavetableBuffers = IdentityDictionary.new;
		tabButtons = List.new;
		arcIndices = List.new;

		// Scan for WAV files on first init
		if(wavFiles.isNil) {
			this.class.scanTablesDirectory;
		};
	}

	// ========== Class Methods for TABLES Directory ==========

	*findTablesPath {
		var candidates, found = nil;

		candidates = [
			// Relative to class file location
			PathName(this.filenameSymbol.asString).pathOnly.dirname +/+ "TABLES",
			// Common locations
			"~/nUPIC/TABLES".standardizePath,
			"/home/user/nUPIC/TABLES"
		];

		candidates.do { |path|
			if(path.notNil and: { File.exists(path) }) {
				found = path;
			};
		};

		tablesPath = found;
		^found
	}

	*scanTablesDirectory {
		var path, files;

		path = tablesPath ?? { this.findTablesPath };
		files = List.new;

		if(path.notNil and: { File.exists(path) }) {
			PathName(path).files.do { |file|
				var ext = file.extension.toLower;
				if(ext == "wav" or: { ext == "aif" } or: { ext == "aiff" }) {
					files.add(file.fileName);
				};
			};
			if(files.size > 0) {
				"NUPICWavetableEditor: Found % audio files in TABLES".format(files.size).postln;
			};
		};

		wavFiles = files;
		^files
	}

	*loadWavFile { |filename, action|
		var fullPath, server;

		if(tablesPath.isNil) { this.findTablesPath };
		if(tablesPath.isNil) {
			"TABLES directory not found".warn;
			^nil
		};

		fullPath = tablesPath +/+ filename;
		server = Server.default;

		if(File.exists(fullPath) and: { server.serverRunning }) {
			Buffer.read(server, fullPath, action: { |buf|
				buf.loadToFloatArray(action: { |floatArray|
					var data, resampled, numChannels, sourceLen, step;

					numChannels = buf.numChannels;
					sourceLen = (floatArray.size / numChannels).asInteger;

					// Mix to mono if stereo
					if(numChannels > 1) {
						data = Array.fill(sourceLen, { |i|
							var sum = 0;
							numChannels.do { |ch|
								sum = sum + floatArray[i * numChannels + ch];
							};
							sum / numChannels
						});
					} {
						data = floatArray.asArray;
					};

					// Resample to 2048 samples
					step = data.size / 2048;
					resampled = Array.fill(2048, { |i|
						var idx = (i * step).floor.asInteger;
						var frac = (i * step) - idx;
						var v0 = data[idx.clip(0, data.size - 1)];
						var v1 = data[(idx + 1).clip(0, data.size - 1)];
						v0 + (frac * (v1 - v0))
					});

					// Normalize
					resampled = resampled / resampled.abs.maxItem.max(0.001);

					buf.free;

					if(action.notNil) {
						action.value(resampled);
					};

					"Loaded wavetable: %".format(filename).postln;
				});
			});
		} {
			"File not found or server not running: %".format(fullPath).warn;
		};
	}

	open { |arcIndex = nil|
		// If already open and a specific arc is requested, add as tab
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not } and: { arcIndex.notNil }) {
			current.addArcAsTab(arcIndex);
			^this
		};

		// Close existing window if open
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not }) {
			current.window.close;
		};

		// Reset tab index when opening
		activeTabIndex = 0;

		// Determine which arcs to edit
		arcIndices = List.new;
		if(arcIndex.notNil) {
			arcIndices.add(arcIndex);
		} {
			// Use all selected arcs
			if(data.notNil and: { data.selectedArcs.notNil } and: { data.selectedArcs.size > 0 }) {
				arcIndices = data.selectedArcs.asArray.sort.asList;
			};
		};

		// Check if we have valid arcs
		if(arcIndices.size == 0 or: { data.arcCount == 0 }) {
			"No arc selected. Draw an arc first, select it, then open the wavetable editor.".warn;
			^nil
		};

		// Filter out invalid indices
		arcIndices = arcIndices.select { |idx| idx < data.arcCount };
		if(arcIndices.size == 0) {
			"Selected arcs are invalid.".warn;
			^nil
		};

		// Filter out arcs that don't use wavetable synthdefs (e.g., SawBL)
		arcIndices = arcIndices.select { |idx|
			var synthDef = data.getSynthDefForArc(idx);
			var synthDefStr = synthDef.asString.toLower;
			synthDefStr.contains("wavetable") or: {
				synthDefStr.contains("upic") and: { synthDefStr.contains("sawbl").not }
			}
		};
		if(arcIndices.size == 0) {
			"Selected arcs use non-wavetable synthdefs (e.g., SawBL). Wavetable editor not applicable.".warn;
			^nil
		};

		// Clear stale buffer references that are not part of the new selection
		// This prevents double-frees when the editor is reused with different arcs
		wavetableBuffers.keys.asArray.do { |oldIdx|
			if(arcIndices.includes(oldIdx).not) {
				wavetableBuffers.removeAt(oldIdx);
			};
		};

		// Sync wavetableBuffers with NUPICData's buffers for current selection
		// (the editor should reference the same buffers as NUPICData)
		arcIndices.do { |idx|
			if(data.notNil and: { data.arcWavetableBuffers[idx].notNil }) {
				wavetableBuffers[idx] = data.arcWavetableBuffers[idx];
			};
		};

		// Pre-load wavetable data for all arcs
		// Always read from stored data if available (handles preset loads)
		arcIndices.do { |idx|
			var storedWt = data.getWavetableForArc(idx);
			if(storedWt.notNil and: { storedWt.size > 0 }) {
				wavetableCache[idx] = storedWt;
			} {
				// Fall back to cached value or default sine if none stored
				if(wavetableCache[idx].isNil) {
					wavetableCache[idx] = Array.fill(numSamples, { |i| sin(2pi * i / numSamples) });
				};
			};
		};

		// Create the window
		this.prCreateWindow;

		current = this;
		window.front;
	}

	addArcAsTab { |arcIndex|
		var synthDef, synthDefStr, storedWt;

		// Check if arc is valid
		if(arcIndex.isNil or: { arcIndex >= data.arcCount }) {
			^this
		};

		// Check if arc already in tabs
		if(arcIndices.indexOf(arcIndex).notNil) {
			// Just switch to that tab
			activeTabIndex = arcIndices.indexOf(arcIndex);
			tabButtons.do { |tb, j| tb.value = if(j == activeTabIndex, 1, 0) };
			drawView.refresh;
			window.front;
			^this
		};

		// Check if arc uses wavetable synthdef
		synthDef = data.getSynthDefForArc(arcIndex);
		synthDefStr = synthDef.asString.toLower;
		if((synthDefStr.contains("wavetable") or: {
			synthDefStr.contains("upic") and: { synthDefStr.contains("sawbl").not }
		}).not) {
			"Arc % uses non-wavetable synthdef. Cannot add to wavetable editor.".format(arcIndex).warn;
			^this
		};

		// Load wavetable data for this arc
		storedWt = data.getWavetableForArc(arcIndex);
		if(storedWt.notNil and: { storedWt.size > 0 }) {
			wavetableCache[arcIndex] = storedWt;
		} {
			if(wavetableCache[arcIndex].isNil) {
				wavetableCache[arcIndex] = Array.fill(numSamples, { |i| sin(2pi * i / numSamples) });
			};
		};

		// Add arc to list
		arcIndices.add(arcIndex);

		// Recreate window with new tab
		window.onClose = nil;  // Prevent deferred callback from clearing current
		window.close;
		this.prCreateWindow;
		current = this;  // Restore current after window recreation

		// Switch to new tab
		activeTabIndex = arcIndices.indexOf(arcIndex);
		tabButtons.do { |tb, j| tb.value = if(j == activeTabIndex, 1, 0) };

		window.front;
	}

	prCreateWindow {
		var winHeight = height + controlHeight;

		// Add tab height if multiple arcs
		if(arcIndices.size > 1) {
			winHeight = winHeight + tabHeight;
		};

		window = Window("nUPIC Wavetable Editor", Rect(100, 100, width, winHeight), resizable: false);
		window.background = Color.new(205/255, 250/255, 205/255);  // B&K green

		// Create tabs if multiple arcs
		if(arcIndices.size > 1) {
			this.prCreateTabs;
		};

		// Create drawing area
		this.prCreateDrawView;

		// Create control area
		this.prCreateControls;

		window.onClose = {
			current = nil;
		};
	}

	prCreateTabs {
		var tabWidth = (width / arcIndices.size).min(80);

		tabView = CompositeView(window, Rect(0, 0, width, tabHeight));
		tabView.background = Color.new(180/255, 230/255, 180/255);

		tabButtons = List.new;
		arcIndices.do { |arcIdx, i|
			var btn = Button(tabView, Rect(i * tabWidth + 2, 2, tabWidth - 4, tabHeight - 4))
			.states_([
				["Arc " ++ arcIdx, Color.black, Color.gray(0.85)],
				["Arc " ++ arcIdx, Color.white, Color.blue(0.7)]
			])
			.font_(Font("Riforma Mono LL", 10, true))
			.value_(if(i == 0, 1, 0))
			.action_({ |b|
				activeTabIndex = tabButtons.indexOf(b);
				tabButtons.do { |tb, j| tb.value = if(j == activeTabIndex, 1, 0) };
				drawView.refresh;
			});
			tabButtons.add(btn);
		};
	}

	prCreateDrawView {
		var drawY = if(arcIndices.size > 1, tabHeight + 10, 10);

		drawView = UserView(window, Rect(10, drawY, width - 20, height - 20));
		drawView.background = Color.white;

		drawView.drawFunc = {
			var wt = this.prGetCurrentWavetable;
			var step = drawView.bounds.width / numSamples;
			var centerY = drawView.bounds.height / 2;
			var dashLen = 4, gapLen = 4;
			var x = 0;

			// Draw grid - dotted lines
			Pen.strokeColor = Color.gray(0.8);
			Pen.width = 1;

			// Horizontal center line (dotted)
			x = 0;
			while { x < drawView.bounds.width } {
				Pen.line(Point(x, centerY), Point((x + dashLen).min(drawView.bounds.width), centerY));
				Pen.stroke;
				x = x + dashLen + gapLen;
			};

			// Vertical lines at quarters (dotted)
			4.do { |i|
				var lineX = (i + 1) * (drawView.bounds.width / 4);
				var y = 0;
				while { y < drawView.bounds.height } {
					Pen.line(Point(lineX, y), Point(lineX, (y + dashLen).min(drawView.bounds.height)));
					Pen.stroke;
					y = y + dashLen + gapLen;
				};
			};

			// Draw wavetable
			Pen.strokeColor = Color.blue;
			Pen.width = 1.5;

			if(wt.notNil and: { wt.size > 0 }) {
				Pen.moveTo(Point(0, centerY - (wt[0] * centerY * 0.9)));

				(numSamples - 1).do { |i|
					var ptX = (i + 1) * step;
					var y = centerY - (wt[i + 1] * centerY * 0.9);
					Pen.lineTo(Point(ptX, y));
				};

				Pen.stroke;
			};

			// Show current arc label
			Pen.stringAtPoint(
				"Arc " ++ this.prGetCurrentArcIndex,
				Point(5, 5),
				Font("Riforma Mono LL", 10, true),
				Color.gray(0.4)
			);
		};

		// Mouse interaction
		drawView.mouseDownAction = { |view, x, y|
			var wt = this.prGetCurrentWavetable;
			var index, value;
			isDrawing = true;
			lastX = x;
			lastY = y;

			index = (x / (drawView.bounds.width / numSamples)).asInteger.clip(0, numSamples - 1);
			value = ((drawView.bounds.height / 2 - y) / (drawView.bounds.height / 2) / 0.9).clip(-1, 1);
			wt[index] = value;
			drawView.refresh;
		};

		drawView.mouseMoveAction = { |view, x, y|
			if(isDrawing) {
				var wt = this.prGetCurrentWavetable;
				var startIndex = (lastX / (drawView.bounds.width / numSamples)).asInteger.clip(0, numSamples - 1);
				var endIndex = (x / (drawView.bounds.width / numSamples)).asInteger.clip(0, numSamples - 1);
				var startValue = ((drawView.bounds.height / 2 - lastY) / (drawView.bounds.height / 2) / 0.9).clip(-1, 1);
				var endValue = ((drawView.bounds.height / 2 - y) / (drawView.bounds.height / 2) / 0.9).clip(-1, 1);

				if(startIndex != endIndex) {
					var numSteps = (endIndex - startIndex).abs;
					(numSteps + 1).do { |i|
						var idx = if(startIndex < endIndex,
							{ startIndex + i },
							{ startIndex - i }
						).clip(0, numSamples - 1);
						var t = i / numSteps;
						wt[idx] = startValue.blend(endValue, t);
					};
				} {
					wt[startIndex] = endValue;
				};

				lastX = x;
				lastY = y;
				drawView.refresh;
			};
		};

		drawView.mouseUpAction = { |view, x, y|
			var idx = this.prGetCurrentArcIndex;
			var wt = wavetableCache[idx];
			isDrawing = false;
			this.prUpdateBuffer;

			// Store wavetable in NUPICData for playback
			if(data.notNil and: { wt.notNil }) {
				data.setWavetableForArc(idx, wt, Server.default);
				"Wavetable updated for arc %".format(idx).postln;
			};
		};
	}

	prCreateControls {
		var xPos = 5;
		var buttons = IdentityDictionary.new;
		var controlY = if(arcIndices.size > 1, height + tabHeight, height);
		var presetMenu;
		var programmaticPresets = ["sine", "saw", "square", "triangle", "pulse", "noise", "organFull", "brass", "fm1", "fm2", "formant"];
		var allPresets;

		controlView = CompositeView(window, Rect(0, controlY, width, controlHeight));
		controlView.background = Color.new(190/255, 240/255, 190/255);

		// Build preset list: programmatic presets + WAV files from TABLES
		allPresets = programmaticPresets.copy;
		if(wavFiles.notNil and: { wavFiles.size > 0 }) {
			allPresets = allPresets ++ ["---"] ++ wavFiles.collect({ |f|
				// Remove extension for display
				f.replace(".wav", "").replace(".aif", "").replace(".aiff", "")
			});
		};

		// Preset menu - programmatic presets + WAV files from TABLES
		presetMenu = PopUpMenu(controlView, Rect(xPos, 5, 90, 22))
		.items_(allPresets)
		.font_(Font("Riforma Mono LL", 9))
		.action_({ |menu|
			var wt;
			var menuVal = menu.value;
			var numProgrammatic = programmaticPresets.size;

			if(menuVal < numProgrammatic) {
				// Programmatic presets
				switch(menuVal)
				// Basic waveforms
				{ 0 } { wt = Array.fill(numSamples, { |i| sin(2pi * i / numSamples) }) }
				{ 1 } { wt = Array.fill(numSamples, { |i| (i / numSamples * 2 - 1) }) }
				{ 2 } { wt = Array.fill(numSamples, { |i| if(i < (numSamples / 2), 1, -1) }) }
				{ 3 } { wt = Array.fill(numSamples, { |i|
					var x = i / numSamples;
					if(x < 0.5, { x * 4 - 1 }, { 3 - (x * 4) })
				}) }
				// Pulse (25% duty cycle)
				{ 4 } { wt = Array.fill(numSamples, { |i| if(i < (numSamples / 4), 1, -1) }) }
				// Noise
				{ 5 } { wt = Array.fill(numSamples, { 1.0.rand2 }) }
				// Organ (rich harmonics)
				{ 6 } { wt = Signal.sineFill(numSamples,
					Array.fill(16, { |i| 1 / (i + 1) })
				).asArray }
				// Brass
				{ 7 } { wt = Signal.sineFill(numSamples,
					[1, 0.7, 0.5, 0.6, 0.4, 0.3, 0.2, 0.15]
				).asArray }
				// FM1 (bell-like)
				{ 8 } { wt = Array.fill(numSamples, { |i|
					var phase = 2pi * i / numSamples;
					sin(phase + (3 * sin(phase * 2.5)))
				}) }
				// FM2 (metallic)
				{ 9 } { wt = Array.fill(numSamples, { |i|
					var phase = 2pi * i / numSamples;
					sin(phase + (5 * sin(phase * 1.5) * sin(phase * 0.5)))
				}) }
				// Formant (vocal-like)
				{ 10 } { wt = Array.fill(numSamples, { |i|
					var phase = 2pi * i / numSamples;
					var fundamental = sin(phase);
					var formant1 = sin(phase * 3) * 0.5;
					var formant2 = sin(phase * 5) * 0.3;
					(fundamental + formant1 + formant2).clip(-1, 1)
				}) };

				// Normalize and apply
				if(wt.notNil) {
					var maxVal = wt.abs.maxItem;
					if(maxVal > 0) { wt = wt / maxVal };
					this.prSetCurrentWavetable(wt);
					drawView.refresh;
				};
			} {
				// Skip the separator "---"
				if(menuVal > numProgrammatic) {
					// WAV file from TABLES - adjust index for separator
					var wavIndex = menuVal - numProgrammatic - 1;
					if(wavIndex >= 0 and: { wavIndex < wavFiles.size }) {
						var filename = wavFiles[wavIndex];
						// Load asynchronously
						this.class.loadWavFile(filename, { |loadedWt|
							{
								this.prSetCurrentWavetable(loadedWt);
								drawView.refresh;
							}.defer;
						});
					};
				};
			};
		});
		xPos = xPos + 95;

		// Reverse
		buttons[\reverse] = Button(controlView, Rect(xPos, 5, 32, 22))
		.states_([["rev", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var wt = this.prGetCurrentWavetable.reverse;
			this.prSetCurrentWavetable(wt);
			drawView.refresh;
		});
		xPos = xPos + 35;

		// Invert
		buttons[\invert] = Button(controlView, Rect(xPos, 5, 32, 22))
		.states_([["inv", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var wt = this.prGetCurrentWavetable.neg;
			this.prSetCurrentWavetable(wt);
			drawView.refresh;
		});
		xPos = xPos + 35;

		// Smooth
		buttons[\smooth] = Button(controlView, Rect(xPos, 5, 32, 22))
		.states_([["smo", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var wt = this.prGetCurrentWavetable.copy;
			3.do {
				wt = wt.collect { |val, i|
					var prev = wt[(i - 1) % numSamples];
					var next = wt[(i + 1) % numSamples];
					(prev + (val * 2) + next) / 4;
				};
			};
			this.prSetCurrentWavetable(wt);
			drawView.refresh;
		});
		xPos = xPos + 35;

		// Normalize
		buttons[\normalize] = Button(controlView, Rect(xPos, 5, 32, 22))
		.states_([["nrm", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var wt = this.prGetCurrentWavetable;
			var maxVal = wt.abs.maxItem;
			if(maxVal > 0) {
				wt = wt / maxVal;
				this.prSetCurrentWavetable(wt);
				drawView.refresh;
			};
		});
		xPos = xPos + 35;

		// Test
		buttons[\test] = Button(controlView, Rect(xPos, 5, 32, 22))
		.states_([["test", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prTestSound;
		});
		xPos = xPos + 35;

		// Arc to Wavetable
		buttons[\fromArc] = Button(controlView, Rect(xPos, 5, 38, 22))
		.states_([["arc→", Color.white, Color.new(0.5, 0.3, 0.6)]])
		.font_(Font("Riforma Mono LL", 9, true))
		.action_({
			var wt = this.prConvertArcToWavetable(this.prGetCurrentArcIndex);
			if(wt.notNil) {
				this.prSetCurrentWavetable(wt);
				drawView.refresh;
				"Arc shape applied as wavetable".postln;
			};
		});
		xPos = xPos + 42;

		// Wavetable to Arc
		buttons[\toArc] = Button(controlView, Rect(xPos, 5, 38, 22))
		.states_([["→arc", Color.white, Color.new(0.3, 0.5, 0.6)]])
		.font_(Font("Riforma Mono LL", 9, true))
		.action_({
			this.prConvertWavetableToArc;
			if(gui.notNil) { gui.refresh };
		});
		xPos = xPos + 42;

		// Apply to All (only if multiple arcs)
		if(arcIndices.size > 1) {
			buttons[\applyAll] = Button(controlView, Rect(xPos, 5, 32, 22))
			.states_([["all", Color.white, Color.new(0.2, 0.5, 0.2)]])
			.font_(Font("Riforma Mono LL", 9, true))
			.action_({
				var currentWt = this.prGetCurrentWavetable;
				var indices = arcIndices.asArray;

				// Use shared buffer approach to avoid buffer exhaustion
				// 1. Update local cache for all arcs (share reference, don't copy 800 times)
				indices.do { |idx|
					wavetableCache[idx] = currentWt;  // Share reference
				};

				// 2. Create ONE shared buffer for editor's local use
				if(Server.default.serverRunning) {
					var sig = Signal.newFrom(currentWt);
					var wtFormat = sig.asWavetable;
					var buffersToFree = Set.new;

					// Collect unique buffers to free from BOTH editor and NUPICData
					// Only collect buffers that belong to the current indices
					indices.do { |idx|
						if(wavetableBuffers[idx].notNil) {
							buffersToFree.add(wavetableBuffers[idx]);
						};
						if(data.notNil and: { data.arcWavetableBuffers[idx].notNil }) {
							buffersToFree.add(data.arcWavetableBuffers[idx]);
						};
					};

					// Free unique buffers safely (Set already deduplicates)
					buffersToFree.do { |buf|
						try { buf.free };
					};

					// Clear old references before creating new buffer
					indices.do { |idx|
						wavetableBuffers[idx] = nil;
					};

					Buffer.loadCollection(Server.default, wtFormat, 1, { |sharedBuf|
						// Point all local buffers to shared buffer
						indices.do { |idx| wavetableBuffers[idx] = sharedBuf };

						// Directly update NUPICData's wavetable storage (no selection callbacks)
						if(data.notNil) {
							data.setWavetableForIndices(indices, currentWt, sharedBuf);
						};
						"Applied wavetable to % arcs (shared buffer, bufnum: %)".format(indices.size, sharedBuf.bufnum).postln;
					});
				};
			});
		};
	}

	// ========== Private Helpers ==========

	prGetCurrentArcIndex {
		// Safety check - ensure activeTabIndex is valid
		if(arcIndices.isNil or: { arcIndices.size == 0 }) { ^0 };
		if(activeTabIndex >= arcIndices.size) { activeTabIndex = 0 };
		^arcIndices[activeTabIndex]
	}

	prGetCurrentWavetable {
		var idx = this.prGetCurrentArcIndex;
		if(wavetableCache[idx].isNil) {
			wavetableCache[idx] = Array.fill(numSamples, { |i| sin(2pi * i / numSamples) });
		};
		^wavetableCache[idx]
	}

	prSetCurrentWavetable { |wt|
		var idx = this.prGetCurrentArcIndex;
		wavetableCache[idx] = wt;
		this.prUpdateBufferForArc(idx);

		// Also store in NUPICData so playback uses this wavetable
		if(data.notNil) {
			data.setWavetableForArc(idx, wt, Server.default);
		};
	}

	prUpdateBuffer {
		this.prUpdateBufferForArc(this.prGetCurrentArcIndex);
	}

	prUpdateBufferForArc { |idx|
		this.prUpdateBufferForArcWithAction(idx, nil);
	}

	prUpdateBufferForArcWithAction { |idx, action|
		var wt = wavetableCache[idx];
		if(wt.notNil and: { Server.default.serverRunning }) {
			var sig, wtFormat;

			if(wavetableBuffers[idx].notNil) {
				var buf = wavetableBuffers[idx];
				// Check if this buffer is shared with other arcs before freeing
				var localRefCount = wavetableBuffers.values.count { |b| b === buf };
				var dataRefCount = if(data.notNil) {
					data.arcWavetableBuffers.count { |b| b === buf }
				} { 0 };
				// Only free if this is the last reference (total refs <= 1)
				if((localRefCount + dataRefCount) <= 1) {
					try { buf.free };
				};
				wavetableBuffers[idx] = nil;
			};

			// Convert to wavetable format for Osc.ar
			sig = Signal.newFrom(wt);
			wtFormat = sig.asWavetable;

			// Use loadCollection with completion for reliable loading
			Buffer.loadCollection(Server.default, wtFormat, 1, { |buf|
				wavetableBuffers[idx] = buf;
				action.value(buf);
			});
		} {
			action.value(nil);
		};
	}

	prTestSound {
		var buffer = wavetableBuffers[this.prGetCurrentArcIndex];
		if(buffer.isNil) {
			this.prUpdateBuffer;
			buffer = wavetableBuffers[this.prGetCurrentArcIndex];
		};

		if(buffer.notNil) {
			fork {
				var synth = Synth(\upicWavetable, [
					\freq, 440,
					\amp, 0.2,
					\gate, 1,
					\bufnum, buffer.bufnum
				]);
				0.5.wait; synth.set(\freq, 550);
				0.5.wait; synth.set(\freq, 330);
				0.5.wait; synth.set(\gate, 0);
			};
		} {
			"No wavetable buffer available for test".warn;
		};
	}

	prConvertArcToWavetable { |arcIdx|
		var arc, wt, minFreq, maxFreq, logMinFreq, logMaxFreq;

		if(data.isNil) { ^nil };
		arc = data.arcs[arcIdx];

		if(arc.notNil and: { arc.size > 1 }) {
			// Use frequency values from arc to create wavetable shape
			minFreq = arc.collect({ |pt| pt[\freq] }).minItem;
			maxFreq = arc.collect({ |pt| pt[\freq] }).maxItem;

			// Use log scale for frequency mapping (perceptually correct)
			logMinFreq = minFreq.log;
			logMaxFreq = maxFreq.log;

			wt = Array.fill(numSamples, { |i|
				var pos = i / (numSamples - 1) * (arc.size - 1);
				var idx1 = pos.floor.asInteger.clip(0, arc.size - 2);
				var idx2 = idx1 + 1;
				var frac = pos - idx1;
				var freq1 = arc[idx1][\freq];
				var freq2 = arc[idx2][\freq];
				// Interpolate in log space
				var logFreq = freq1.log.blend(freq2.log, frac);
				// Map log frequency to -1 to 1 range
				logFreq.linlin(logMinFreq, logMaxFreq, -1, 1)
			});
		};
		^wt
	}

	prConvertWavetableToArc {
		var wt = this.prGetCurrentWavetable;
		var arcIdx = this.prGetCurrentArcIndex;
		var arc, startX, endX, minFreq, maxFreq, newPoints;

		if(data.isNil or: { wt.isNil }) { ^this };

		arc = data.arcs[arcIdx];
		if(arc.isNil or: { arc.size < 2 }) { ^this };

		// Get original arc bounds
		startX = arc.collect({ |pt| pt[\x] }).minItem;
		endX = arc.collect({ |pt| pt[\x] }).maxItem;
		minFreq = arc.collect({ |pt| pt[\freq] }).minItem;
		maxFreq = arc.collect({ |pt| pt[\freq] }).maxItem;

		// Create new arc points from wavetable
		newPoints = List.new;
		256.do { |i|
			var x = startX + (i / 255 * (endX - startX));
			var wtIdx = (i / 255 * (numSamples - 1)).asInteger;
			var wtVal = wt[wtIdx];  // -1 to 1
			// Map wavetable value to frequency (using log scale)
			var logFreq = wtVal.linlin(-1, 1, minFreq.log, maxFreq.log);
			var freq = logFreq.exp;
			// Calculate screen Y from frequency (will be recalculated by GUI)
			var y = freq.explin(20, 20000, 1, 0);  // Approximate normalized y
			newPoints.add((x: x, y: y, freq: freq));
		};

		// Replace arc in data
		data.prSaveUndoState;
		data.arcs[arcIdx] = newPoints;
		data.onDataChanged.value;

		"Arc % shape replaced with wavetable".format(arcIdx).postln;
	}

	// ========== Public Methods ==========

	getWavetableBuffer { |arcIndex|
		^wavetableBuffers[arcIndex]
	}

	getWavetable { |arcIndex|
		^wavetableCache[arcIndex]
	}

	setWavetable { |arcIndex, wt|
		wavetableCache[arcIndex] = wt;
		this.prUpdateBufferForArc(arcIndex);
	}

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
	}

	updateForSelection {
		var newIndices;
		// Only update if window is open
		if(window.isNil or: { window.isClosed }) { ^this };

		// Get new selection
		if(data.notNil and: { data.selectedArcs.notNil } and: { data.selectedArcs.size > 0 }) {
			newIndices = data.selectedArcs.asArray.sort.asList;
		} {
			// No selection - close the editor
			this.close;
			^this
		};

		// If selection changed, reopen with new arcs
		if(newIndices != arcIndices) {
			this.close;
			this.open;
		};
	}
}
