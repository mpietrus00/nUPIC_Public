/*
 * NUPICAmplitudeEditor - Amplitude envelope editing window for nUPIC
 *
 * Allows drawing and editing amplitude envelopes for selected arcs
 * Supports tabbed interface for multiple arcs
 */

NUPICAmplitudeEditor {
	// References
	var <>data;           // NUPICData reference
	var <>gui;            // NUPICGUI reference for refresh

	// Window elements
	var <window;
	var <tabView;
	var <tabButtons;
	var <editorContainer;
	var <ampView;
	var <overlayView;
	var <toolbarView;
	var <controlView;

	// State
	var <arcIndices;
	var <activeTabIndex = 0;
	var <zoomSettings;    // Per-arc zoom settings

	// Undo/Redo
	var <undoStacks;
	var <redoStacks;

	// Constants
	var width = 700;
	var height = 450;
	var tabHeight = 30;
	var toolbarHeight = 30;
	var controlHeight = 35;

	// Class variable
	classvar <>current;

	*new { |dataRef, guiRef|
		^super.new.init(dataRef, guiRef)
	}

	init { |dataRef, guiRef|
		data = dataRef;
		gui = guiRef;
		tabButtons = List.new;
		arcIndices = List.new;
		zoomSettings = IdentityDictionary.new;
		undoStacks = IdentityDictionary.new;
		redoStacks = IdentityDictionary.new;
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
			"No arc selected. Draw an arc first, select it, then open the amplitude editor.".warn;
			^nil
		};

		// Filter out invalid indices
		arcIndices = arcIndices.select { |idx| idx < data.arcCount };
		if(arcIndices.size == 0) {
			"Selected arcs are invalid.".warn;
			^nil
		};

		// Initialize zoom settings for each arc
		arcIndices.do { |idx|
			if(zoomSettings[idx].isNil) {
				zoomSettings[idx] = (zoomMin: 0, zoomMax: 1, zoomValue: 0, panValue: 0);
			};
		};

		// Create the window
		this.prCreateWindow;

		current = this;
		window.front;
	}

	addArcAsTab { |arcIndex|
		// Check if arc is valid
		if(arcIndex.isNil or: { arcIndex >= data.arcCount }) {
			^this
		};

		// Check if arc already in tabs
		if(arcIndices.indexOf(arcIndex).notNil) {
			// Just switch to that tab
			activeTabIndex = arcIndices.indexOf(arcIndex);
			tabButtons.do { |tb, j| tb.value = if(j == activeTabIndex, 1, 0) };
			this.prCreateEditorContent(arcIndices[activeTabIndex]);
			window.front;
			^this
		};

		// Initialize zoom settings for this arc
		if(zoomSettings[arcIndex].isNil) {
			zoomSettings[arcIndex] = (zoomMin: 0, zoomMax: 1, zoomValue: 0, panValue: 0);
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
		var winHeight = height;
		var editorY = 0;
		var editorHeight;

		// Add tab height if multiple arcs
		if(arcIndices.size > 1) {
			winHeight = winHeight + tabHeight;
			editorY = tabHeight;
		};

		window = Window("nUPIC Amplitude Editor", Rect(200, 100, width, winHeight), resizable: false);
		window.background = Color.new(205/255, 250/255, 205/255);  // B&K green

		// Create tabs if multiple arcs
		if(arcIndices.size > 1) {
			this.prCreateTabs;
		};

		// Create editor content
		editorHeight = winHeight - editorY;
		editorContainer = CompositeView(window, Rect(0, editorY, width, editorHeight));

		// Create content for first arc
		this.prCreateEditorContent(arcIndices[0]);

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
			var arcColor = this.prGetArcColor(arcIdx);
			var btn = Button(tabView, Rect(i * tabWidth + 2, 2, tabWidth - 4, tabHeight - 4))
			.states_([
				["Arc " ++ arcIdx, Color.black, Color.gray(0.85)],
				["Arc " ++ arcIdx, Color.white, arcColor]
			])
			.font_(Font("Riforma Mono LL", 10, true))
			.value_(if(i == 0, 1, 0))
			.action_({ |b|
				activeTabIndex = tabButtons.indexOf(b);
				tabButtons.do { |tb, j| tb.value = if(j == activeTabIndex, 1, 0) };
				this.prCreateEditorContent(arcIndices[activeTabIndex]);
			});
			tabButtons.add(btn);
		};
	}

	prGetArcColor { |arcIdx|
		var maxArcs = data.arcCount.max(1);
		^Color.hsv((arcIdx / maxArcs) * 0.8, 0.7, 0.9)
	}

	prCreateEditorContent { |arcIdx|
		var viewHeight = editorContainer.bounds.height - toolbarHeight - controlHeight;
		var viewWidth = editorContainer.bounds.width;

		// Clear existing content
		editorContainer.children.do { |child| child.remove };

		// Create toolbar
		this.prCreateToolbar(arcIdx);

		// Create amplitude editor view (MultiSliderView)
		this.prCreateAmpView(arcIdx, viewWidth, viewHeight);

		// Create overlay for grid and arc shape
		this.prCreateOverlay(arcIdx, viewWidth, viewHeight);

		// Create control buttons
		this.prCreateControls(arcIdx);

		// Initialize display
		{ this.prUpdateDisplay(arcIdx) }.defer(0.1);
	}

	prCreateToolbar { |arcIdx|
		var xPos = 5;
		var zoom = zoomSettings[arcIdx];

		toolbarView = CompositeView(editorContainer, Rect(0, 0, width, toolbarHeight));
		toolbarView.background = Color.new(180/255, 230/255, 180/255);

		// Arc info
		StaticText(toolbarView, Rect(xPos, 5, 60, 20))
		.string_("Arc " ++ arcIdx)
		.font_(Font("Riforma Mono LL", 9))
		.stringColor_(Color.gray(0.3));
		xPos = xPos + 65;

		// Zoom label
		StaticText(toolbarView, Rect(xPos, 5, 40, 20))
		.string_("zoom")
		.font_(Font("Riforma Mono LL", 9))
		.stringColor_(Color.gray(0.3));
		xPos = xPos + 40;

		// Zoom slider
		Slider(toolbarView, Rect(xPos, 5, 120, 20))
		.value_(zoom[\zoomValue] ? 0)
		.action_({ |slider|
			this.prUpdateZoom(arcIdx, slider.value);
		});
		xPos = xPos + 130;

		// Position label
		StaticText(toolbarView, Rect(xPos, 5, 30, 20))
		.string_("pos")
		.font_(Font("Riforma Mono LL", 9))
		.stringColor_(Color.gray(0.3));
		xPos = xPos + 30;

		// Position slider
		Slider(toolbarView, Rect(xPos, 5, 120, 20))
		.value_(zoom[\panValue] ? 0)
		.action_({ |slider|
			this.prUpdatePan(arcIdx, slider.value);
		});
		xPos = xPos + 130;

		// Reset button
		Button(toolbarView, Rect(xPos, 5, 50, 20))
		.states_([["reset", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			zoomSettings[arcIdx] = (zoomMin: 0, zoomMax: 1, zoomValue: 0, panValue: 0);
			this.prCreateEditorContent(arcIdx);
		});
		xPos = xPos + 55;

		// Apply to all button (only if multiple arcs)
		if(arcIndices.size > 1) {
			Button(toolbarView, Rect(xPos, 5, 70, 20))
			.states_([["apply all", Color.black, Color.gray(0.9)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prApplyToAll(arcIdx);
			});
		};
	}

	prCreateAmpView { |arcIdx, viewWidth, viewHeight|
		ampView = MultiSliderView(editorContainer, Rect(0, toolbarHeight, viewWidth, viewHeight));
		ampView.background = Color.white;
		ampView.fillColor = Color.red.alpha_(0.6);
		ampView.strokeColor = Color.red;
		ampView.drawRects = false;
		ampView.drawLines = true;
		ampView.thumbSize = 4;
		ampView.gap = 0;
		ampView.isFilled = true;
		ampView.elasticMode = true;

		ampView.action = { |view|
			this.prHandleEnvelopeEdit(arcIdx, view.value);
		};
	}

	prCreateOverlay { |arcIdx, viewWidth, viewHeight|
		var zoom = zoomSettings[arcIdx];

		overlayView = UserView(editorContainer, Rect(0, toolbarHeight, viewWidth, viewHeight));
		overlayView.acceptsMouse = false;

		overlayView.drawFunc = {
			// Draw grid
			this.prDrawGrid(viewWidth, viewHeight, zoom[\zoomMin] ? 0, zoom[\zoomMax] ? 1);
		};
	}

	prDrawGrid { |viewWidth, viewHeight, zoomMin, zoomMax|
		// Horizontal lines (amplitude levels)
		Pen.strokeColor = Color.gray(0.8, 0.5);
		Pen.width = 0.5;

		5.do { |i|
			var y = i * (viewHeight / 4);
			Pen.line(Point(0, y), Point(viewWidth, y));
			Pen.stroke;

			// Labels
			Pen.fillColor = Color.gray(0.5);
			Pen.stringAtPoint(((4 - i) * 25).asString ++ "%",
				Point(5, y - 15), Font("Riforma Mono LL", 9));
		};

		// Vertical lines (time markers)
		10.do { |i|
			var x = (i + 1) * (viewWidth / 11);
			var normalizedPos = (i + 1) / 11;
			var actualPos = normalizedPos.linlin(0, 1, zoomMin, zoomMax);
			var percentLabel = (actualPos * 100).round(1).asString ++ "%";

			Pen.strokeColor = Color.gray(0.8, 0.5);
			Pen.line(Point(x, 0), Point(x, viewHeight));
			Pen.stroke;

			// Position labels
			Pen.fillColor = Color.gray(0.5);
			Pen.stringAtPoint(percentLabel, Point(x - 15, viewHeight - 15), Font("Riforma Mono LL", 8));
		};
	}

	prCreateControls { |arcIdx|
		var xPos = 10;
		var controlY = editorContainer.bounds.height - controlHeight;

		controlView = CompositeView(editorContainer, Rect(0, controlY, width, controlHeight));
		controlView.background = Color.new(190/255, 240/255, 190/255);

		// Preset buttons
		[\fadeIn, \fadeOut, \constant, \pulse, \adsr].do { |preset|
			Button(controlView, Rect(xPos, 5, 55, 25))
			.states_([["_" ++ preset.asString.toLower, Color.black, Color.gray(0.9)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prSaveUndo(arcIdx);
				this.prApplyPreset(arcIdx, preset);
			});
			xPos = xPos + 60;
		};

		// Normalize
		Button(controlView, Rect(xPos, 5, 60, 25))
		.states_([["_normalize", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prSaveUndo(arcIdx);
			this.prNormalize(arcIdx);
		});
		xPos = xPos + 65;

		// Smooth
		Button(controlView, Rect(xPos, 5, 55, 25))
		.states_([["_smooth", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prSaveUndo(arcIdx);
			this.prSmooth(arcIdx);
		});
		xPos = xPos + 60;

		// Clear
		Button(controlView, Rect(xPos, 5, 50, 25))
		.states_([["_clear", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prSaveUndo(arcIdx);
			this.prClear(arcIdx);
		});
		xPos = xPos + 55;

		// Undo
		Button(controlView, Rect(xPos, 5, 45, 25))
		.states_([["undo", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prUndo(arcIdx);
		});
		xPos = xPos + 50;

		// Redo
		Button(controlView, Rect(xPos, 5, 45, 25))
		.states_([["redo", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prRedo(arcIdx);
		});
	}

	// ========== Envelope Operations ==========

	prHandleEnvelopeEdit { |arcIdx, values|
		var arc = data.arcs[arcIdx];
		var zoom = zoomSettings[arcIdx];
		var zoomMin = zoom[\zoomMin] ? 0;
		var zoomMax = zoom[\zoomMax] ? 1;
		var arcLength, newEnv, existingEnv;

		if(arc.isNil or: { arc.size < 2 }) { ^this };

		arcLength = (arc.last[\x] - arc.first[\x]).abs;
		existingEnv = data.amplitudeEnvelopes[arcIdx];
		newEnv = List.new;

		// Keep points before zoom range
		if(existingEnv.notNil) {
			existingEnv.do { |point|
				if(point[\x] < (zoomMin * arcLength)) {
					newEnv.add(point);
				};
			};
		};

		// Add new points from slider values
		values.do { |amp, i|
			var normalizedPos = i / (values.size - 1).max(1);
			var zoomedPos = normalizedPos.linlin(0, 1, zoomMin, zoomMax);
			var x = zoomedPos * arcLength;
			newEnv.add((x: x, amp: amp));
		};

		// Keep points after zoom range
		if(existingEnv.notNil) {
			existingEnv.do { |point|
				if(point[\x] > (zoomMax * arcLength)) {
					newEnv.add(point);
				};
			};
		};

		// Sort and remove duplicates
		newEnv = newEnv.sort({ |a, b| a[\x] < b[\x] });

		// Store in data
		data.setAmplitudeEnvelope(arcIdx, newEnv);

		if(gui.notNil) { gui.refresh };
	}

	prUpdateDisplay { |arcIdx|
		var arc, ampEnv, zoom, zoomMin, zoomMax, arcLength;
		var resolution, values;

		arc = data.arcs[arcIdx];
		ampEnv = data.amplitudeEnvelopes[arcIdx];
		zoom = zoomSettings[arcIdx];
		zoomMin = zoom[\zoomMin] ? 0;
		zoomMax = zoom[\zoomMax] ? 1;

		if(arc.isNil or: { arc.size < 2 }) { ^this };
		if(ampEnv.isNil or: { ampEnv.size == 0 }) {
			// Default flat envelope at full amplitude
			ampView.value = Array.fill(200, 1.0);
			^this
		};

		arcLength = (arc.last[\x] - arc.first[\x]).abs;

		// Calculate dynamic resolution based on zoom
		resolution = ((200 / (zoomMax - zoomMin).max(0.1)).clip(50, 800)).asInteger;

		values = Array.fill(resolution, { |i|
			var normalizedPos = i / (resolution - 1);
			var zoomedPos = normalizedPos.linlin(0, 1, zoomMin, zoomMax);
			var xPos = zoomedPos * arcLength;
			this.prGetAmplitudeAt(ampEnv, xPos)
		});

		ampView.value = values;
		overlayView.refresh;
	}

	prGetAmplitudeAt { |ampEnv, xPos|
		var amp = 1.0;

		if(ampEnv.isNil or: { ampEnv.size == 0 }) { ^amp };

		if(ampEnv.size == 1) { ^ampEnv.first[\amp] };

		// Find surrounding points
		block { |break|
			(ampEnv.size - 1).do { |i|
				if(ampEnv[i][\x] <= xPos and: { ampEnv[i + 1][\x] >= xPos }) {
					var factor = (xPos - ampEnv[i][\x]) / (ampEnv[i + 1][\x] - ampEnv[i][\x]).max(0.001);
					amp = ampEnv[i][\amp].blend(ampEnv[i + 1][\amp], factor);
					break.value;
				};
			};
		};

		// Edge cases
		if(xPos <= ampEnv.first[\x]) { ^ampEnv.first[\amp] };
		if(xPos >= ampEnv.last[\x]) { ^ampEnv.last[\amp] };

		^amp
	}

	// ========== Zoom/Pan ==========

	prUpdateZoom { |arcIdx, zoomValue|
		var zoomRange = 1.0 - (zoomValue * 0.95);
		var currentPan = zoomSettings[arcIdx][\panValue] ? 0;
		var maxPan = 1 - zoomRange;
		var zoomStart = (currentPan * maxPan).clip(0, maxPan);
		var zoomEnd = (zoomStart + zoomRange).min(1.0);

		zoomSettings[arcIdx][\zoomMin] = zoomStart;
		zoomSettings[arcIdx][\zoomMax] = zoomEnd;
		zoomSettings[arcIdx][\zoomValue] = zoomValue;

		{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
	}

	prUpdatePan { |arcIdx, panValue|
		var zoom = zoomSettings[arcIdx];
		var zoomRange = (zoom[\zoomMax] ? 1) - (zoom[\zoomMin] ? 0);
		var maxPan = 1 - zoomRange;
		var newMin = (panValue * maxPan).clip(0, maxPan);
		var newMax = (newMin + zoomRange).min(1.0);

		zoomSettings[arcIdx][\zoomMin] = newMin;
		zoomSettings[arcIdx][\zoomMax] = newMax;
		zoomSettings[arcIdx][\panValue] = panValue;

		{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
	}

	// ========== Presets ==========

	prApplyPreset { |arcIdx, preset|
		var arc = data.arcs[arcIdx];
		var arcLength, newEnv;

		if(arc.isNil or: { arc.size < 2 }) { ^this };

		arcLength = (arc.last[\x] - arc.first[\x]).abs;

		newEnv = switch(preset)
		{ \fadeIn } { List[(x: 0, amp: 0), (x: arcLength, amp: 1)] }
		{ \fadeOut } { List[(x: 0, amp: 1), (x: arcLength, amp: 0)] }
		{ \constant } { List[(x: 0, amp: 0.7), (x: arcLength, amp: 0.7)] }
		{ \pulse } {
			List[
				(x: 0, amp: 0),
				(x: arcLength * 0.1, amp: 1),
				(x: arcLength * 0.2, amp: 0),
				(x: arcLength, amp: 0)
			]
		}
		{ \adsr } {
			List[
				(x: 0, amp: 0),
				(x: arcLength * 0.1, amp: 1),
				(x: arcLength * 0.3, amp: 0.6),
				(x: arcLength * 0.8, amp: 0.6),
				(x: arcLength, amp: 0)
			]
		}
		{ List[(x: 0, amp: 1.0), (x: arcLength, amp: 1.0)] };

		data.setAmplitudeEnvelope(arcIdx, newEnv);
		{ this.prUpdateDisplay(arcIdx) }.defer(0.05);

		"Applied % envelope to arc %".format(preset, arcIdx).postln;

		if(gui.notNil) { gui.refresh };
	}

	prNormalize { |arcIdx|
		var ampEnv = data.amplitudeEnvelopes[arcIdx];
		var maxAmp;

		if(ampEnv.isNil or: { ampEnv.size == 0 }) { ^this };

		maxAmp = ampEnv.collect({ |pt| pt[\amp] }).maxItem;

		if(maxAmp > 0) {
			var normalizedEnv = ampEnv.collect { |point|
				(x: point[\x], amp: point[\amp] / maxAmp)
			};
			data.setAmplitudeEnvelope(arcIdx, normalizedEnv);
			{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
			"Normalized amplitude for arc %".format(arcIdx).postln;
		};

		if(gui.notNil) { gui.refresh };
	}

	prSmooth { |arcIdx, windowSize = 5|
		var ampEnv = data.amplitudeEnvelopes[arcIdx];
		var smoothedEnv;

		if(ampEnv.isNil or: { ampEnv.size <= windowSize }) { ^this };

		smoothedEnv = List.new;

		ampEnv.do { |point, i|
			var startIdx = (i - (windowSize.div(2))).max(0);
			var endIdx = (i + (windowSize.div(2))).min(ampEnv.size - 1);
			var avgAmp = 0;
			var count = 0;

			(startIdx..endIdx).do { |j|
				avgAmp = avgAmp + ampEnv[j][\amp];
				count = count + 1;
			};

			smoothedEnv.add((x: point[\x], amp: avgAmp / count));
		};

		data.setAmplitudeEnvelope(arcIdx, smoothedEnv);
		{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
		"Smoothed amplitude for arc %".format(arcIdx).postln;

		if(gui.notNil) { gui.refresh };
	}

	prClear { |arcIdx|
		var arc = data.arcs[arcIdx];
		var arcLength, defaultEnv;

		if(arc.isNil or: { arc.size < 2 }) { ^this };

		arcLength = (arc.last[\x] - arc.first[\x]).abs;

		defaultEnv = List[
			(x: 0, amp: 1.0),
			(x: arcLength, amp: 1.0)
		];

		data.setAmplitudeEnvelope(arcIdx, defaultEnv);
		{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
		"Cleared amplitude for arc %".format(arcIdx).postln;

		if(gui.notNil) { gui.refresh };
	}

	prApplyToAll { |sourceArcIdx|
		var sourceEnv = data.amplitudeEnvelopes[sourceArcIdx];
		var sourceArc = data.arcs[sourceArcIdx];
		var sourceLength, appliedCount = 0;

		if(sourceEnv.isNil or: { sourceArc.isNil }) { ^this };

		sourceLength = (sourceArc.last[\x] - sourceArc.first[\x]).abs.max(1);

		arcIndices.do { |targetIdx|
			if(targetIdx != sourceArcIdx) {
				var targetArc = data.arcs[targetIdx];
				if(targetArc.notNil and: { targetArc.size >= 2 }) {
					var targetLength = (targetArc.last[\x] - targetArc.first[\x]).abs.max(1);
					var scaleFactor = targetLength / sourceLength;

					var scaledEnv = sourceEnv.collect { |point|
						(x: point[\x] * scaleFactor, amp: point[\amp])
					};

					data.setAmplitudeEnvelope(targetIdx, scaledEnv);
					appliedCount = appliedCount + 1;
				};
			};
		};

		"Applied amplitude from arc % to % arcs".format(sourceArcIdx, appliedCount).postln;

		if(gui.notNil) { gui.refresh };
	}

	// ========== Undo/Redo ==========

	prSaveUndo { |arcIdx|
		var currentEnv = data.amplitudeEnvelopes[arcIdx];
		var envCopy;

		if(undoStacks[arcIdx].isNil) {
			undoStacks[arcIdx] = List.new;
		};

		if(currentEnv.notNil) {
			envCopy = currentEnv.collect { |pt| (x: pt[\x], amp: pt[\amp]) };
			undoStacks[arcIdx].add(envCopy);

			// Limit stack size
			if(undoStacks[arcIdx].size > 20) {
				undoStacks[arcIdx].removeAt(0);
			};
		};

		// Clear redo stack
		redoStacks[arcIdx] = List.new;
	}

	prUndo { |arcIdx|
		var undoStack = undoStacks[arcIdx];
		var currentEnv, envCopy, previousState;

		if(undoStack.notNil and: { undoStack.size > 0 }) {
			// Save current to redo
			currentEnv = data.amplitudeEnvelopes[arcIdx];
			if(redoStacks[arcIdx].isNil) { redoStacks[arcIdx] = List.new };
			if(currentEnv.notNil) {
				envCopy = currentEnv.collect { |pt| (x: pt[\x], amp: pt[\amp]) };
				redoStacks[arcIdx].add(envCopy);
			};

			// Restore from undo
			previousState = undoStack.pop;
			data.setAmplitudeEnvelope(arcIdx, previousState);
			{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
			"Undid amplitude edit for arc %".format(arcIdx).postln;
		};
	}

	prRedo { |arcIdx|
		var redoStack = redoStacks[arcIdx];
		var currentEnv, envCopy, nextState;

		if(redoStack.notNil and: { redoStack.size > 0 }) {
			// Save current to undo
			currentEnv = data.amplitudeEnvelopes[arcIdx];
			if(undoStacks[arcIdx].isNil) { undoStacks[arcIdx] = List.new };
			if(currentEnv.notNil) {
				envCopy = currentEnv.collect { |pt| (x: pt[\x], amp: pt[\amp]) };
				undoStacks[arcIdx].add(envCopy);
			};

			// Restore from redo
			nextState = redoStack.pop;
			data.setAmplitudeEnvelope(arcIdx, nextState);
			{ this.prUpdateDisplay(arcIdx) }.defer(0.05);
			"Redid amplitude edit for arc %".format(arcIdx).postln;
		};
	}

	// ========== Public Methods ==========

	refresh {
		if(arcIndices.size > 0) {
			{ this.prUpdateDisplay(arcIndices[activeTabIndex]) }.defer(0.05);
		};
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
