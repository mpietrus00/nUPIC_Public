/*
 * NUPICSpatialEditor - Spatial envelope editing window for nUPIC
 *
 * Draw spatialization curves for selected arcs
 * Vertical axis = channels (0 to max), Horizontal axis = arc length
 */

NUPICSpatialEditor {
	// References
	var <>data;
	var <>gui;

	// Window elements
	var <window;
	var <tabView;
	var <tabButtons;
	var <editorContainer;
	var <spatialView;
	var <overlayView;
	var <channelLabelView;
	var <controlView;

	// State
	var <arcIndices;
	var <activeTabIndex = 0;
	var <isDrawing = false;
	var <originalEnvelope;
	var <drawingStartX;
	var <drawingEndX;
	var <newPoints;
	var lastX, lastChannel;

	// Constants
	var width = 800;
	var height = 500;
	var tabHeight = 30;
	var controlHeight = 40;
	var channelLabelWidth = 60;
	var channelLabelGap = 5;
	var verticalPadding = 15;

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
		newPoints = List.new;
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
			"No arc selected. Draw an arc first, select it, then open the spatial editor.".warn;
			^nil
		};

		// Filter out invalid indices
		arcIndices = arcIndices.select { |idx| idx < data.arcCount };
		if(arcIndices.size == 0) {
			"Selected arcs are invalid.".warn;
			^nil
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

		window = Window("nUPIC Spatial Editor", Rect(400, 200, width, winHeight), resizable: false);
		window.background = Color.new(205/255, 250/255, 205/255);

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
		var tabWidth = (width / arcIndices.size).min(100);

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
				this.prCreateEditorContent(arcIndices[activeTabIndex]);
			});
			tabButtons.add(btn);
		};
	}

	prCreateEditorContent { |arcIdx|
		var viewHeight = editorContainer.bounds.height - controlHeight;
		var viewWidth = editorContainer.bounds.width;
		var editorLeft = channelLabelWidth + channelLabelGap;
		var editorWidth = viewWidth - editorLeft - 10;
		var numChannels, maxChannel, arcLength, currentEnvelope;
		var arc, synthDef;

		// Clear existing content
		editorContainer.children.do { |child| child.remove };

		// Get arc info
		arc = data.arcs[arcIdx];
		if(arc.isNil or: { arc.size < 2 }) {
			arcLength = 600;
		} {
			arcLength = (arc.last[\x] - arc.first[\x]).abs;
		};

		// Get channel count from synthdef
		synthDef = data.getSynthDefForArc(arcIdx);
		numChannels = this.prGetChannelCount(synthDef);
		maxChannel = numChannels - 1;

		// Get or create envelope
		currentEnvelope = data.spatialEnvelopes[arcIdx];
		if(currentEnvelope.isNil) {
			currentEnvelope = List[
				(x: 0, channel: 0),
				(x: arcLength, channel: maxChannel)
			];
			data.setSpatialEnvelope(arcIdx, currentEnvelope);
		};

		// Create channel labels
		this.prCreateChannelLabels(viewHeight, numChannels, maxChannel);

		// Create spatial drawing view
		this.prCreateSpatialView(editorLeft, editorWidth, viewHeight, arcIdx, arcLength, numChannels, maxChannel, currentEnvelope);

		// Create overlay for mouse interaction
		this.prCreateOverlay(editorLeft, editorWidth, viewHeight, arcIdx, arcLength, numChannels, maxChannel);

		// Create controls
		this.prCreateControls(arcIdx, arcLength, numChannels, maxChannel, synthDef);
	}

	prGetChannelCount { |synthDefName|
		var name = synthDefName.asString;
		if(name.contains("24ch")) { ^24 };
		if(name.contains("15ch")) { ^15 };
		if(name.contains("12ch")) { ^12 };
		if(name.contains("8ch")) { ^8 };
		if(name.contains("4ch")) { ^4 };
		if(name.contains("3ch")) { ^3 };
		if(name.contains("2ch")) { ^2 };
		^2  // Default stereo
	}

	prCreateChannelLabels { |viewHeight, numChannels, maxChannel|
		var drawableHeight = viewHeight - (2 * verticalPadding);

		channelLabelView = UserView(editorContainer, Rect(0, 0, channelLabelWidth, viewHeight));
		channelLabelView.background = Color.white;

		channelLabelView.drawFunc = {
			numChannels.do { |i|
				var y = verticalPadding + (((maxChannel - i) / maxChannel.max(1)) * drawableHeight);
				var label = "Ch" ++ i;

				Pen.fillColor = Color.gray(0.3);
				Pen.stringAtPoint(label, Point(8, y - 6), Font("Riforma Mono LL", 9, true), Color.gray(0.3));

				Pen.strokeColor = Color.gray(0.4);
				Pen.width = 1.5;
				Pen.line(Point(channelLabelWidth - 12, y), Point(channelLabelWidth, y));
				Pen.stroke;
			};
		};
	}

	prCreateSpatialView { |editorLeft, editorWidth, viewHeight, arcIdx, arcLength, numChannels, maxChannel, currentEnvelope|
		var drawableHeight = viewHeight - (2 * verticalPadding);
		var synthDef = data.getSynthDefForArc(arcIdx);

		spatialView = UserView(editorContainer, Rect(editorLeft, 0, editorWidth, viewHeight));
		spatialView.background = Color.white;

		spatialView.drawFunc = {
			var w = spatialView.bounds.width;
			var h = spatialView.bounds.height;
			var env = data.spatialEnvelopes[arcIdx];

			// Draw grid lines for channels
			numChannels.do { |i|
				var y = verticalPadding + (((maxChannel - i) / maxChannel.max(1)) * drawableHeight);
				var dashLen = 4, gapLen = 4;
				var x = 0;

				if(i == 0 or: { i == maxChannel }) {
					Pen.strokeColor = Color.gray(0.5);
					Pen.width = 1;
				} {
					Pen.strokeColor = Color.gray(0.7);
					Pen.width = 0.5;
				};

				while { x < w } {
					Pen.line(Point(x, y), Point((x + dashLen).min(w), y));
					Pen.stroke;
					x = x + dashLen + gapLen;
				};
			};

			// Draw vertical time divisions
			10.do { |i|
				var x = (i + 1) / 11 * w;
				var y = 0;
				var dashLen = 3, gapLen = 5;

				Pen.strokeColor = Color.gray(0.85);
				Pen.width = 0.5;

				while { y < h } {
					Pen.line(Point(x, y), Point(x, (y + dashLen).min(h)));
					Pen.stroke;
					y = y + dashLen + gapLen;
				};
			};

			// Draw envelope
			if(env.notNil and: { env.size > 1 }) {
				Pen.strokeColor = Color.blue;
				Pen.width = 2;

				env.do { |point, i|
					var ptX = (point[\x] / arcLength) * w;
					var ptY = verticalPadding + (((maxChannel - point[\channel]) / maxChannel.max(1)) * drawableHeight);

					if(i == 0) {
						Pen.moveTo(Point(ptX, ptY));
					} {
						Pen.lineTo(Point(ptX, ptY));
					};
				};
				Pen.stroke;
			};

			// Info text
			Pen.fillColor = Color.gray(0.4);
			Pen.stringAtPoint(
				"Arc " ++ arcIdx ++ " | " ++ synthDef ++ " | Ch0-Ch" ++ maxChannel,
				Point(w - 200, 5),
				Font("Riforma Mono LL", 9),
				Color.gray(0.4)
			);
		};
	}

	prCreateOverlay { |editorLeft, editorWidth, viewHeight, arcIdx, arcLength, numChannels, maxChannel|
		var drawableHeight = viewHeight - (2 * verticalPadding);

		overlayView = UserView(editorContainer, Rect(editorLeft, 0, editorWidth, viewHeight));
		overlayView.background = Color.clear;

		overlayView.mouseDownAction = { |view, x, y, mods, btn, clicks|
			var w = view.bounds.width;
			var h = view.bounds.height;
			var timePos = (x / w) * arcLength;
			var channel = maxChannel - (((y - verticalPadding) / drawableHeight) * maxChannel);
			var currentEnvelope = data.spatialEnvelopes[arcIdx];
			channel = channel.clip(0, maxChannel);

			isDrawing = true;
			drawingStartX = timePos;
			drawingEndX = timePos;

			// Save original envelope before drawing
			if(currentEnvelope.notNil) {
				originalEnvelope = currentEnvelope.copy;
			} {
				originalEnvelope = List.new;
			};

			// Start fresh new points list
			newPoints = List.new;
			newPoints.add((x: timePos, channel: channel));
			lastX = x;
			lastChannel = channel;

			// Update envelope to show drawing immediately
			this.prUpdateEnvelopeForDrawing(arcIdx, arcLength, maxChannel);
			spatialView.refresh;
		};

		overlayView.mouseMoveAction = { |view, x, y, mods|
			if(isDrawing) {
				var w = view.bounds.width;
				var timePos = (x / w) * arcLength;
				var channel = maxChannel - (((y - verticalPadding) / drawableHeight) * maxChannel);
				channel = channel.clip(0, maxChannel);

				if(timePos < drawingStartX) { drawingStartX = timePos };
				if(timePos > drawingEndX) { drawingEndX = timePos };

				if((x - lastX).abs > 2 or: { (channel - lastChannel).abs > 0.05 }) {
					newPoints.add((x: timePos, channel: channel));
					lastX = x;
					lastChannel = channel;
					this.prUpdateEnvelopeForDrawing(arcIdx, arcLength, maxChannel);
					spatialView.refresh;
				};
			};
		};

		overlayView.mouseUpAction = { |view, x, y, mods, btn|
			if(isDrawing) {
				isDrawing = false;
				this.prFinalizeEnvelope(arcIdx, arcLength, maxChannel);
				spatialView.refresh;
			};
		};
	}

	prUpdateEnvelopeForDrawing { |arcIdx, arcLength, maxChannel|
		var mergedEnvelope, sortedNewPoints;

		mergedEnvelope = List.new;

		// Keep original points outside the drawn range
		if(originalEnvelope.notNil) {
			originalEnvelope.do { |point|
				if(point[\x] < drawingStartX or: { point[\x] > drawingEndX }) {
					mergedEnvelope.add(point);
				};
			};
		};

		// Add new points (sorted by x)
		sortedNewPoints = newPoints.sort({ |a, b| a[\x] < b[\x] });
		sortedNewPoints.do { |point|
			mergedEnvelope.add(point);
		};

		// Sort combined envelope by x
		mergedEnvelope = mergedEnvelope.sort({ |a, b| a[\x] < b[\x] });

		// Set the envelope for display (not interpolated yet)
		data.setSpatialEnvelope(arcIdx, mergedEnvelope);
	}

	prFinalizeEnvelope { |arcIdx, arcLength, maxChannel|
		var mergedEnvelope, sortedEnvelope, interpolated, sortedNewPoints;
		var numOutputPoints = 2048;

		mergedEnvelope = List.new;

		// Keep original points outside the drawn range
		if(originalEnvelope.notNil) {
			originalEnvelope.do { |point|
				if(point[\x] < drawingStartX or: { point[\x] > drawingEndX }) {
					mergedEnvelope.add(point);
				};
			};
		};

		// Add new points
		sortedNewPoints = newPoints.sort({ |a, b| a[\x] < b[\x] });
		sortedNewPoints.do { |point|
			mergedEnvelope.add(point);
		};

		// Sort by x
		sortedEnvelope = mergedEnvelope.sort({ |a, b| a[\x] < b[\x] });

		// Ensure start point
		if(sortedEnvelope.size == 0 or: { sortedEnvelope[0][\x] > 0 }) {
			var startCh = if(sortedEnvelope.size > 0) { sortedEnvelope[0][\channel] } { maxChannel / 2 };
			sortedEnvelope = List[(x: 0, channel: startCh)] ++ sortedEnvelope;
		};

		// Ensure end point
		if(sortedEnvelope.size > 0 and: { sortedEnvelope.last[\x] < arcLength }) {
			sortedEnvelope.add((x: arcLength, channel: sortedEnvelope.last[\channel]));
		};

		// Interpolate to 2048 points for smooth playback
		if(sortedEnvelope.size >= 2) {
			interpolated = List.new;
			numOutputPoints.do { |i|
				var t = i / (numOutputPoints - 1);
				var targetX = t * arcLength;
				var channel = this.prGetChannelAt(sortedEnvelope, targetX, maxChannel);
				interpolated.add((x: targetX, channel: channel.clip(0, maxChannel)));
			};
			data.setSpatialEnvelope(arcIdx, interpolated);
		} {
			data.setSpatialEnvelope(arcIdx, sortedEnvelope);
		};

		// Clear state
		originalEnvelope = nil;
		newPoints = List.new;

		"Spatial envelope saved for arc %".format(arcIdx).postln;
	}

	prGetChannelAt { |envelope, xPos, maxChannel|
		var channel = maxChannel / 2;

		if(envelope.isNil or: { envelope.size == 0 }) { ^channel };
		if(envelope.size == 1) { ^envelope.first[\channel] };

		block { |break|
			(envelope.size - 1).do { |i|
				if(envelope[i][\x] <= xPos and: { envelope[i + 1][\x] >= xPos }) {
					var factor = (xPos - envelope[i][\x]) / (envelope[i + 1][\x] - envelope[i][\x]).max(0.001);
					channel = envelope[i][\channel].blend(envelope[i + 1][\channel], factor);
					break.value;
				};
			};
		};

		if(xPos <= envelope.first[\x]) { ^envelope.first[\channel] };
		if(xPos >= envelope.last[\x]) { ^envelope.last[\channel] };

		^channel
	}

	prCreateControls { |arcIdx, arcLength, numChannels, maxChannel, synthDef|
		var xPos = 10;
		var viewHeight = editorContainer.bounds.height - controlHeight;

		controlView = CompositeView(editorContainer, Rect(0, viewHeight, width, controlHeight));
		controlView.background = Color.new(190/255, 240/255, 190/255);

		// Preset buttons
		[\center, \circle, \spiral, \random, \sweep, \sweepRev, \smooth].do { |preset|
			Button(controlView, Rect(xPos, 5, 55, 25))
			.states_([["_" ++ preset.asString.toLower, Color.black, Color.gray(0.9)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prApplyPreset(arcIdx, preset, arcLength, maxChannel);
			});
			xPos = xPos + 60;
		};

		// Channel offset label and control
		xPos = xPos + 20;
		StaticText(controlView, Rect(xPos, 8, 50, 20))
		.string_("Out Ch:")
		.font_(Font("Riforma Mono LL", 10))
		.stringColor_(Color.gray(0.3));
		xPos = xPos + 55;

		NumberBox(controlView, Rect(xPos, 5, 45, 25))
		.value_(data.getChannelOffset(arcIdx) ? 0)
		.clipLo_(0)
		.clipHi_(127)
		.step_(1)
		.scroll_step_(1)
		.font_(Font("Riforma Mono LL", 11))
		.action_({ |nb|
			data.setChannelOffset(arcIdx, nb.value.asInteger);
			"Arc % output channel offset: %".format(arcIdx, nb.value.asInteger).postln;
		});
	}

	prApplyPreset { |arcIdx, preset, arcLength, maxChannel|
		var numPts = 2048;
		var center = maxChannel / 2;
		var envelope = List.new;

		switch(preset)
		{ \center } {
			numPts.do { |i|
				envelope.add((x: i / (numPts - 1) * arcLength, channel: center));
			};
		}
		{ \circle } {
			numPts.do { |i|
				var phase = i / (numPts - 1) * 2pi;
				var ch = center + ((maxChannel / 2) * sin(phase));
				envelope.add((x: i / (numPts - 1) * arcLength, channel: ch.clip(0, maxChannel)));
			};
		}
		{ \spiral } {
			numPts.do { |i|
				var t = i / (numPts - 1);
				var phase = t * 4pi;
				var radius = t * center;
				var ch = center + (radius * sin(phase));
				envelope.add((x: t * arcLength, channel: ch.clip(0, maxChannel)));
			};
		}
		{ \random } {
			var prevCh = center;
			numPts.do { |i|
				var delta = rrand(-0.1, 0.1) * maxChannel;
				prevCh = (prevCh + delta).clip(0, maxChannel);
				envelope.add((x: i / (numPts - 1) * arcLength, channel: prevCh));
			};
		}
		{ \sweep } {
			numPts.do { |i|
				var t = i / (numPts - 1);
				envelope.add((x: t * arcLength, channel: t * maxChannel));
			};
		}
		{ \sweepRev } {
			numPts.do { |i|
				var t = i / (numPts - 1);
				envelope.add((x: t * arcLength, channel: maxChannel - (t * maxChannel)));
			};
		}
		{ \smooth } {
			var currentEnv = data.spatialEnvelopes[arcIdx];
			if(currentEnv.notNil and: { currentEnv.size > 2 }) {
				3.do {
					var smoothed = List.new;
					currentEnv.do { |point, i|
						if(i == 0 or: { i == (currentEnv.size - 1) }) {
							smoothed.add(point);
						} {
							var prev = currentEnv[i - 1][\channel];
							var next = currentEnv[i + 1][\channel];
							var avg = (prev + point[\channel] + next) / 3;
							smoothed.add((x: point[\x], channel: avg));
						};
					};
					currentEnv = smoothed;
				};
				envelope = currentEnv;
			} {
				^this  // Nothing to smooth
			};
		};

		data.setSpatialEnvelope(arcIdx, envelope);
		spatialView.refresh;
		"Applied % preset to arc %".format(preset, arcIdx).postln;
	}

	// ========== Public Methods ==========

	refresh {
		if(arcIndices.size > 0) {
			spatialView.refresh;
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
