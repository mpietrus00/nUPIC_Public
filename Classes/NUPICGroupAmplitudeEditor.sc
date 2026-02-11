/*
 * NUPICGroupAmplitudeEditor - Group amplitude envelope editor for nUPIC
 *
 * Allows drawing an amplitude envelope over a group of selected arcs.
 * The envelope is then distributed to each individual arc's amplitude
 * envelope based on the arc's temporal position within the group.
 */

NUPICGroupAmplitudeEditor {
	// References
	var <>data;           // NUPICData reference
	var <>gui;            // NUPICGUI reference for refresh

	// Window elements
	var <window;
	var <ampView;        // MultiSliderView for drawing envelope
	var <overlayView;    // Overlay for arc position markers
	var <toolbarView;
	var <infoLabel;

	// State
	var <arcIndices;
	var <timeMin, <timeMax;  // Temporal extent of selected arcs
	var <numSliders = 100;   // Resolution of envelope

	// Constants
	var width = 700;
	var height = 350;
	var toolbarHeight = 40;
	var margin = 10;

	// Class variable
	classvar <>current;

	*new { |dataRef, guiRef|
		^super.new.init(dataRef, guiRef)
	}

	init { |dataRef, guiRef|
		data = dataRef;
		gui = guiRef;
	}

	open {
		// Close existing window if open
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not }) {
			current.window.close;
		};

		// Get selected arcs
		arcIndices = List.new;
		if(data.notNil and: { data.selectedArcs.notNil } and: { data.selectedArcs.size > 0 }) {
			arcIndices = data.selectedArcs.asArray.sort.asList;
		};

		// Check if we have valid arcs
		if(arcIndices.size == 0 or: { data.arcCount == 0 }) {
			"No arcs selected. Select arcs first, then open the group amplitude editor.".warn;
			^nil
		};

		// Filter out invalid indices
		arcIndices = arcIndices.select { |idx| idx < data.arcCount };
		if(arcIndices.size == 0) {
			"Selected arcs are invalid.".warn;
			^nil
		};

		// Calculate temporal extent
		this.prCalculateTimeExtent;

		// Create the window
		this.prCreateWindow;

		current = this;
		window.front;
	}

	prCalculateTimeExtent {
		timeMin = inf;
		timeMax = 0;

		arcIndices.do { |idx|
			var arc = data.arcs[idx];
			arc.do { |pt|
				var x = pt[\x];
				if(x < timeMin) { timeMin = x };
				if(x > timeMax) { timeMax = x };
			};
		};

		// Ensure valid range
		if(timeMin >= timeMax) {
			timeMin = 0;
			timeMax = 1;
		};
	}

	prCreateWindow {
		var viewHeight = height - toolbarHeight - 35;
		var viewWidth = width - margin * 2;
		var btnColor = Color.new(180/255, 230/255, 180/255);  // B&K button green
		var editor = this;  // Capture reference for use in action blocks

		window = Window("nUPIC Group Amplitude Editor", Rect(200, 150, width, height), resizable: false);
		window.background = Color.new(205/255, 250/255, 205/255);  // B&K green (matches other editors)

		// Info label
		infoLabel = StaticText(window, Rect(margin, 5, width - margin * 2, 20))
		.string_("Selected: % arcs | Time span: % - % | Draw envelope, then click Apply".format(
			arcIndices.size,
			timeMin.round(0.001),
			timeMax.round(0.001)
		))
		.font_(Font("Riforma Mono LL", 10))
		.align_(\center);

		// MultiSliderView for drawing envelope (like individual amp editor)
		ampView = MultiSliderView(window, Rect(margin, 30, viewWidth, viewHeight));
		ampView.background = Color.white;
		ampView.fillColor = Color.red.alpha_(0.6);
		ampView.strokeColor = Color.red;
		ampView.drawRects = false;
		ampView.drawLines = true;
		ampView.thumbSize = 6;
		ampView.gap = 0;
		ampView.isFilled = true;
		ampView.elasticMode = false;  // Disabled - visual must match stored values
		ampView.value = Array.fill(numSliders, { 1.0 });  // Default: flat at 1.0

		ampView.action = { |view|
			editor.prHandleEnvelopeEdit(view.value);
		};

		// Overlay for arc position markers
		overlayView = UserView(window, Rect(margin, 30, viewWidth, viewHeight));
		overlayView.acceptsMouse = false;
		overlayView.drawFunc = { editor.prDrawOverlay(viewWidth, viewHeight) };

		// Toolbar
		toolbarView = CompositeView(window, Rect(0, height - toolbarHeight, width, toolbarHeight));
		toolbarView.background = Color.new(180/255, 230/255, 180/255);  // B&K toolbar green

		// Preset buttons
		Button(toolbarView, Rect(10, 5, 60, 30))
		.states_([["Flat", Color.black, btnColor]])
		.font_(Font("Riforma Mono LL", 10))
		.action_({ editor.prSetPreset(\flat) });

		Button(toolbarView, Rect(75, 5, 60, 30))
		.states_([["Cresc", Color.black, btnColor]])
		.font_(Font("Riforma Mono LL", 10))
		.action_({ editor.prSetPreset(\crescendo) });

		Button(toolbarView, Rect(140, 5, 60, 30))
		.states_([["Decresc", Color.black, btnColor]])
		.font_(Font("Riforma Mono LL", 10))
		.action_({ editor.prSetPreset(\decrescendo) });

		Button(toolbarView, Rect(205, 5, 60, 30))
		.states_([["Bell", Color.black, btnColor]])
		.font_(Font("Riforma Mono LL", 10))
		.action_({ editor.prSetPreset(\bell) });

		Button(toolbarView, Rect(270, 5, 60, 30))
		.states_([["Swell", Color.black, btnColor]])
		.font_(Font("Riforma Mono LL", 10))
		.action_({ editor.prSetPreset(\swell) });

		// Apply button
		Button(toolbarView, Rect(width - 90, 5, 80, 30))
		.states_([["Apply", Color.white, Color.new(0.2, 0.5, 0.2)]])
		.font_(Font("Riforma Mono LL", 11, true))
		.action_({ editor.applyToArcs });

		window.onClose = {
			current = nil;
		};
	}

	// Handle envelope edit from MultiSliderView - just refreshes overlay
	prHandleEnvelopeEdit { |values|
		// Values are stored directly in ampView.value
		// Refresh overlay to update any display
		if(overlayView.notNil) {
			overlayView.refresh;
		};
	}

	// Draw arc position markers on overlay
	prDrawOverlay { |viewWidth, viewHeight|
		var timeSpan = timeMax - timeMin;

		if(timeSpan <= 0) { ^this };

		// Draw arc positions as vertical bars
		arcIndices.do { |idx|
			var arc = data.arcs[idx];
			var arcMinX = inf, arcMaxX = 0;
			var normStart, normEnd, x1, x2;

			// Find actual min/max x of this arc
			arc.do { |pt|
				if(pt[\x] < arcMinX) { arcMinX = pt[\x] };
				if(pt[\x] > arcMaxX) { arcMaxX = pt[\x] };
			};
			normStart = (arcMinX - timeMin) / timeSpan;
			normEnd = (arcMaxX - timeMin) / timeSpan;
			x1 = normStart * viewWidth;
			x2 = normEnd * viewWidth;

			// Draw arc span as a light bar
			Pen.fillColor = Color.blue.alpha_(0.15);
			Pen.fillRect(Rect(x1, 0, x2 - x1, viewHeight));

			// Draw arc boundaries
			Pen.strokeColor = Color.blue.alpha_(0.4);
			Pen.width = 1;
			Pen.line(Point(x1, 0), Point(x1, viewHeight));
			Pen.line(Point(x2, 0), Point(x2, viewHeight));
			Pen.stroke;
		};

		// Draw grid lines
		Pen.strokeColor = Color.gray(0.7, 0.5);
		Pen.width = 0.5;

		// Horizontal lines (amplitude levels)
		5.do { |i|
			var y = i * (viewHeight / 4);
			Pen.line(Point(0, y), Point(viewWidth, y));
		};

		// Vertical lines (time markers)
		10.do { |i|
			var x = (i + 1) * (viewWidth / 11);
			Pen.line(Point(x, 0), Point(x, viewHeight));
		};
		Pen.stroke;

		// Labels
		Pen.fillColor = Color.gray(0.4);
		Pen.font = Font("Riforma Mono LL", 9);
		Pen.stringAtPoint("1.0", Point(5, 5));
		Pen.stringAtPoint("0.0", Point(5, viewHeight - 15));
	}

	prSetPreset { |preset|
		var values;

		if(ampView.isNil) { ^this };

		values = switch(preset,
			\flat, {
				Array.fill(numSliders, { 1.0 });
			},
			\crescendo, {
				// Smooth S-curve rise using sine
				Array.fill(numSliders, { |i|
					var t = i / (numSliders - 1);
					(sin((t - 0.5) * pi) + 1) / 2  // S-curve 0 to 1
				});
			},
			\decrescendo, {
				// Smooth S-curve fall using sine
				Array.fill(numSliders, { |i|
					var t = i / (numSliders - 1);
					1.0 - ((sin((t - 0.5) * pi) + 1) / 2)  // S-curve 1 to 0
				});
			},
			\bell, {
				// Smooth bell curve (starts/ends at 0.1 for audibility)
				Array.fill(numSliders, { |i|
					var t = i / (numSliders - 1);
					var bell = sin(t * pi);  // 0 -> 1 -> 0
					bell.max(0) * 0.9 + 0.1  // Scale to 0.1 - 1.0
				});
			},
			\swell, {
				// Smooth swell (sine bell, starts/ends at 0)
				Array.fill(numSliders, { |i|
					var t = i / (numSliders - 1);
					sin(t * pi).max(0)  // Pure sine bell: 0 -> 1 -> 0
				});
			},
			// Default: flat
			{ Array.fill(numSliders, { 1.0 }) }
		);

		ampView.value = values;
		if(overlayView.notNil) { overlayView.refresh };
	}

	// Sample amplitude from MultiSliderView at a given normalized time (0-1)
	prSampleFromSliders { |time|
		var values, index, indexFloat, i1, i2, frac, v1, v2;

		if(ampView.isNil) { ^1.0 };

		values = ampView.value;
		if(values.isNil or: { values.size == 0 }) { ^1.0 };

		time = time.clip(0, 1);

		// Calculate fractional index
		indexFloat = time * (values.size - 1);
		i1 = indexFloat.floor.asInteger.clip(0, values.size - 1);
		i2 = indexFloat.ceil.asInteger.clip(0, values.size - 1);
		frac = indexFloat - i1;

		// Linear interpolation between adjacent slider values
		v1 = values[i1] ? 1.0;
		v2 = values[i2] ? 1.0;

		^(v1 + (frac * (v2 - v1))).clip(0, 1)
	}

	applyToArcs {
		var timeSpan = timeMax - timeMin;

		if(arcIndices.size == 0) {
			"No arcs to apply envelope to".warn;
			^this
		};

		arcIndices.do { |idx|
			var arc = data.arcs[idx];
			var arcEnvelope = List.new;
			var arcMinX = inf, arcMaxX = 0;
			var arcDuration, numPoints;

			// Find actual min/max x of this arc
			arc.do { |pt|
				if(pt[\x] < arcMinX) { arcMinX = pt[\x] };
				if(pt[\x] > arcMaxX) { arcMaxX = pt[\x] };
			};

			arcDuration = (arcMaxX - arcMinX).max(0.001);
			numPoints = (arcDuration * 20).max(3).min(20).asInteger;

			// Sample the group envelope at this arc's temporal position
			numPoints.do { |i|
				var localTime = i / (numPoints - 1);
				var globalTime = arcMinX + (localTime * arcDuration);
				var normalizedGlobalTime = (globalTime - timeMin) / timeSpan;
				var ampValue = this.prSampleFromSliders(normalizedGlobalTime);
				var xPos = localTime * arcDuration;

				arcEnvelope.add((x: xPos, amp: ampValue));
			};

			data.amplitudeEnvelopes[idx] = arcEnvelope;
		};

		// Notify of change
		if(data.onDataChanged.notNil) {
			data.onDataChanged.value;
		};

		// Refresh GUI
		if(gui.notNil) {
			gui.refresh;
		};

		"Applied group amplitude envelope to % arcs".format(arcIndices.size).postln;
	}
}
