/*
 * NUPICGUI - Main GUI class for nUPIC
 *
 * Contains all UI elements: window, canvas, controls, editors
 * Matches the legacy UI layout exactly
 */

NUPICGUI {
	// Window and views
	var <window;
	var <canvas;
	var <freqLabelView;
	var <pageTabButtons;
	var <controls;

	// Dimensions
	var <width, <height;
	var <canvasWidth, <canvasHeight;
	var <controlsHeight;

	// Layout constants
	var margin = 5;
	var freqLabelWidth = 80;
	var freqLabelGap = 8;
	var rangeSliderSize = 20;
	var rangeSliderGap = 5;
	var pageTabHeight = 30;

	// UI State
	var <isDrawing = false;
	var <eraseMode = false;
	var <selectMode = false;
	var <scissorsMode = false;
	var <>showGrid = true;
	var <>showArcNumbers = false;
	var <currentArc;
	var <mouseX = 0, <mouseY = 0;

	// Marquee selection state
	var <isMarqueeSelecting = false;
	var <marqueeStartX = 0, <marqueeStartY = 0;
	var <marqueeCurrentX = 0, <marqueeCurrentY = 0;

	// Drag state
	var <isDragging = false;
	var <dragStartX = 0, <dragStartY = 0;
	var <dragLastX = 0, <dragLastY = 0;

	// Scissors preview state
	var <scissorsCutX = nil, <scissorsCutY = nil;
	var <scissorsArcIdx = nil;

	// Grid/Scale settings
	var <gridType = \equalTemperament;
	var gridTypes;  // Array of grid type symbols

	// Oversampling mode - affects synth names displayed
	var <>useOversampling = false;
	// Multichannel mode - true = all channel variants, false = Mono/2ch only
	var <>useMultichannel = false;

	// Zoom state - absolute frequency range
	var freqRangeMin = 20;    // Absolute minimum frequency
	var freqRangeMax = 7500;  // Absolute maximum frequency
	var <zoomFreqMin = 20;
	var <zoomFreqMax = 7500;
	var <zoomTimeMin = 0;
	var <zoomTimeMax = 1;

	// Playback state
	var <isPlaying = false;
	var <playbackPosition = 0;
	var <playDuration = 10;
	var <isScrubbing = false;  // For DRAG mode scrubbing

	// Colors
	var <colors;

	// Callbacks (set by external code)
	var <>onArcDrawn;
	var <>onArcErased;
	var <>onSelectionChanged;
	var <>onPlaybackToggle;
	var <>onScrub;  // Called with normalized time (0-1) during DRAG mode

	// Data reference (set externally)
	var <>data;
	var <>playback;  // For checking playback state during paste

	// Sub-editors
	var <wavetableEditor;
	var <amplitudeEditor;
	var <groupAmplitudeEditor;
	var <widthEditor;
	var <spatialEditor;
	var <multiArcEditor;
	var <arcsListView;
	var <analyzerGUI;
	var <frequencyTableEditor;

	*new { |argWidth = 2050, argHeight = 670, argControlsHeight = 32|
		^super.new.init(argWidth, argHeight, argControlsHeight)
	}

	init { |argWidth, argHeight, argControlsHeight|
		width = argWidth;
		height = argHeight;
		controlsHeight = argControlsHeight;

		// Calculate canvas dimensions
		canvasWidth = width - margin - freqLabelWidth - freqLabelGap - margin - rangeSliderSize - rangeSliderGap;
		canvasHeight = height - margin - margin - rangeSliderSize - rangeSliderGap - pageTabHeight - margin;

		// Initialize colors
		colors = (
			background: Color.white,
			grid: Color.gray(0.3, 0.5),
			gridText: Color.gray(0.4),
			arc: Color.black,
			selected: Color.blue,
			playback: Color.red,
			bkGreen: Color.new(205/255, 250/255, 205/255)
		);

		// Initialize controls dictionary
		controls = IdentityDictionary.new;

		// Initialize grid types array (matches menu order)
		gridTypes = [\equalTemperament, \fokker31, \harmonicSeries, \just, \quarterTone, \pythagorean];
	}

	// ========== Window Creation ==========

	open {
		if(window.notNil and: { window.isClosed.not }) {
			window.front;
			^this
		};

		this.createWindow;
		this.createScaleButton;  // _scale button above freq labels
		this.createPageTabs;
		this.createFreqLabels;
		this.createCanvas;
		this.createRangeSliders;
		this.createControlPanel;

		window.front;
		"NUPICGUI opened".postln;
	}

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
	}

	isOpen {
		^(window.notNil and: { window.isClosed.not })
	}

	createWindow {
		window = Window("nUPIC - Arc Synthesis System",
			Rect(100, 100, width, height + controlsHeight),
			resizable: false
		);
		window.background = colors[\bkGreen];

		window.onClose = {
			this.prCleanup;
			"NUPICGUI closed".postln;
		};

		// Window-level keyboard handling (works regardless of focus)
		window.view.keyDownAction = { |view, char, mods, unicode, keycode|
			this.prHandleKeyDown(char, mods, unicode, keycode);
		};
	}

	// ========== Scale Button (above freq labels) ==========

	createScaleButton {
		controls[\scaleButton] = Button(window, Rect(
			margin,
			margin,
			freqLabelWidth,
			pageTabHeight - 5
		))
		.states_([["_scale", Color.white, Color.new(0.2, 0.5, 0.3)]])
		.font_(Font("Riforma Mono LL", 11))
		.action_({
			this.openFrequencyTableEditor;
		});
	}

	// ========== Page Tabs ==========

	createPageTabs {
		var labels = ["_a", "_b", "_c", "_d"];
		var tabStartX = margin + freqLabelWidth + freqLabelGap;  // Align with canvas
		var tabAreaWidth = canvasWidth;
		var tabWidth = (tabAreaWidth - 12) / 4;

		pageTabButtons = List.new;

		labels.do { |label, i|
			var btn = Button(window, Rect(
				tabStartX + (i * (tabWidth + 3)),
				margin,
				tabWidth,
				pageTabHeight - 5
			))
			.states_([
				[label, Color.black, Color.gray(0.85)],
				[label, Color.white, Color.new(0.2, 0.5, 0.2)]
			])
			.font_(Font("Riforma Mono LL", 11))
			.value_(if(i == 0) { 1 } { 0 })
			.action_({ |b|
				this.prSwitchPage(pageTabButtons.indexOf(b));
			});

			pageTabButtons.add(btn);
		};
	}

	prSwitchPage { |index|
		// Update button states
		pageTabButtons.do { |btn, i|
			btn.value = if(i == index) { 1 } { 0 };
		};

		// Switch page in data layer
		if(data.notNil) {
			data.switchPage(index);
		};

		this.refresh;
	}

	// ========== Frequency Labels ==========

	createFreqLabels {
		var yPos = margin + pageTabHeight;

		freqLabelView = UserView(window, Rect(margin, yPos, freqLabelWidth, canvasHeight));
		freqLabelView.background = Color.white;

		freqLabelView.drawFunc = {
			this.prDrawFreqLabels;
		};
	}

	prDrawFreqLabels {
		var frequencies = this.prGetGridFrequencies;
		var minSpacing = 15;  // Minimum pixels between labels
		var lastY = -1000;
		var fMin, fMax;
		var useScaleTable = data.notNil and: { data.isFrequencyTableEnabled };

		// Get frequency range (page range if table enabled, otherwise zoom range)
		if(useScaleTable) {
			fMin = data.getFrequencyRangeMin;
			fMax = data.getFrequencyRangeMax;
		} {
			fMin = zoomFreqMin;
			fMax = zoomFreqMax;
		};

		// Clear background
		Pen.fillColor = colors[\background];
		Pen.fillRect(Rect(0, 0, freqLabelWidth, canvasHeight));

		Pen.font = Font("Riforma Mono LL", 9);

		// Draw labels using same mapping as grid
		frequencies.do { |freq|
			var y, yNorm, label;

			// Convert frequency to normalized Y, then to screen
			if(useScaleTable) {
				yNorm = data.freqToYWithTable(freq, fMin, fMax);
			} {
				// Convert frequency to normalized Y in full range
				yNorm = freq.explin(freqRangeMin, freqRangeMax, 0, 1);
			};
			y = this.yNormToScreen(yNorm);

			// Only draw if within visible bounds AND far enough from last label
			if(y >= 10 and: { y <= (canvasHeight - 10) } and: { (y - lastY).abs >= minSpacing }) {
				label = this.prFormatFreqLabel(freq);

				Pen.fillColor = colors[\gridText];
				Pen.stringAtPoint(label, Point(3, y - 6));

				// Draw tick mark connecting to grid line
				Pen.strokeColor = colors[\grid];
				Pen.width = 2;
				Pen.line(Point(freqLabelWidth - 12, y), Point(freqLabelWidth, y));
				Pen.stroke;

				lastY = y;
			};
		};
	}

	prFormatFreqLabel { |freq|
		// Format frequency label - always show Hz value
		^freq.round(1).asInteger.asString ++ " Hz"
	}

	prGetGridFrequencies {
		// If frequency table is enabled, use its pitches (from cached sorted table)
		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			var sortedTable = data.frequencyTableSortedByFreq;
			if(sortedTable.notNil and: { sortedTable.size > 0 }) {
				var fMin = data.getFrequencyRangeMin;
				var fMax = data.getFrequencyRangeMax;
				// Extract frequencies from pre-sorted table, filter to range
				^sortedTable.collect { |pt| pt.freq }.select { |f| f >= fMin and: { f <= fMax } }
			};
		};

		// Otherwise use grid type selector
		^case
		{ gridType == \equalTemperament } { this.prGet12TETFrequencies }
		{ gridType == \fokker31 } { this.prGet31TETFrequencies }
		{ gridType == \harmonicSeries } { this.prGetHarmonicFrequencies }
		{ gridType == \just } { this.prGetJustFrequencies }
		{ gridType == \quarterTone } { this.prGet24TETFrequencies }
		{ gridType == \pythagorean } { this.prGetPythagoreanFrequencies }
		{ true } { this.prGet12TETFrequencies }  // Default
	}

	prGet12TETFrequencies {
		// 12-tone equal temperament - ALL semitones
		var frequencies = List.new;
		var baseFreq = 440;
		var semitone = 2 ** (1/12);
		var startNote = (zoomFreqMin / baseFreq).log2 * 12;
		var endNote = (zoomFreqMax / baseFreq).log2 * 12;

		(startNote.floor.asInteger..(endNote.ceil.asInteger)).do { |noteNum|
			var freq = baseFreq * (semitone ** noteNum);
			if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
				frequencies.add(freq);
			};
		};

		^frequencies
	}

	prGet31TETFrequencies {
		// 31-tone equal temperament (Fokker) - ALL 31 steps per octave
		var frequencies = List.new;
		var baseFreq = 440;
		var step31 = 2 ** (1/31);
		var startStep = (zoomFreqMin / baseFreq).log2 * 31;
		var endStep = (zoomFreqMax / baseFreq).log2 * 31;

		(startStep.floor.asInteger..(endStep.ceil.asInteger)).do { |stepNum|
			var freq = baseFreq * (step31 ** stepNum);
			if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
				frequencies.add(freq);
			};
		};

		^frequencies
	}

	prGet24TETFrequencies {
		// 24-tone equal temperament - ALL quarter tones
		var frequencies = List.new;
		var baseFreq = 440;
		var quarterTone = 2 ** (1/24);
		var startQuarter = (zoomFreqMin / baseFreq).log2 * 24;
		var endQuarter = (zoomFreqMax / baseFreq).log2 * 24;

		(startQuarter.floor.asInteger..(endQuarter.ceil.asInteger)).do { |quarterNum|
			var freq = baseFreq * (quarterTone ** quarterNum);
			if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
				frequencies.add(freq);
			};
		};

		^frequencies
	}

	prGetHarmonicFrequencies {
		// Harmonic series based on A2 = 110Hz - ALL harmonics
		var frequencies = List.new;
		var fundamental = 110;
		var startHarm = (zoomFreqMin / fundamental).ceil.max(1).asInteger;
		var endHarm = (zoomFreqMax / fundamental).floor.asInteger;

		(startHarm..endHarm).do { |harm|
			var freq = fundamental * harm;
			if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
				frequencies.add(freq);
			};
		};

		^frequencies
	}

	prGetJustFrequencies {
		// Just intonation - ALL ratios per octave
		var frequencies = List.new;
		var baseFreq = 440;
		var justRatios = [1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2];
		var octave = (zoomFreqMin / baseFreq).log2.floor.asInteger;

		while { (baseFreq * (2 ** octave)) < zoomFreqMax } {
			justRatios.do { |ratio|
				var freq = baseFreq * (2 ** octave) * ratio;
				if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
					frequencies.add(freq);
				};
			};
			octave = octave + 1;
		};

		^frequencies.asSet.asArray.sort
	}

	prGetPythagoreanFrequencies {
		// Pythagorean tuning - ALL ratios per octave
		var frequencies = List.new;
		var baseFreq = 440;
		var pythRatios = [1, 256/243, 9/8, 32/27, 81/64, 4/3, 729/512, 3/2, 128/81, 27/16, 16/9, 243/128, 2];
		var octave = (zoomFreqMin / baseFreq).log2.floor.asInteger;

		while { (baseFreq * (2 ** octave)) < zoomFreqMax } {
			pythRatios.do { |ratio|
				var freq = baseFreq * (2 ** octave) * ratio;
				if(freq >= zoomFreqMin and: { freq <= zoomFreqMax }) {
					frequencies.add(freq);
				};
			};
			octave = octave + 1;
		};

		^frequencies.asSet.asArray.sort
	}

	// ========== Canvas ==========

	createCanvas {
		var xPos = margin + freqLabelWidth + freqLabelGap;
		var yPos = margin + pageTabHeight;

		canvas = UserView(window, Rect(xPos, yPos, canvasWidth, canvasHeight));
		canvas.background = Color.white;

		canvas.drawFunc = {
			this.prDrawCanvas;
		};

		canvas.mouseDownAction = { |view, x, y, mods|
			this.prHandleMouseDown(x, y, mods);
		};

		canvas.mouseMoveAction = { |view, x, y|
			this.prHandleMouseMove(x, y);
		};

		canvas.mouseUpAction = { |view, x, y|
			this.prHandleMouseUp(x, y);
		};

		canvas.mouseOverAction = { |view, x, y|
			mouseX = x;
			mouseY = y;
			if(eraseMode) { canvas.refresh };
			if(scissorsMode) {
				this.prUpdateScissorsPreview(x, y);
				canvas.refresh;
			};
		};

		// Enable keyboard focus and add key handling
		canvas.acceptsMouseOver = true;
		canvas.canFocus = true;

		canvas.keyDownAction = { |view, char, mods, unicode, keycode|
			this.prHandleKeyDown(char, mods, unicode, keycode);
		};
	}

	prDrawCanvas {
		// Background
		Pen.fillColor = colors[\background];
		Pen.fillRect(Rect(0, 0, canvasWidth, canvasHeight));

		// Grid
		if(showGrid) {
			this.prDrawGrid;
		};

		// Arcs
		this.prDrawArcs;

		// Current arc being drawn
		if(currentArc.notNil and: { currentArc.size > 1 }) {
			this.prDrawCurrentArc;
		};

		// Playback position
		if(isPlaying) {
			this.prDrawPlaybackLine;
		};

		// Erase cursor
		if(eraseMode) {
			this.prDrawEraseCursor;
		};

		// Marquee selection rectangle
		if(isMarqueeSelecting) {
			this.prDrawMarquee;
		};

		// Scissors cut preview
		if(scissorsMode and: { scissorsCutX.notNil }) {
			this.prDrawScissorsPreview;
		};
	}

	prDrawMarquee {
		var left, right, top, bottom;

		left = min(marqueeStartX, marqueeCurrentX);
		right = max(marqueeStartX, marqueeCurrentX);
		top = min(marqueeStartY, marqueeCurrentY);
		bottom = max(marqueeStartY, marqueeCurrentY);

		// Draw selection rectangle - 80% transparent fill
		Pen.strokeColor = Color.blue(0.7);
		Pen.fillColor = Color.blue(0.2, 0.2);  // 20% opacity (80% transparent)
		Pen.width = 1;
		Pen.addRect(Rect(left, top, right - left, bottom - top));
		Pen.fillStroke;
	}

	prDrawGrid {
		var frequencies = this.prGetGridFrequencies;
		var minSpacing = 15;  // Same as labels
		var lastY = -1000;
		var fMin, fMax;
		var useScaleTable = data.notNil and: { data.isFrequencyTableEnabled };

		// Get frequency range (page range if table enabled, otherwise zoom range)
		if(useScaleTable) {
			fMin = data.getFrequencyRangeMin;
			fMax = data.getFrequencyRangeMax;
		} {
			fMin = zoomFreqMin;
			fMax = zoomFreqMax;
		};

		Pen.strokeColor = colors[\grid];
		Pen.width = 0.5;

		// Horizontal lines at frequency positions
		frequencies.do { |freq|
			var y, yNorm;

			// Convert frequency to normalized Y, then to screen
			if(useScaleTable) {
				yNorm = data.freqToYWithTable(freq, fMin, fMax);
			} {
				// Convert frequency to normalized Y in full range
				yNorm = freq.explin(freqRangeMin, freqRangeMax, 0, 1);
			};
			y = this.yNormToScreen(yNorm);

			// Use same filtering as labels so grid lines match exactly
			if(y >= 10 and: { y <= (canvasHeight - 10) } and: { (y - lastY).abs >= minSpacing }) {
				Pen.line(Point(0, y), Point(canvasWidth, y));
				Pen.stroke;
				lastY = y;
			};
		};
	}

	prDrawArcs {
		var arcList;

		if(data.isNil) { ^this };

		// Get arcs from data layer
		arcList = data.arcs;
		if(arcList.isNil) { ^this };

		arcList.do { |arc, i|
			var isSelected, firstPt, screenX, screenY, lastX, visible;
			var ampEnv, arcStartX, arcEndX, arcLength;

			if(arc.size > 1) {
				// Quick visibility check using first and last points
				firstPt = arc[0];
				screenX = this.spatialToScreen(firstPt[\x]);
				lastX = this.spatialToScreen(arc.last[\x]);

				// Skip if entirely outside visible area
				visible = (screenX <= canvasWidth and: { lastX >= 0 }) or:
				          { screenX >= 0 and: { lastX <= canvasWidth } };

				if(visible) {
					isSelected = data.isSelected(i);
					ampEnv = data.amplitudeEnvelopes[i];

					// Get arc spatial bounds for amplitude lookup
					arcStartX = firstPt[\x];
					arcEndX = arc.last[\x];
					arcLength = (arcEndX - arcStartX).max(1);

					Pen.width = if(isSelected) { 2 } { 1 };

					// Draw arc with amplitude-based coloring if envelope exists
					if(ampEnv.notNil and: { ampEnv.size > 0 }) {
						// Draw each segment with its own color
						(arc.size - 1).do { |j|
							var pt1 = arc[j];
							var pt2 = arc[j + 1];
							var x1 = this.spatialToScreen(pt1[\x]);
							var y1 = this.ptToScreenY(pt1);
							var x2 = this.spatialToScreen(pt2[\x]);
							var y2 = this.ptToScreenY(pt2);
							var midX = (pt1[\x] + pt2[\x]) / 2;
							var amp = this.prGetAmplitudeAt(ampEnv, midX - arcStartX, arcLength);
							var segColor;

							// Color: blue (quiet) -> red (loud)
							// Selected arcs use orange tint
							if(isSelected) {
								segColor = Color.new(
									0.5 + (amp * 0.5),  // R: 0.5-1.0
									0.3 * (1 - amp),     // G: 0.3-0
									0,                   // B: 0
									1
								);
							} {
								segColor = Color.new(
									amp,           // R: 0-1 (more red = louder)
									0,             // G: 0
									1 - amp,       // B: 1-0 (more blue = quieter)
									1
								);
							};

							Pen.strokeColor = segColor;
							Pen.moveTo(Point(x1, y1));
							Pen.lineTo(Point(x2, y2));
							Pen.stroke;
						};
					} {
						// No amplitude envelope - use solid color
						Pen.strokeColor = if(isSelected) {
							Color.new(1, 0.5, 0, 1)
						} {
							colors[\arc]
						};

						// Draw arc using direct iteration
						screenY = this.ptToScreenY(firstPt);
						Pen.moveTo(Point(screenX, screenY));

						(1..(arc.size-1)).do { |j|
							var pt = arc[j];
							screenX = this.spatialToScreen(pt[\x]);
							screenY = this.ptToScreenY(pt);
							Pen.lineTo(Point(screenX, screenY));
						};
						Pen.stroke;
					};

					// Draw arc number if enabled
					if(showArcNumbers) {
						var labelX = this.spatialToScreen(firstPt[\x]) + 3;
						var labelY = this.ptToScreenY(firstPt) - 10;
						var labelColor = if(isSelected) {
							Color.new(0.8, 0.3, 0, 1)  // Orange for selected
						} {
							Color.hsv((i / data.arcCount.max(1)) * 0.8, 0.8, 0.5)  // Color-coded
						};

						Pen.font = Font("Riforma Mono LL", 9, true);
						Pen.fillColor = labelColor;
						Pen.stringAtPoint(i.asString, Point(labelX, labelY));
					};
				};
			};
		};
	}

	// Helper to get amplitude at a position within an arc
	prGetAmplitudeAt { |ampEnv, relativeX, arcLength|
		var amp = 0.3;
		var xPos = relativeX;

		if(ampEnv.isNil or: { ampEnv.size == 0 }) { ^amp };

		// Scale xPos to match envelope coordinates (envelope uses arc-relative x)
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

	prDrawCurrentArc {
		var pt;
		Pen.strokeColor = Color.black;
		Pen.width = 2;

		pt = currentArc[0];
		Pen.moveTo(Point(this.spatialToScreen(pt[\x]), this.ptToScreenY(pt)));
		(1..currentArc.size-1).do { |j|
			pt = currentArc[j];
			Pen.lineTo(Point(this.spatialToScreen(pt[\x]), this.ptToScreenY(pt)));
		};
		Pen.stroke;
	}

	prDrawPlaybackLine {
		var viewStartTime = zoomTimeMin * playDuration;
		var viewEndTime = zoomTimeMax * playDuration;
		var x;

		// Guard against nil playbackPosition
		if(playbackPosition.isNil) { ^this };

		// Only draw playback position on the page that's actually playing
		if(playback.notNil and: { data.notNil } and: { playback.playbackPageIndex != data.currentPageIndex }) {
			^this
		};

		// Only draw if playback position is within current view
		if(playbackPosition >= viewStartTime and: { playbackPosition <= viewEndTime }) {
			x = playbackPosition.linlin(viewStartTime, viewEndTime, 0, canvasWidth);

			Pen.strokeColor = colors[\playback];
			Pen.width = 2;
			Pen.line(Point(x, 0), Point(x, canvasHeight));
			Pen.stroke;
		};
	}

	prDrawEraseCursor {
		var radius = 6;
		Pen.strokeColor = Color.red;
		Pen.width = 2;
		Pen.strokeOval(Rect(mouseX - radius, mouseY - radius, radius * 2, radius * 2));
	}

	// ========== Mouse Handling ==========

	prHandleMouseDown { |x, y, mods|
		// Check if we're in DRAG playback mode - scrubbing takes priority
		if(isPlaying and: { this.prIsDragPlaybackMode }) {
			isScrubbing = true;
			this.prScrubToScreenX(x);
			canvas.refresh;
			^this;
		};

		if(eraseMode) {
			this.prEraseAt(x, y);
		} {
			if(scissorsMode) {
				// Cut arc at click position
				this.prCutArcAt(x, y);
			} {
				if(selectMode) {
					// Check if clicking on a selected arc - start drag
					if(this.prIsClickOnSelectedArc(x, y)) {
						isDragging = true;
						dragStartX = x;
						dragStartY = y;
						dragLastX = x;
						dragLastY = y;
						// Save undo state before dragging
						data.prSaveUndoState;
					} {
						// Start marquee selection
						isMarqueeSelecting = true;
						marqueeStartX = x;
						marqueeStartY = y;
						marqueeCurrentX = x;
						marqueeCurrentY = y;

						// Clear selection unless holding Shift
						if((mods.bitAnd(131072) == 0) and: { data.notNil }) {
							data.deselectAll;
						};
					};
				} {
					// Start drawing
					isDrawing = true;
					currentArc = List.new;
					this.prAddPointToCurrentArc(x, y);
				};
			};
		};
		canvas.refresh;
	}

	prSelectAt { |x, y, mods|
		var foundIndex;
		var shiftHeld = (mods.bitAnd(131072) > 0);  // Shift modifier

		"prSelectAt called: x=%, y=%, arcs=%".format(x, y, data.arcs.size).postln;

		if(data.isNil) { "data is nil".postln; ^this };

		// Convert screen coords to find arc at this position
		data.arcs.do { |arc, i|
			arc.do { |pt|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				var dist = ((screenX - x).squared + (screenY - y).squared).sqrt;
				if(dist < 30) {  // Increased radius for easier selection
					"Found arc % at dist %".format(i, dist.round(1)).postln;
					foundIndex = i;
				};
			};
		};

		if(foundIndex.notNil) {
			"Selecting arc %".format(foundIndex).postln;
			if(shiftHeld) {
				// Toggle selection with shift
				data.toggleSelect(foundIndex);
			} {
				// Replace selection without shift
				data.deselectAll;
				data.select(foundIndex);
			};
			onSelectionChanged.value(data.selectedArcs);
			this.refresh;  // Force refresh after selection
		} {
			// Click on empty space deselects all
			"No arc found at click position".postln;
			if(shiftHeld.not) {
				data.deselectAll;
				onSelectionChanged.value(data.selectedArcs);
			};
		};
	}

	prHandleMouseMove { |x, y|
		mouseX = x;
		mouseY = y;

		// Handle scrubbing in DRAG playback mode
		if(isScrubbing) {
			this.prScrubToScreenX(x);
			canvas.refresh;
			^this;
		};

		if(isDrawing and: { eraseMode.not }) {
			// Only refresh if a point was actually added
			if(this.prAddPointToCurrentArc(x, y)) {
				canvas.refresh;
			};
		};

		if(isDragging) {
			this.prDragSelectedArcs(x, y);
			dragLastX = x;
			dragLastY = y;
			canvas.refresh;
		};

		if(isMarqueeSelecting) {
			marqueeCurrentX = x;
			marqueeCurrentY = y;
			canvas.refresh;
		};

		if(eraseMode) {
			this.prEraseAt(x, y);
			canvas.refresh;
		};

		if(scissorsMode) {
			this.prUpdateScissorsPreview(x, y);
			canvas.refresh;
		};
	}

	prHandleMouseUp { |x, y|
		// Stop scrubbing
		if(isScrubbing) {
			isScrubbing = false;
			canvas.refresh;
			^this;
		};

		if(isDrawing and: { eraseMode.not }) {
			isDrawing = false;

			if(currentArc.notNil and: { currentArc.size > 1 }) {
				var sortedArc;
				// Sort arc points by X coordinate to handle right-to-left drawing
				sortedArc = currentArc.sort({ |a, b| a[\x] < b[\x] });

				if(sortedArc[0][\x] != currentArc[0][\x]) {
					"Arc was drawn right-to-left - automatically sorted".postln;
				};

				// Add arc to data layer
				if(data.notNil) {
					data.addArc(sortedArc.copy);
				};
				// Notify callback
				onArcDrawn.value(sortedArc.copy);
			};

			currentArc = nil;
			canvas.refresh;
		};

		// Finalize drag
		if(isDragging) {
			isDragging = false;
			canvas.refresh;
		};

		// Finalize marquee selection
		if(isMarqueeSelecting) {
			this.prSelectArcsInMarquee;
			isMarqueeSelecting = false;
			canvas.refresh;
		};
	}

	prSelectArcsInMarquee {
		var left, right, top, bottom;

		if(data.isNil) { ^this };

		// Get marquee bounds (handle any drag direction)
		left = min(marqueeStartX, marqueeCurrentX);
		right = max(marqueeStartX, marqueeCurrentX);
		top = min(marqueeStartY, marqueeCurrentY);
		bottom = max(marqueeStartY, marqueeCurrentY);

		// Find arcs with points inside the marquee
		data.arcs.do { |arc, i|
			arc.do { |pt|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);

				if(screenX >= left and: { screenX <= right } and: {
					screenY >= top } and: { screenY <= bottom }) {
					data.select(i);
				};
			};
		};

		onSelectionChanged.value(data.selectedArcs);
	}

	prIsClickOnSelectedArc { |x, y|
		// Check if click position is on any selected arc
		if(data.isNil or: { data.selectedArcs.size == 0 }) { ^false };

		data.selectedArcs.do { |arcIdx|
			var arc = data.arcs[arcIdx];
			if(arc.notNil) {
				arc.do { |pt|
					var screenX = this.spatialToScreen(pt[\x]);
					var screenY = this.ptToScreenY(pt);
					var dist = ((screenX - x).squared + (screenY - y).squared).sqrt;
					if(dist < 20) {
						^true
					};
				};
			};
		};
		^false
	}

	prDragSelectedArcs { |x, y|
		var deltaScreenX, deltaScreenY;
		var deltaSpatial, deltaYNorm;

		if(data.isNil or: { data.selectedArcs.size == 0 }) { ^this };

		deltaScreenX = x - dragLastX;
		deltaScreenY = y - dragLastY;

		// Convert screen delta to data delta
		deltaSpatial = this.screenToSpatial(x) - this.screenToSpatial(dragLastX);

		// Convert screen Y delta to normalized Y delta (inverted because screen Y is flipped)
		deltaYNorm = this.screenToYNorm(y) - this.screenToYNorm(dragLastY);

		// Move selected arcs
		data.selectedArcs.do { |arcIdx|
			var arc = data.arcs[arcIdx];
			if(arc.notNil) {
				data.arcs[arcIdx] = List.newFrom(arc.collect { |pt|
					var oldY, newY;
					// Handle freq-based arcs (from generators) - freq > 1 means valid Hz
					if(pt[\freq].notNil and: { pt[\freq] > 1 }) {
						// Convert freq (Hz) to normalized y, then apply delta
						oldY = pt[\freq].explin(20, 7500, 0, 1).clip(0, 1);
						newY = (oldY + deltaYNorm).clip(0, 1);
						(x: pt[\x] + deltaSpatial, y: newY)
					} {
						// Standard format - apply delta to normalized y
						newY = ((pt[\y] ?? 0.5) + deltaYNorm).clip(0, 1);
						(x: pt[\x] + deltaSpatial, y: newY)
					}
				});
			};
		};

		data.onDataChanged.value;
	}

	prCutArcAt { |x, y|
		var foundArcIdx, foundPointIdx, closestDist = inf;

		if(data.isNil) { ^this };

		// Find the closest arc point to the click
		data.arcs.do { |arc, arcIdx|
			arc.do { |pt, ptIdx|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				var dist = ((screenX - x).squared + (screenY - y).squared).sqrt;
				if(dist < closestDist and: { dist < 30 }) {
					closestDist = dist;
					foundArcIdx = arcIdx;
					foundPointIdx = ptIdx;
				};
			};
		};

		if(foundArcIdx.notNil and: { foundPointIdx.notNil }) {
			this.prSplitArc(foundArcIdx, foundPointIdx);
		};
	}

	prSplitArc { |arcIdx, pointIdx|
		var arc = data.arcs[arcIdx];
		var leftArc, rightArc;
		var origSynthDef, origWavetable, origWavetableBuffer, origChannelOffset;

		if(arc.isNil or: { arc.size < 3 }) { ^this };  // Need at least 3 points to split
		if(pointIdx < 1 or: { pointIdx >= (arc.size - 1) }) { ^this };  // Can't split at endpoints

		// Save undo state
		data.prSaveUndoState;

		// Get original arc's settings to copy to both halves
		origSynthDef = data.arcSynthDefs[arcIdx];
		origWavetable = data.arcWavetables[arcIdx];
		origWavetableBuffer = data.arcWavetableBuffers[arcIdx];
		origChannelOffset = data.channelOffsets[arcIdx];

		// Create two new arcs from the split
		leftArc = List.newFrom(arc[0..pointIdx]);
		rightArc = List.newFrom(arc[pointIdx..]);

		// Update original arc to be left part
		data.arcs[arcIdx] = leftArc;

		// Insert the right arc after
		data.arcs.insert(arcIdx + 1, rightArc);

		// Copy settings to the new arc (inherit from original)
		data.arcSynthDefs.insert(arcIdx + 1, origSynthDef);
		data.arcWavetables.insert(arcIdx + 1, origWavetable);
		data.arcWavetableBuffers.insert(arcIdx + 1, origWavetableBuffer);
		data.amplitudeEnvelopes.insert(arcIdx + 1, nil);  // New envelope
		data.spatialEnvelopes.insert(arcIdx + 1, nil);     // New envelope
		data.channelOffsets.insert(arcIdx + 1, origChannelOffset);

		// Select only the right (new) part to show the cut visually
		data.deselectAll;
		data.select(arcIdx + 1);

		// Clear scissors preview
		scissorsCutX = nil;
		scissorsCutY = nil;
		scissorsArcIdx = nil;

		data.onDataChanged.value;
		"Arc % split at point % into two arcs".format(arcIdx, pointIdx).postln;
	}

	prUpdateScissorsPreview { |x, y|
		var closestDist = inf;
		var foundX, foundY, foundArcIdx;

		if(data.isNil) {
			scissorsCutX = nil;
			scissorsCutY = nil;
			scissorsArcIdx = nil;
			^this
		};

		// Find the closest arc point to cursor
		data.arcs.do { |arc, arcIdx|
			arc.do { |pt, ptIdx|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				var dist = ((screenX - x).squared + (screenY - y).squared).sqrt;
				// Only allow cutting at interior points (not endpoints)
				if(dist < closestDist and: { dist < 40 } and: { ptIdx > 0 } and: { ptIdx < (arc.size - 1) }) {
					closestDist = dist;
					foundX = screenX;
					foundY = screenY;
					foundArcIdx = arcIdx;
				};
			};
		};

		if(foundX.notNil) {
			scissorsCutX = foundX;
			scissorsCutY = foundY;
			scissorsArcIdx = foundArcIdx;
		} {
			scissorsCutX = nil;
			scissorsCutY = nil;
			scissorsArcIdx = nil;
		};
	}

	prDrawScissorsPreview {
		var dashLen = 8, gapLen = 6;
		var y = 0;
		var arc, leftColor, rightColor;

		// Draw semi-transparent overlay on left and right sides
		Pen.fillColor = Color.new(1, 0.6, 0, 0.15);  // Light orange tint for left
		Pen.fillRect(Rect(0, 0, scissorsCutX, canvasHeight));

		Pen.fillColor = Color.new(0, 0.6, 1, 0.15);  // Light blue tint for right
		Pen.fillRect(Rect(scissorsCutX, 0, canvasWidth - scissorsCutX, canvasHeight));

		// Highlight the arc being cut with different colors on each side
		if(scissorsArcIdx.notNil and: { data.notNil } and: { data.arcs[scissorsArcIdx].notNil }) {
			arc = data.arcs[scissorsArcIdx];
			leftColor = Color.new(1, 0.5, 0);  // Orange for left part
			rightColor = Color.new(0, 0.5, 1);  // Blue for right part

			// Draw left part of arc (before cut)
			Pen.strokeColor = leftColor;
			Pen.width = 4;
			arc.do { |pt, i|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				if(screenX <= scissorsCutX) {
					if(i == 0 or: { this.spatialToScreen(arc[i-1][\x]) > scissorsCutX }) {
						Pen.moveTo(Point(screenX, screenY));
					} {
						Pen.lineTo(Point(screenX, screenY));
					};
				};
			};
			Pen.stroke;

			// Draw right part of arc (after cut)
			Pen.strokeColor = rightColor;
			Pen.width = 4;
			arc.do { |pt, i|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				if(screenX >= scissorsCutX) {
					if(i == 0 or: { this.spatialToScreen(arc[i-1][\x]) < scissorsCutX }) {
						Pen.moveTo(Point(screenX, screenY));
					} {
						Pen.lineTo(Point(screenX, screenY));
					};
				};
			};
			Pen.stroke;
		};

		// Draw thin vertical cut line
		Pen.strokeColor = Color.red;
		Pen.width = 1;
		while { y < canvasHeight } {
			Pen.line(Point(scissorsCutX, y), Point(scissorsCutX, (y + dashLen).min(canvasHeight)));
			Pen.stroke;
			y = y + dashLen + gapLen;
		};

		// Draw small X at cut point
		Pen.strokeColor = Color.red;
		Pen.width = 2;
		Pen.line(Point(scissorsCutX - 5, scissorsCutY - 5), Point(scissorsCutX + 5, scissorsCutY + 5));
		Pen.stroke;
		Pen.line(Point(scissorsCutX - 5, scissorsCutY + 5), Point(scissorsCutX + 5, scissorsCutY - 5));
		Pen.stroke;

		// Draw small filled circle at cut point
		Pen.fillColor = Color.red;
		Pen.addArc(Point(scissorsCutX, scissorsCutY), 4, 0, 2pi);
		Pen.fill;

		// Draw outer ring
		Pen.strokeColor = Color.white;
		Pen.width = 1;
		Pen.addArc(Point(scissorsCutX, scissorsCutY), 7, 0, 2pi);
		Pen.stroke;
	}

	prAddPointToCurrentArc { |x, y|
		var spatial = this.screenToSpatial(x);
		var yNorm = this.screenToYNorm(y);  // Normalized Y (0=bottom, 1=top)
		var point = (x: spatial, y: yNorm);  // Store normalized Y, freq calculated at playback
		var minDist = 3;  // Minimum pixel distance between points

		// Only add point if it's far enough from the last one (reduces point count)
		if(currentArc.size == 0) {
			currentArc.add(point);
			^true  // Point was added
		} {
			var lastPt = currentArc.last;
			var lastX = this.spatialToScreen(lastPt[\x]);
			var lastY = this.yNormToScreen(lastPt[\y]);
			var dist = ((x - lastX).squared + (y - lastY).squared).sqrt;

			if(dist >= minDist) {
				currentArc.add(point);
				^true  // Point was added
			};
		};
		^false  // No point added
	}

	prEraseAt { |x, y|
		var eraseRadius = 6;
		var arcsToProcess = List.new;
		var modified = false;

		if(data.isNil) { ^this };

		// Find which arcs have points within eraser radius
		data.arcs.do { |arc, arcIdx|
			var pointsToKeep = List.new;
			var hasErasedPoints = false;

			arc.do { |pt, ptIdx|
				var screenX = this.spatialToScreen(pt[\x]);
				var screenY = this.ptToScreenY(pt);
				var dist = ((screenX - x).squared + (screenY - y).squared).sqrt;

				if(dist < eraseRadius) {
					hasErasedPoints = true;
				} {
					pointsToKeep.add(pt);
				};
			};

			if(hasErasedPoints) {
				arcsToProcess.add((
					index: arcIdx,
					remainingPoints: pointsToKeep
				));
			};
		};

		// Process arcs that had points erased (in reverse order to maintain indices)
		if(arcsToProcess.size > 0) {
			data.prSaveUndoState;

			arcsToProcess.reverse.do { |item|
				var arcIdx = item[\index];
				var remaining = item[\remainingPoints];

				if(remaining.size < 2) {
					// Too few points left - remove entire arc
					data.removeArc(arcIdx);
				} {
					// Check if we need to split (gap in the middle)
					var segments = this.prFindContiguousSegments(remaining);

					if(segments.size == 1) {
						// Single contiguous segment - just update the arc
						data.arcs[arcIdx] = List.newFrom(remaining);
					} {
						// Multiple segments - remove original and add new arcs for each segment
						var originalSynthDef = data.getSynthDefForArc(arcIdx);
						var originalAmpEnv = data.getAmplitudeEnvelope(arcIdx);
						var originalSpatialEnv = data.getSpatialEnvelope(arcIdx);
						var originalWidthEnv = data.getWidthEnvelope(arcIdx);
						var originalWavetable = data.getWavetableForArc(arcIdx);

						// Remove original arc
						data.removeArc(arcIdx);

						// Add each segment as a new arc (only segments with 2+ points)
						segments.do { |seg|
							if(seg.size >= 2) {
								var newIdx = data.addArc(seg);
								if(newIdx.notNil) {
									data.setSynthDefForArc(newIdx, originalSynthDef);
									if(originalWavetable.notNil) {
										data.setWavetableForArc(newIdx, originalWavetable);
									};
								};
							};
						};
					};
				};

				modified = true;
			};

			if(modified) {
				data.onDataChanged.value;
			};
		};
	}

	// Find contiguous segments in a list of points (based on original drawing order proximity)
	prFindContiguousSegments { |points|
		var segments = List.new;
		var currentSegment = List.new;
		var maxGap = 20;  // Max pixel gap to consider points contiguous

		if(points.size == 0) { ^segments };

		currentSegment.add(points[0]);

		(1..(points.size - 1)).do { |i|
			var prevPt = points[i - 1];
			var currPt = points[i];
			var prevX = this.spatialToScreen(prevPt[\x]);
			var prevY = this.ptToScreenY(prevPt);
			var currX = this.spatialToScreen(currPt[\x]);
			var currY = this.ptToScreenY(currPt);
			var dist = ((currX - prevX).squared + (currY - prevY).squared).sqrt;

			if(dist > maxGap) {
				// Gap found - start new segment
				if(currentSegment.size > 0) {
					segments.add(currentSegment);
				};
				currentSegment = List.new;
			};

			currentSegment.add(currPt);
		};

		// Add final segment
		if(currentSegment.size > 0) {
			segments.add(currentSegment);
		};

		^segments
	}

	// ========== Keyboard Handling ==========

	prHandleKeyDown { |char, mods, unicode, keycode|
		var cmdKey = mods.bitAnd(262144) > 0;  // Cmd on Mac
		var shiftKey = mods.bitAnd(131072) > 0;
		var moveStepX = 0.01;  // 1% of time range
		var moveStepFreq = 1.0594630943593;  // Semitone ratio (2^(1/12))

		// Cmd+Z = Undo
		if(cmdKey and: { char == $z } and: { shiftKey.not }) {
			if(data.notNil) {
				if(data.undo) {
					"Undo".postln;
					this.refresh;
				};
			};
			^true
		};

		// Cmd+Shift+Z = Redo
		if(cmdKey and: { char == $z } and: { shiftKey }) {
			if(data.notNil) {
				if(data.redo) {
					"Redo".postln;
					this.refresh;
				};
			};
			^true
		};

		// Cmd+A = Select All
		if(cmdKey and: { char == $a }) {
			if(data.notNil) {
				data.selectAll;
				"Select All".postln;
				this.refresh;
			};
			^true
		};

		// Cmd+C = Copy
		if(cmdKey and: { char == $c }) {
			if(data.notNil and: { data.selectedCount > 0 }) {
				if(data.copy) {
					"Copied % arc(s)".format(data.selectedCount).postln;
				};
			};
			^true
		};

		// Cmd+V = Paste (to playback page if playing, else to current page)
		if(cmdKey and: { char == $v }) {
			if(data.notNil) {
				var pasted = false;
				if(playback.notNil and: { playback.isPlaying }) {
					// Paste to playback page (allows browsing other pages while playing)
					pasted = data.pasteToPage(playback.playbackPageIndex, 0.02, 0.02);
					if(pasted) { "Pasted arc(s) to playback page".postln };
				} {
					pasted = data.paste;
					if(pasted) { "Pasted arc(s)".postln };
				};
				if(pasted) { this.refresh };
			};
			^true
		};

		// Delete or Backspace = Remove selected arcs
		if(keycode == 51 or: { keycode == 117 }) {  // Backspace=51, Delete=117
			if(data.notNil and: { data.selectedCount > 0 }) {
				var indices = data.selectedArcs.copy;
				data.removeArcs(indices);
				"Deleted % arcs".format(indices.size).postln;
				this.refresh;
			};
			^true
		};

		// Escape = Deselect all, exit modes
		if(keycode == 53) {  // Escape
			if(data.notNil) { data.deselectAll };
			selectMode = false;
			eraseMode = false;
			if(controls[\selectButton].notNil) { { controls[\selectButton].value = 0 }.defer };
			if(controls[\eraseButton].notNil) { { controls[\eraseButton].value = 0 }.defer };
			"Deselect all".postln;
			this.refresh;
			^true
		};

		// Arrow keys = Move selected arcs
		// Left arrow (keycode 123)
		if(keycode == 123) {
			if(data.notNil and: { data.selectedCount > 0 }) {
				data.moveSelected(moveStepX.neg, 1);
				this.refresh;
			};
			^true
		};

		// Right arrow (keycode 124)
		if(keycode == 124) {
			if(data.notNil and: { data.selectedCount > 0 }) {
				data.moveSelected(moveStepX, 1);
				this.refresh;
			};
			^true
		};

		// Up arrow (keycode 126) = Move frequency up
		if(keycode == 126) {
			if(data.notNil and: { data.selectedCount > 0 }) {
				data.moveSelected(0, moveStepFreq);
				this.refresh;
			};
			^true
		};

		// Down arrow (keycode 125) = Move frequency down
		if(keycode == 125) {
			if(data.notNil and: { data.selectedCount > 0 }) {
				data.moveSelected(0, 1/moveStepFreq);
				this.refresh;
			};
			^true
		};

		// Spacebar = Toggle playback
		if(keycode == 49) {  // Space
			if(controls[\playButton].notNil) {
				var newValue = 1 - controls[\playButton].value;
				controls[\playButton].valueAction_(newValue);
			};
			^true
		};

		^false  // Key not handled
	}

	// ========== Range Sliders ==========

	createRangeSliders {
		var canvasX = margin + freqLabelWidth + freqLabelGap;
		var canvasY = margin + pageTabHeight;

		// Vertical (frequency) range slider
		controls[\freqRangeSlider] = RangeSlider(window, Rect(
			canvasX + canvasWidth + rangeSliderGap,
			canvasY,
			rangeSliderSize,
			canvasHeight
		))
		.background_(Color.gray(0.8))
		.knobColor_(Color.green)
		.lo_(0.0)
		.hi_(1.0)
		.orientation_(\vertical)
		.action_({ |sl|
			this.prUpdateFreqZoom(sl.lo, sl.hi);
		});

		// Horizontal (time) range slider
		controls[\timeRangeSlider] = RangeSlider(window, Rect(
			canvasX,
			canvasY + canvasHeight + rangeSliderGap,
			canvasWidth,
			rangeSliderSize
		))
		.background_(Color.gray(0.8))
		.knobColor_(Color.green)
		.lo_(0.0)
		.hi_(1.0)
		.orientation_(\horizontal)
		.action_({ |sl|
			this.prUpdateTimeZoom(sl.lo, sl.hi);
		});
	}

	prUpdateFreqZoom { |lo, hi|
		var minGrabArea = 0.025;
		var range, center;

		// Enforce minimum grab area
		range = hi - lo;
		if(range < minGrabArea) {
			center = (lo + hi) * 0.5;
			lo = (center - (minGrabArea * 0.5)).clip(0, 1 - minGrabArea);
			hi = lo + minGrabArea;

			// Update slider to reflect minimum
			if(controls[\freqRangeSlider].notNil) {
				{
					controls[\freqRangeSlider].lo_(lo);
					controls[\freqRangeSlider].hi_(hi);
				}.defer;
			};
		};

		// Exponential mapping from normalized slider to frequency range
		// This makes zoom feel more natural (equal slider distance = equal pitch distance)
		// Bottom of slider (lo=0) = freqRangeMin (20 Hz)
		// Top of slider (hi=1) = freqRangeMax (7500 Hz)
		zoomFreqMin = lo.linexp(0, 1, freqRangeMin, freqRangeMax);
		zoomFreqMax = hi.linexp(0, 1, freqRangeMin, freqRangeMax);

		this.refresh;
	}

	prUpdateTimeZoom { |lo, hi|
		var minGrabArea = 0.025;
		var range, center;

		// Enforce minimum grab area
		range = hi - lo;
		if(range < minGrabArea) {
			center = (lo + hi) * 0.5;
			lo = (center - (minGrabArea * 0.5)).clip(0, 1 - minGrabArea);
			hi = lo + minGrabArea;

			// Update slider to reflect minimum
			if(controls[\timeRangeSlider].notNil) {
				{
					controls[\timeRangeSlider].lo_(lo);
					controls[\timeRangeSlider].hi_(hi);
				}.defer;
			};
		};

		zoomTimeMin = lo;
		zoomTimeMax = hi;
		this.refresh;
	}

	// ========== Control Panel ==========

	createControlPanel {
		var yStart = height - margin;
		var groupX = margin;  // Flush with frequency labels on left side
		var groupH = 30;
		var btnH = 20;
		var gap = 3;

		// ===== PLAYBACK GROUP (green tint) =====
		this.prCreatePlaybackGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 550 + 5;  // Smaller play btn, longer 200px vol slider

		// ===== MODE/ACTION GROUP (light red tint) =====
		this.prCreateModeGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 416 + 5;  // select, erase, scissors, eraseAll, cp, ps, rev, inv, arcs, #

		// ===== TRANSFORM GROUP (light blue) =====
		this.prCreateTransformGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 148 + 5;

		// ===== SYNTH GROUP (white) - grid controls moved to _scale editor =====
		this.prCreateScaleSynthGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 245 + 5;  // _synth + menu + _wt

		// ===== AMPLITUDE EDIT GROUP (light orange) =====
		this.prCreateAmpEditGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 50 + 5;

		// ===== WIDTH EDIT GROUP (light orange, for VarSaw) =====
		this.prCreateWidthEditGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 55 + 5;

		// ===== SPATIAL EDIT GROUP (light magenta) =====
		this.prCreateSpatialEditGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 103 + 5;  // _spat + _ana buttons (_freq moved to _scale above labels)

		// Edit WT button is now part of Scale/Synth group

		// ===== HELP GROUP (light purple) =====
		this.prCreateHelpGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 60 + 5;

		// ===== PRESETS GROUP (light blue) =====
		this.prCreatePresetsGroup(groupX, yStart, groupH, btnH, gap);
		groupX = groupX + 209 + 5;

		// ===== SERVER GROUP (light yellow) =====
		this.prCreateServerGroup(groupX, yStart, groupH, btnH, gap);
	}

	prDrawGroupBorder { |view, bgColor|
		Pen.fillColor = bgColor.alpha_(0.4);
		Pen.fillRect(view.bounds.moveTo(0, 0));
		Pen.strokeColor = Color.white;
		Pen.width = 2;
		Pen.strokeRect(view.bounds.moveTo(0, 0));
	}

	prCreatePlaybackGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 55 + gap + 50 + gap + 115 + gap + 50 + gap + 25 + gap + 30 + gap + 200;  // Smaller play btn, longer vol slider
		var groupW = contentW + 10;
		var borderView, groupView;
		var ctrlX = 0;
		var bkGreen = Color.new(205/255, 250/255, 205/255);

		// Border view with green background
		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, bkGreen) };

		// Composite view for controls
		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Play button
		controls[\playButton] = Button(groupView, Rect(ctrlX, 0, 55, btnH))
		.states_([
			["_play", Color.white, Color.green],
			["_stop", Color.white, Color.red]
		])
		.font_(Font("Riforma Mono LL", 10, true))
		.action_({ |btn| onPlaybackToggle.value(btn.value == 1) });
		ctrlX = ctrlX + 55 + gap;

		// Direction menu
		controls[\directionMenu] = PopUpMenu(groupView, Rect(ctrlX, 0, 50, btnH))
		.items_(["FWD", "REV", "PAL", "LOOP", "DRAG"])
		.font_(Font("Riforma Mono LL", 9, true))
		.value_(0);
		ctrlX = ctrlX + 50 + gap;

		// Duration label
		StaticText(groupView, Rect(ctrlX, 0, 115, btnH))
		.string_("_total duration")
		.font_(Font("Riforma Mono LL", 10))
		.align_(\center)
		.background_(Color.white);
		ctrlX = ctrlX + 115 + gap;

		// Duration numberbox
		controls[\durationBox] = NumberBox(groupView, Rect(ctrlX, 0, 50, btnH))
		.value_(playDuration)
		.decimals_(1)
		.clipLo_(0.1)
		.clipHi_(1200)
		.font_(Font("Riforma Mono LL", 11))
		.action_({ |nb| playDuration = nb.value });
		ctrlX = ctrlX + 50 + gap;

		// Seconds label
		StaticText(groupView, Rect(ctrlX, 0, 25, btnH))
		.string_("sec")
		.font_(Font("Riforma Mono LL", 11))
		.stringColor_(Color.gray(0.6));
		ctrlX = ctrlX + 30;

		// Master volume label
		StaticText(groupView, Rect(ctrlX, 0, 30, btnH))
		.string_("_vol")
		.font_(Font("Riforma Mono LL", 10))
		.stringColor_(Color.gray(0.4));
		ctrlX = ctrlX + 32;

		// Master volume slider (18px tall to match arc volume sliders)
		controls[\masterVolSlider] = Slider(groupView, Rect(ctrlX, 1, 200, 18))
		.value_(0.3)  // Default masterAmp
		.knobColor_(Color.gray(0.4))
		.action_({ |sl|
			if(playback.notNil) {
				playback.setMasterAmp(sl.value);
			};
		});
	}

	prCreateModeGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 50 + gap + 35 + gap + 50 + gap + 50 + gap + 55 + gap + 28 + gap + 28 + gap + 30 + gap + 28 + gap + 35 + gap + 25;  // select, selAll, erase, scissors, eraseAll, cp, ps, rev, inv, arcs, #
		var groupW = contentW + 10;
		var borderView, groupView;
		var ctrlX = 0;
		var lightRed = Color.new(250/255, 205/255, 205/255);

		// Border view with light red background
		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightRed) };

		// Composite view for controls
		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Select button
		controls[\selectButton] = Button(groupView, Rect(ctrlX, 0, 50, btnH))
		.states_([
			["_select", Color.black, Color.gray(0.9)],
			["_select", Color.white, Color.blue]
		])
		.font_(Font("Riforma Mono LL", 9))
		.action_({ |btn|
			selectMode = btn.value == 1;
			if(selectMode) {
				eraseMode = false; controls[\eraseButton].value = 0;
				scissorsMode = false; controls[\scissorsButton].value = 0;
			};
		});
		ctrlX = ctrlX + 50 + gap;

		// Select All button
		controls[\selectAllButton] = Button(groupView, Rect(ctrlX, 0, 35, btnH))
		.states_([["_selAll", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) { data.selectAll };
			// Switch to select mode
			selectMode = true;
			eraseMode = false;
			scissorsMode = false;
			controls[\selectButton].value = 1;
			controls[\eraseButton].value = 0;
			controls[\scissorsButton].value = 0;
			this.refresh;
		});
		ctrlX = ctrlX + 35 + gap;

		// Eraser button
		controls[\eraseButton] = Button(groupView, Rect(ctrlX, 0, 50, btnH))
		.states_([
			["_eraser", Color.black, Color.gray(0.9)],
			["_erasing", Color.white, Color.red]
		])
		.font_(Font("Riforma Mono LL", 9))
		.action_({ |btn|
			eraseMode = btn.value == 1;
			if(eraseMode) {
				selectMode = false; controls[\selectButton].value = 0;
				scissorsMode = false; controls[\scissorsButton].value = 0;
			};
			canvas.refresh;
		});
		ctrlX = ctrlX + 50 + gap;

		// Scissors button
		controls[\scissorsButton] = Button(groupView, Rect(ctrlX, 0, 50, btnH))
		.states_([
			["_scissors", Color.black, Color.gray(0.9)],
			["_cutting", Color.white, Color.new(0.8, 0.5, 0)]  // Orange
		])
		.font_(Font("Riforma Mono LL", 9))
		.action_({ |btn|
			scissorsMode = btn.value == 1;
			if(scissorsMode) {
				selectMode = false; controls[\selectButton].value = 0;
				eraseMode = false; controls[\eraseButton].value = 0;
			};
			canvas.refresh;
		});
		ctrlX = ctrlX + 50 + gap;

		// EraseAll button
		controls[\eraseAllButton] = Button(groupView, Rect(ctrlX, 0, 55, btnH))
		.states_([["_eraseAll", Color.white, Color.red.alpha_(0.7)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) { data.clearAll };
			this.refresh;
		});
		ctrlX = ctrlX + 55 + gap;

		// Copy button
		controls[\copyButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_cp", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) { data.copy };
		});
		ctrlX = ctrlX + 28 + gap;

		// Paste button
		controls[\pasteButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_ps", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) {
				// Paste to playback page if playing, else to current page
				if(playback.notNil and: { playback.isPlaying }) {
					data.pasteToPage(playback.playbackPageIndex, 0.02, 0);
				} {
					data.paste(0.02, 0);
				};
			};
			this.refresh;
		});
		ctrlX = ctrlX + 28 + gap;

		// Reverse button
		controls[\reverseButton] = Button(groupView, Rect(ctrlX, 0, 30, btnH))
		.states_([["_rev", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) { data.reverseSelected };
			this.refresh;
		});
		ctrlX = ctrlX + 30 + gap;

		// Inverse button
		controls[\inverseButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_inv", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			if(data.notNil) { data.inverseSelected };
			this.refresh;
		});
		ctrlX = ctrlX + 28 + gap;

		// Arcs list button
		controls[\arcsListButton] = Button(groupView, Rect(ctrlX, 0, 35, btnH))
		.states_([["_arcs", Color.black, Color.new(180/255, 230/255, 180/255)]])  // B&K green
		.font_(Font("Riforma Mono LL", 8))
		.action_({
			this.prOpenArcsListView;
		});
		ctrlX = ctrlX + 35 + gap;

		// Arc numbers toggle button
		controls[\arcNumbersButton] = Button(groupView, Rect(ctrlX, 0, 25, btnH))
		.states_([
			["_#", Color.black, Color.gray(0.9)],
			["_#", Color.white, Color.new(0.2, 0.6, 0.2)]  // Green when active
		])
		.font_(Font("Riforma Mono LL", 8))
		.value_(if(showArcNumbers, 1, 0))
		.action_({ |btn|
			showArcNumbers = btn.value == 1;
			this.refresh;
		});
	}

	prCreateScaleSynthGroup { |x, yStart, groupH, btnH, gap|
		// Grid controls (_scl, _grid on/off) moved to _scale editor
		var contentW = 50 + 2 + 150 + 5 + 28;  // _synth + menu + _wt
		var groupW = contentW + 10;
		var borderView, groupView;
		var ctrlX = 0;

		// Border view with white background
		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, Color.white) };

		// Composite view for controls
		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Synth label
		StaticText(groupView, Rect(ctrlX, 0, 50, btnH))
		.string_("_synth")
		.font_(Font("Riforma Mono LL", 9))
		.background_(Color.white)
		.align_(\center);
		ctrlX = ctrlX + 52;

		// SynthDef menu - comprehensive list matching original system
		controls[\synthDefMenu] = PopUpMenu(groupView, Rect(ctrlX, 0, 150, btnH))
		.items_(this.prGetSynthMenuItems)
		.font_(Font("Riforma Mono LL", 9))
		.value_(0)
		.action_({ |menu|
			var selectedSynth = this.prGetSynthDefNames[menu.value];

			if(data.notNil) {
				// Always set as new default for future arcs
				data.defaultSynthDef = selectedSynth;

				// Also apply to selected arcs if any
				if(data.selectedCount > 0) {
					data.setSynthDefForSelected(selectedSynth);
					"Applied % to % selected arcs (now default)".format(
						selectedSynth, data.selectedCount
					).postln;
				} {
					"Set default synthdef to %".format(selectedSynth).postln;
				};
			};

			// Update edit WT button visibility
			this.prUpdateEditWTVisibility;
			// Update edit Spatial button visibility
			this.prUpdateEditSpatialVisibility;
			// Update edit Width button visibility
			this.prUpdateEditWidthVisibility;
		});
		ctrlX = ctrlX + 155;

		// Edit Wavetable button - only visible for wavetable-type synthdefs
		controls[\editWTButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_wt", Color.white, Color.green.alpha_(0.8)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prOpenWavetableEditor;
		});
	}

	prGetSynthMenuItems {
		var sawName = if(useOversampling) { "SawBL" } { "Saw" };
		var varSawName = if(useOversampling) { "VarSawOS" } { "VarSaw" };

		if(useMultichannel) {
			^[
				// UPIC Wavetable variants
				"UPIC Wavetable (Mono)",
				"UPIC Wavetable (2ch)",
				"UPIC Wavetable (3ch)",
				"UPIC Wavetable (4ch)",
				"UPIC Wavetable (8ch)",
				"UPIC Wavetable (12ch)",
				"UPIC Wavetable (15ch)",
				"UPIC Wavetable (24ch)",
				// Saw/SawBL variants
				sawName ++ " (Mono)",
				sawName ++ " (2ch)",
				sawName ++ " (3ch)",
				sawName ++ " (4ch)",
				sawName ++ " (8ch)",
				sawName ++ " (12ch)",
				sawName ++ " (15ch)",
				sawName ++ " (24ch)",
				// VarSaw/VarSawOS variants
				varSawName ++ " (Mono)",
				varSawName ++ " (2ch)",
				varSawName ++ " (3ch)",
				varSawName ++ " (4ch)",
				varSawName ++ " (8ch)",
				varSawName ++ " (12ch)",
				varSawName ++ " (15ch)",
				varSawName ++ " (24ch)"
			]
		} {
			// Stereo mode: only Mono and 2ch variants
			^[
				"UPIC Wavetable (Mono)",
				"UPIC Wavetable (2ch)",
				sawName ++ " (Mono)",
				sawName ++ " (2ch)",
				varSawName ++ " (Mono)",
				varSawName ++ " (2ch)"
			]
		}
	}

	prGetSynthDefNames {
		if(useMultichannel) {
			^[
				// Wavetable variants
				\upicWavetable,
				\upicWavetable2ch,
				\upicWavetable3ch,
				\upicWavetable4ch,
				\upicWavetable8ch,
				\upicWavetable12ch,
				\upicWavetable15ch,
				\upicWavetable24ch,
				// Saw/SawBL variants
				\upicSawBL,
				\upicSawBL2ch,
				\upicSawBL3ch,
				\upicSawBL4ch,
				\upicSawBL8ch,
				\upicSawBL12ch,
				\upicSawBL15ch,
				\upicSawBL24ch,
				// VarSaw/VarSawOS variants
				\upicVarSaw,
				\upicVarSaw2ch,
				\upicVarSaw3ch,
				\upicVarSaw4ch,
				\upicVarSaw8ch,
				\upicVarSaw12ch,
				\upicVarSaw15ch,
				\upicVarSaw24ch
			]
		} {
			// Stereo mode: only Mono and 2ch variants
			^[
				\upicWavetable,
				\upicWavetable2ch,
				\upicSawBL,
				\upicSawBL2ch,
				\upicVarSaw,
				\upicVarSaw2ch
			]
		}
	}

	prUpdateEditWTVisibility {
		// Hide edit WT button when non-wavetable synthdef is selected
		var synthDefName, isWavetable;

		if(controls[\editWTButton].notNil and: { controls[\synthDefMenu].notNil }) {
			synthDefName = this.prGetSynthDefNames[controls[\synthDefMenu].value];
			isWavetable = synthDefName.asString.contains("Wavetable");
			controls[\editWTButton].visible = isWavetable;
		};
	}

	prUpdateEditSpatialVisibility {
		// Always show spatial button - mono synths get channel selector, others get spatial editor
		if(controls[\editSpatialButton].notNil) {
			controls[\editSpatialButton].visible = true;
			// Update button label: 0 = _spat (multi-channel), 1 = _ch (mono)
			controls[\editSpatialButton].value = if(this.prIsMonoSynthSelected) { 1 } { 0 };
		};
	}

	prIsMonoSynthSelected {
		var synthDefName;
		if(controls[\synthDefMenu].notNil) {
			synthDefName = this.prGetSynthDefNames[controls[\synthDefMenu].value];
			// Mono synths don't have a channel suffix (no "2ch", "3ch", etc.)
			^(synthDefName == \upicWavetable) or: { synthDefName == \upicSawBL } or: { synthDefName == \upicVarSaw };
		};
		^false
	}

	prUpdateEditWidthVisibility {
		// Show edit Width button only for VarSaw synthdefs
		var synthDefName, isVarSaw;

		if(controls[\editWidthButton].notNil and: { controls[\synthDefMenu].notNil }) {
			synthDefName = this.prGetSynthDefNames[controls[\synthDefMenu].value];
			isVarSaw = synthDefName.asString.contains("VarSaw");
			controls[\editWidthButton].visible = isVarSaw;
		};
	}

	// Update UI to reflect the selected arc's synth settings
	updateUIForSelection {
		var selectedSynth, menuIndex;
		var synthDefNames = this.prGetSynthDefNames;

		if(data.isNil or: { controls[\synthDefMenu].isNil }) { ^this };

		// Update arcs list view if open
		if(arcsListView.notNil) {
			arcsListView.updateForSelection;
		};

		// If exactly one arc is selected, show its synth
		if(data.selectedCount == 1) {
			var arcIdx = data.selectedArcs.asArray.first;
			selectedSynth = data.getSynthDefForArc(arcIdx);
		} {
			// Multiple arcs selected - check if they all have the same synth
			if(data.selectedCount > 1) {
				var synths = data.selectedArcs.asArray.collect { |idx|
					data.getSynthDefForArc(idx)
				};
				var uniqueSynths = synths.asSet;
				if(uniqueSynths.size == 1) {
					selectedSynth = synths.first;
				} {
					// Mixed synths - don't change the menu, multi-arc editor handles it
					^this
				};
			} {
				// No selection - don't change the menu
				^this
			};
		};

		// Find menu index for this synth dynamically
		menuIndex = synthDefNames.indexOf(selectedSynth.asSymbol);
		if(menuIndex.notNil) {
			{
				controls[\synthDefMenu].value = menuIndex;
				this.prUpdateEditWTVisibility;
				this.prUpdateEditSpatialVisibility;
				this.prUpdateEditWidthVisibility;
			}.defer;
		};

		// Update all open editors when selection changes
		if(NUPICAmplitudeEditor.current.notNil) {
			NUPICAmplitudeEditor.current.updateForSelection;
		};
		if(NUPICSpatialEditor.current.notNil) {
			NUPICSpatialEditor.current.updateForSelection;
		};
		if(NUPICWidthEditor.current.notNil) {
			NUPICWidthEditor.current.updateForSelection;
		};
		if(NUPICWavetableEditor.current.notNil) {
			NUPICWavetableEditor.current.updateForSelection;
		};
	}

	prOpenWavetableEditor { |arcIndex = nil|
		// Create wavetable editor if needed, or update data reference
		if(wavetableEditor.isNil) {
			wavetableEditor = NUPICWavetableEditor(data, this);
		} {
			// Update data reference in case it changed
			wavetableEditor.data = data;
			wavetableEditor.gui = this;
		};

		// Open the editor (it handles validation internally)
		wavetableEditor.open(arcIndex);
	}

	prCreateTransformGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 28 + gap + 28 + 10 + 28 + gap + 28;
		var groupW = contentW + 10;
		var borderView, groupView;
		var lightBlue = Color.new(205/255, 220/255, 250/255);

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightBlue) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Time compress ><
		controls[\timeCompressBtn] = Button(groupView, Rect(0, 0, 28, btnH))
		.states_([["><", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 10, true))
		.action_({
			if(data.notNil) { data.timeCompressSelected(0.5) };
			this.refresh;
		});

		// Time expand <>
		controls[\timeExpandBtn] = Button(groupView, Rect(31, 0, 28, btnH))
		.states_([["<>", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 10, true))
		.action_({
			if(data.notNil) { data.timeExpandSelected(2.0) };
			this.refresh;
		});

		// Freq compress ^v
		controls[\freqCompressBtn] = Button(groupView, Rect(69, 0, 28, btnH))
		.states_([["^v", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 10, true))
		.action_({
			if(data.notNil) { data.freqCompressSelected(0.5) };
			this.refresh;
		});

		// Freq expand v^
		controls[\freqExpandBtn] = Button(groupView, Rect(100, 0, 28, btnH))
		.states_([["v^", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 10, true))
		.action_({
			if(data.notNil) { data.freqExpandSelected(2.0) };
			this.refresh;
		});
	}

	prCreateAmpEditGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 40;
		var groupW = contentW + 10;
		var borderView, groupView;
		var lightOrange = Color.new(255/255, 245/255, 220/255);

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightOrange) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		controls[\editAmpButton] = Button(groupView, Rect(0, 0, 40, btnH))
		.states_([["_amp", Color.white, Color.blue]])
		.font_(Font("Riforma Mono LL", 9, true))
		.action_({
			this.openAmplitudeEditor;
		});
	}

	openAmplitudeEditor { |arcIndex = nil|
		// Create or reuse amplitude editor
		if(amplitudeEditor.isNil) {
			amplitudeEditor = NUPICAmplitudeEditor(data, this);
		} {
			// Update references in case they changed
			amplitudeEditor.data = data;
			amplitudeEditor.gui = this;
		};

		// Open the editor
		amplitudeEditor.open(arcIndex);
	}

	openGroupAmplitudeEditor {
		// Create or reuse group amplitude editor
		if(groupAmplitudeEditor.isNil) {
			groupAmplitudeEditor = NUPICGroupAmplitudeEditor(data, this);
		} {
			// Update references in case they changed
			groupAmplitudeEditor.data = data;
			groupAmplitudeEditor.gui = this;
		};

		// Open the editor for selected arcs
		groupAmplitudeEditor.open;
	}

	prCreateWidthEditGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 45;
		var groupW = contentW + 10;
		var borderView, groupView;
		var lightOrange = Color.new(255/255, 235/255, 210/255);  // Slightly different orange

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightOrange) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		controls[\editWidthButton] = Button(groupView, Rect(0, 0, 45, btnH))
		.states_([["_width", Color.white, Color.new(0.9, 0.5, 0.0)]])  // Orange
		.font_(Font("Riforma Mono LL", 9, true))
		.visible_(false)  // Hidden by default, shown only for VarSaw synths
		.action_({
			this.openWidthEditor;
		});
	}

	openWidthEditor { |arcIndex = nil|
		// Create or reuse width editor
		if(widthEditor.isNil) {
			widthEditor = NUPICWidthEditor(data, this);
		} {
			// Update references in case they changed
			widthEditor.data = data;
			widthEditor.gui = this;
		};

		// Open the editor
		widthEditor.open(arcIndex);
	}

	prOpenArcsListView {
		// Create or reuse arcs list view
		if(arcsListView.isNil) {
			arcsListView = NUPICArcsListView(data, this);
		} {
			// Update references in case they changed
			arcsListView.data = data;
			arcsListView.gui = this;
		};

		// Open the view
		arcsListView.open;
	}

	prCreateSpatialEditGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 50 + gap + 40;  // _spat + _ana (removed _freq - now _scale button above labels)
		var groupW = contentW + 10;
		var borderView, groupView;
		var lightMagenta = Color.new(255/255, 220/255, 255/255);
		var ctrlX = 0;

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightMagenta) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		controls[\editSpatialButton] = Button(groupView, Rect(ctrlX, 0, 50, btnH))
		.states_([
			["_spat", Color.white, Color.magenta.blend(Color.gray, 0.3)],
			["_ch", Color.white, Color.new(0.3, 0.5, 0.8)]  // Blue tint for channel
		])
		.font_(Font("Riforma Mono LL", 9, true))
		.visible_(true)
		.action_({
			if(this.prIsMonoSynthSelected) {
				this.openMonoChannelSelector;
			} {
				this.openSpatialEditor;
			};
		});
		ctrlX = ctrlX + 50 + gap;

		// Audio analyzer button
		controls[\analyzeButton] = Button(groupView, Rect(ctrlX, 0, 40, btnH))
		.states_([["_ana", Color.white, Color.new(0.3, 0.5, 0.7)]])
		.font_(Font("Riforma Mono LL", 9, true))
		.action_({
			this.openAnalyzer;
		});
	}

	openSpatialEditor { |arcIndex = nil|
		// Create or reuse spatial editor
		if(spatialEditor.isNil) {
			spatialEditor = NUPICSpatialEditor(data, this);
		} {
			// Update references in case they changed
			spatialEditor.data = data;
			spatialEditor.gui = this;
		};

		// Open the editor
		spatialEditor.open(arcIndex);
	}

	openMonoChannelSelector { |arcIndex = nil|
		var win, channelBox, selectedIndices, currentChannel;

		// Use provided arcIndex or fall back to selected arcs
		if(arcIndex.notNil) {
			selectedIndices = [arcIndex];
		} {
			// Get selected arc indices from data.selectedArcs
			if(data.selectedArcs.isNil or: { data.selectedArcs.size == 0 }) {
				"No arcs selected".postln;
				^this
			};
			selectedIndices = data.selectedArcs.asArray;
		};

		// Get current channel from first selected arc (default to 0)
		currentChannel = data.getChannelOffset(selectedIndices[0]);

		// Create a small window
		win = Window("Mono Output Channel", Rect(400, 400, 200, 80))
		.front
		.alwaysOnTop_(true);

		StaticText(win, Rect(10, 10, 180, 20))
		.string_("Output Channel:")
		.font_(Font("Riforma Mono LL", 11));

		channelBox = NumberBox(win, Rect(10, 35, 80, 25))
		.value_(currentChannel)
		.clipLo_(0)
		.clipHi_(127)
		.step_(1)
		.scroll_step_(1)
		.font_(Font("Riforma Mono LL", 12))
		.action_({ |nb|
			var channel = nb.value.asInteger;
			// Update all selected arcs using channelOffsets
			selectedIndices.do { |idx|
				data.setChannelOffset(idx, channel);
			};
			data.setChanged;
			"Set output channel to % for % arc(s)".format(channel, selectedIndices.size).postln;
		});
	}

	openFrequencyTableEditor {
		// Create or reuse frequency table editor
		if(frequencyTableEditor.isNil) {
			frequencyTableEditor = NUPICFrequencyTableEditor(data, this);
		} {
			// Update references in case they changed
			frequencyTableEditor.data = data;
			frequencyTableEditor.gui = this;
		};

		// Open the editor
		frequencyTableEditor.open;
	}

	prCreateHelpGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 50;
		var groupW = contentW + 10;
		var borderView, groupView;
		var lightPurple = Color.new(220/255, 220/255, 255/255);

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightPurple) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		controls[\groupAmpButton] = Button(groupView, Rect(0, 0, 50, btnH))
		.states_([["_ampGrp", Color.white, Color.blue.blend(Color.cyan, 0.3)]])
		.font_(Font("Riforma Mono LL", 9, true))
		.action_({
			this.openGroupAmplitudeEditor;
		});
	}

	prCreatePresetsGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 100 + gap + 28 + gap + 28 + gap + 28;  // Compact buttons: SV, LD, RW
		var groupW = contentW + 10;
		var borderView, groupView;
		var ctrlX = 0;
		var lightBlue = Color.new(220/255, 230/255, 255/255);

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightBlue) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Preset menu - starts empty, populated by refreshPresetMenu
		controls[\presetMenu] = PopUpMenu(groupView, Rect(ctrlX, 0, 100, btnH))
		.items_(["(no presets)"])
		.font_(Font("Riforma Mono LL", 9))
		.value_(0);
		ctrlX = ctrlX + 100 + gap;

		// Save button - saves current page with auto-generated name
		controls[\presetSaveButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_SV", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			if(data.notNil) {
				data.savePreset(nil, playDuration);
				this.refreshPresetMenu;
			};
		});
		ctrlX = ctrlX + 28 + gap;

		// Load button - loads selected preset
		controls[\presetLoadButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_LD", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var menu = controls[\presetMenu];
			var presets = data.listPresets;
			if(data.notNil and: { presets.size > 0 }) {
				var selectedPreset = presets[menu.value];
				if(selectedPreset.notNil) {
					var result = data.loadPreset(selectedPreset, Server.default);
					if(result.success) {
						// Restore page duration from preset
						if(result.duration.notNil) {
							this.setPlayDuration(result.duration);
						};
						this.refresh;
					};
				};
			};
		});
		ctrlX = ctrlX + 28 + gap;

		// Rewrite button - overwrites selected preset with current data
		controls[\presetRewriteButton] = Button(groupView, Rect(ctrlX, 0, 28, btnH))
		.states_([["_RW", Color.black, Color.gray(0.9)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var menu = controls[\presetMenu];
			var presets = data.listPresets;
			if(data.notNil and: { presets.size > 0 }) {
				var selectedPreset = presets[menu.value];
				if(selectedPreset.notNil) {
					data.rewritePreset(selectedPreset, playDuration);
					"Preset overwritten: %".format(selectedPreset).postln;
				};
			};
		});

		// Initial menu population
		this.refreshPresetMenu;
	}

	refreshPresetMenu {
		var menu = controls[\presetMenu];
		var presets;

		if(menu.notNil and: { data.notNil }) {
			presets = data.listPresets;
			if(presets.size > 0) {
				menu.items_(presets);
				menu.value_(0);
			} {
				menu.items_(["(no presets)"]);
				menu.value_(0);
			};
		};
	}

	prCreateServerGroup { |x, yStart, groupH, btnH, gap|
		var contentW = 45 + gap + 45;
		var groupW = contentW + 10;
		var borderView, groupView;
		var ctrlX = 0;
		var lightYellow = Color.new(255/255, 255/255, 220/255);

		borderView = UserView(window, Rect(x, yStart, groupW, groupH));
		borderView.drawFunc = { this.prDrawGroupBorder(borderView, lightYellow) };

		groupView = CompositeView(borderView, Rect(5, 5, contentW, btnH));

		// Meter button
		controls[\meterButton] = Button(groupView, Rect(ctrlX, 0, 45, btnH))
		.states_([["_meter", Color.white, Color.green.alpha_(0.8)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			var s = Server.default;
			var numIns = s.options.numInputBusChannels;
			var numOuts = s.options.numOutputBusChannels;
			// Width: each channel ~20px, height: 220
			var w = Window("Meter", Rect(0, 0, (numIns + numOuts) * 20 + 50, 220));
			ServerMeterView(s, w, 0@0, numIns, numOuts);
			w.front;
		});
		ctrlX = ctrlX + 45 + gap;

		// Server button
		controls[\serverButton] = Button(groupView, Rect(ctrlX, 0, 45, btnH))
		.states_([["_server", Color.white, Color.gray(0.6)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			Server.default.makeWindow;
		});
	}

	// ========== Public Setters ==========

	setGridType { |type|
		var index;
		gridType = type;

		// Update menu if it exists
		index = gridTypes.indexOf(type);
		if(index.notNil and: { controls[\scaleMenu].notNil }) {
			{ controls[\scaleMenu].value = index }.defer;
		};

		this.refresh;
	}

	setFreqZoom { |minFreq, maxFreq|
		zoomFreqMin = minFreq.clip(freqRangeMin, freqRangeMax);
		zoomFreqMax = maxFreq.clip(freqRangeMin, freqRangeMax);

		// Update slider if it exists (use explin for logarithmic mapping)
		if(controls[\freqRangeSlider].notNil) {
			var lo = zoomFreqMin.explin(freqRangeMin, freqRangeMax, 0, 1);
			var hi = zoomFreqMax.explin(freqRangeMin, freqRangeMax, 0, 1);
			{
				controls[\freqRangeSlider].lo_(lo);
				controls[\freqRangeSlider].hi_(hi);
			}.defer;
		};

		this.refresh;
	}

	setTimeZoom { |minNorm, maxNorm|
		zoomTimeMin = minNorm.clip(0, 1);
		zoomTimeMax = maxNorm.clip(0, 1);

		// Update slider if it exists
		if(controls[\timeRangeSlider].notNil) {
			{
				controls[\timeRangeSlider].lo_(zoomTimeMin);
				controls[\timeRangeSlider].hi_(zoomTimeMax);
			}.defer;
		};

		this.refresh;
	}

	resetZoom {
		// Reset to full view
		zoomFreqMin = freqRangeMin;
		zoomFreqMax = freqRangeMax;
		zoomTimeMin = 0;
		zoomTimeMax = 1;

		// Update sliders
		if(controls[\freqRangeSlider].notNil) {
			{ controls[\freqRangeSlider].lo_(0).hi_(1) }.defer;
		};
		if(controls[\timeRangeSlider].notNil) {
			{ controls[\timeRangeSlider].lo_(0).hi_(1) }.defer;
		};

		this.refresh;
	}

	// ========== Audio Analyzer ==========

	openAnalyzer {
		// Open or bring to front the audio analyzer GUI
		analyzerGUI = NUPICAnalyzerGUI.open(data, this);
	}

	// ========== Coordinate Conversion ==========

	screenToFreq { |y|
		// y=0 is top (high freq), y=canvasHeight is bottom (low freq)
		// Normalize to 0-1 where 0=bottom, 1=top
		var normalized = 1 - (y / canvasHeight);
		var fMin, fMax;

		// Use page's frequency range when table is enabled
		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			fMin = data.getFrequencyRangeMin;
			fMax = data.getFrequencyRangeMax;
			^data.yToFreqWithTable(normalized, fMin, fMax)
		};

		^normalized.linexp(0, 1, zoomFreqMin, zoomFreqMax)
	}

	freqToScreen { |freq|
		// Convert frequency to screen y coordinate
		// Always use fast exponential mapping for display
		// Frequency table only affects screenToFreq (drawing new arcs)
		var fMin, fMax, normalized;

		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			// Use page's frequency range for display bounds
			fMin = data.getFrequencyRangeMin;
			fMax = data.getFrequencyRangeMax;
		} {
			fMin = zoomFreqMin;
			fMax = zoomFreqMax;
		};

		normalized = freq.explin(fMin, fMax, 0, 1);
		^(1 - normalized) * canvasHeight
	}

	screenToTime { |x|
		^x.linlin(0, canvasWidth, zoomTimeMin * playDuration, zoomTimeMax * playDuration)
	}

	timeToScreen { |time|
		^time.linlin(zoomTimeMin * playDuration, zoomTimeMax * playDuration, 0, canvasWidth)
	}

	// Convert screen X to normalized spatial position (0-1)
	screenToSpatial { |x|
		^x.linlin(0, canvasWidth, zoomTimeMin, zoomTimeMax)
	}

	// Convert normalized spatial position (0-1) to screen X
	spatialToScreen { |spatial|
		^spatial.linlin(zoomTimeMin, zoomTimeMax, 0, canvasWidth)
	}

	// Convert screen Y to normalized Y position (0=bottom/low freq, 1=top/high freq)
	// Accounts for vertical zoom
	screenToYNorm { |screenY|
		var zoomYMin, zoomYMax, screenNorm, result;

		// Convert frequency zoom bounds to normalized Y (0-1)
		zoomYMin = zoomFreqMin.explin(freqRangeMin, freqRangeMax, 0, 1);
		zoomYMax = zoomFreqMax.explin(freqRangeMin, freqRangeMax, 0, 1);

		// Map screen Y to normalized Y using zoom range
		// Screen Y is inverted: 0=top, canvasHeight=bottom
		// Use explicit math since linlin with reversed range doesn't work correctly
		screenNorm = 1.0 - (screenY / canvasHeight);  // 0=bottom, 1=top
		result = zoomYMin + (screenNorm * (zoomYMax - zoomYMin));

		^result
	}

	// Convert normalized Y (0=bottom, 1=top) to screen Y coordinate
	// Accounts for vertical zoom
	yNormToScreen { |yNorm|
		var zoomYMin, zoomYMax;

		// Convert frequency zoom bounds to normalized Y (0-1)
		zoomYMin = zoomFreqMin.explin(freqRangeMin, freqRangeMax, 0, 1);
		zoomYMax = zoomFreqMax.explin(freqRangeMin, freqRangeMax, 0, 1);

		// Map normalized Y to screen using zoom range
		// Screen Y is inverted: 0=top, canvasHeight=bottom
		^yNorm.linlin(zoomYMin, zoomYMax, canvasHeight, 0)
	}

	// Get screen Y from arc point - handles both old (freq) and new (yNorm) formats
	ptToScreenY { |pt|
		// If freq is provided and > 1 (valid Hz value), use it
		// Otherwise use normalized y (0-1)
		if(pt[\freq].notNil and: { pt[\freq] > 1 }) {
			// Freq provided - convert Hz to screen position
			^this.freqToScreen(pt[\freq])
		} {
			// Use normalized y directly
			^this.yNormToScreen(pt[\y] ?? 0.5)
		}
	}

	// ========== Refresh ==========

	refresh {
		if(canvas.notNil) { { canvas.refresh }.defer };
		if(freqLabelView.notNil) { { freqLabelView.refresh }.defer };
	}

	// ========== Playback State ==========

	setPlaybackPosition { |pos|
		playbackPosition = pos ?? 0;
		{ canvas.refresh }.defer;
	}

	setPlaying { |playing|
		isPlaying = playing;
		if(controls[\playButton].notNil) {
			{ controls[\playButton].value = playing.asInteger }.defer;
		};
		{ canvas.refresh }.defer;
	}

	setPlayDuration { |duration|
		playDuration = duration ?? 10;
		if(controls[\durationBox].notNil) {
			{ controls[\durationBox].value = playDuration }.defer;
		};
	}

	// Check if we're in DRAG playback mode
	prIsDragPlaybackMode {
		if(controls[\directionMenu].notNil) {
			// DRAG is index 4 in the direction menu (FWD=0, REV=1, PAL=2, LOOP=3, DRAG=4)
			^(controls[\directionMenu].value == 4)
		};
		^false
	}

	// Convert screen X to normalized time and call scrub callback
	prScrubToScreenX { |screenX|
		var timeNorm;
		// Convert screen X to normalized time (0-1)
		// Account for zoom
		timeNorm = screenX.linlin(0, canvasWidth, zoomTimeMin, zoomTimeMax);
		timeNorm = timeNorm.clip(0, 1);

		// Update local playback position for drawing
		playbackPosition = timeNorm * playDuration;

		// Call the scrub callback
		if(onScrub.notNil) {
			onScrub.value(timeNorm);
		};
	}

	// ========== Cleanup ==========

	prCleanup {
		// Stop any playback
		if(isPlaying) {
			onPlaybackToggle.value(false);
		};

		window = nil;
		canvas = nil;
		freqLabelView = nil;
		pageTabButtons = nil;
	}
}
