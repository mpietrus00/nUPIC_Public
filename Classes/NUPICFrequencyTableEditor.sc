/*
 * NUPICFrequencyTableEditor - Frequency table editor for nUPIC
 *
 * Allows defining custom pitch scales per page
 * Like original UPIC's 4 frequency tables with 16K entries
 * Supports discrete (scale) and continuous (glissando) modes
 */

NUPICFrequencyTableEditor {
	// References
	var <>data;           // NUPICData reference
	var <>gui;            // NUPICGUI reference for refresh

	// Window elements
	var <window;
	var <drawView;
	var <pageTabView;
	var <controlView;
	var <pageButtons;
	var <enableButton;
	var <presetPopup;
	var <divisionsField;
	var <freqMinField;
	var <freqMaxField;
	var <refFreqField;
	var <gridButton;

	// State
	var <currentPageIndex = 0;
	var <tableCache;      // Cache tables while editing
	var <isDrawing = false;
	var <isEnabled = false;
	var <currentMode = \continuous;
	var lastX, lastY;

	// Display settings
	var <freqMin = 20;
	var <freqMax = 7500;
	var <refFreq = 440;

	// Constants
	var width = 500;
	var height = 345;  // Increased for grid controls row
	var canvasHeight = 200;
	var tabHeight = 25;
	var controlHeight = 115;  // Increased for grid controls row
	var margin = 10;
	var btnH = 20;

	// Colors (matching nUPIC style)
	var bkGreen;
	var canvasColor;
	var gridColor;
	var curveColor;
	var pointColor;
	var discreteColor;

	// Class variable
	classvar <>current;

	*new { |dataRef, guiRef|
		^super.new.init(dataRef, guiRef)
	}

	init { |dataRef, guiRef|
		data = dataRef;
		gui = guiRef;
		tableCache = IdentityDictionary.new;
		pageButtons = List.new;

		// Colors matching main page style
		bkGreen = Color.new(205/255, 250/255, 205/255);
		canvasColor = Color.white;
		gridColor = Color.gray(0.85);
		curveColor = Color.new(0.2, 0.5, 0.3);
		pointColor = Color.new(0.8, 0.4, 0.1);
		discreteColor = Color.new(0.3, 0.5, 0.8);
	}

	// ========== Public Interface ==========

	open {
		if(window.notNil and: { window.isClosed.not }) {
			window.front;
			^this
		};

		// Sync state from data
		currentPageIndex = data.currentPageIndex;
		isEnabled = data.isFrequencyTableEnabled;
		currentMode = data.getFrequencyTableMode;
		freqMin = data.getFrequencyRangeMin;
		freqMax = data.getFrequencyRangeMax;

		// Cache current table
		this.prLoadTableFromData;

		this.prCreateWindow;
		window.front;
		current = this;
	}

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
		current = nil;
	}

	refresh {
		if(drawView.notNil) {
			drawView.refresh;
		};
	}

	// ========== Window Creation ==========

	prCreateWindow {
		var totalHeight = tabHeight + canvasHeight + controlHeight + (margin * 2);

		window = Window("_scale editor", Rect(200, 200, width, totalHeight), resizable: false);
		window.background = bkGreen;

		window.onClose = {
			current = nil;
		};

		this.prCreatePageTabs;
		this.prCreateDrawView;
		this.prCreateControls;
	}

	prCreatePageTabs {
		var tabWidth = 40;
		var labels = ["_a", "_b", "_c", "_d"];

		pageTabView = CompositeView(window, Rect(0, 0, width, tabHeight));
		pageTabView.background = bkGreen;

		// Title
		StaticText(pageTabView, Rect(margin, 2, 150, tabHeight - 4))
			.string_("_scale editor")
			.font_(Font("Riforma Mono LL", 11))
			.stringColor_(Color.black);

		// Page buttons
		pageButtons = labels.collect { |label, i|
			var btn = Button(pageTabView, Rect(width - ((4 - i) * (tabWidth + 4)) - margin, 2, tabWidth, tabHeight - 4))
				.states_([
					[label, Color.black, Color.gray(0.85)],
					[label, Color.white, Color.new(0.2, 0.5, 0.3)]
				])
				.font_(Font("Riforma Mono LL", 9))
				.action_({ |btn|
					this.prSwitchPage(i);
				});
			btn
		};

		// Highlight current page
		pageButtons[currentPageIndex].value = 1;
	}

	prCreateDrawView {
		var canvasY = tabHeight;

		drawView = UserView(window, Rect(0, canvasY, width, canvasHeight));
		drawView.background = canvasColor;
		drawView.drawFunc = { this.prDrawCanvas };

		drawView.mouseDownAction = { |view, x, y, mod, buttonNum|
			if(buttonNum == 0) {
				isDrawing = true;
				lastX = x;
				lastY = y;

				if(currentMode == \discrete) {
					// In discrete mode, click adds/removes points
					this.prTogglePoint(x, y);
				} {
					// In continuous mode, start drawing curve
					this.prAddPointAtPosition(x, y);
				};
				view.refresh;
			};
		};

		drawView.mouseMoveAction = { |view, x, y, mod|
			if(isDrawing and: { currentMode == \continuous }) {
				this.prAddPointAtPosition(x, y);
				lastX = x;
				lastY = y;
				view.refresh;
			};
		};

		drawView.mouseUpAction = { |view, x, y|
			isDrawing = false;
			this.prSaveTableToData;
			view.refresh;
		};
	}

	prCreateControls {
		var controlY = tabHeight + canvasHeight;
		var col1 = margin;
		var col2 = 130;
		var col3 = 260;
		var row1 = 8;
		var row2 = 35;
		var row3 = 62;
		var labelW = 55;
		var fieldW = 50;

		controlView = CompositeView(window, Rect(0, controlY, width, controlHeight));
		controlView.background = bkGreen;

		// Row 1: Enable/Disable, Mode toggle, Preset popup
		enableButton = Button(controlView, Rect(col1, row1, 70, btnH))
			.states_([
				["_disabled", Color.gray(0.4), Color.gray(0.85)],
				["_enabled", Color.white, Color.new(0.2, 0.5, 0.3)]
			])
			.font_(Font("Riforma Mono LL", 9))
			.value_(isEnabled.asInteger)
			.action_({ |btn|
				isEnabled = btn.value == 1;
				data.setFrequencyTableEnabled(isEnabled);
				gui.refresh;  // Refresh both canvas and frequency labels
			});

		StaticText(controlView, Rect(col1 + 80, row1, 50, btnH))
			.string_("preset:")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.black);

		presetPopup = PopUpMenu(controlView, Rect(col1 + 130, row1, 100, btnH))
			.items_(["exponential", "12-TET", "24-TET", "31-TET", "just", "pythagorean", "harmonic", "custom..."])
			.font_(Font("Riforma Mono LL", 9))
			.action_({ |popup|
				this.prApplyPreset(popup.value);
			});

		// Clear button
		Button(controlView, Rect(col3 + 90, row1, 50, btnH))
			.states_([["_clear", Color.black, Color.gray(0.85)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prClearTable;
			});

		// Row 2: Frequency range
		StaticText(controlView, Rect(col1, row2, labelW, btnH))
			.string_("min Hz:")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.black);

		freqMinField = TextField(controlView, Rect(col1 + labelW, row2, fieldW, btnH))
			.string_(freqMin.asString)
			.font_(Font("Riforma Mono LL", 9))
			.action_({ |field|
				freqMin = field.string.asFloat.max(1);
				data.setFrequencyRange(freqMin, freqMax);
				this.prRegenerateTable;
				gui.refresh;
			});

		StaticText(controlView, Rect(col1 + labelW + fieldW + 10, row2, labelW, btnH))
			.string_("max Hz:")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.black);

		freqMaxField = TextField(controlView, Rect(col1 + (labelW * 2) + fieldW + 10, row2, fieldW, btnH))
			.string_(freqMax.asString)
			.font_(Font("Riforma Mono LL", 9))
			.action_({ |field|
				freqMax = field.string.asFloat.max(freqMin + 1);
				data.setFrequencyRange(freqMin, freqMax);
				this.prRegenerateTable;
				gui.refresh;
			});

		StaticText(controlView, Rect(col3, row2, labelW, btnH))
			.string_("ref Hz:")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.black);

		refFreqField = TextField(controlView, Rect(col3 + labelW, row2, fieldW, btnH))
			.string_(refFreq.asString)
			.font_(Font("Riforma Mono LL", 9))
			.action_({ |field|
				refFreq = field.string.asFloat.max(1);
				this.prRegenerateTable;
			});

		// Row 3: Custom divisions (for n-TET)
		StaticText(controlView, Rect(col1, row3, 80, btnH))
			.string_("divisions:")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.black);

		divisionsField = TextField(controlView, Rect(col1 + 80, row3, 40, btnH))
			.string_("12")
			.font_(Font("Riforma Mono LL", 9));

		Button(controlView, Rect(col1 + 130, row3, 80, btnH))
			.states_([["_generate", Color.black, Color.gray(0.85)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				var divisions = divisionsField.string.asInteger.max(1);
				this.prGenerateCustomDivisions(divisions);
			});

		// Apply to main GUI button
		Button(controlView, Rect(col3 + 90, row3, 50, btnH))
			.states_([["_apply", Color.white, Color.new(0.2, 0.5, 0.3)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prApplyToMainGUI;
			});

		// Row 4: Grid display controls (affects main canvas)
		this.prCreateGridControls(col1, 89);
	}

	prCreateGridControls { |xStart, yPos|
		// Grid on/off toggle (grid type set automatically by preset selection)
		gridButton = Button(controlView, Rect(xStart, yPos, 60, btnH))
			.states_([
				["_grid on", Color.black, Color.gray(0.9)],
				["_grid off", Color.white, Color.gray(0.6)]
			])
			.font_(Font("Riforma Mono LL", 9))
			.value_(if(gui.notNil) { if(gui.showGrid) { 0 } { 1 } } { 0 })
			.action_({ |btn|
				if(gui.notNil) {
					gui.showGrid = (btn.value == 0);
					gui.refresh;
				};
			});

		// Snap selected arcs to grid
		Button(controlView, Rect(xStart + 65, yPos, 50, btnH))
			.states_([["_snap", Color.black, Color.gray(0.9)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				if(data.notNil) {
					data.snapSelectedToGrid;
					if(gui.notNil) { gui.refresh };
				};
			});

		// Restore selected arcs to pre-snap state
		Button(controlView, Rect(xStart + 120, yPos, 60, btnH))
			.states_([["_de-snap", Color.black, Color.gray(0.9)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				if(data.notNil) {
					data.smoothSelected;
					if(gui.notNil) { gui.refresh };
				};
			});
	}

	// ========== Drawing ==========

	prDrawCanvas {
		var table = tableCache[currentPageIndex];
		var viewWidth = drawView.bounds.width;
		var viewHeight = drawView.bounds.height;

		// Draw frequency grid lines (logarithmic)
		this.prDrawFrequencyGrid(viewWidth, viewHeight);

		// Draw the table
		if(table.notNil and: { table.size > 0 }) {
			if(currentMode == \discrete) {
				this.prDrawDiscreteTable(table, viewWidth, viewHeight);
			} {
				this.prDrawContinuousTable(table, viewWidth, viewHeight);
			};
		};

		// Draw reference line (default exponential)
		this.prDrawReferenceCurve(viewWidth, viewHeight);
	}

	prDrawFrequencyGrid { |viewWidth, viewHeight|
		var freqs = [50, 100, 200, 440, 1000, 2000, 4000];

		Pen.strokeColor = gridColor;
		Pen.width = 0.5;

		// Horizontal frequency lines
		freqs.do { |freq|
			if(freq >= freqMin and: { freq <= freqMax }) {
				var yNorm = freq.explin(freqMin, freqMax, 0, 1);
				var y = viewHeight * (1 - yNorm);

				Pen.moveTo(0 @ y);
				Pen.lineTo(viewWidth @ y);

				// Label
				Pen.stringAtPoint(freq.asString ++ " Hz", 5 @ (y - 12),
					Font("Riforma Mono LL", 8), gridColor);
			};
		};

		// Vertical position lines
		[0.25, 0.5, 0.75].do { |xNorm|
			var x = viewWidth * xNorm;
			Pen.moveTo(x @ 0);
			Pen.lineTo(x @ viewHeight);
		};

		Pen.stroke;
	}

	prDrawReferenceCurve { |viewWidth, viewHeight|
		var numPoints = 100;

		// Draw default exponential curve as reference (dashed)
		Pen.strokeColor = Color.gray(0.4);
		Pen.width = 1;

		Pen.moveTo(0 @ viewHeight);
		numPoints.do { |i|
			var xNorm = i / (numPoints - 1);
			var freq = xNorm.linexp(0, 1, freqMin, freqMax);
			var yNorm = freq.explin(freqMin, freqMax, 0, 1);
			var x = viewWidth * xNorm;
			var y = viewHeight * (1 - yNorm);
			Pen.lineTo(x @ y);
		};
		Pen.stroke;
	}

	prDrawDiscreteTable { |table, viewWidth, viewHeight|
		// Draw discrete pitch points as horizontal lines
		Pen.strokeColor = discreteColor;
		Pen.width = 1;

		table.do { |pt|
			var y = viewHeight * (1 - pt.y);

			// Horizontal line across full width
			Pen.moveTo(0 @ y);
			Pen.lineTo(viewWidth @ y);
		};
		Pen.stroke;

		// Draw points
		Pen.fillColor = pointColor;
		table.do { |pt|
			var x = viewWidth * pt.y;  // Use y as x for visualization
			var y = viewHeight * (1 - pt.y);
			Pen.fillOval(Rect(x - 4, y - 4, 8, 8));
		};
	}

	prDrawContinuousTable { |table, viewWidth, viewHeight|
		var sorted;

		if(table.size < 2) {
			// Just draw points
			Pen.fillColor = pointColor;
			table.do { |pt|
				var x = viewWidth * pt.y;
				var y = viewHeight * (1 - pt.y);
				Pen.fillOval(Rect(x - 4, y - 4, 8, 8));
			};
			^this
		};

		// Sort by y position for drawing
		sorted = table.sort { |a, b| a.y < b.y };

		// Draw curve
		Pen.strokeColor = curveColor;
		Pen.width = 2;

		Pen.moveTo((viewWidth * sorted.first.y) @ (viewHeight * (1 - sorted.first.y)));
		sorted.do { |pt, i|
			if(i > 0) {
				var x = viewWidth * pt.y;
				var y = viewHeight * (1 - pt.y);
				Pen.lineTo(x @ y);
			};
		};
		Pen.stroke;

		// Draw points
		Pen.fillColor = pointColor;
		sorted.do { |pt|
			var x = viewWidth * pt.y;
			var y = viewHeight * (1 - pt.y);
			Pen.fillOval(Rect(x - 3, y - 3, 6, 6));
		};
	}

	// ========== Table Editing ==========

	prAddPointAtPosition { |x, y|
		var table = tableCache[currentPageIndex] ?? List.new;
		var viewWidth = drawView.bounds.width;
		var viewHeight = drawView.bounds.height;
		var yNorm = 1 - (y / viewHeight);
		var freq = yNorm.linexp(0, 1, freqMin, freqMax);
		var threshold = 0.01;  // 1% tolerance to avoid duplicate points

		yNorm = yNorm.clip(0, 1);

		// Only add if not too close to an existing point (prevents thousands of points)
		if(table.any { |pt| (pt.y - yNorm).abs < threshold }.not) {
			table.add((y: yNorm, freq: freq));
		};

		tableCache[currentPageIndex] = table;
	}

	prTogglePoint { |x, y|
		var table = tableCache[currentPageIndex] ?? List.new;
		var viewWidth = drawView.bounds.width;
		var viewHeight = drawView.bounds.height;
		var yNorm = 1 - (y / viewHeight);
		var freq = yNorm.linexp(0, 1, freqMin, freqMax);
		var found = false;
		var threshold = 0.03;  // 3% tolerance for clicking existing points

		yNorm = yNorm.clip(0, 1);

		// Check if clicking near existing point
		table.do { |pt, i|
			if((pt.y - yNorm).abs < threshold) {
				// Remove point
				table.removeAt(i);
				found = true;
			};
		};

		if(found.not) {
			// Add new point
			table.add((y: yNorm, freq: freq));
		};

		tableCache[currentPageIndex] = table;
	}

	prClearTable {
		tableCache[currentPageIndex] = List.new;
		this.prSaveTableToData;
		drawView.refresh;
	}

	// ========== Presets ==========

	prApplyPreset { |presetIndex|
		var table;

		switch(presetIndex,
			0, {
				// Exponential (default) - clear custom table
				tableCache[currentPageIndex] = nil;
				data.setFrequencyTableEnabled(false);
				isEnabled = false;
				enableButton.value = 0;
				if(gui.notNil) { gui.setGridType(\equalTemperament) };
			},
			1, {
				// 12-TET
				table = NUPICData.generateChromaticTable(freqMin, freqMax, refFreq);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\equalTemperament) };
			},
			2, {
				// 24-TET
				table = NUPICData.generate24TETTable(freqMin, freqMax, refFreq);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\quarterTone) };
			},
			3, {
				// 31-TET
				table = NUPICData.generate31TETTable(freqMin, freqMax, refFreq);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\fokker31) };
			},
			4, {
				// Just Intonation
				table = NUPICData.generateJustIntonationTable(freqMin, freqMax, refFreq);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\just) };
			},
			5, {
				// Pythagorean
				table = NUPICData.generatePythagoreanTable(freqMin, freqMax, refFreq);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\pythagorean) };
			},
			6, {
				// Harmonic series
				table = NUPICData.generateHarmonicSeriesTable(freqMin, freqMax, 55);
				tableCache[currentPageIndex] = List.newFrom(table);
				if(gui.notNil) { gui.setGridType(\harmonicSeries) };
			},
			7, {
				// Custom - do nothing, let user draw
				tableCache[currentPageIndex] = List.new;
				// Keep current grid type for custom
			}
		);

		data.setFrequencyTableMode(currentMode);
		this.prSaveTableToData;
		drawView.refresh;
	}

	prGenerateCustomDivisions { |divisions|
		var table = NUPICData.generateCustomEqualDivisions(divisions, freqMin, freqMax, refFreq);
		tableCache[currentPageIndex] = List.newFrom(table);
		data.setFrequencyTableMode(currentMode);
		this.prSaveTableToData;
		drawView.refresh;
	}

	prRegenerateTable {
		var currentPreset = presetPopup.value;
		if(currentPreset > 0 and: { currentPreset < 7 }) {
			this.prApplyPreset(currentPreset);
		};
	}

	// ========== Data Sync ==========

	prLoadTableFromData {
		var table = data.getFrequencyTable;
		if(table.notNil) {
			tableCache[currentPageIndex] = List.newFrom(table);
		} {
			tableCache[currentPageIndex] = nil;
		};
	}

	prSaveTableToData {
		var table = tableCache[currentPageIndex];
		if(table.notNil and: { table.size > 0 }) {
			// Simplify table if too many points (max 500 for performance)
			if(table.size > 500) {
				table = this.prSimplifyTable(table, 500);
				tableCache[currentPageIndex] = table;
				"Frequency table simplified: % points".format(table.size).postln;
			};
			data.setFrequencyTable(table.asArray);
			"Frequency table saved: % points".format(table.size).postln;
			if(isEnabled.not) {
				isEnabled = true;
				data.setFrequencyTableEnabled(true);
				enableButton.value = 1;
			};
		} {
			data.setFrequencyTable(nil);
		};
	}

	prSimplifyTable { |table, maxPoints|
		// Reduce table to maxPoints by keeping evenly-spaced samples
		var sorted, step, result;
		if(table.size <= maxPoints) { ^table };

		sorted = table.copy.sort { |a, b| a.y < b.y };
		step = (sorted.size / maxPoints).ceil.asInteger.max(1);
		result = List.new;

		sorted.do { |pt, i|
			if((i % step) == 0) {
				result.add(pt);
			};
		};

		// Always include first and last
		if(result.first.y != sorted.first.y) {
			result.insert(0, sorted.first);
		};
		if(result.last.y != sorted.last.y) {
			result.add(sorted.last);
		};

		^result
	}

	prSwitchPage { |pageIndex|
		// Save current page table and range
		this.prSaveTableToData;
		data.setFrequencyRange(freqMin, freqMax);

		// Update buttons
		pageButtons[currentPageIndex].value = 0;
		currentPageIndex = pageIndex;
		pageButtons[currentPageIndex].value = 1;

		// Switch data page
		data.switchPage(pageIndex);

		// Load new page state
		isEnabled = data.isFrequencyTableEnabled;
		currentMode = data.getFrequencyTableMode;
		freqMin = data.getFrequencyRangeMin;
		freqMax = data.getFrequencyRangeMax;

		enableButton.value = isEnabled.asInteger;
		freqMinField.string = freqMin.asString;
		freqMaxField.string = freqMax.asString;

		// Load table
		this.prLoadTableFromData;

		drawView.refresh;
		gui.refresh;  // Refresh both canvas and frequency labels
	}

	prApplyToMainGUI {
		var table = tableCache[currentPageIndex];
		// Save the current table (with simplification if needed)
		this.prSaveTableToData;
		// Force refresh of main GUI to reflect frequency table changes
		if(gui.notNil) {
			gui.refresh;  // Refresh both canvas and frequency labels
		};
		if(table.notNil) {
			"Frequency table applied to page % (% points)".format(
				["_a", "_b", "_c", "_d"][currentPageIndex],
				table.size
			).postln;
		} {
			"Frequency table cleared on page %".format(["_a", "_b", "_c", "_d"][currentPageIndex]).postln;
		};
	}
}
