/*
 * NUPICMultiArcEditor - Popup window for editing multiple selected arcs
 *
 * Shows a list of selected arcs with their synth settings
 * and quick access to amplitude, spatial, and width editors
 * Uses B&K green color scheme to match other editors
 */

NUPICMultiArcEditor {
	// References
	var <>data;
	var <>gui;

	// Window elements
	var <window;
	var <scrollView;
	var <arcRows;

	// State
	var <arcIndices;

	// Constants
	var windowWidth = 520;
	var rowHeight = 30;
	var headerHeight = 28;

	// Synth name mappings
	var synthDefNames;
	var synthDefToIndex;

	// Class variable
	classvar <>current;

	*new { |dataRef, guiRef|
		^super.new.init(dataRef, guiRef)
	}

	init { |dataRef, guiRef|
		data = dataRef;
		gui = guiRef;
		arcRows = List.new;
		arcIndices = List.new;

		// Initialize synth mappings - names depend on oversampling mode
		this.prUpdateSynthMappings;
	}

	prUpdateSynthMappings {
		var sawName = if(gui.notNil and: { gui.useOversampling }) { "SawBL" } { "Saw" };
		var varSawName = if(gui.notNil and: { gui.useOversampling }) { "VarSawOS" } { "VarSaw" };

		synthDefNames = [
			// Wavetable variants (0-7)
			"Wavetable (Mono)",
			"Wavetable (2ch)",
			"Wavetable (3ch)",
			"Wavetable (4ch)",
			"Wavetable (8ch)",
			"Wavetable (12ch)",
			"Wavetable (15ch)",
			"Wavetable (24ch)",
			// Saw/SawBL variants (8-15)
			sawName ++ " (Mono)",
			sawName ++ " (2ch)",
			sawName ++ " (3ch)",
			sawName ++ " (4ch)",
			sawName ++ " (8ch)",
			sawName ++ " (12ch)",
			sawName ++ " (15ch)",
			sawName ++ " (24ch)",
			// VarSaw/VarSawOS variants (16-23)
			varSawName ++ " (Mono)",
			varSawName ++ " (2ch)",
			varSawName ++ " (3ch)",
			varSawName ++ " (4ch)",
			varSawName ++ " (8ch)",
			varSawName ++ " (12ch)",
			varSawName ++ " (15ch)",
			varSawName ++ " (24ch)"
		];

		synthDefToIndex = IdentityDictionary[
			// Wavetable variants (0-7)
			\upicWavetable -> 0,
			\upicWavetable2ch -> 1,
			\upicWavetable3ch -> 2,
			\upicWavetable4ch -> 3,
			\upicWavetable8ch -> 4,
			\upicWavetable12ch -> 5,
			\upicWavetable15ch -> 6,
			\upicWavetable24ch -> 7,
			// Saw/SawBL variants (8-15)
			\upicSawBL -> 8,
			\upicSawBL2ch -> 9,
			\upicSawBL3ch -> 10,
			\upicSawBL4ch -> 11,
			\upicSawBL8ch -> 12,
			\upicSawBL12ch -> 13,
			\upicSawBL15ch -> 14,
			\upicSawBL24ch -> 15,
			// VarSaw/VarSawOS variants (16-23)
			\upicVarSaw -> 16,
			\upicVarSaw2ch -> 17,
			\upicVarSaw3ch -> 18,
			\upicVarSaw4ch -> 19,
			\upicVarSaw8ch -> 20,
			\upicVarSaw12ch -> 21,
			\upicVarSaw15ch -> 22,
			\upicVarSaw24ch -> 23
		];
	}

	open {
		// Get selected arcs
		if(data.isNil or: { data.selectedCount < 2 }) {
			^nil
		};

		arcIndices = data.selectedArcs.asArray.sort.asList;

		// Close existing window if open
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not }) {
			current.window.close;
		};

		this.prCreateWindow;
		current = this;
		window.front;
	}

	prCreateWindow {
		var numArcs = arcIndices.size;
		var contentHeight = numArcs * rowHeight;
		var windowHeight = contentHeight + headerHeight;

		window = Window("Selected Arcs (" ++ numArcs ++ ")",
			Rect(300, 200, windowWidth, windowHeight),
			resizable: false);  // Non-resizable
		window.alwaysOnTop_(true);  // Stay above main UI
		window.background = Color.new(205/255, 250/255, 205/255);  // B&K green

		// Header
		this.prCreateHeader;

		// Content area (no scrolling - shows all arcs)
		scrollView = CompositeView(window, Rect(0, headerHeight, windowWidth, contentHeight));
		scrollView.background = Color.white;

		// Create content
		this.prCreateContent(contentHeight);

		window.onClose = {
			current = nil;
		};
	}

	prCreateHeader {
		var headerView = CompositeView(window, Rect(0, 0, windowWidth, headerHeight));
		headerView.background = Color.new(180/255, 230/255, 180/255);  // B&K darker green

		// Column headers
		StaticText(headerView, Rect(12, 4, 40, 20))
		.string_("Arc")
		.font_(Font("Riforma Mono LL", 10, true))
		.stringColor_(Color.gray(0.2));

		StaticText(headerView, Rect(55, 4, 140, 20))
		.string_("Synth")
		.font_(Font("Riforma Mono LL", 10, true))
		.stringColor_(Color.gray(0.2));

		StaticText(headerView, Rect(210, 4, 280, 20))
		.string_("Editors")
		.font_(Font("Riforma Mono LL", 10, true))
		.stringColor_(Color.gray(0.2));
	}

	prCreateContent { |contentHeight|
		var contentView = CompositeView(scrollView, Rect(0, 0, windowWidth, contentHeight));
		contentView.background = Color.white;

		arcRows = List.new;

		arcIndices.do { |arcIdx, i|
			this.prCreateArcRow(contentView, arcIdx, i);
		};
	}

	prCreateArcRow { |parent, arcIdx, rowIndex|
		var y = rowIndex * rowHeight;
		var synthDef = data.getSynthDefForArc(arcIdx);
		var synthIndex = synthDefToIndex[synthDef.asSymbol] ? 0;
		var isVarSaw = synthDef.asString.containsi("VarSaw");
		var isMono = synthDef.asString.containsi("Mono") or: {
			(synthDef == \upicWavetable) or:
			{ synthDef == \upicSawBL } or:
			{ synthDef == \upicVarSaw }
		};
		var isWavetable = synthDef.asString.containsi("Wavetable") or: { synthDef == \upicWavetable };
		var rowColor = if(rowIndex.even) {
			Color.new(240/255, 255/255, 240/255)  // Light green tint
		} {
			Color.white
		};
		var row, xPos;

		row = CompositeView(parent, Rect(0, y, windowWidth, rowHeight));
		row.background = rowColor;

		// Arc index label with color indicator
		StaticText(row, Rect(12, 5, 35, 20))
		.string_(arcIdx.asString)
		.font_(Font("Riforma Mono LL", 11, true))
		.stringColor_(Color.hsv((arcIdx / data.arcCount.max(1)) * 0.8, 0.8, 0.5));

		// Synth dropdown
		PopUpMenu(row, Rect(50, 3, 150, 24))
		.items_(synthDefNames)
		.font_(Font("Riforma Mono LL", 9))
		.value_(synthIndex)
		.action_({ |menu|
			var newSynthDefs = [
				// Wavetable variants (0-7)
				\upicWavetable,
				\upicWavetable2ch,
				\upicWavetable3ch,
				\upicWavetable4ch,
				\upicWavetable8ch,
				\upicWavetable12ch,
				\upicWavetable15ch,
				\upicWavetable24ch,
				// Saw/SawBL variants (8-15)
				\upicSawBL,
				\upicSawBL2ch,
				\upicSawBL3ch,
				\upicSawBL4ch,
				\upicSawBL8ch,
				\upicSawBL12ch,
				\upicSawBL15ch,
				\upicSawBL24ch,
				// VarSaw/VarSawOS variants (16-23)
				\upicVarSaw,
				\upicVarSaw2ch,
				\upicVarSaw3ch,
				\upicVarSaw4ch,
				\upicVarSaw8ch,
				\upicVarSaw12ch,
				\upicVarSaw15ch,
				\upicVarSaw24ch
			];
			var newSynth = newSynthDefs[menu.value];
			data.setSynthDefForArc(arcIdx, newSynth);
			"Arc % synth changed to %".format(arcIdx, newSynth).postln;
			// Refresh this window to update button visibility
			{ this.prRefreshContent }.defer(0.1);
		});

		xPos = 210;

		// Amplitude button (always visible) - B&K green
		Button(row, Rect(xPos, 3, 55, 24))
		.states_([["_amp", Color.black, Color.new(180/255, 230/255, 180/255)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prOpenAmplitudeEditor(arcIdx);
		});
		xPos = xPos + 60;

		// Spatial button (hidden for mono) - B&K blue tint
		if(isMono.not) {
			Button(row, Rect(xPos, 3, 55, 24))
			.states_([["_spat", Color.black, Color.new(180/255, 210/255, 240/255)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prOpenSpatialEditor(arcIdx);
			});
		};
		xPos = xPos + 60;

		// Width button (only for VarSaw) - B&K orange tint
		if(isVarSaw) {
			Button(row, Rect(xPos, 3, 55, 24))
			.states_([["_width", Color.black, Color.new(250/255, 220/255, 180/255)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prOpenWidthEditor(arcIdx);
			});
		};
		xPos = xPos + 60;

		// Wavetable button (only for wavetable synths)
		if(isWavetable) {
			Button(row, Rect(xPos, 3, 45, 24))
			.states_([["_wt", Color.black, Color.new(200/255, 240/255, 200/255)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prOpenWavetableEditor(arcIdx);
			});
		};

		arcRows.add(row);
	}

	prRefreshContent {
		var contentHeight;

		if(scrollView.isNil or: { window.isNil } or: { window.isClosed }) { ^this };

		// Remove old content
		scrollView.children.do { |child| child.remove };

		// Recreate content
		contentHeight = arcIndices.size * rowHeight;
		this.prCreateContent(contentHeight);
	}

	prOpenAmplitudeEditor { |arcIdx|
		var editor = NUPICAmplitudeEditor(data, gui);
		editor.open(arcIdx);
	}

	prOpenSpatialEditor { |arcIdx|
		var editor = NUPICSpatialEditor(data, gui);
		editor.open(arcIdx);
	}

	prOpenWidthEditor { |arcIdx|
		var editor = NUPICWidthEditor(data, gui);
		editor.open(arcIdx);
	}

	prOpenWavetableEditor { |arcIdx|
		// Open wavetable editor with all selected arcs (don't change selection)
		// This allows "apply to all" to work correctly
		if(gui.notNil) {
			gui.prOpenWavetableEditor;
		};
	}

	// Public method to update when selection changes
	updateForSelection {
		var newIndices;

		if(data.isNil or: { data.selectedCount < 2 }) {
			// Close if less than 2 arcs selected
			this.close;
			^this
		};

		// Get new selection
		newIndices = data.selectedArcs.asArray.sort.asList;

		// If selection changed, close and reopen to resize window
		if(newIndices != arcIndices) {
			arcIndices = newIndices;
			if(window.notNil and: { window.isClosed.not }) {
				window.close;
			};
			this.prCreateWindow;
			window.front;
		};
	}

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
		current = nil;
	}
}
