/*
 * NUPICArcsListView - Window showing all arcs with selection highlighting
 *
 * Shows a list of all arcs with their synth settings and quick access
 * to editors. Selected arcs are highlighted. Click to select/deselect.
 * Uses B&K green color scheme to match other editors.
 */

NUPICArcsListView {
	// References
	var <>data;
	var <>gui;

	// Window elements
	var <window;
	var <scrollView;
	var <contentView;
	var <arcRows;

	// Constants
	var windowWidth = 760;
	var rowHeight = 30;
	var headerHeight = 28;
	var maxVisibleRows = 15;

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

		// Initialize synth mappings
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
		if(data.isNil) { ^nil };

		// Close existing window if open
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not }) {
			current.window.close;
		};

		this.prCreateWindow;
		current = this;
		window.front;
	}

	prCreateWindow {
		var numArcs = data.arcCount;
		var visibleRows = min(numArcs.max(1), maxVisibleRows);
		var contentHeight = numArcs.max(1) * rowHeight;
		var scrollHeight = visibleRows * rowHeight;
		var windowHeight = scrollHeight + headerHeight + 10;

		window = Window("Arcs List (" ++ numArcs ++ ")",
			Rect(300, 200, windowWidth, windowHeight),
			resizable: true);
		window.alwaysOnTop_(true);
		window.background = Color.new(205/255, 250/255, 205/255);  // B&K green

		// Header
		this.prCreateHeader;

		// Scroll view for arcs
		scrollView = ScrollView(window, Rect(0, headerHeight, windowWidth, scrollHeight + 5));
		scrollView.background = Color.white;
		scrollView.hasHorizontalScroller = false;
		scrollView.hasVerticalScroller = true;

		// Content view inside scroll
		contentView = CompositeView(scrollView, Rect(0, 0, windowWidth - 20, contentHeight));
		contentView.background = Color.white;

		// Create content
		this.prCreateContent;

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

	prCreateContent {
		var numArcs = data.arcCount;

		arcRows = List.new;

		if(numArcs == 0) {
			// Show empty message
			StaticText(contentView, Rect(10, 10, 200, 20))
			.string_("No arcs")
			.font_(Font("Riforma Mono LL", 10))
			.stringColor_(Color.gray(0.5));
		} {
			numArcs.do { |arcIdx|
				this.prCreateArcRow(arcIdx);
			};
		};
	}

	prCreateArcRow { |arcIdx|
		var y = arcIdx * rowHeight;
		var synthDef = data.getSynthDefForArc(arcIdx);
		var synthIndex = synthDefToIndex[synthDef.asSymbol] ? 0;
		var isVarSaw = synthDef.asString.containsi("VarSaw");
		var isMono = synthDef.asString.containsi("Mono") or: {
			(synthDef == \upicWavetable) or:
			{ synthDef == \upicSawBL } or:
			{ synthDef == \upicVarSaw }
		};
		var isWavetable = synthDef.asString.containsi("Wavetable") or: { synthDef == \upicWavetable };
		var isSelected = data.isSelected(arcIdx);
		var rowColor = this.prGetRowColor(arcIdx, isSelected);
		var row, xPos;

		row = CompositeView(contentView, Rect(0, y, windowWidth - 20, rowHeight));
		row.background = rowColor;

		// Arc index button - clickable to select arc
		Button(row, Rect(5, 3, 40, 24))
		.states_([[arcIdx.asString, Color.hsv((arcIdx / data.arcCount.max(1)) * 0.8, 0.8, 0.5), rowColor]])
		.font_(Font("Riforma Mono LL", 11, true))
		.action_({ |btn, modifiers|
			var shiftKey = modifiers.bitAnd(131072) > 0;  // Shift modifier
			if(shiftKey) {
				data.toggleSelect(arcIdx);
			} {
				data.deselectAll;
				data.select(arcIdx);
			};
			this.prUpdateRowHighlights;
			if(gui.notNil) { gui.refresh };
		});

		// Synth dropdown
		PopUpMenu(row, Rect(50, 3, 150, 24))
		.items_(synthDefNames)
		.font_(Font("Riforma Mono LL", 9))
		.value_(synthIndex)
		.action_({ |menu|
			var newSynthDefs = [
				\upicWavetable, \upicWavetable2ch, \upicWavetable3ch, \upicWavetable4ch,
				\upicWavetable8ch, \upicWavetable12ch, \upicWavetable15ch, \upicWavetable24ch,
				\upicSawBL, \upicSawBL2ch, \upicSawBL3ch, \upicSawBL4ch,
				\upicSawBL8ch, \upicSawBL12ch, \upicSawBL15ch, \upicSawBL24ch,
				\upicVarSaw, \upicVarSaw2ch, \upicVarSaw3ch, \upicVarSaw4ch,
				\upicVarSaw8ch, \upicVarSaw12ch, \upicVarSaw15ch, \upicVarSaw24ch
			];
			var newSynth = newSynthDefs[menu.value];
			data.setSynthDefForArc(arcIdx, newSynth);
			// Refresh to update button visibility
			{ this.prRefreshContent }.defer(0.1);
		});

		xPos = 210;

		// Amplitude button (always visible)
		Button(row, Rect(xPos, 3, 55, 24))
		.states_([["_amp", Color.black, Color.new(180/255, 230/255, 180/255)]])
		.font_(Font("Riforma Mono LL", 9))
		.action_({
			this.prOpenAmplitudeEditor(arcIdx);
		});
		xPos = xPos + 60;

		// Spatial button for multi-channel, or channel label+numberbox for mono
		if(isMono) {
			var currentChannel = data.getChannelOffset(arcIdx);
			// Label
			StaticText(row, Rect(xPos, 5, 25, 18))
			.string_("_ch")
			.font_(Font("Riforma Mono LL", 9))
			.stringColor_(Color.gray(0.3));
			// NumberBox for channel
			NumberBox(row, Rect(xPos + 25, 3, 30, 20))
			.value_(currentChannel)
			.clipLo_(0)
			.clipHi_(127)
			.step_(1)
			.scroll_step_(1)
			.font_(Font("Riforma Mono LL", 9))
			.action_({ |nb|
				data.setChannelOffset(arcIdx, nb.value.asInteger);
				data.setChanged;
			});
		} {
			Button(row, Rect(xPos, 3, 55, 24))
			.states_([["_spat", Color.black, Color.new(180/255, 210/255, 240/255)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({
				this.prOpenSpatialEditor(arcIdx);
			});
		};
		xPos = xPos + 60;

		// Width button (only for VarSaw)
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
			xPos = xPos + 50;
		};

		// Amplitude scaler slider
		StaticText(row, Rect(xPos, 5, 25, 18))
		.string_("_vol")
		.font_(Font("Riforma Mono LL", 8))
		.stringColor_(Color.gray(0.3));

		Slider(row, Rect(xPos + 26, 5, 200, 18))
		.value_(data.getAmplitudeScaler(arcIdx) ? 1.0)
		.knobColor_(Color.gray(0.4))
		.action_({ |sl|
			data.setAmplitudeScaler(arcIdx, sl.value);
		});

		arcRows.add((view: row, index: arcIdx));
	}

	prGetRowColor { |arcIdx, isSelected|
		if(isSelected) {
			// Selected: light orange/yellow highlight
			^Color.new(255/255, 230/255, 180/255)
		} {
			// Alternating colors for non-selected
			if(arcIdx.even) {
				^Color.new(240/255, 255/255, 240/255)  // Light green tint
			} {
				^Color.white
			}
		}
	}

	prUpdateRowHighlights {
		arcRows.do { |rowData|
			var arcIdx = rowData.index;
			var row = rowData.view;
			var isSelected = data.isSelected(arcIdx);
			row.background = this.prGetRowColor(arcIdx, isSelected);
		};
	}

	prRefreshContent {
		var numArcs, contentHeight;

		if(contentView.isNil or: { window.isNil } or: { window.isClosed }) { ^this };

		numArcs = data.arcCount;
		contentHeight = numArcs.max(1) * rowHeight;

		// Remove old content
		contentView.children.do { |child| child.remove };

		// Resize content view
		contentView.bounds = Rect(0, 0, windowWidth - 20, contentHeight);

		// Recreate content
		this.prCreateContent;

		// Update window title
		window.name = "Arcs List (" ++ numArcs ++ ")";
	}

	prOpenAmplitudeEditor { |arcIdx|
		// Pass arcIdx directly - editor will add as tab if already open
		if(gui.notNil) {
			gui.openAmplitudeEditor(arcIdx);
		};
	}

	prOpenSpatialEditor { |arcIdx|
		// Pass arcIdx directly - editor will add as tab if already open
		if(gui.notNil) {
			gui.openSpatialEditor(arcIdx);
		};
	}

	prOpenWidthEditor { |arcIdx|
		// Pass arcIdx directly - editor will add as tab if already open
		if(gui.notNil) {
			gui.openWidthEditor(arcIdx);
		};
	}

	prOpenWavetableEditor { |arcIdx|
		// Pass arcIdx directly - editor will add as tab if already open
		if(gui.notNil) {
			gui.prOpenWavetableEditor(arcIdx);
		};
	}

	prOpenMonoChannelSelector { |arcIdx|
		// Open channel selector for mono synth
		if(gui.notNil) {
			gui.openMonoChannelSelector(arcIdx);
		};
	}

	// Public method to update when selection changes externally
	updateForSelection {
		if(window.isNil or: { window.isClosed }) { ^this };
		this.prUpdateRowHighlights;
	}

	// Public method to update when arcs are added/removed
	updateForDataChange {
		if(window.isNil or: { window.isClosed }) { ^this };
		this.prRefreshContent;
	}

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
		current = nil;
	}
}
