/*
 * NUPICAnalyzerGUI - GUI for audio analysis and import
 *
 * Provides interface for FluCoMa-based audio analysis
 */

NUPICAnalyzerGUI {
	var <analyzer;
	var <data;        // NUPICData reference
	var <gui;         // NUPICGUI reference for refresh

	// GUI elements
	var <window;
	var filePathText;
	var loadButton;
	var analyzeButton;
	var importButton;
	var previewButton;

	// Options
	var modeMenu;
	var segmentMenu;
	var confidenceSlider;
	var smoothingSlider;

	// Display
	var waveformView;
	var infoText;
	var statusText;

	// Preview
	var previewArcs;

	classvar current;

	*new { |data, gui|
		^super.new.init(data, gui)
	}

	*open { |data, gui|
		if(current.notNil and: { current.window.notNil } and: { current.window.isClosed.not }) {
			current.window.front;
			^current
		};
		current = this.new(data, gui);
		current.createWindow;
		^current
	}

	init { |argData, argGui|
		data = argData;
		gui = argGui;
		analyzer = NUPICAudioAnalyzer.new;
		previewArcs = List.new;
	}

	createWindow {
		var margin = 10;
		var width = 600;
		var height = 500;
		var btnH = 20;
		var bkGreen = Color.new(205/255, 250/255, 205/255);  // B&K green
		var darkerGreen = Color.new(180/255, 230/255, 180/255);

		window = Window("_audio analyzer", Rect(200, 200, width, height), resizable: false);
		window.background = bkGreen;

		// File loading section
		StaticText(window, Rect(margin, margin, 80, btnH))
			.string_("_sound file")
			.font_(Font("Riforma Mono LL", 10));

		filePathText = TextField(window, Rect(margin, margin + 22, width - 120, btnH))
			.string_("No file loaded")
			.enabled_(false)
			.font_(Font("Riforma Mono LL", 10));

		loadButton = Button(window, Rect(width - 100, margin + 22, 90, btnH))
			.states_([["_load", Color.black, Color.gray(0.85)]])
			.font_(Font("Riforma Mono LL", 9))
			.action_({ this.prLoadFile });

		// Waveform display
		waveformView = UserView(window, Rect(margin, 60, width - (margin * 2), 100));
		waveformView.background = Color.gray(0.2);
		waveformView.drawFunc = { this.prDrawWaveform };

		// Info text
		infoText = StaticText(window, Rect(margin, 165, width - (margin * 2), btnH))
			.string_("Load a sound file to analyze")
			.font_(Font("Riforma Mono LL", 10))
			.stringColor_(Color.gray(0.4));

		// Options section
		StaticText(window, Rect(margin, 195, width - (margin * 2), btnH))
			.string_("_analysis options")
			.font_(Font("Riforma Mono LL", 10, true));

		// Mode
		StaticText(window, Rect(margin, 220, 100, btnH))
			.string_("_mode")
			.font_(Font("Riforma Mono LL", 9));

		modeMenu = PopUpMenu(window, Rect(margin + 80, 218, 150, btnH))
			.items_(["Pitch Tracking", "Spectral Centroid"])
			.font_(Font("Riforma Mono LL", 9))
			.value_(0);

		// Segmentation
		StaticText(window, Rect(margin + 260, 220, 100, btnH))
			.string_("_segment")
			.font_(Font("Riforma Mono LL", 9));

		segmentMenu = PopUpMenu(window, Rect(margin + 340, 218, 150, btnH))
			.items_(["Continuous", "Split at Onsets"])
			.font_(Font("Riforma Mono LL", 9))
			.value_(0);

		// Confidence threshold
		StaticText(window, Rect(margin, 250, 120, btnH))
			.string_("_min confidence")
			.font_(Font("Riforma Mono LL", 9));

		confidenceSlider = Slider(window, Rect(margin + 130, 250, 150, btnH))
			.value_(0.7)
			.action_({ |sl|
				sl.view.next.string = sl.value.round(0.01).asString;
			});

		StaticText(window, Rect(margin + 290, 250, 50, btnH))
			.string_("0.7")
			.font_(Font("Riforma Mono LL", 9));

		// Smoothing
		StaticText(window, Rect(margin, 280, 120, btnH))
			.string_("_smoothing")
			.font_(Font("Riforma Mono LL", 9));

		smoothingSlider = Slider(window, Rect(margin + 130, 280, 150, btnH))
			.value_(0.1)  // Maps to 1-20 frames
			.action_({ |sl|
				var frames = (sl.value * 19 + 1).round.asInteger;
				sl.view.next.string = frames.asString ++ " frames";
			});

		StaticText(window, Rect(margin + 290, 280, 80, btnH))
			.string_("3 frames")
			.font_(Font("Riforma Mono LL", 9));

		// Preview area
		StaticText(window, Rect(margin, 315, width - (margin * 2), btnH))
			.string_("_preview")
			.font_(Font("Riforma Mono LL", 10, true));

		// Preview canvas showing detected arcs
		UserView(window, Rect(margin, 340, width - (margin * 2), 100))
			.background_(Color.gray(0.15))
			.drawFunc_({ |view|
				this.prDrawPreview(view);
			});

		// Status text
		statusText = StaticText(window, Rect(margin, 445, width - (margin * 2), btnH))
			.string_("Ready")
			.font_(Font("Riforma Mono LL", 10))
			.stringColor_(Color.gray(0.4));

		// Action buttons
		analyzeButton = Button(window, Rect(margin, height - 35, 80, btnH + 4))
			.states_([
				["_analyze", Color.black, Color.gray(0.85)],
				["_analyzing", Color.gray(0.5), Color.gray(0.7)]
			])
			.font_(Font("Riforma Mono LL", 9))
			.enabled_(false)
			.action_({ this.prAnalyze });

		previewButton = Button(window, Rect(margin + 90, height - 35, 80, btnH + 4))
			.states_([["_preview", Color.black, Color.gray(0.85)]])
			.font_(Font("Riforma Mono LL", 9))
			.enabled_(false)
			.action_({ this.prPreview });

		importButton = Button(window, Rect(width - 90, height - 35, 80, btnH + 4))
			.states_([["_import", Color.white, Color.new(0.2, 0.5, 0.2)]])
			.font_(Font("Riforma Mono LL", 9, true))
			.enabled_(false)
			.action_({ this.prImport });

		window.front;
	}

	// ========== Actions ==========

	prLoadFile {
		Dialog.openPanel({ |path|
			filePathText.string = path.basename;
			statusText.string = "Loading file...";

			analyzer.loadFile(path, { |buf|
				{
					infoText.string = "Duration: % sec | Sample Rate: % Hz | Channels: %".format(
						buf.duration.round(0.01),
						buf.sampleRate.asInteger,
						buf.numChannels
					);
					statusText.string = "File loaded - ready to analyze";
					analyzeButton.enabled = true;
					waveformView.refresh;
				}.defer;
			});
		}, {
			// Cancelled
		});
	}

	prAnalyze {
		analyzeButton.value = 1;
		analyzeButton.enabled = false;
		statusText.string = "Analyzing with FluCoMa...";

		analyzer.analyze({
			{
				analyzeButton.value = 0;
				analyzeButton.enabled = true;
				previewButton.enabled = true;
				importButton.enabled = true;
				statusText.string = "Analysis complete! % pitch frames, % onsets detected".format(
					analyzer.pitchData.size,
					analyzer.onsets.size
				);

				// Auto-preview
				this.prPreview;
			}.defer;
		});
	}

	prPreview {
		var options = this.prGetOptions;
		previewArcs = analyzer.toArcs(options);
		statusText.string = "Preview: % arcs generated".format(previewArcs.size);

		// Refresh preview view
		window.view.children.do { |child|
			if(child.bounds.top == 340) { child.refresh };
		};
	}

	prImport {
		var options = this.prGetOptions;
		var numArcs;

		if(data.isNil) {
			"No NUPICData connected".error;
			^this
		};

		numArcs = analyzer.importToData(data, options);
		statusText.string = "Imported % arcs to nUPIC".format(numArcs);

		// Refresh main GUI
		if(gui.notNil) {
			gui.refresh;
		};
	}

	prGetOptions {
		var mode = if(modeMenu.value == 0) { \pitch } { \spectral };
		var segment = if(segmentMenu.value == 0) { \continuous } { \onsets };
		var confidence = confidenceSlider.value;
		var smoothing = (smoothingSlider.value * 19 + 1).round.asInteger;

		^(
			mode: mode,
			segment: segment,
			minConfidence: confidence,
			smoothing: smoothing
		)
	}

	// ========== Drawing ==========

	prDrawWaveform {
		var buf = analyzer.sourceBuffer;

		if(buf.isNil) {
			Pen.fillColor = Color.gray(0.5);
			Pen.stringCenteredIn("No file loaded", waveformView.bounds.moveTo(0, 0), Font("Riforma Mono LL", 10));
			^this
		};

		// Draw simple waveform representation
		Pen.strokeColor = Color.green(0.7);
		Pen.width = 1;

		// Request waveform data asynchronously if not cached
		// For now, draw placeholder
		Pen.fillColor = Color.gray(0.3);
		Pen.fillRect(Rect(0, 0, waveformView.bounds.width, waveformView.bounds.height));

		Pen.fillColor = Color.green(0.6);
		Pen.stringCenteredIn(
			analyzer.filePath.basename,
			waveformView.bounds.moveTo(0, 0),
			Font("Riforma Mono LL", 10)
		);
	}

	prDrawPreview { |view|
		var bounds = view.bounds;
		var w = bounds.width;
		var h = bounds.height;
		var minFreq = 50;
		var maxFreq = 8000;

		// Background grid
		Pen.strokeColor = Color.gray(0.25);
		Pen.width = 1;

		// Horizontal lines (frequency)
		5.do { |i|
			var y = i * h / 4;
			Pen.line(Point(0, y), Point(w, y));
		};

		// Vertical lines (time)
		10.do { |i|
			var x = i * w / 9;
			Pen.line(Point(x, 0), Point(x, h));
		};
		Pen.stroke;

		// Draw preview arcs
		if(previewArcs.size > 0) {
			previewArcs.do { |arc, i|
				var hue = (i * 0.15) % 1;

				Pen.strokeColor = Color.hsv(hue, 0.7, 0.9);
				Pen.width = 2;

				Pen.moveTo(Point(
					arc[0][\x] * w,
					arc[0][\freq].explin(minFreq, maxFreq, h, 0)
				));

				arc.do { |pt|
					var x = pt[\x] * w;
					var y = pt[\freq].explin(minFreq, maxFreq, h, 0);
					Pen.lineTo(Point(x, y));
				};

				Pen.stroke;
			};
		} {
			Pen.fillColor = Color.gray(0.5);
			Pen.stringCenteredIn("Run analysis to see preview", bounds.moveTo(0, 0), Font("Riforma Mono LL", 10));
		};
	}

	// ========== Cleanup ==========

	close {
		if(window.notNil and: { window.isClosed.not }) {
			window.close;
		};
		analyzer.free;
	}
}
