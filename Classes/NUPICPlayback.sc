/*
 * NUPICPlayback - Audio playback engine for nUPIC
 *
 * Handles scheduling synths based on arc data and time position
 */

NUPICPlayback {
	// References
	var <>data;       // NUPICData reference
	var <>gui;        // NUPICGUI reference for playback cursor

	// Playback state
	var <isPlaying = false;
	var <duration = 10;
	var <playbackPosition = 0;
	var <playbackMode = \forward;  // \forward, \reverse, \palindrome, \loop
	var <playbackPageIndex = 0;    // Page being played (allows browsing other pages)
	var <isReleasing = false;      // True during synth release phase (prevents node errors)

	// Audio
	var <synths;
	var <synthGroup;
	var synthOutChannels;  // Track current output channel per synth to detect changes
	var synthsActive;      // Track which synths are currently active (playing)
	var playbackRoutine;
	var cursorRoutine;

	// Settings
	var <synthDefName = \upicWavetable;
	var <masterAmp = 0.3;
	var audioUpdateRate = 30;  // Hz for audio parameter updates (lower = less CPU)
	var visualUpdateRate = 30;  // Hz for visual cursor updates

	// Callbacks
	var <>onPlaybackStart;
	var <>onPlaybackStop;
	var <>onPositionChanged;
	var wavetableChangedCallback;  // For live wavetable updates during playback

	// NRT (Non-Real-Time) rendering
	var <isRendering = false;
	var <renderedBuffer;           // Buffer containing rendered audio
	var <renderedDuration;         // Duration of rendered audio
	var <renderedPageIndex;        // Which page was rendered
	var nrtPlaybackSynth;          // Synth for playing rendered buffer
	var <>onRenderComplete;        // Callback when NRT render finishes
	var <>onRenderProgress;        // Callback for render progress updates

	*new {
		^super.new.init
	}

	init {
		synths = List.new;
		synthOutChannels = List.new;
		synthsActive = List.new;
		playbackMode = \forward;
		isRendering = false;
	}

	// ========== Playback Control ==========

	play { |argDuration|
		// If currently playing, stop first
		if(isPlaying) { this.stop };

		// Wait for any ongoing cleanup to complete before starting new playback
		if(isReleasing) {
			fork {
				while { isReleasing } { 0.01.wait };
				this.prDoPlay(argDuration);
			};
			^this
		};

		this.prDoPlay(argDuration);
	}

	prDoPlay { |argDuration|
		if(data.isNil) {
			"NUPICPlayback: No data connected".warn;
			^this
		};

		if(data.arcCount == 0) {
			"NUPICPlayback: No arcs to play".warn;
			^this
		};

		duration = argDuration ? duration;
		isPlaying = true;

		// Store which page we're playing (allows browsing other pages while playing)
		playbackPageIndex = data.currentPageIndex;

		// Initialize position based on mode
		playbackPosition = switch(playbackMode)
			{ \reverse } { duration }
			{ true } { 0 };

		// Create synth group
		synthGroup = Group.new;

		// Set up callback for live wavetable updates during playback
		wavetableChangedCallback = { |index, buffer|
			if(isPlaying and: { isReleasing.not } and: { synths[index].notNil } and: { buffer.notNil }) {
				try { synths[index].set(\bufnum, buffer.bufnum) };
			};
		};
		data.onWavetableChanged = wavetableChangedCallback;

		// Start playback routine
		this.prStartPlayback;

		// Start cursor update routine
		this.prStartCursor;

		onPlaybackStart.value;
		"Playback started (% seconds, % arcs, mode: %)".format(
			duration, data.arcCount, playbackMode
		).postln;
	}

	stop {
		// Already stopping or stopped
		if(isPlaying.not and: { isReleasing.not }) { ^this };

		isPlaying = false;
		isReleasing = true;  // Prevent further synth updates

		// Remove wavetable change callback
		if(data.notNil) {
			data.onWavetableChanged = nil;
		};
		wavetableChangedCallback = nil;

		// Stop cursor routine immediately
		if(cursorRoutine.notNil) {
			cursorRoutine.stop;
			cursorRoutine = nil;
		};

		// Stop playback routine - it will trigger cleanup
		if(playbackRoutine.notNil) {
			playbackRoutine.stop;
			playbackRoutine = nil;
		};

		// Schedule synth cleanup after a small delay to allow routine to fully stop
		{
			// Release all synths gracefully
			synths.do { |synth|
				if(synth.notNil) {
					try { synth.set(\gate, 0) };
				};
			};
			synths.clear;
			synthOutChannels.clear;
			synthsActive.clear;

			// Wait for synth envelopes to release before freeing group
			0.05.wait;

			// Free group
			if(synthGroup.notNil) {
				synthGroup.free;
				synthGroup = nil;
			};

			playbackPosition = 0;
			if(gui.notNil) {
				{ gui.setPlaybackPosition(0) }.defer;
				{ gui.setPlaying(false) }.defer;
			};

			isReleasing = false;  // Reset for next playback
			onPlaybackStop.value;
			"Playback stopped".postln;
		}.fork;
	}

	toggle {
		if(isPlaying) { this.stop } { this.play };
	}

	// ========== NRT (Non-Real-Time) Rendering ==========

	// Render current page to audio file offline - no CPU limits
	// Duration is taken from the GUI play duration setting
	renderNRT { |outputPath, onComplete|
		var score, arcList, numArcs, sampleRate, headerFormat, sampleFormat;
		var oscPath, options;

		if(isRendering) {
			"NUPICPlayback: Already rendering".warn;
			^this
		};

		if(data.isNil) {
			"NUPICPlayback: No data connected".warn;
			^this
		};

		if(data.arcCount == 0) {
			"NUPICPlayback: No arcs to render".warn;
			^this
		};

		isRendering = true;
		// Always use the current duration setting from GUI
		if(gui.notNil) {
			renderedDuration = gui.playDuration;
		} {
			renderedDuration = duration;
		};
		renderedPageIndex = data.currentPageIndex;
		arcList = data.getPageArcs(renderedPageIndex);
		numArcs = arcList.size;

		// Default output path in temp directory
		outputPath = outputPath ? (Platform.defaultTempDir +/+ "nupic_render_" ++ Date.getDate.stamp ++ ".wav");
		oscPath = Platform.defaultTempDir +/+ "nupic_render_" ++ Date.getDate.stamp ++ ".osc";

		sampleRate = Server.default.sampleRate ? 44100;
		headerFormat = "wav";
		sampleFormat = "int24";

		"NRT Render: Starting offline render of % arcs (% seconds)...".format(numArcs, renderedDuration).postln;
		onRenderProgress.value(0, "Generating score...");

		fork {
			var scoreObj, scoreList, prevTempo;

			// CRITICAL: Score uses TempoClock.default.tempo to convert beat times to seconds
			// We need tempo = 1 so that beat values equal seconds for NRT
			prevTempo = TempoClock.default.tempo;
			TempoClock.default.tempo = 1;
			"NRT: Set TempoClock.default.tempo to 1 (was %)".format(prevTempo).postln;

			// Generate the OSC score as a list
			scoreList = this.prGenerateNRTScore(arcList, renderedDuration, numArcs, renderedPageIndex);

			// Debug: print score info
			"Score has % entries".format(scoreList.size).postln;
			if(scoreList.size > 0) {
				"First entry time: %".format(scoreList[0][0]).postln;
				"Last entry time: %".format(scoreList.last[0]).postln;
			};

			onRenderProgress.value(0.1, "Score generated, rendering audio...");

			// Create a Score object from the list
			scoreObj = Score(scoreList);

			// Debug: Verify Score object has correct times
			if(scoreObj.score.size > 0) {
				var lateEntries, entriesAfter50;
				"Score object first entry time: %".format(scoreObj.score[0][0]).postln;
				"Score object last entry time: %".format(scoreObj.score.last[0]).postln;

				// Check for entries after 50 seconds
				entriesAfter50 = scoreObj.score.select { |e| e[0] > 50 };
				"Score entries after 50s: % entries".format(entriesAfter50.size).postln;
				if(entriesAfter50.size > 0) {
					"  First entry >50s: time=%, cmd=%".format(entriesAfter50[0][0], entriesAfter50[0][1][0]).postln;
					"  Last entry >50s: time=%, cmd=%".format(entriesAfter50.last[0], entriesAfter50.last[1][0]).postln;
				};

				// Check the last 10 entries
				"Last 10 score entries:".postln;
				scoreObj.score.copyToEnd(scoreObj.score.size - 10).do { |e|
					"  time=%: %".format(e[0].round(0.001), e[1][0]).postln;
				};
			};

			// Set up server options for NRT
			options = ServerOptions.new;
			options.numOutputBusChannels = 2;
			options.sampleRate = sampleRate;

			// Write OSC file using Score's built-in method
			// This produces a properly formatted binary OSC file
			"NRT: Writing OSC file to %".format(oscPath).postln;
			scoreObj.writeOSCFile(oscPath);

			// Check file size
			{
				var f = File(oscPath, "r");
				if(f.notNil and: { f.isOpen }) {
					var size = f.length;
					f.close;
					"NRT: OSC file written, size = % bytes (% MB)".format(size, (size / 1024.0 / 1024.0).round(0.01)).postln;
				} {
					"NRT: Warning - could not check OSC file size".warn;
				};
			}.value;

			// Read OSC file back to verify timestamps
			{
				var readScore = Score.newFromFile(oscPath);
				if(readScore.notNil and: { readScore.score.size > 0 }) {
					"NRT: Verified OSC file - % bundles, last time: %".format(
						readScore.score.size,
						readScore.score.last[0]
					).postln;
				} {
					"NRT: Warning - could not verify OSC file".warn;
				};
			}.value;

			// Build scsynth command manually with explicit duration
			{
				var cmd, nrtDuration, scsynthPath;
				var numBuffers;
				nrtDuration = renderedDuration + 1.0;  // Extra padding

				// Calculate required buffers: bufferStartID (100) + numArcs + some extra
				// Default scsynth has only 1024 buffers which isn't enough for 1200+ arcs
				numBuffers = (100 + numArcs + 100).max(2048);

				// Get scsynth path - Server.program may have "exec " prefix, remove it
				scsynthPath = Server.program.replace("exec ", "");
				// Remove any existing quotes
				scsynthPath = scsynthPath.replace("'", "").replace("\"", "");

				// -b numBuffers: allocate enough buffers for all arcs
				// -o 2: stereo output
				cmd = "% -N % _ % % % % -o 2 -b %".format(
					scsynthPath.shellQuote,
					oscPath.shellQuote,
					outputPath.shellQuote,
					sampleRate.asInteger,
					headerFormat,
					sampleFormat,
					numBuffers.asInteger
				);

				"NRT: Allocating % buffers (arcs: %)".format(numBuffers.asInteger, numArcs).postln;

				"NRT: scsynth path: %".format(scsynthPath).postln;
				"NRT: Running scsynth command:".postln;
				cmd.postln;

				// Run scsynth and handle completion
				cmd.unixCmd({ |exitCode|
					"NRT: scsynth exited with code %".format(exitCode).postln;

					// Restore previous tempo
					TempoClock.default.tempo = prevTempo;

					// Load rendered audio into buffer
					{
						if(renderedBuffer.notNil) { renderedBuffer.free };
						renderedBuffer = Buffer.read(Server.default, outputPath, action: { |buf|
							isRendering = false;
							"NRT Render: Complete! (% samples, % seconds)".format(
								buf.numFrames, buf.duration
							).postln;
							onRenderProgress.value(1.0, "Render complete");
							onRenderComplete.value(outputPath);
							onComplete.value(outputPath);
							// Keep OSC file for debugging
							"NRT: OSC file kept at: %".format(oscPath).postln;
						});
					}.defer(0.1);
				});
			}.value;
		};
	}

	// Play the rendered audio with synchronized cursor
	playRendered { |argDuration|
		var playDuration;

		if(renderedBuffer.isNil) {
			"NUPICPlayback: No rendered audio. Call renderNRT first.".warn;
			^this
		};

		if(isPlaying) { this.stop };

		// Wait for cleanup if needed
		if(isReleasing) {
			fork {
				while { isReleasing } { 0.01.wait };
				this.prDoPlayRendered(argDuration);
			};
			^this
		};

		this.prDoPlayRendered(argDuration);
	}

	prDoPlayRendered { |argDuration|
		var playDuration;

		playDuration = argDuration ? renderedDuration ? renderedBuffer.duration;
		duration = playDuration;
		isPlaying = true;
		playbackPageIndex = renderedPageIndex;
		playbackPosition = 0;

		// Load the NRT playback synthdef if needed
		this.prLoadNRTPlaybackSynthDef;

		// Notify GUI that playback is starting
		if(gui.notNil) {
			{ gui.setPlaying(true) }.defer;
		};

		fork {
			var synthEndOSC, synthNodeID;
			Server.default.sync;

			// Create playback synth
			nrtPlaybackSynth = Synth(\nupicNRTPlayback, [
				\bufnum, renderedBuffer.bufnum,
				\rate, 1.0,
				\amp, masterAmp
			]);
			synthNodeID = nrtPlaybackSynth.nodeID;  // Store nodeID for OSCFunc

			// Register OSCFunc to detect when synth ends naturally
			// This prevents node-not-found errors when stopRendered is called
			synthEndOSC = OSCFunc({ |msg|
				if(msg[1] == synthNodeID) {
					nrtPlaybackSynth = nil;
					synthEndOSC.free;
				};
			}, '/n_end', Server.default.addr);

			onPlaybackStart.value;
			"Playing rendered audio (% seconds)".format(playDuration).postln;

			// Combined cursor update and playback monitoring
			playbackRoutine = fork {
				var startTime = Main.elapsedTime;
				var frameTime = 1 / visualUpdateRate;

				while { isPlaying and: { (Main.elapsedTime - startTime) < playDuration } } {
					playbackPosition = Main.elapsedTime - startTime;

					// Update GUI cursor
					{
						if(gui.notNil) {
							gui.setPlaybackPosition(playbackPosition);
						};
						onPositionChanged.value(playbackPosition);
					}.defer;

					frameTime.wait;
				};

				// Playback ended naturally
				if(isReleasing.not and: { isPlaying }) {
					{ this.stopRendered }.defer;
				};
			};
		};
	}

	// Stop rendered audio playback
	stopRendered {
		if(nrtPlaybackSynth.notNil) {
			// Use try to avoid node-not-found errors if synth already ended
			try { nrtPlaybackSynth.free };
			nrtPlaybackSynth = nil;
		};

		if(cursorRoutine.notNil) {
			cursorRoutine.stop;
			cursorRoutine = nil;
		};

		if(playbackRoutine.notNil) {
			playbackRoutine.stop;
			playbackRoutine = nil;
		};

		isPlaying = false;
		playbackPosition = 0;

		if(gui.notNil) {
			{ gui.setPlaybackPosition(0) }.defer;
			{ gui.setPlaying(false) }.defer;
		};

		onPlaybackStop.value;
		"Rendered playback stopped".postln;
	}

	// Check if we have a valid rendered buffer for current page
	hasRenderedAudio {
		^(renderedBuffer.notNil and: { renderedPageIndex == data.currentPageIndex })
	}

	// Clear rendered audio buffer
	clearRenderedAudio {
		if(renderedBuffer.notNil) {
			renderedBuffer.free;
			renderedBuffer = nil;
		};
		renderedDuration = nil;
		renderedPageIndex = nil;
	}

	// Export rendered audio to a specific path
	exportRenderedAudio { |outputPath|
		if(renderedBuffer.isNil) {
			"NUPICPlayback: No rendered audio to export".warn;
			^this
		};

		renderedBuffer.write(outputPath, "wav", "int24");
		"Exported rendered audio to: %".format(outputPath).postln;
	}

	// ========== Configuration ==========

	setSynthDef { |name|
		synthDefName = name.asSymbol;
	}

	setDuration { |dur|
		duration = dur.max(0.1);
	}

	setMode { |mode|
		playbackMode = mode.asSymbol;
	}

	setMasterAmp { |amp|
		masterAmp = amp.clip(0, 1);
	}

	// Scrub to a specific normalized time position (0-1) - used by DRAG mode
	scrubTo { |timeNorm|
		var arcList;

		if(data.isNil or: { isPlaying.not } or: { isReleasing }) { ^this };

		timeNorm = timeNorm.clip(0, 1);
		playbackPosition = timeNorm * duration;
		arcList = data.arcs;

		// Update each arc's synth
		arcList.do { |arc, i|
			var synth = synths[i];
			if(synth.notNil and: { arc.size > 1 }) {
				this.prUpdateSynthForArc(synth, arc, timeNorm, i);
			};
		};

		// Update GUI cursor
		if(gui.notNil) {
			{ gui.setPlaybackPosition(playbackPosition) }.defer;
		};
	}

	// ========== Private Methods ==========

	prStartPlayback {
		playbackRoutine = fork {
			var startTime = Main.elapsedTime;
			var frameTime = 1 / audioUpdateRate;
			// Use playback page's arcs (allows browsing other pages while playing)
			var arcList = data.getPageArcs(playbackPageIndex);
			var numArcs = arcList.size;

			// Ensure all arcs have wavetable buffers for the playback page
			data.ensurePageWavetableBuffers(playbackPageIndex, Server.default);

			// Sync with server to ensure buffers are loaded
			Server.default.sync;

			// Additional small delay for safety
			0.05.wait;

			// Initialize synths for each arc (silent at start)
			numArcs.do { |i|
				var arc = arcList[i];
				var startFreq = if(arc.size > 0) {
					// Calculate freq from normalized Y using current scale
					var yNorm = this.prGetYNorm(arc[0]);
					if(data.notNil and: { data.isFrequencyTableEnabled }) {
						data.yToFreqWithTable(yNorm, data.getFrequencyRangeMin, data.getFrequencyRangeMax)
					} {
						yNorm.linexp(0, 1, 20, 7500)
					}
				} { 440 };
				// Use per-arc synthdef if available, otherwise use default
				var arcSynthDef = data.getPageSynthDefForArc(playbackPageIndex, i) ? synthDefName;
				var wtBuffer = data.getPageWavetableBufferForArc(playbackPageIndex, i);
				var bufnum = if(wtBuffer.notNil) { wtBuffer.bufnum } { 0 };
				var outChannel = data.getPageChannelOffset(playbackPageIndex, i);
				var synth;

				try {
					synth = Synth(arcSynthDef, [
						\freq, startFreq,
						\amp, 0,
						\pan, 0,
						\gate, 1,
						\numSynths, numArcs,
						\bufnum, bufnum,
						\out, outChannel
					], synthGroup);
				} {
					// Fallback to default if synthdef not found
					"SynthDef % not found, using default".format(arcSynthDef).warn;
					synth = Synth(synthDefName, [
						\freq, startFreq,
						\amp, 0,
						\pan, 0,
						\gate, 1,
						\numSynths, numArcs,
						\bufnum, bufnum,
						\out, outChannel
					], synthGroup);
				};
				synths.add(synth);
				synthOutChannels.add(outChannel);
				synthsActive.add(false);  // Initially inactive
			};

			// Main playback loop
			while { isPlaying } {
				var elapsedTime = Main.elapsedTime - startTime;
				var timeNorm, continuePlayback = true;

				// Handle different playback modes
				switch(playbackMode)
				{ \forward } {
					timeNorm = elapsedTime / duration;
					if(timeNorm >= 1) {
						continuePlayback = false;
					};
				}
				{ \reverse } {
					timeNorm = 1 - (elapsedTime / duration);
					if(timeNorm <= 0) {
						continuePlayback = false;
					};
				}
				{ \palindrome } {
					var cycleTime = elapsedTime % (duration * 2);
					if(cycleTime < duration) {
						timeNorm = cycleTime / duration;
					} {
						timeNorm = 1 - ((cycleTime - duration) / duration);
					};
				}
				{ \loop } {
					timeNorm = (elapsedTime % duration) / duration;
				}
				{ \drag } {
					// DRAG mode: don't auto-advance, wait for scrubTo calls
					// Just keep synths alive and skip time update
					timeNorm = playbackPosition / duration;
					// Stay in drag mode indefinitely until stopped
				}
				{ true } {
					timeNorm = elapsedTime / duration;
					if(timeNorm >= 1) { continuePlayback = false };
				};

				timeNorm = timeNorm.clip(0, 1);
				playbackPosition = timeNorm * duration;

				// Check for new arcs added during playback (on the playback page)
				if(data.getPageArcCount(playbackPageIndex) > synths.size) {
					var newArcCount = data.getPageArcCount(playbackPageIndex);
					var newArcIndices = (synths.size..(newArcCount - 1)).asArray;
					var playbackArcs = data.getPageArcs(playbackPageIndex);

					// Ensure wavetable buffers for all arcs on playback page
					data.ensurePageWavetableBuffers(playbackPageIndex, Server.default);

					// Wait for server to process buffer commands
					Server.default.sync;

					// Create synths for new arcs
					newArcIndices.do { |i|
						var arc = playbackArcs[i];
						var startFreq, arcSynthDef, wtBuffer, bufnum, outChannel, synth;

						if(arc.notNil and: { arc.size > 0 }) {
							startFreq = block { |break|
								var yNorm = this.prGetYNorm(arc[0]);
								if(data.notNil and: { data.isFrequencyTableEnabled }) {
									data.yToFreqWithTable(yNorm, data.getFrequencyRangeMin, data.getFrequencyRangeMax)
								} {
									yNorm.linexp(0, 1, 20, 7500)
								}
							};
							arcSynthDef = data.getPageSynthDefForArc(playbackPageIndex, i) ? synthDefName;
							wtBuffer = data.getPageWavetableBufferForArc(playbackPageIndex, i);
							bufnum = if(wtBuffer.notNil) { wtBuffer.bufnum } { 0 };
							outChannel = data.getPageChannelOffset(playbackPageIndex, i);

							try {
								synth = Synth(arcSynthDef, [
									\freq, startFreq,
									\amp, 0,
									\pan, 0,
									\gate, 1,
									\numSynths, newArcCount,
									\bufnum, bufnum,
									\out, outChannel
								], synthGroup);
							} {
								synth = Synth(synthDefName, [
									\freq, startFreq,
									\amp, 0,
									\pan, 0,
									\gate, 1,
									\numSynths, newArcCount,
									\bufnum, bufnum,
									\out, outChannel
								], synthGroup);
							};
							synths.add(synth);
							synthOutChannels.add(outChannel);
							synthsActive.add(false);
						} {
							// Add nil placeholder to keep indices aligned
							synths.add(nil);
							synthOutChannels.add(0);
							synthsActive.add(false);
						};
					};
					numArcs = newArcCount;
				};

				// Update each arc's synth using playback page's arcs
				// Skip updates if we're in the process of releasing synths
				if(isReleasing.not) {
					data.getPageArcs(playbackPageIndex).do { |arc, i|
						var synth = synths[i];
						if(synth.notNil and: { arc.notNil } and: { arc.size > 1 }) {
							this.prUpdateSynthForArc(synth, arc, timeNorm, i);
						};
					};
				};

				if(continuePlayback.not) {
					isPlaying = false;
				};

				frameTime.wait;
			};

			// Playback ended naturally - only call stop if not already stopping
			{
				if(isReleasing.not) {
					this.stop;
				};
			}.defer;
		};
	}

	prUpdateSynthForArc { |synth, arc, timeNorm, index|
		var xValues = arc.collect { |pt| pt[\x] };
		var arcStart = xValues.minItem;
		var arcEnd = xValues.maxItem;
		var isReversed = arc.first[\x] > arc.last[\x];
		var outChannel = data.getPageChannelOffset(playbackPageIndex, index);
		var prevChannel = synthOutChannels[index];
		var channelChanged = prevChannel != outChannel;

		// If output channel changed, silence first to prevent click
		if(channelChanged) {
			try { synth.set(\amp, 0, \out, outChannel) };
			synthOutChannels[index] = outChannel;
			^this  // Skip this frame, resume normal playback next frame
		};

		// Check if playhead is within this arc's time range
		if(timeNorm >= arcStart and: { timeNorm <= arcEnd }) {
			var arcTimeNorm, freq, amp, pan, spatialPos, numChannels;
			var synthDef, isVarSaw, widthVal;

			// Mark as active
			synthsActive[index] = true;

			// Calculate position within arc (0-1)
			arcTimeNorm = if(arcEnd > arcStart) {
				(timeNorm - arcStart) / (arcEnd - arcStart)
			} { 0 };
			arcTimeNorm = arcTimeNorm.clip(0, 1);

			// For right-to-left arcs, reverse the interpolation direction
			if(isReversed) {
				arcTimeNorm = 1 - arcTimeNorm;
			};

			// Interpolate frequency from arc points
			freq = this.prInterpolateFreq(arc, arcTimeNorm);

			// Get amplitude from envelope or default
			amp = this.prGetAmplitude(arc, index, arcTimeNorm);

			// Get spatial position from envelope
			numChannels = this.prGetNumChannels(index);
			spatialPos = this.prGetSpatialPosition(arc, index, arcTimeNorm, numChannels);

			// Convert spatial position to pan (-1 to 1 for stereo, 0 to 1 for multichannel)
			if(numChannels <= 2) {
				// Stereo: channel 0 = left (-1), channel 1 = right (1)
				pan = spatialPos.linlin(0, (numChannels - 1).max(1), -1, 1);
			} {
				// Multichannel: normalize to 0-1 range for PanAz
				// PanAz uses -1 to 1 where the value wraps around
				pan = spatialPos.linlin(0, numChannels - 1, -1, 1);
			};

			// Check if this is a VarSaw synth (supports width parameter)
			synthDef = data.getPageSynthDefForArc(playbackPageIndex, index);
			isVarSaw = synthDef.asString.containsi("VarSaw");

			if(isVarSaw) {
				// Get width from envelope or default
				widthVal = this.prGetWidth(arc, index, arcTimeNorm);
				try { synth.set(\freq, freq, \amp, amp * masterAmp, \pan, pan, \width, widthVal) };
			} {
				try { synth.set(\freq, freq, \amp, amp * masterAmp, \pan, pan) };
			};
		} {
			// Outside arc range - only silence if was previously active (skip redundant updates)
			if(synthsActive[index] == true) {
				try { synth.set(\amp, 0) };
				synthsActive[index] = false;
			};
		};
	}

	prGetNumChannels { |arcIndex|
		var synthDef, name;

		if(data.notNil) {
			synthDef = data.getPageSynthDefForArc(playbackPageIndex, arcIndex);
			name = synthDef.asString;

			if(name.contains("24ch")) { ^24 };
			if(name.contains("15ch")) { ^15 };
			if(name.contains("12ch")) { ^12 };
			if(name.contains("8ch")) { ^8 };
			if(name.contains("4ch")) { ^4 };
			if(name.contains("3ch")) { ^3 };
			if(name.contains("2ch")) { ^2 };
		};

		^2  // Default stereo
	}

	prGetSpatialPosition { |arc, arcIndex, timeNorm, numChannels|
		var spatialEnv, arcPixelLength, currentX;
		var firstX = arc.first[\x];
		var lastX = arc.last[\x];

		// Get spatial envelope for this arc from playback page
		if(data.notNil) {
			spatialEnv = data.getPageSpatialEnvelope(playbackPageIndex, arcIndex);
		};

		// If no envelope, default to center channel
		if(spatialEnv.isNil or: { spatialEnv.size == 0 }) {
			^((numChannels - 1) / 2)  // Center position
		};

		// Convert timeNorm to pixel position within arc (use abs for right-to-left arcs)
		arcPixelLength = (lastX - firstX).abs.max(0.001);
		currentX = timeNorm * arcPixelLength;

		// Find surrounding spatial points and interpolate
		spatialEnv.do { |pt, i|
			if(i < (spatialEnv.size - 1)) {
				var pt1 = pt;
				var pt2 = spatialEnv[i + 1];
				if(currentX >= pt1[\x] and: { currentX <= pt2[\x] }) {
					var interpFactor = (currentX - pt1[\x]) / (pt2[\x] - pt1[\x]).max(0.001);
					var channel = pt1[\channel].blend(pt2[\channel], interpFactor);
					^channel.clip(0, numChannels - 1)
				};
			};
		};

		// Edge cases
		if(spatialEnv.size > 0) {
			if(currentX <= spatialEnv.first[\x]) {
				^spatialEnv.first[\channel].clip(0, numChannels - 1)
			};
			if(currentX >= spatialEnv.last[\x]) {
				^spatialEnv.last[\channel].clip(0, numChannels - 1)
			};
		};

		^((numChannels - 1) / 2)  // Default center
	}

	prInterpolateFreq { |arc, timeNorm|
		// Use sequential index interpolation (follows drawing order)
		// This preserves the intended frequency path for loops and curves
		var numPoints = arc.size;
		var floatIndex = timeNorm * (numPoints - 1);
		var index1 = floatIndex.floor.asInteger.clip(0, numPoints - 2);
		var index2 = (index1 + 1).clip(0, numPoints - 1);
		var frac = floatIndex - index1;
		var pt1 = arc[index1];
		var pt2 = arc[index2];
		var y1, y2, yNorm, freq;

		// Get Y values - handle both old (freq) and new (normalized y) formats
		y1 = this.prGetYNorm(pt1);
		y2 = this.prGetYNorm(pt2);

		// Linear interpolation of Y position
		yNorm = y1 + ((y2 - y1) * frac);
		yNorm = yNorm.clip(0, 1);

		// Convert Y position to frequency using current scale
		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			freq = data.yToFreqWithTable(yNorm, data.getFrequencyRangeMin, data.getFrequencyRangeMax);
		} {
			// Default exponential mapping 20-7500 Hz
			freq = yNorm.linexp(0, 1, 20, 7500);
		};

		^freq
	}

	// Get normalized Y (0-1) from arc point - handles both old and new formats
	prGetYNorm { |pt|
		// If freq is provided and > 1 (valid Hz value), use it
		// Otherwise use normalized y (0-1)
		if(pt[\freq].notNil and: { pt[\freq] > 1 }) {
			// Freq provided - convert Hz to normalized y
			^(pt[\freq]).explin(20, 7500, 0, 1).clip(0, 1)
		} {
			// Use normalized y directly
			^(pt[\y] ?? 0.5)
		}
	}

	// X-based frequency interpolation for loops and complex paths
	prInterpolateFreqByX { |arc, targetX|
		var numPoints = arc.size;
		var prevPt, nextPt, prevIdx, nextIdx;
		var minDist = inf;

		// Find the two adjacent points that bracket targetX
		// For loops, this finds the first pair encountered in drawing order
		(numPoints - 1).do { |i|
			var pt1 = arc[i];
			var pt2 = arc[i + 1];
			var x1 = pt1[\x];
			var x2 = pt2[\x];
			var minX = min(x1, x2);
			var maxX = max(x1, x2);

			if(targetX >= minX and: { targetX <= maxX }) {
				// Found a segment containing targetX
				var frac = if(x2 != x1) {
					(targetX - x1) / (x2 - x1)
				} { 0 };
				var y1 = this.prGetYNorm(pt1);
				var y2 = this.prGetYNorm(pt2);
				var yNorm = y1 + ((y2 - y1) * frac.abs);
				var freq;

				yNorm = yNorm.clip(0, 1);

				// Convert Y position to frequency using current scale
				if(data.notNil and: { data.isFrequencyTableEnabled }) {
					freq = data.yToFreqWithTable(yNorm, data.getFrequencyRangeMin, data.getFrequencyRangeMax);
				} {
					freq = yNorm.linexp(0, 1, 20, 7500);
				};

				^freq
			};
		};

		// Fallback: find closest point if no bracketing segment found
		arc.do { |pt, i|
			var dist = (pt[\x] - targetX).abs;
			if(dist < minDist) {
				minDist = dist;
				prevPt = pt;
			};
		};

		// Convert fallback point to frequency
		if(prevPt.notNil) {
			var yNorm = this.prGetYNorm(prevPt);
			if(data.notNil and: { data.isFrequencyTableEnabled }) {
				^data.yToFreqWithTable(yNorm, data.getFrequencyRangeMin, data.getFrequencyRangeMax)
			} {
				^yNorm.linexp(0, 1, 20, 7500)
			}
		};

		^440  // Ultimate fallback
	}

	prGetAmplitude { |arc, arcIndex, timeNorm|
		var ampEnv, ampEnvList, baseAmp, scaler;

		// Try to get amplitude envelope for this arc from playback page
		if(data.notNil) {
			ampEnvList = data.getPageAmplitudeEnvelope(playbackPageIndex, arcIndex);
		};

		// If amplitude envelope exists and has points, interpolate
		if(ampEnvList.notNil and: { ampEnvList.size > 0 }) {
			baseAmp = this.prInterpolateAmpEnvelope(ampEnvList, arc, timeNorm);
		} {
			// Default: simple fade in/out envelope
			baseAmp = this.prDefaultAmplitude(timeNorm);
		};

		// Apply per-arc amplitude scaler
		scaler = if(data.notNil) { data.getPageAmplitudeScaler(playbackPageIndex, arcIndex) } { 1.0 };

		^(baseAmp * scaler)
	}

	prInterpolateAmpEnvelope { |ampEnv, arc, timeNorm|
		var arcPixelLength, currentX, amp;
		var firstX = arc.first[\x];
		var lastX = arc.last[\x];

		// Convert timeNorm to pixel position within arc (use abs for right-to-left arcs)
		arcPixelLength = (lastX - firstX).abs.max(0.001);
		currentX = timeNorm * arcPixelLength;

		// Find surrounding amplitude points
		ampEnv.do { |pt, i|
			if(i < (ampEnv.size - 1)) {
				var pt1 = pt;
				var pt2 = ampEnv[i + 1];
				if(currentX >= pt1[\x] and: { currentX <= pt2[\x] }) {
					var interpFactor = (currentX - pt1[\x]) / (pt2[\x] - pt1[\x]).max(0.001);
					amp = pt1[\amp].blend(pt2[\amp], interpFactor);
					^amp.clip(0, 1)
				};
			};
		};

		// Edge cases
		if(ampEnv.size > 0) {
			if(currentX <= ampEnv.first[\x]) {
				^ampEnv.first[\amp].clip(0, 1)
			};
			if(currentX >= ampEnv.last[\x]) {
				^ampEnv.last[\amp].clip(0, 1)
			};
		};

		^1.0
	}

	prDefaultAmplitude { |timeNorm|
		// Minimal anti-click ramp
		var fadeTime = 0.002;  // 0.2% - just enough to prevent clicks

		if(timeNorm < fadeTime) {
			^(timeNorm / fadeTime)
		};

		if(timeNorm > (1 - fadeTime)) {
			^((1 - timeNorm) / fadeTime)
		};

		^1.0
	}

	prGetWidth { |arc, arcIndex, timeNorm|
		var widthEnv, widthEnvList;

		// Try to get width envelope for this arc from playback page
		if(data.notNil) {
			widthEnvList = data.getPageWidthEnvelope(playbackPageIndex, arcIndex);
		};

		// If width envelope exists and has points, interpolate
		if(widthEnvList.notNil and: { widthEnvList.size > 0 }) {
			^this.prInterpolateWidthEnvelope(widthEnvList, arc, timeNorm)
		};

		// Default: constant 50% width
		^0.5
	}

	prInterpolateWidthEnvelope { |widthEnv, arc, timeNorm|
		var arcPixelLength, currentX, w;
		var firstX = arc.first[\x];
		var lastX = arc.last[\x];

		// Convert timeNorm to pixel position within arc (use abs for right-to-left arcs)
		arcPixelLength = (lastX - firstX).abs.max(0.001);
		currentX = timeNorm * arcPixelLength;

		// Find surrounding width points
		widthEnv.do { |pt, i|
			if(i < (widthEnv.size - 1)) {
				var pt1 = pt;
				var pt2 = widthEnv[i + 1];
				if(currentX >= pt1[\x] and: { currentX <= pt2[\x] }) {
					var interpFactor = (currentX - pt1[\x]) / (pt2[\x] - pt1[\x]).max(0.001);
					w = pt1[\width].blend(pt2[\width], interpFactor);
					^w.clip(0, 1)
				};
			};
		};

		// Edge cases
		if(widthEnv.size > 0) {
			if(currentX <= widthEnv.first[\x]) {
				^widthEnv.first[\width].clip(0, 1)
			};
			if(currentX >= widthEnv.last[\x]) {
				^widthEnv.last[\width].clip(0, 1)
			};
		};

		^0.5
	}

	prStartCursor {
		cursorRoutine = fork {
			var frameTime = 1 / visualUpdateRate;

			while { isPlaying } {
				{
					if(gui.notNil) {
						gui.setPlaybackPosition(playbackPosition);
					};
					onPositionChanged.value(playbackPosition);
				}.defer;

				frameTime.wait;
			};
		};
	}

	// ========== Utility ==========

	loadSynthDefs { |useOversampling = false|
		// Load SynthDefs based on oscillator mode
		if(useOversampling) {
			this.prLoadOversampledSynthDefs;
		} {
			this.prLoadStandardSynthDefs;
		};
	}

	// ========== Standard Oscillator SynthDefs (no extensions required) ==========

	prLoadStandardSynthDefs {
		// UPIC Wavetable (default) - uses Osc.ar for proper wavetable interpolation
		if(SynthDescLib.global[\upicWavetable].isNil) {
			SynthDef(\upicWavetable, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, out = 0, detune = 0;
				var sig, env;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);

				// Osc.ar uses wavetable format buffer with proper interpolation
				sig = Osc.ar(bufnum, freq);
				// Add detuned oscillator for chorus (only if detune > 0)
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));

				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// UPIC Wavetable 2ch - Stereo with Pan2
		if(SynthDescLib.global[\upicWavetable2ch].isNil) {
			SynthDef(\upicWavetable2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// UPIC Wavetable 3ch
		if(SynthDescLib.global[\upicWavetable3ch].isNil) {
			SynthDef(\upicWavetable3ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(3, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 4ch
		if(SynthDescLib.global[\upicWavetable4ch].isNil) {
			SynthDef(\upicWavetable4ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(4, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 8ch
		if(SynthDescLib.global[\upicWavetable8ch].isNil) {
			SynthDef(\upicWavetable8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(8, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 12ch
		if(SynthDescLib.global[\upicWavetable12ch].isNil) {
			SynthDef(\upicWavetable12ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(12, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 15ch
		if(SynthDescLib.global[\upicWavetable15ch].isNil) {
			SynthDef(\upicWavetable15ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(15, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 24ch
		if(SynthDescLib.global[\upicWavetable24ch].isNil) {
			SynthDef(\upicWavetable24ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				sig = Osc.ar(bufnum, freq);
				sig = sig + (Osc.ar(bufnum, freq * (1 + detune)) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(24, sig, panLag, 1, 2, 0));
			}).add;
		};

		// Saw (Mono) - Standard band-limited sawtooth
		if(SynthDescLib.global[\upicSawBL].isNil) {
			SynthDef(\upicSawBL, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, out = 0;
				var sig, sig2, env, freqLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);

				sig = Saw.ar(freqLag);
				sig2 = Saw.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// Saw (2ch) - Stereo
		if(SynthDescLib.global[\upicSawBL2ch].isNil) {
			SynthDef(\upicSawBL2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1;
				var sig, sig2, env, freqLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);

				sig = Saw.ar(freqLag);
				sig2 = Saw.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// Saw (8ch)
		if(SynthDescLib.global[\upicSawBL8ch].isNil) {
			SynthDef(\upicSawBL8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);

				sig = Saw.ar(freqLag);
				sig2 = Saw.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(8, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (Mono) - Standard variable-width sawtooth
		if(SynthDescLib.global[\upicVarSaw].isNil) {
			SynthDef(\upicVarSaw, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, out = 0;
				var sig, sig2, env, freqLag, widthLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);

				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// VarSaw (2ch) - Stereo
		if(SynthDescLib.global[\upicVarSaw2ch].isNil) {
			SynthDef(\upicVarSaw2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5;
				var sig, sig2, env, freqLag, widthLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);

				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// VarSaw (8ch)
		if(SynthDescLib.global[\upicVarSaw8ch].isNil) {
			SynthDef(\upicVarSaw8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);

				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(8, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (3ch)
		if(SynthDescLib.global[\upicVarSaw3ch].isNil) {
			SynthDef(\upicVarSaw3ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(3, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (4ch)
		if(SynthDescLib.global[\upicVarSaw4ch].isNil) {
			SynthDef(\upicVarSaw4ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(4, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (12ch)
		if(SynthDescLib.global[\upicVarSaw12ch].isNil) {
			SynthDef(\upicVarSaw12ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(12, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (15ch)
		if(SynthDescLib.global[\upicVarSaw15ch].isNil) {
			SynthDef(\upicVarSaw15ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(15, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSaw (24ch)
		if(SynthDescLib.global[\upicVarSaw24ch].isNil) {
			SynthDef(\upicVarSaw24ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSaw.ar(freqLag, width: widthLag);
				sig2 = VarSaw.ar(freqLag * 1.002, width: widthLag);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(24, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		"NUPICPlayback: Standard SynthDefs loaded (no extensions required)".postln;
	}

	// ========== Oversampled Oscillator SynthDefs (requires OversamplingOscillators) ==========

	prLoadOversampledSynthDefs {
		// UPIC Wavetable (default) - reads from buffer using OscOS with oversampling
		if(SynthDescLib.global[\upicWavetable].isNil) {
			SynthDef(\upicWavetable, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, out = 0, detune = 0;
				var sig, env, phase;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				// Add detuned oscillator for chorus (only if detune > 0)
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// UPIC Wavetable 2ch
		if(SynthDescLib.global[\upicWavetable2ch].isNil) {
			SynthDef(\upicWavetable2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// UPIC Wavetable 3ch
		if(SynthDescLib.global[\upicWavetable3ch].isNil) {
			SynthDef(\upicWavetable3ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(3, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 4ch
		if(SynthDescLib.global[\upicWavetable4ch].isNil) {
			SynthDef(\upicWavetable4ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(4, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 8ch
		if(SynthDescLib.global[\upicWavetable8ch].isNil) {
			SynthDef(\upicWavetable8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(8, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 12ch
		if(SynthDescLib.global[\upicWavetable12ch].isNil) {
			SynthDef(\upicWavetable12ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(12, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 15ch
		if(SynthDescLib.global[\upicWavetable15ch].isNil) {
			SynthDef(\upicWavetable15ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(15, sig, panLag, 1, 2, 0));
			}).add;
		};

		// UPIC Wavetable 24ch
		if(SynthDescLib.global[\upicWavetable24ch].isNil) {
			SynthDef(\upicWavetable24ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0, detune = 0;
				var sig, env, phase, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				panLag = Lag.kr(pan, 0.05);
				phase = LFSaw.ar(freq).range(0, 1);
				sig = OscOS.ar(bufnum, phase, oversample: 2);
				sig = sig + (OscOS.ar(bufnum, LFSaw.ar(freq * (1 + detune)).range(0, 1), oversample: 2) * 0.3 * (detune > 0));
				sig = sig * env * amp * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(24, sig, panLag, 1, 2, 0));
			}).add;
		};

		// SawBL (Mono) - Band-limited sawtooth with oversampling
		if(SynthDescLib.global[\upicSawBL].isNil) {
			SynthDef(\upicSawBL, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, out = 0;
				var sig, sig2, env, freqLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);

				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// SawBL (2ch) - Stereo
		if(SynthDescLib.global[\upicSawBL2ch].isNil) {
			SynthDef(\upicSawBL2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1;
				var sig, sig2, env, freqLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);

				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// SawBL (3ch)
		if(SynthDescLib.global[\upicSawBL3ch].isNil) {
			SynthDef(\upicSawBL3ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(3, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// SawBL (4ch)
		if(SynthDescLib.global[\upicSawBL4ch].isNil) {
			SynthDef(\upicSawBL4ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(4, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// SawBL (8ch)
		if(SynthDescLib.global[\upicSawBL8ch].isNil) {
			SynthDef(\upicSawBL8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(8, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// SawBL (12ch)
		if(SynthDescLib.global[\upicSawBL12ch].isNil) {
			SynthDef(\upicSawBL12ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(12, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// SawBL (15ch)
		if(SynthDescLib.global[\upicSawBL15ch].isNil) {
			SynthDef(\upicSawBL15ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(15, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// SawBL (24ch)
		if(SynthDescLib.global[\upicSawBL24ch].isNil) {
			SynthDef(\upicSawBL24ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, panWidth = 2;
				var sig, sig2, env, freqLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = SawBL.ar(freqLag);
				sig2 = SawBL.ar(freqLag * 1.002);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(24, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (Mono) - Variable-width sawtooth with oversampling
		if(SynthDescLib.global[\upicVarSaw].isNil) {
			SynthDef(\upicVarSaw, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, out = 0;
				var sig, sig2, env, freqLag, widthLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);

				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(out, sig);
			}).add;
		};

		// VarSawOS (2ch) - Stereo
		if(SynthDescLib.global[\upicVarSaw2ch].isNil) {
			SynthDef(\upicVarSaw2ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5;
				var sig, sig2, env, freqLag, widthLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);

				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, Pan2.ar(sig, pan));
			}).add;
		};

		// VarSawOS (8ch)
		if(SynthDescLib.global[\upicVarSaw8ch].isNil) {
			SynthDef(\upicVarSaw8ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;

				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);

				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);

				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;

				Out.ar(0, PanAz.ar(8, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (3ch)
		if(SynthDescLib.global[\upicVarSaw3ch].isNil) {
			SynthDef(\upicVarSaw3ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(3, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (4ch)
		if(SynthDescLib.global[\upicVarSaw4ch].isNil) {
			SynthDef(\upicVarSaw4ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(4, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (12ch)
		if(SynthDescLib.global[\upicVarSaw12ch].isNil) {
			SynthDef(\upicVarSaw12ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(12, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (15ch)
		if(SynthDescLib.global[\upicVarSaw15ch].isNil) {
			SynthDef(\upicVarSaw15ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(15, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		// VarSawOS (24ch)
		if(SynthDescLib.global[\upicVarSaw24ch].isNil) {
			SynthDef(\upicVarSaw24ch, {
				arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, width = 0.5, panWidth = 2;
				var sig, sig2, env, freqLag, widthLag, panLag;
				env = EnvGen.kr(Env.asr(0.002, 1, 0.002), gate, doneAction: 2);
				freqLag = Lag.kr(freq, 0.05);
				widthLag = Lag.kr(width, 0.05);
				panLag = Lag.kr(pan, 0.05);
				sig = VarSawOS.ar(freqLag, width: widthLag, oversample: 2);
				sig2 = VarSawOS.ar(freqLag * 1.002, width: widthLag, oversample: 2);
				sig = sig + (sig2 * 0.3);
				sig = sig * amp * env * (1 / numSynths.sqrt.max(1));
				sig = sig.tanh * 0.8;
				Out.ar(0, PanAz.ar(24, sig, panLag, 1, panWidth, 0));
			}).add;
		};

		"NUPICPlayback: Oversampled SynthDefs loaded (OversamplingOscillators)".postln;
	}

	// ========== NRT Helper Methods ==========

	prGenerateNRTScore { |arcList, scoreDuration, numArcs, pageIndex|
		// EFFICIENT PATTERN-BASED APPROACH:
		// Instead of generating millions of n_set commands (which hangs),
		// we create synths with EMBEDDED frequency envelopes.
		// Each synth handles its own frequency trajectory internally.

		var score, nodeID, silenceNodeID;
		var freqMin, freqMax;
		var bufferStartID;
		var maxArcEndTime = 0, arcsProcessed = 0;
		var arcEndTimes = List.new;

		score = List.new;
		nodeID = 1000;
		bufferStartID = 100;

		// Get frequency range
		freqMin = if(data.notNil) { data.getFrequencyRangeMin } { 20 };
		freqMax = if(data.notNil) { data.getFrequencyRangeMax } { 7500 };

		"NRT: Generating efficient score with embedded envelopes...".postln;
		"NRT: arcList.size=%, numArcs=%, duration=% seconds".format(arcList.size, numArcs, scoreDuration).postln;

		// Add synthdef with envelope support at time 0
		score.add([0.0, [\d_recv, this.prNRTEnvSynthDefBytes]]);
		score.add([0.0, [\d_recv, this.prNRTSilenceSynthDefBytes]]);

		// Pre-calculate arc data (do this ONCE per arc, not per frame)
		arcList.do { |arc, i|
			if(arc.notNil and: { arc.size > 1 }) {
				var xValues, yValues, arcStart, arcEnd, arcDuration;
				var startFreq, endFreq;
				var wtPath;
				var arcEndAbsolute;

				// Collect x and y values (do this ONCE)
				xValues = arc.collect { |pt| pt[\x] };
				yValues = arc.collect { |pt| pt[\y] ?? 0.5 };

				arcStart = xValues.minItem;
				arcEnd = xValues.maxItem;
				arcDuration = (arcEnd - arcStart) * scoreDuration;
				arcEndAbsolute = arcEnd * scoreDuration;

				// Track end times for diagnostics
				arcEndTimes.add(arcEndAbsolute);
				if(arcEndAbsolute > maxArcEndTime) {
					maxArcEndTime = arcEndAbsolute;
				};
				arcsProcessed = arcsProcessed + 1;

				// Get frequency at start and end of arc
				startFreq = this.prYToFreq(yValues.first, freqMin, freqMax);
				endFreq = this.prYToFreq(yValues.last, freqMin, freqMax);

				// Write wavetable file
				wtPath = this.prWriteNRTWavetableFile(i, pageIndex);
				score.add([0.0, [\b_allocRead, bufferStartID + i, wtPath]]);

				// Create synth with embedded envelope at arc start time
				// The synth will handle its own frequency glide and amplitude envelope
				score.add([
					arcStart * scoreDuration,
					[\s_new, \nupicNRTEnvSynth, nodeID + i, 0, 0,
						\startFreq, startFreq,
						\endFreq, endFreq,
						\dur, arcDuration.max(0.01),
						\amp, masterAmp / numArcs.sqrt.max(1),
						\pan, 0,
						\bufnum, bufferStartID + i
					]
				]);

				// Progress feedback every 100 arcs
				if(i > 0 and: { i % 100 == 0 }) {
					"NRT: Processed % / % arcs (maxEndTime so far: %s)".format(i, numArcs, maxArcEndTime.round(0.01)).postln;
				};
			};
		};

		// Diagnostic output: arc timing distribution
		"NRT: Processed % arcs total".format(arcsProcessed).postln;
		"NRT: Maximum arc end time: % seconds (normalized: %)".format(maxArcEndTime.round(0.01), (maxArcEndTime / scoreDuration).round(0.001)).postln;
		if(arcEndTimes.size > 0) {
			var sortedEnds = arcEndTimes.sort;
			var arcsAfter40 = sortedEnds.select { |t| t > 40 }.size;
			var arcsAfter50 = sortedEnds.select { |t| t > 50 }.size;
			"NRT: Arcs ending after 40s: %, after 50s: %".format(arcsAfter40, arcsAfter50).postln;
		};

		// Add silence synth for full duration
		silenceNodeID = nodeID + numArcs;
		score.add([0.001, [\s_new, \nupicNRTSilence, silenceNodeID, 0, 0]]);

		// Release silence synth at end
		score.add([scoreDuration + 0.1, [\n_set, silenceNodeID, \gate, 0]]);

		// End marker
		score.add([scoreDuration + 0.2, [\c_set, 0, 0]]);

		"NRT: Score generated with % entries (was ~% with old method)".format(
			score.size,
			(scoreDuration * 60 * numArcs).asInteger
		).postln;

		// Sort by time
		score = score.sort { |a, b| a[0] <= b[0] };

		^score.asArray
	}

	prYToFreq { |yNorm, freqMin, freqMax|
		// Convert normalized Y (0-1) to frequency
		yNorm = yNorm.clip(0, 1);
		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			^data.yToFreqWithTable(yNorm, freqMin, freqMax);
		} {
			^yNorm.linexp(0, 1, freqMin, freqMax);
		};
	}

	prNRTEnvSynthDefBytes {
		// Synth with EMBEDDED frequency envelope - no external n_set needed!
		// Frequency glides from startFreq to endFreq over dur seconds
		// Amplitude has attack/release envelope built in
		^SynthDef(\nupicNRTEnvSynth, {
			arg startFreq = 440, endFreq = 440, dur = 1, amp = 0.1, pan = 0, bufnum = 0;
			var sig, env, freqEnv, freq;
			var attackTime = 0.005;
			var releaseTime = 0.01;
			var sustainTime = (dur - attackTime - releaseTime).max(0);

			// Frequency glide envelope (line from start to end)
			freqEnv = EnvGen.kr(
				Env([startFreq, endFreq], [dur.max(0.001)], \exp),
				doneAction: 0
			);
			freq = freqEnv;

			// Amplitude envelope with attack/release (sustainTime clamped to avoid negatives)
			env = EnvGen.kr(
				Env([0, 1, 1, 0], [attackTime, sustainTime, releaseTime], [\sin, \lin, \sin]),
				doneAction: 2
			);

			// Wavetable oscillator
			sig = Osc.ar(bufnum, freq);
			sig = sig * env * amp;
			sig = sig.tanh * 0.8;

			Out.ar(0, Pan2.ar(sig, pan));
		}).asBytes;
	}

	prGetArcFreqAtTime { |arc, timeNorm, freqMin, freqMax|
		var numPoints, floatIndex, index1, index2, frac, pt1, pt2, y1, y2, yNorm, freq;

		numPoints = arc.size;
		floatIndex = timeNorm * (numPoints - 1);
		index1 = floatIndex.floor.asInteger.clip(0, numPoints - 2);
		index2 = (index1 + 1).clip(0, numPoints - 1);
		frac = floatIndex - index1;
		pt1 = arc[index1];
		pt2 = arc[index2];

		// Get Y values
		y1 = pt1[\y] ?? 0.5;
		y2 = pt2[\y] ?? 0.5;

		// Interpolate
		yNorm = y1 + ((y2 - y1) * frac);
		yNorm = yNorm.clip(0, 1);

		// Convert to frequency
		if(data.notNil and: { data.isFrequencyTableEnabled }) {
			freq = data.yToFreqWithTable(yNorm, freqMin, freqMax);
		} {
			freq = yNorm.linexp(0, 1, freqMin, freqMax);
		};

		^freq
	}

	prGetNRTAmplitude { |arcTimeNorm|
		// Simple fade envelope for NRT
		var fadeTime = 0.01;

		if(arcTimeNorm < fadeTime) {
			^(arcTimeNorm / fadeTime)
		};

		if(arcTimeNorm > (1 - fadeTime)) {
			^((1 - arcTimeNorm) / fadeTime)
		};

		^1.0
	}

	prWriteOSCFileManually { |scoreArray, path|
		// Write OSC file manually with correct bundle timestamps
		// This avoids issues with Score.write that can put all messages at time 0
		var file, groupedByTime, sortedTimes, lastTime = 0;

		file = File(path, "wb");
		if(file.isOpen.not) {
			"NRT: Failed to open OSC file for writing".error;
			^nil
		};

		// Group entries by time (rounded to avoid floating point issues)
		groupedByTime = Dictionary.new;
		scoreArray.do { |entry|
			var time = entry[0];
			var msg = entry[1];
			var key = time.round(0.00001);  // Round to avoid fp comparison issues
			if(groupedByTime[key].isNil) {
				groupedByTime[key] = List.new;
			};
			groupedByTime[key].add([time, msg]);
		};

		// Sort times
		sortedTimes = groupedByTime.keys.asArray.sort;

		"NRT: Writing % unique timestamps (from % to %)".format(
			sortedTimes.size,
			sortedTimes.first.round(0.01),
			sortedTimes.last.round(0.01)
		).postln;

		// Write each bundle with error handling
		sortedTimes.do { |timeKey, i|
			var entries = groupedByTime[timeKey];
			var time = entries[0][0];  // Use actual time from first entry
			var msgs = entries.collect { |e| e[1] };

			try {
				this.prWriteOSCBundle(file, time, msgs);
				lastTime = time;
			} { |error|
				"NRT: Error writing bundle at time %: %".format(time, error).error;
			};

			// Progress indicator every 1000 bundles
			if(i % 1000 == 0) {
				"NRT: Written % / % bundles (time: %)".format(i, sortedTimes.size, time.round(0.01)).postln;
			};
		};

		file.close;
		"NRT: Finished writing OSC file with % bundles, last time: %".format(sortedTimes.size, lastTime.round(0.01)).postln;
	}

	prWriteOSCBundle { |file, time, msgs|
		// Write a single OSC bundle with the given time and messages
		var bundleData, timetag, msgData, totalSize;

		// Calculate timetag (NTP format: seconds since 1900 as 64-bit fixed point)
		// For NRT, we use a simple conversion: time in seconds -> NTP timetag
		// NRT expects: upper 32 bits = seconds, lower 32 bits = fractional
		timetag = this.prTimeToNTPTag(time);

		// Build message data first to calculate total size
		msgData = List.new;
		msgs.do { |msg, i|
			var encoded;
			try {
				encoded = msg.asRawOSC;
				if(encoded.isNil or: { encoded.size == 0 }) {
					"NRT: Warning - empty OSC message at time %, msg %: %".format(time, i, msg).warn;
				} {
					// asRawOSC already returns 4-byte aligned data, no extra padding needed
					msgData.add(encoded);
				};
			} { |error|
				"NRT: Error encoding message at time %, msg %: % - %".format(time, i, msg, error).error;
			};
		};

		// Calculate total bundle size:
		// 8 bytes for "#bundle\0" + 8 bytes for timetag + sum of (4 + size) for each msg
		// Note: asRawOSC returns data already padded to 4-byte boundary
		totalSize = 16;  // #bundle + timetag
		msgData.do { |encoded| totalSize = totalSize + 4 + encoded.size };

		// Write bundle size (not including the size field itself)
		file.putInt32(totalSize);

		// Write bundle header
		file.write("#bundle");
		file.putChar(0.asAscii);  // Null terminator

		// Write timetag (8 bytes, big-endian)
		file.putInt32(timetag[0]);  // Upper 32 bits (seconds)
		file.putInt32(timetag[1]);  // Lower 32 bits (fraction)

		// Write each message
		msgData.do { |encoded|
			// Write message size
			file.putInt32(encoded.size);

			// Write message data (must write bytes individually, not as string)
			encoded.do { |byte|
				file.putInt8(byte);
			};
		};
	}

	prTimeToNTPTag { |time|
		// Convert time in seconds to NTP timetag
		// NTP epoch is January 1, 1900
		// For NRT, we add an offset to avoid time 0 (which means "immediately")
		var seconds, fraction;

		// Add 1 second offset so time 0 in the score becomes timetag 1
		// (timetag 1 means "immediately" in OSC, but for NRT it's fine)
		time = time + 1.0;

		seconds = time.floor.asInteger;
		fraction = ((time - seconds) * (2**32)).asInteger;

		^[seconds, fraction]
	}

	prNRTSynthDefBytes {
		// Return SynthDef bytes for NRT rendering - uses wavetable buffers
		// Note: This returns bytes for the main synth. The silence synth is added separately.
		^SynthDef(\nupicNRTSynth, {
			arg freq = 440, amp = 0.1, pan = 0, gate = 1, numSynths = 1, bufnum = 0;
			var sig, env;

			env = EnvGen.kr(Env.asr(0.002, 1, 0.05), gate, doneAction: 2);
			// Use Osc.ar for wavetable playback (same as regular synths)
			sig = Osc.ar(bufnum, freq);
			sig = sig * env * amp;
			sig = sig.tanh * 0.8;

			Out.ar(0, Pan2.ar(sig, pan));
		}).asBytes;
	}

	prNRTSilenceSynthDefBytes {
		// Return SynthDef bytes for a synth that keeps NRT running for full duration
		// Uses gate-controlled envelope that runs until explicitly released
		// Uses audible frequency (100 Hz) at very low amplitude to ensure NRT renders
		^SynthDef(\nupicNRTSilence, {
			arg gate = 1;
			var env = EnvGen.kr(Env.asr(0.001, 1, 0.001), gate, doneAction: 2);
			// Use 100 Hz (audible) at -80dB (0.0001) - inaudible but NRT will render it
			var sig = SinOsc.ar(100) * 0.0001 * env;
			Out.ar(0, sig ! 2);
		}).asBytes;
	}

	prNRTAnchorSynthDefBytes {
		// A synth that exists briefly then frees itself
		// Used to create "anchor points" in the score that force NRT to process to that time
		// NRT will process any bundle regardless of audio output
		^SynthDef(\nupicNRTAnchor, {
			// Use Line to create a brief envelope then free - produces DC silence
			var env = Line.kr(1, 0, 0.01, doneAction: 2);
			// Output true silence (DC.ar(0)) - NRT will still process to this time
			Out.ar(0, DC.ar(0) ! 2);
		}).asBytes;
	}

	prWriteNRTWavetableFile { |arcIndex, pageIndex|
		// Write wavetable data to temp file for NRT buffer loading
		var wtData, sig, wtFormat, path, sf;
		var targetSize = 2048;

		// Get wavetable data for this arc from the specific page
		if(data.notNil) {
			wtData = data.getPageWavetableForArc(pageIndex, arcIndex);
		};

		if(wtData.isNil) {
			// Default sine wavetable
			wtData = Array.fill(targetSize, { |i| sin(2pi * i / targetSize) });
		};

		// Resample to power-of-two size if needed
		if(wtData.size != targetSize) {
			wtData = this.prResampleWavetable(wtData, targetSize);
		};

		// Convert to wavetable format (doubles size for Osc.ar)
		sig = Signal.newFrom(wtData);
		wtFormat = sig.asWavetable;

		// Write to temp file
		path = Platform.defaultTempDir +/+ "nupic_nrt_wt_" ++ arcIndex ++ "_" ++ Date.getDate.stamp ++ ".aiff";
		sf = SoundFile.new;
		sf.headerFormat = "AIFF";
		sf.sampleFormat = "float";
		sf.numChannels = 1;
		sf.sampleRate = 44100;

		if(sf.openWrite(path)) {
			sf.writeData(wtFormat);
			sf.close;
		} {
			"NRT: Failed to write wavetable file for arc %".format(arcIndex).warn;
		};

		^path
	}

	prResampleWavetable { |wtData, targetSize|
		// Resample wavetable to target size using linear interpolation
		^Array.fill(targetSize, { |i|
			var srcPos = i * (wtData.size - 1) / (targetSize - 1);
			var idx1 = srcPos.floor.asInteger.clip(0, wtData.size - 2);
			var idx2 = (idx1 + 1).clip(0, wtData.size - 1);
			var frac = srcPos - idx1;
			wtData[idx1] + ((wtData[idx2] - wtData[idx1]) * frac)
		})
	}

	prLoadNRTPlaybackSynthDef {
		// SynthDef for playing back rendered audio
		if(SynthDescLib.global[\nupicNRTPlayback].isNil) {
			SynthDef(\nupicNRTPlayback, {
				arg bufnum = 0, rate = 1, amp = 0.5, gate = 1;
				var sig, env;

				env = EnvGen.kr(Env.asr(0.01, 1, 0.01), gate, doneAction: 2);
				sig = PlayBuf.ar(2, bufnum, rate * BufRateScale.kr(bufnum), doneAction: 2);
				sig = sig * env * amp;

				Out.ar(0, sig);
			}).add;
		};
	}
}
