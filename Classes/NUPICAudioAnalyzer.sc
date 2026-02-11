/*
 * NUPICAudioAnalyzer - Audio analysis using FluCoMa
 *
 * Analyzes sound files and converts to arc data for nUPIC
 * Requires FluCoMa toolkit: https://www.flucoma.org/
 */

NUPICAudioAnalyzer {
	// Analysis buffers
	var <sourceBuffer;
	var <pitchBuffer;
	var <loudnessBuffer;
	var <spectralBuffer;
	var <onsetBuffer;

	// Analysis results
	var <pitchData;      // Array of [time, freq, confidence]
	var <loudnessData;   // Array of [time, loudness]
	var <spectralData;   // Array of [time, centroid, spread, ...]
	var <onsets;         // Array of onset times

	// Settings
	var <hopSize = 512;
	var <windowSize = 2048;
	var <minFreq = 20;
	var <maxFreq = 20000;
	var <confidenceThreshold = 0.7;

	// State
	var <server;
	var <filePath;
	var <duration;
	var <sampleRate;
	var <numChannels;

	// Callbacks
	var <>onAnalysisComplete;
	var <>onProgress;

	*new { |server|
		^super.new.init(server)
	}

	init { |argServer|
		server = argServer ? Server.default;
		pitchData = List.new;
		loudnessData = List.new;
		spectralData = List.new;
		onsets = List.new;
	}

	// ========== Loading ==========

	loadFile { |path, action|
		filePath = path;

		// Free existing buffer
		if(sourceBuffer.notNil) { sourceBuffer.free };

		Buffer.read(server, path, action: { |buf|
			sourceBuffer = buf;
			duration = buf.duration;
			sampleRate = buf.sampleRate;
			numChannels = buf.numChannels;

			"Loaded: % (% sec, % Hz, % ch)".format(
				path.basename, duration.round(0.01), sampleRate, numChannels
			).postln;

			action.value(buf);
		});
	}

	// ========== Analysis ==========

	analyze { |action|
		if(sourceBuffer.isNil) {
			"NUPICAudioAnalyzer: No file loaded".error;
			^this
		};

		// Check if FluCoMa is available
		if(\FluidBufPitch.asClass.isNil) {
			"NUPICAudioAnalyzer: FluCoMa not installed!".error;
			"Download from: https://www.flucoma.org/download/".postln;
			^this
		};

		"Starting FluCoMa analysis...".postln;

		// Run analyses in sequence
		this.prAnalyzePitch({
			this.prAnalyzeLoudness({
				this.prAnalyzeSpectral({
					this.prAnalyzeOnsets({
						"Analysis complete!".postln;
						onAnalysisComplete.value(this);
						action.value(this);
					});
				});
			});
		});
	}

	prAnalyzePitch { |action|
		var numFrames = (sourceBuffer.numFrames / hopSize).ceil.asInteger;

		"  Analyzing pitch...".postln;

		// Allocate pitch buffer (2 channels: pitch, confidence)
		pitchBuffer = Buffer.alloc(server, numFrames, 2);

		FluidBufPitch.processBlocking(
			server,
			source: sourceBuffer,
			features: pitchBuffer,
			windowSize: windowSize,
			hopSize: hopSize,
			minFreq: minFreq,
			maxFreq: maxFreq,
			algorithm: 2,  // YIN algorithm (more accurate)
			action: {
				// Read pitch data from buffer
				pitchBuffer.loadToFloatArray(action: { |data|
					pitchData.clear;
					(data.size / 2).do { |i|
						var time = i * hopSize / sampleRate;
						var freq = data[i * 2];
						var confidence = data[i * 2 + 1];
						pitchData.add([time, freq, confidence]);
					};
					"    Found % pitch frames".format(pitchData.size).postln;
					action.value;
				});
			}
		);
	}

	prAnalyzeLoudness { |action|
		var numFrames = (sourceBuffer.numFrames / hopSize).ceil.asInteger;

		"  Analyzing loudness...".postln;

		// Allocate loudness buffer (2 channels: loudness, truePeak)
		loudnessBuffer = Buffer.alloc(server, numFrames, 2);

		FluidBufLoudness.processBlocking(
			server,
			source: sourceBuffer,
			features: loudnessBuffer,
			windowSize: windowSize,
			hopSize: hopSize,
			action: {
				loudnessBuffer.loadToFloatArray(action: { |data|
					loudnessData.clear;
					(data.size / 2).do { |i|
						var time = i * hopSize / sampleRate;
						var loudness = data[i * 2];      // LUFS
						var truePeak = data[i * 2 + 1];  // dB
						loudnessData.add([time, loudness, truePeak]);
					};
					"    Found % loudness frames".format(loudnessData.size).postln;
					action.value;
				});
			}
		);
	}

	prAnalyzeSpectral { |action|
		var numFrames = (sourceBuffer.numFrames / hopSize).ceil.asInteger;

		"  Analyzing spectral shape...".postln;

		// Allocate spectral buffer (7 channels: centroid, spread, skewness, kurtosis, rolloff, flatness, crest)
		spectralBuffer = Buffer.alloc(server, numFrames, 7);

		FluidBufSpectralShape.processBlocking(
			server,
			source: sourceBuffer,
			features: spectralBuffer,
			windowSize: windowSize,
			hopSize: hopSize,
			action: {
				spectralBuffer.loadToFloatArray(action: { |data|
					spectralData.clear;
					(data.size / 7).do { |i|
						var time = i * hopSize / sampleRate;
						var centroid = data[i * 7];
						var spread = data[i * 7 + 1];
						var skewness = data[i * 7 + 2];
						var kurtosis = data[i * 7 + 3];
						var rolloff = data[i * 7 + 4];
						var flatness = data[i * 7 + 5];
						var crest = data[i * 7 + 6];
						spectralData.add([time, centroid, spread, skewness, kurtosis, rolloff, flatness, crest]);
					};
					"    Found % spectral frames".format(spectralData.size).postln;
					action.value;
				});
			}
		);
	}

	prAnalyzeOnsets { |action|
		"  Detecting onsets...".postln;

		// Allocate onset buffer
		onsetBuffer = Buffer.alloc(server, sourceBuffer.numFrames / hopSize);

		FluidBufOnsetSlice.processBlocking(
			server,
			source: sourceBuffer,
			indices: onsetBuffer,
			threshold: 0.5,
			metric: 9,  // Spectral flux
			action: {
				onsetBuffer.loadToFloatArray(action: { |data|
					onsets.clear;
					data.do { |sampleIdx|
						if(sampleIdx > 0) {
							onsets.add(sampleIdx / sampleRate);
						};
					};
					"    Found % onsets".format(onsets.size).postln;
					action.value;
				});
			}
		);
	}

	// ========== Arc Conversion ==========

	toArcs { |options|
		/*
		 * Convert analysis data to nUPIC arc format
		 *
		 * Options:
		 *   \mode - \pitch (pitch tracking), \spectral (centroid), \both
		 *   \segment - \continuous (one arc), \onsets (split at onsets)
		 *   \minConfidence - minimum pitch confidence (0-1)
		 *   \smoothing - number of frames to smooth
		 *   \timeScale - duration in nUPIC (default: analysis duration)
		 *   \freqMin, \freqMax - frequency range mapping
		 */

		var mode = options[\mode] ? \pitch;
		var segment = options[\segment] ? \continuous;
		var minConf = options[\minConfidence] ? confidenceThreshold;
		var smoothing = options[\smoothing] ? 3;
		var timeScale = options[\timeScale] ? duration;
		var freqMin = options[\freqMin] ? 20;
		var freqMax = options[\freqMax] ? 20000;

		var arcs = List.new;
		var currentArc = List.new;

		if(pitchData.size == 0) {
			"No analysis data - run analyze() first".error;
			^[]
		};

		// Process based on mode
		if(mode == \pitch or: { mode == \both }) {
			arcs = this.prPitchToArcs(minConf, smoothing, timeScale, segment);
		};

		if(mode == \spectral) {
			arcs = this.prSpectralToArcs(smoothing, timeScale, segment);
		};

		"Generated % arcs".format(arcs.size).postln;
		^arcs
	}

	prPitchToArcs { |minConf, smoothing, timeScale, segment|
		var arcs = List.new;
		var currentArc = List.new;
		var timeRatio = timeScale / duration;
		var onsetIdx = 0;
		var nextOnset = if(onsets.size > 0) { onsets[0] } { inf };
		var gapCounter = 0;
		var maxGapFrames = 5;  // Allow up to 5 low-confidence frames before breaking
		var minArcPoints = 10;  // Minimum points for a valid arc
		var lastValidFreq = 440;  // For interpolation
		var decimationFactor = 4;  // Only keep every Nth point to reduce density

		pitchData.do { |frame, i|
			var time = frame[0];
			var freq = frame[1];
			var confidence = frame[2];
			var xNorm = (time / duration).clip(0, 1);

			// Check for onset boundary
			if(segment == \onsets and: { time >= nextOnset }) {
				// Save current arc if it has enough points
				if(currentArc.size >= minArcPoints) {
					arcs.add(currentArc.asArray);
				};
				currentArc = List.new;
				gapCounter = 0;

				// Move to next onset
				onsetIdx = onsetIdx + 1;
				nextOnset = if(onsetIdx < onsets.size) { onsets[onsetIdx] } { inf };
			};

			// Check if this is a valid point
			if(confidence >= minConf and: { freq > 0 } and: { freq >= minFreq } and: { freq <= maxFreq }) {
				// Reset gap counter on valid point
				gapCounter = 0;
				lastValidFreq = freq;

				// Decimate: only add every Nth point
				if((i % decimationFactor) == 0) {
					currentArc.add((
						x: xNorm,
						freq: freq,
						confidence: confidence
					));
				};
			} {
				// Low confidence frame
				gapCounter = gapCounter + 1;

				// If gap is short, interpolate using last valid frequency
				if(gapCounter <= maxGapFrames and: { currentArc.size > 0 }) {
					// Keep the arc going with interpolated value (don't add point, just continue)
				} {
					// Gap too long - break the arc
					if(currentArc.size >= minArcPoints) {
						arcs.add(currentArc.asArray);
					};
					currentArc = List.new;
					gapCounter = 0;
				};
			};
		};

		// Add final arc
		if(currentArc.size >= minArcPoints) {
			arcs.add(currentArc.asArray);
		};

		// Apply smoothing - first median filter to remove outliers, then weighted average
		if(smoothing > 1) {
			arcs = arcs.collect { |arc|
				var filtered = this.prMedianFilter(arc, smoothing);
				this.prSmoothArc(filtered, smoothing)
			};
		};

		// Filter out very short arcs (less than 2% of total duration)
		arcs = arcs.select { |arc|
			var arcDuration = arc.last[\x] - arc.first[\x];
			arcDuration >= 0.02
		};

		^arcs
	}

	prSpectralToArcs { |smoothing, timeScale, segment|
		var arcs = List.new;
		var currentArc = List.new;
		var timeRatio = timeScale / duration;
		var onsetIdx = 0;
		var nextOnset = if(onsets.size > 0) { onsets[0] } { inf };
		var minArcPoints = 10;
		var decimationFactor = 4;

		spectralData.do { |frame, i|
			var time = frame[0];
			var centroid = frame[1];  // Use spectral centroid as "frequency"
			var xNorm = (time / duration).clip(0, 1);

			// Check for onset boundary
			if(segment == \onsets and: { time >= nextOnset }) {
				if(currentArc.size >= minArcPoints) {
					arcs.add(currentArc.asArray);
				};
				currentArc = List.new;
				onsetIdx = onsetIdx + 1;
				nextOnset = if(onsetIdx < onsets.size) { onsets[onsetIdx] } { inf };
			};

			// Decimate and add point (centroid is already in Hz)
			if((i % decimationFactor) == 0 and: { centroid > 20 } and: { centroid < 8000 }) {
				currentArc.add((
					x: xNorm,
					freq: centroid
				));
			};
		};

		// Add final arc
		if(currentArc.size >= minArcPoints) {
			arcs.add(currentArc.asArray);
		};

		// Apply smoothing - first median filter to remove outliers, then weighted average
		if(smoothing > 1) {
			arcs = arcs.collect { |arc|
				var filtered = this.prMedianFilter(arc, smoothing);
				this.prSmoothArc(filtered, smoothing)
			};
		};

		// Filter out very short arcs
		arcs = arcs.select { |arc|
			var arcDuration = arc.last[\x] - arc.first[\x];
			arcDuration >= 0.02
		};

		^arcs
	}

	prSmoothArc { |arc, windowSize|
		var smoothed = List.new;
		var halfWin = (windowSize / 2).floor.asInteger.max(1);

		arc.do { |pt, i|
			var startIdx = (i - halfWin).max(0);
			var endIdx = (i + halfWin).min(arc.size - 1);
			var freqSum = 0;
			var weightSum = 0;

			// Weighted average - center points weighted more
			(startIdx..endIdx).do { |j|
				var dist = (j - i).abs;
				var weight = 1.0 / (1 + dist);  // Closer points weighted more
				freqSum = freqSum + (arc[j][\freq] * weight);
				weightSum = weightSum + weight;
			};

			smoothed.add((
				x: pt[\x],
				freq: freqSum / weightSum
			));
		};

		^smoothed.asArray
	}

	// Additional smoothing pass for very noisy data
	prMedianFilter { |arc, windowSize|
		var filtered = List.new;
		var halfWin = (windowSize / 2).floor.asInteger.max(1);

		arc.do { |pt, i|
			var startIdx = (i - halfWin).max(0);
			var endIdx = (i + halfWin).min(arc.size - 1);
			var freqs = List.new;

			(startIdx..endIdx).do { |j|
				freqs.add(arc[j][\freq]);
			};

			// Sort and take median
			freqs = freqs.sort;
			filtered.add((
				x: pt[\x],
				freq: freqs[freqs.size div: 2]
			));
		};

		^filtered.asArray
	}

	// ========== Amplitude Envelope ==========

	getAmplitudeEnvelope { |arcIndex, arcData|
		/*
		 * Generate simple amplitude envelope for an arc
		 * Just creates fade in/out to avoid clicks
		 * Note: x values are in the same normalized range as arc x values
		 */
		var envelope = List.new;
		var arcStartX = arcData.first[\x];
		var arcEndX = arcData.last[\x];
		var arcLength = (arcEndX - arcStartX).abs.max(0.001);
		var fadeLength = arcLength * 0.05;  // 5% fade at each end

		// Simple 4-point envelope: fade in, sustain, fade out
		envelope.add((x: 0, amp: 0));
		envelope.add((x: fadeLength, amp: 0.6));  // Lower max amplitude
		envelope.add((x: arcLength - fadeLength, amp: 0.6));
		envelope.add((x: arcLength, amp: 0));

		^envelope.asArray
	}

	// ========== Import to NUPICData ==========

	importToData { |nupicData, options|
		/*
		 * Import analysis results directly into NUPICData
		 *
		 * Options:
		 *   \clearExisting - clear existing arcs first (default: false)
		 *   \page - which page to add to (default: current)
		 *   + all options from toArcs()
		 */

		var clearExisting = options[\clearExisting] ? false;
		var arcs = this.toArcs(options);

		if(clearExisting) {
			nupicData.clearAllArcs;
		};

		arcs.do { |arcData|
			var arcIndex = nupicData.addArc(arcData);

			// Add amplitude envelope if available
			if(loudnessData.size > 0) {
				var ampEnv = this.getAmplitudeEnvelope(arcIndex, arcData);
				if(ampEnv.size > 0) {
					nupicData.amplitudeEnvelopes[arcIndex] = ampEnv;
				};
			};
		};

		"Imported % arcs to NUPICData".format(arcs.size).postln;
		^arcs.size
	}

	// ========== Cleanup ==========

	free {
		[sourceBuffer, pitchBuffer, loudnessBuffer, spectralBuffer, onsetBuffer].do { |buf|
			if(buf.notNil) { buf.free };
		};
		pitchData.clear;
		loudnessData.clear;
		spectralData.clear;
		onsets.clear;
	}
}
