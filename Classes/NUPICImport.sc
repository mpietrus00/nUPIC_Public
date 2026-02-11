/*
 * NUPICImport - Import utilities for nUPIC
 *
 * Converts data from various sources (MycAlpha, arrays, etc.) into nUPIC format.
 * Arc format in nUPIC: List of points where each point is (x: normalizedTime, y: normalizedFreq)
 * - x: 0-1 representing position in the composition's duration
 * - y: 0-1 representing pitch (0=lowest, 1=highest in the freq range)
 */

NUPICImport {

	// ========== MycAlpha Import ==========

	/*
	 * Import arcs from MycAlpha format
	 *
	 * MycAlpha format: Array of arcs, each arc is [[t, f], [t, f], ...]
	 * - t: time in seconds
	 * - f: frequency in octaves (0-5) or Hz depending on method
	 *
	 * Arguments:
	 *   data: NUPICData instance to import into
	 *   mycAlphaArcs: Array of arcs from MycAlpha
	 *   freqMode: \octaves (0-5) or \hz
	 *   freqRange: [minFreq, maxFreq] in Hz for normalization (default [20, 8000])
	 *   timeOffset: offset to add to all time values (default 0)
	 */
	*fromMycAlpha { |data, mycAlphaArcs, freqMode = \octaves, freqRange, timeOffset = 0|
		var minTime = inf, maxTime = 0;
		var fMin, fMax;
		var nupicArcs = List.new;

		if(data.isNil) {
			"NUPICImport: data is nil".error;
			^nil
		};

		if(mycAlphaArcs.isNil or: { mycAlphaArcs.isEmpty }) {
			"NUPICImport: No arcs to import".warn;
			^0
		};

		// Set frequency range
		freqRange = freqRange ?? [20, 8000];
		fMin = freqRange[0];
		fMax = freqRange[1];

		// First pass: find time extent
		mycAlphaArcs.do { |arc|
			arc.do { |pt|
				var t = pt[0] + timeOffset;
				if(t < minTime) { minTime = t };
				if(t > maxTime) { maxTime = t };
			};
		};

		// Handle edge case
		if(maxTime <= minTime) { maxTime = minTime + 1 };

		// Convert each arc
		mycAlphaArcs.do { |mycArc|
			var nupicArc = List.new;

			mycArc.do { |pt|
				var t = pt[0] + timeOffset;
				var f = pt[1];
				var xNorm, yNorm, freqHz;

				// Normalize time to 0-1
				xNorm = (t - minTime) / (maxTime - minTime);

				// Convert frequency to Hz if needed, then normalize
				freqHz = if(freqMode == \octaves) {
					// Octaves 0-5 mapped to frequency range
					// Octave 0 = fMin, Octave 5 = fMax (exponential)
					fMin * pow(fMax / fMin, f / 5.0)
				} {
					f  // Already in Hz
				};

				// Normalize frequency (logarithmic for perceptual scaling)
				yNorm = log2(freqHz / fMin) / log2(fMax / fMin);
				yNorm = yNorm.clip(0, 1);

				nupicArc.add((x: xNorm, y: yNorm));
			};

			if(nupicArc.size >= 2) {
				nupicArcs.add(nupicArc);
			};
		};

		// Add all arcs to data
		nupicArcs.do { |arc|
			data.addArc(arc);
		};

		"Imported % arcs from MycAlpha (time: % to % sec)".format(
			nupicArcs.size, minTime.round(0.01), maxTime.round(0.01)
		).postln;

		^nupicArcs.size
	}

	/*
	 * Import a MycAlpha segment with Hz frequencies
	 * Convenience method that sets freqMode to \hz
	 */
	*fromMycAlphaHz { |data, mycAlphaArcs, freqRange, timeOffset = 0|
		^this.fromMycAlpha(data, mycAlphaArcs, \hz, freqRange, timeOffset)
	}

	/*
	 * Import MycAlpha shapes directly
	 * For use with MycAlpha.cloud, MycAlpha.mountain, etc.
	 */
	*fromMycAlphaShape { |data, shapeArcs, freqMode = \octaves, freqRange, timeOffset = 0|
		^this.fromMycAlpha(data, shapeArcs, freqMode, freqRange, timeOffset)
	}

	// ========== Generic Array Import ==========

	/*
	 * Import arcs from simple array format
	 * Format: [[[x, y], [x, y], ...], [[x, y], [x, y], ...], ...]
	 * - x: time (will be normalized to 0-1)
	 * - y: frequency in Hz (will be normalized logarithmically)
	 */
	*fromArrays { |data, arcsArray, freqRange|
		^this.fromMycAlpha(data, arcsArray, \hz, freqRange, 0)
	}

	// ========== Utilities ==========

	/*
	 * Clear all arcs before importing
	 */
	*clearAndImport { |data, mycAlphaArcs, freqMode = \octaves, freqRange, timeOffset = 0|
		if(data.notNil) {
			data.clear;
		};
		^this.fromMycAlpha(data, mycAlphaArcs, freqMode, freqRange, timeOffset)
	}

	/*
	 * Import and set duration to match the source material
	 */
	*fromMycAlphaWithDuration { |data, gui, mycAlphaArcs, freqMode = \octaves, freqRange, timeOffset = 0|
		var minTime = inf, maxTime = 0;
		var duration, imported;

		// Find time extent
		mycAlphaArcs.do { |arc|
			arc.do { |pt|
				var t = pt[0] + timeOffset;
				if(t < minTime) { minTime = t };
				if(t > maxTime) { maxTime = t };
			};
		};

		duration = maxTime - minTime;
		if(duration <= 0) { duration = 10 };

		// Import the arcs
		imported = this.fromMycAlpha(data, mycAlphaArcs, freqMode, freqRange, timeOffset);

		// Set the GUI duration if available
		if(gui.notNil and: { gui.respondsTo(\setPlayDuration) }) {
			gui.setPlayDuration(duration.max(1).round(0.1));
			"Set playback duration to % seconds".format(duration.round(0.1)).postln;
		};

		^imported
	}
}
