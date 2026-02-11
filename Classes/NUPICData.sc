/*
 * NUPICData - Data management class for nUPIC
 *
 * Handles arc storage, pages, selection, and transformations
 */

NUPICData {
	// Pages - each page contains arcs, envelopes, etc.
	var <pages;
	var <currentPageIndex = 0;
	var maxPages = 24;

	// Current page data (shortcuts)
	var <arcs;
	var <selectedArcs;
	var <amplitudeEnvelopes;
	var <widthEnvelopes;
	var <spatialEnvelopes;
	var <arcSynthDefs;
	var <arcWavetables;       // Wavetable data per arc (Array of floats)
	var <arcWavetableBuffers; // Buffer objects per arc
	var <channelOffsets;      // Output channel offset per arc
	var <arcAmplitudeScalers; // Per-arc amplitude scale (0-1, default 1)
	var <preSnapStates;       // Stores arc state before snap for smooth/restore

	// Frequency table (per-page, like original UPIC)
	var <frequencyTable;      // Array of (y: 0-1, freq: Hz) points
	var <frequencyTableMode;  // \continuous or \discrete
	var <frequencyTableEnabled; // Whether to use table or default exponential
	var <frequencyRangeMin;   // Min frequency for this page (Hz)
	var <frequencyRangeMax;   // Max frequency for this page (Hz)
	var <frequencyTableSortedByY;    // Cached: table sorted by y position
	var <frequencyTableSortedByFreq; // Cached: table sorted by frequency

	// Default synthdef for new arcs
	var <>defaultSynthDef = \upicWavetable;

	// Clipboard for copy/paste
	var <clipboard;

	// Undo/redo
	var undoStack;
	var redoStack;
	var maxUndoSteps = 50;

	// Callbacks
	var <>onDataChanged;
	var <>onSelectionChanged;
	var <>onPageChanged;
	var <>onWavetableChanged;  // Called when a wavetable buffer is loaded (index, buffer)

	*new {
		^super.new.init
	}

	init {
		pages = List.new;
		undoStack = List.new;
		redoStack = List.new;
		clipboard = nil;
		preSnapStates = Dictionary.new;

		// Create default pages (A, B, C, D)
		["_a", "_b", "_c", "_d"].do { |label|
			this.prCreatePage(label);
		};

		// Sync to first page
		this.prSyncToCurrentPage;
	}

	// ========== Page Management ==========

	prCreatePage { |label = "Untitled"|
		var page = (
			label: label,
			arcs: List.new,
			selectedArcs: Set.new,
			amplitudeEnvelopes: List.new,
			widthEnvelopes: List.new,
			spatialEnvelopes: List.new,
			arcSynthDefs: List.new,
			arcWavetables: List.new,
			arcWavetableBuffers: List.new,
			channelOffsets: List.new,
			arcAmplitudeScalers: List.new,  // Per-arc amplitude scale (0-1)
			// Frequency table (per-page)
			frequencyTable: nil,          // nil = use default exponential
			frequencyTableMode: \continuous,
			frequencyTableEnabled: false,
			frequencyRangeMin: 20,        // Default min frequency
			frequencyRangeMax: 7500,      // Default max frequency
			frequencyTableSortedByY: nil,
			frequencyTableSortedByFreq: nil
		);
		pages.add(page);
		^pages.size - 1
	}

	prSyncToCurrentPage {
		var page = pages[currentPageIndex];
		if(page.notNil) {
			arcs = page.arcs;
			selectedArcs = page.selectedArcs;
			amplitudeEnvelopes = page.amplitudeEnvelopes;
			widthEnvelopes = page.widthEnvelopes;
			spatialEnvelopes = page.spatialEnvelopes;
			arcSynthDefs = page.arcSynthDefs;
			arcWavetables = page.arcWavetables;
			arcWavetableBuffers = page.arcWavetableBuffers;
			channelOffsets = page.channelOffsets;
			arcAmplitudeScalers = page.arcAmplitudeScalers;
			// Frequency table
			frequencyTable = page.frequencyTable;
			frequencyTableMode = page.frequencyTableMode;
			frequencyTableEnabled = page.frequencyTableEnabled;
			frequencyRangeMin = page.frequencyRangeMin ?? 20;
			frequencyRangeMax = page.frequencyRangeMax ?? 7500;
			frequencyTableSortedByY = page.frequencyTableSortedByY;
			frequencyTableSortedByFreq = page.frequencyTableSortedByFreq;
		};
	}

	switchPage { |index|
		if(index >= 0 and: { index < pages.size }) {
			currentPageIndex = index;
			this.prSyncToCurrentPage;
			onPageChanged.value(index);
			onDataChanged.value;
		};
	}

	pageCount { ^pages.size }

	currentPage { ^pages[currentPageIndex] }

	// ========== Arc Management ==========

	addArc { |arc|
		var arcList;

		// Convert to List if needed
		arcList = if(arc.isKindOf(List)) { arc } { List.newFrom(arc) };

		// Validate arc has at least 2 points
		if(arcList.size < 2) { ^nil };

		// Save undo state
		this.prSaveUndoState;

		arcs.add(arcList);
		amplitudeEnvelopes.add(nil);
		widthEnvelopes.add(nil);
		spatialEnvelopes.add(nil);
		arcSynthDefs.add(defaultSynthDef);
		arcWavetables.add(nil);        // Will use default sine if nil
		arcWavetableBuffers.add(nil);
		channelOffsets.add(0);         // Default channel offset
		arcAmplitudeScalers.add(1.0);  // Default full volume

		onDataChanged.value;
		^arcs.size - 1  // Return index
	}

	removeArc { |index|
		if(index >= 0 and: { index < arcs.size }) {
			var bufToFree;
			this.prSaveUndoState;

			// Check if buffer is shared before freeing
			bufToFree = arcWavetableBuffers[index];
			if(bufToFree.notNil) {
				// Count references to this buffer
				var refCount = arcWavetableBuffers.count { |b| b === bufToFree };
				// Only free if this is the last reference
				if(refCount <= 1) {
					try { bufToFree.free };
				};
			};

			arcs.removeAt(index);
			amplitudeEnvelopes.removeAt(index);
			widthEnvelopes.removeAt(index);
			spatialEnvelopes.removeAt(index);
			arcSynthDefs.removeAt(index);
			arcWavetables.removeAt(index);
			arcWavetableBuffers.removeAt(index);
			channelOffsets.removeAt(index);
			arcAmplitudeScalers.removeAt(index);

			// Update selection
			selectedArcs.remove(index);
			// Adjust indices in selection
			selectedArcs = selectedArcs.collect { |i|
				if(i > index) { i - 1 } { i }
			}.asSet;

			onDataChanged.value;
			^true
		};
		^false
	}

	removeArcs { |indices|
		var sortedIndices, buffersToFree;

		if(indices.isEmpty) { ^this };

		this.prSaveUndoState;

		// Collect unique buffers to free (avoid double-free for shared buffers)
		buffersToFree = Set.new;
		indices.do { |index|
			if(index >= 0 and: { index < arcWavetableBuffers.size }) {
				if(arcWavetableBuffers[index].notNil) {
					buffersToFree.add(arcWavetableBuffers[index]);
				};
			};
		};

		// Remove from highest index first to avoid shifting issues
		sortedIndices = indices.asArray.sort.reverse;
		sortedIndices.do { |index|
			if(index >= 0 and: { index < arcs.size }) {
				arcs.removeAt(index);
				amplitudeEnvelopes.removeAt(index);
				widthEnvelopes.removeAt(index);
				spatialEnvelopes.removeAt(index);
				arcSynthDefs.removeAt(index);
				arcWavetables.removeAt(index);
				arcWavetableBuffers.removeAt(index);
				channelOffsets.removeAt(index);
			};
		};

		// Free collected buffers (only if not still referenced by remaining arcs)
		buffersToFree.do { |buf|
			if(arcWavetableBuffers.includes(buf).not) {
				try { buf.free };
			};
		};

		selectedArcs.clear;
		onDataChanged.value;
	}

	clearAll {
		this.prSaveUndoState;

		// Free all unique wavetable buffers (avoid double-free for shared buffers)
		// Filter out nil values before converting to Set (Set cannot contain nil)
		arcWavetableBuffers.select { |b| b.notNil }.asSet.do { |buf|
			try { buf.free };
		};

		arcs.clear;
		amplitudeEnvelopes.clear;
		widthEnvelopes.clear;
		spatialEnvelopes.clear;
		arcSynthDefs.clear;
		arcWavetables.clear;
		arcWavetableBuffers.clear;
		channelOffsets.clear;
		selectedArcs.clear;

		onDataChanged.value;
	}

	arcCount { ^arcs.size }

	getArc { |index|
		^arcs[index]
	}

	// ========== Page-specific Access (for playback while browsing other pages) ==========

	getPageArcs { |pageIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			^pages[pageIndex].arcs
		};
		^List.new
	}

	getPageArcCount { |pageIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			^pages[pageIndex].arcs.size
		};
		^0
	}

	getPageSynthDefForArc { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.arcSynthDefs.size }) {
				^page.arcSynthDefs[arcIndex]
			};
		};
		^defaultSynthDef
	}

	getPageWavetableBufferForArc { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.arcWavetableBuffers.size }) {
				^page.arcWavetableBuffers[arcIndex]
			};
		};
		^nil
	}

	getPageWavetableForArc { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.arcWavetables.size }) {
				^page.arcWavetables[arcIndex]
			};
		};
		^nil
	}

	getPageChannelOffset { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.channelOffsets.size }) {
				^page.channelOffsets[arcIndex]
			};
		};
		^0
	}

	getPageAmplitudeEnvelope { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.amplitudeEnvelopes.size }) {
				^page.amplitudeEnvelopes[arcIndex]
			};
		};
		^nil
	}

	getPageWidthEnvelope { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.widthEnvelopes.size }) {
				^page.widthEnvelopes[arcIndex]
			};
		};
		^nil
	}

	getPageSpatialEnvelope { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.spatialEnvelopes.size }) {
				^page.spatialEnvelopes[arcIndex]
			};
		};
		^nil
	}

	getPageAmplitudeScaler { |pageIndex, arcIndex|
		if(pageIndex >= 0 and: { pageIndex < pages.size }) {
			var page = pages[pageIndex];
			if(arcIndex >= 0 and: { arcIndex < page.arcAmplitudeScalers.size }) {
				^page.arcAmplitudeScalers[arcIndex] ?? 1.0
			};
		};
		^1.0
	}

	ensurePageWavetableBuffers { |pageIndex, server|
		var page;
		server = server ?? { Server.default };

		if(server.serverRunning.not) { ^this };
		if(pageIndex < 0 or: { pageIndex >= pages.size }) { ^this };

		page = pages[pageIndex];

		// Ensure arrays are sized correctly
		while { page.arcWavetableBuffers.size < page.arcs.size } {
			page.arcWavetableBuffers.add(nil)
		};
		while { page.arcWavetables.size < page.arcs.size } {
			page.arcWavetables.add(nil)
		};

		page.arcs.size.do { |i|
			if(page.arcWavetableBuffers[i].isNil) {
				if(page.arcWavetables[i].notNil) {
					var wavetableData = page.arcWavetables[i];
					var targetSize = 2048;
					var resampledData = this.prResampleToPowerOfTwo(wavetableData, targetSize);
					var sig = Signal.newFrom(resampledData);
					var wt = sig.asWavetable;
					var buf;

					// Create buffer and set immediately (data loads asynchronously)
					buf = Buffer.loadCollection(server, wt, 1);
					page.arcWavetableBuffers[i] = buf;
				} {
					var buf = Buffer.alloc(server, 2048, 1);
					buf.sine1([1], true, true, true);
					page.arcWavetableBuffers[i] = buf;
				};
			};
		};
	}

	// ========== SynthDef Management ==========

	getSynthDefForArc { |index|
		if(index >= 0 and: { index < arcSynthDefs.size }) {
			^arcSynthDefs[index]
		};
		^defaultSynthDef
	}

	setSynthDefForArc { |index, synthDefName|
		if(index >= 0 and: { index < arcSynthDefs.size }) {
			arcSynthDefs[index] = synthDefName.asSymbol;
		};
	}

	setSynthDefForSelected { |synthDefName|
		var name = synthDefName.asSymbol;
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;
			selectedArcs.do { |index|
				if(index < arcSynthDefs.size) {
					arcSynthDefs[index] = name;
				};
			};
			// Also set as new default
			defaultSynthDef = name;
			onDataChanged.value;
			^true
		};
		// No selection - just change default
		defaultSynthDef = name;
		^false
	}

	// ========== Wavetable Management ==========

	getWavetableForArc { |index|
		if(index >= 0 and: { index < arcWavetables.size }) {
			^arcWavetables[index]
		};
		^nil
	}

	getWavetableBufferForArc { |index|
		if(index >= 0 and: { index < arcWavetableBuffers.size }) {
			^arcWavetableBuffers[index]
		};
		^nil
	}

	setWavetableForArc { |index, wavetableData, server, action|
		// wavetableData is an Array of floats (-1 to 1)
		// action is an optional completion callback
		if(index >= 0 and: { index < arcWavetables.size }) {
			var oldBuffer;
			server = server ?? { Server.default };

			// Store the data
			arcWavetables[index] = wavetableData;

			// Check if old buffer is shared before freeing
			oldBuffer = arcWavetableBuffers[index];
			if(oldBuffer.notNil) {
				var refCount = arcWavetableBuffers.count { |b| b === oldBuffer };
				// Only free if this is the last reference
				if(refCount <= 1) {
					try { oldBuffer.free };
				};
				arcWavetableBuffers[index] = nil;
			};

			// Create new buffer with wavetable format for Osc.ar
			if(server.serverRunning) {
				// Osc.ar requires buffer size to be power of 2
				// Standard wavetable size is 2048 samples
				var targetSize = 2048;
				var resampledData = this.prResampleToPowerOfTwo(wavetableData, targetSize);
				var sig = Signal.newFrom(resampledData);
				var wt = sig.asWavetable;  // Converts to wavetable format (doubles size to 4096)

				// Use loadCollection with completion for reliable loading
				Buffer.loadCollection(server, wt, 1, { |buf|
					arcWavetableBuffers[index] = buf;
					// Notify that wavetable changed (for live update during playback)
					onWavetableChanged.value(index, buf);
					action.value(buf);
				});
			} {
				action.value(nil);
			};
		} {
			action.value(nil);
		};
	}

	// Resample wavetable data to a power-of-2 size using linear interpolation
	prResampleToPowerOfTwo { |data, targetSize = 2048|
		var srcSize = data.size;
		var result;

		if(srcSize == targetSize) { ^data };
		if(srcSize < 2) { ^Signal.sineFill(targetSize, [1]).asArray };

		// Linear interpolation resampling
		result = Array.fill(targetSize, { |i|
			var srcPos = i * (srcSize - 1) / (targetSize - 1);
			var srcIndex = srcPos.floor.asInteger;
			var frac = srcPos - srcIndex;
			var val1 = data[srcIndex];
			var val2 = data[(srcIndex + 1).min(srcSize - 1)];
			val1 + (frac * (val2 - val1))
		});

		^result
	}

	// Debug: Check buffer status for an arc
	checkWavetableBuffer { |index|
		var buf = arcWavetableBuffers[index];
		var data = arcWavetables[index];

		"=== Wavetable Buffer Status for Arc % ===".format(index).postln;
		"  Data stored: %".format(data.notNil).postln;
		if(data.notNil) {
			"  Data size: % samples".format(data.size).postln;
			"  Data range: % to %".format(data.minItem.round(0.001), data.maxItem.round(0.001)).postln;
		};
		"  Buffer exists: %".format(buf.notNil).postln;
		if(buf.notNil) {
			"  Buffer num: %".format(buf.bufnum).postln;
			"  Buffer frames: %".format(buf.numFrames).postln;
			"  Buffer channels: %".format(buf.numChannels).postln;
		};
		"==========================================".postln;
	}

	// Debug: Plot a wavetable buffer
	plotWavetableBuffer { |index|
		var buf = arcWavetableBuffers[index];
		if(buf.notNil) {
			buf.plot("Wavetable Buffer for Arc %".format(index));
		} {
			"No buffer for arc %".format(index).warn;
		};
	}

	// Debug: Check all wavetable buffers
	checkAllWavetableBuffers {
		"=== All Wavetable Buffer Status ===".postln;
		arcs.size.do { |i|
			var buf = arcWavetableBuffers[i];
			var data = arcWavetables[i];
			"Arc %: data=%, buffer=%".format(
				i,
				if(data.notNil, { "% samples".format(data.size) }, { "nil" }),
				if(buf.notNil, { "bufnum %".format(buf.bufnum) }, { "nil" })
			).postln;
		};
		"===================================".postln;
	}

	setWavetableForSelected { |wavetableData, server, doneAction|
		// Use a SHARED buffer when applying the same wavetable to multiple arcs
		// This avoids buffer exhaustion when setting wavetables for 800+ arcs
		if(selectedArcs.size > 0) {
			var indices = selectedArcs.asArray;
			var buffersToFree = Set.new;
			server = server ?? { Server.default };

			// Collect unique buffers to free (handles shared buffers correctly)
			indices.do { |index|
				if(arcWavetableBuffers[index].notNil) {
					buffersToFree.add(arcWavetableBuffers[index]);
					arcWavetableBuffers[index] = nil;
				};
				// Store wavetable data for each arc
				arcWavetables[index] = wavetableData.copy;
			};

			// Free unique buffers safely (Set already deduplicates)
			buffersToFree.do { |buf|
				try { buf.free };
			};

			// Use fork to create Routine context for server.sync
			// (required because this may be called from GUI/AppClock context)
			fork {
				// Wait for server to process frees, then create ONE shared buffer
				server.sync;

				if(server.serverRunning) {
					var targetSize = 2048;
					var resampledData = this.prResampleToPowerOfTwo(wavetableData, targetSize);
					var sig = Signal.newFrom(resampledData);
					var wt = sig.asWavetable;

					// Create a single shared buffer
					Buffer.loadCollection(server, wt, 1, { |sharedBuf|
						// Point all selected arcs to this shared buffer
						// Skip per-arc callbacks - all arcs share the same buffer
						indices.do { |index|
							arcWavetableBuffers[index] = sharedBuf;
						};
						"Loaded shared wavetable buffer for % arcs (bufnum: %)".format(indices.size, sharedBuf.bufnum).postln;
						doneAction.value;
					});
				} {
					doneAction.value;
				};
			};
			^true
		};
		^false
	}

	// Direct method to set wavetable for specific indices (bypasses selection)
	// Used by wavetable editor for efficient bulk operations
	setWavetableForIndices { |indices, wavetableData, sharedBuffer|
		// All arcs share the same wavetable data and buffer (no per-arc copies needed)
		indices.do { |index|
			if(index >= 0 and: { index < arcs.size }) {
				arcWavetables[index] = wavetableData;  // Share reference, don't copy
				arcWavetableBuffers[index] = sharedBuffer;
			};
		};
	}

	// Ensure all per-arc arrays are properly sized
	prEnsureArraySizes {
		var targetSize = arcs.size;

		while { amplitudeEnvelopes.size < targetSize } { amplitudeEnvelopes.add(nil) };
		while { spatialEnvelopes.size < targetSize } { spatialEnvelopes.add(nil) };
		while { arcSynthDefs.size < targetSize } { arcSynthDefs.add(defaultSynthDef) };
		while { arcWavetables.size < targetSize } { arcWavetables.add(nil) };
		while { arcWavetableBuffers.size < targetSize } { arcWavetableBuffers.add(nil) };
		while { channelOffsets.size < targetSize } { channelOffsets.add(0) };
		while { arcAmplitudeScalers.size < targetSize } { arcAmplitudeScalers.add(1.0) };
	}

	// Ensure all arcs have wavetable buffers (called before playback)
	ensureWavetableBuffers { |server|
		server = server ?? { Server.default };

		if(server.serverRunning.not) { ^this };

		// Ensure all arrays are properly sized first
		this.prEnsureArraySizes;

		arcs.size.do { |i|
			// Check if buffer needs to be created
			if(arcWavetableBuffers[i].isNil) {
				if(arcWavetables[i].notNil) {
					// Has custom wavetable data - create buffer from it
					var wavetableData = arcWavetables[i];
					var targetSize = 2048;
					var resampledData = this.prResampleToPowerOfTwo(wavetableData, targetSize);
					var sig = Signal.newFrom(resampledData);
					var wt = sig.asWavetable;

					Buffer.loadCollection(server, wt, 1, { |buf|
						arcWavetableBuffers[i] = buf;
					});
				} {
					// No custom wavetable - create default sine buffer
					var buf = Buffer.alloc(server, 2048, 1);
					buf.sine1([1], true, true, true);  // Pure sine wave in wavetable format
					arcWavetableBuffers[i] = buf;
				};
			};
		};
	}

	// Create buffers from loaded wavetable data (called after loading preset)
	prLoadWavetableBuffers { |server|
		server = server ?? { Server.default };

		if(server.serverRunning.not) {
			"Warning: Server not running, wavetable buffers will be created on next play".postln;
			^this
		};

		arcs.size.do { |i|
			if(arcWavetables[i].notNil) {
				// Arc has custom wavetable data - create buffer from it
				var wavetableData = arcWavetables[i];
				var targetSize = 2048;
				var resampledData = this.prResampleToPowerOfTwo(wavetableData, targetSize);
				var sig = Signal.newFrom(resampledData);
				var wt = sig.asWavetable;  // Converts to wavetable format (doubles size)

				// Use loadCollection with completion for reliable loading
				Buffer.loadCollection(server, wt, 1, { |buf|
					arcWavetableBuffers[i] = buf;
				});
			} {
				// No custom wavetable - create default sine buffer
				var buf = Buffer.alloc(server, 2048, 1);
				buf.sine1([1], true, true, true);
				arcWavetableBuffers[i] = buf;
			};
		};
	}

	// ========== Amplitude Envelope Management ==========

	getAmplitudeEnvelope { |index|
		if(index >= 0 and: { index < amplitudeEnvelopes.size }) {
			^amplitudeEnvelopes[index]
		};
		^nil
	}

	setAmplitudeEnvelope { |index, envelope|
		// envelope is a List of (x: xPos, amp: amplitude) events
		if(index >= 0 and: { index < amplitudeEnvelopes.size }) {
			amplitudeEnvelopes[index] = envelope;
		};
	}

	setAmplitudeEnvelopeForSelected { |envelope|
		if(selectedArcs.size > 0) {
			selectedArcs.do { |index|
				if(index < amplitudeEnvelopes.size) {
					amplitudeEnvelopes[index] = envelope.deepCopy;
				};
			};
			^true
		};
		^false
	}

	// ========== Width Envelope Management ==========

	getWidthEnvelope { |index|
		if(index >= 0 and: { index < widthEnvelopes.size }) {
			^widthEnvelopes[index]
		};
		^nil
	}

	setWidthEnvelope { |index, envelope|
		// envelope is a List of (x: xPos, width: widthValue) events
		// width is 0-1 range (0.5 = 50% duty cycle)
		if(index >= 0 and: { index < widthEnvelopes.size }) {
			widthEnvelopes[index] = envelope;
		};
	}

	setWidthEnvelopeForSelected { |envelope|
		if(selectedArcs.size > 0) {
			selectedArcs.do { |index|
				if(index < widthEnvelopes.size) {
					widthEnvelopes[index] = envelope.deepCopy;
				};
			};
			^true
		};
		^false
	}

	// ========== Spatial Envelope Management ==========

	getSpatialEnvelope { |index|
		if(index >= 0 and: { index < spatialEnvelopes.size }) {
			^spatialEnvelopes[index]
		};
		^nil
	}

	setSpatialEnvelope { |index, envelope|
		// envelope is a List of (x: xPos, channel: channelNum) events
		if(index >= 0 and: { index < spatialEnvelopes.size }) {
			spatialEnvelopes[index] = envelope;
		};
	}

	setSpatialEnvelopeForSelected { |envelope|
		if(selectedArcs.size > 0) {
			selectedArcs.do { |index|
				if(index < spatialEnvelopes.size) {
					spatialEnvelopes[index] = envelope.deepCopy;
				};
			};
			^true
		};
		^false
	}

	// ========== Channel Offset Management ==========

	getChannelOffset { |index|
		if(index >= 0 and: { index < channelOffsets.size }) {
			^channelOffsets[index] ?? 0
		};
		^0
	}

	setChannelOffset { |index, offset|
		if(index >= 0 and: { index < channelOffsets.size }) {
			channelOffsets[index] = offset.asInteger;
		};
	}

	// ========== Amplitude Scaler Management ==========

	getAmplitudeScaler { |index|
		if(index >= 0 and: { index < arcAmplitudeScalers.size }) {
			^arcAmplitudeScalers[index] ?? 1.0
		};
		^1.0
	}

	setAmplitudeScaler { |index, scaler|
		if(index >= 0 and: { index < arcAmplitudeScalers.size }) {
			arcAmplitudeScalers[index] = (scaler ?? 1.0).clip(0, 1);
			// Note: Don't call onDataChanged here - it causes UI refresh issues with sliders
			// Playback reads the value directly on each frame
		};
	}

	// ========== Frequency Table Management ==========

	getFrequencyTable {
		^frequencyTable
	}

	setFrequencyTable { |table|
		var page = pages[currentPageIndex];
		frequencyTable = table;

		// Pre-sort and cache for fast lookup
		if(table.notNil and: { table.size > 0 }) {
			frequencyTableSortedByY = table.copy.sort { |a, b| a.y < b.y };
			frequencyTableSortedByFreq = table.copy.sort { |a, b| a.freq < b.freq };
		} {
			frequencyTableSortedByY = nil;
			frequencyTableSortedByFreq = nil;
		};

		if(page.notNil) {
			page.frequencyTable = table;
			page.frequencyTableSortedByY = frequencyTableSortedByY;
			page.frequencyTableSortedByFreq = frequencyTableSortedByFreq;
		};
		onDataChanged.value;
	}

	getFrequencyTableMode {
		^frequencyTableMode ?? \continuous
	}

	setFrequencyTableMode { |mode|
		var page = pages[currentPageIndex];
		frequencyTableMode = mode;
		if(page.notNil) {
			page.frequencyTableMode = mode;
		};
		onDataChanged.value;
	}

	isFrequencyTableEnabled {
		^frequencyTableEnabled ?? false
	}

	setFrequencyTableEnabled { |enabled|
		var page = pages[currentPageIndex];
		frequencyTableEnabled = enabled;
		if(page.notNil) {
			page.frequencyTableEnabled = enabled;
		};
		onDataChanged.value;
	}

	getFrequencyRangeMin {
		^frequencyRangeMin ?? 20
	}

	getFrequencyRangeMax {
		^frequencyRangeMax ?? 7500
	}

	setFrequencyRange { |minFreq, maxFreq|
		var page = pages[currentPageIndex];
		frequencyRangeMin = minFreq;
		frequencyRangeMax = maxFreq;
		if(page.notNil) {
			page.frequencyRangeMin = minFreq;
			page.frequencyRangeMax = maxFreq;
		};
		onDataChanged.value;
	}

	// Convert normalized Y position (0-1) to frequency using table
	// yNorm: 0 = bottom (low freq), 1 = top (high freq)
	yToFreqWithTable { |yNorm, freqMin = 20, freqMax = 7500|
		var table, mode, result;

		// Clamp input
		yNorm = yNorm.clip(0, 1);
		freqMin = freqMin.max(1);
		freqMax = freqMax.max(freqMin + 1);

		// If table not enabled or nil/empty, use default exponential mapping
		if(frequencyTableEnabled.not or: { frequencyTable.isNil } or: { frequencyTable.size < 1 }) {
			^yNorm.linexp(0, 1, freqMin, freqMax)
		};

		table = frequencyTable;
		mode = frequencyTableMode ?? \continuous;

		result = if(mode == \discrete) {
			// Find nearest pitch in table
			this.prFindNearestPitch(yNorm, table)
		} {
			// Interpolate between table points
			this.prInterpolateTable(yNorm, table)
		};

		// Safety: ensure result is valid
		^result.clip(1, 20000)
	}

	// Convert frequency to normalized Y position (0-1) using table
	freqToYWithTable { |freq, freqMin = 20, freqMax = 7500|
		var table, result;

		// Safety checks
		freq = freq.max(1);
		freqMin = freqMin.max(1);
		freqMax = freqMax.max(freqMin + 1);

		// If table not enabled or nil/empty, use default exponential mapping
		if(frequencyTableEnabled.not or: { frequencyTable.isNil } or: { frequencyTable.size < 1 }) {
			^freq.explin(freqMin, freqMax, 0, 1).clip(0, 1)
		};

		table = frequencyTable;
		result = this.prFreqToYFromTable(freq, table);

		// Safety: ensure result is valid
		^result.clip(0, 1)
	}

	// Find nearest pitch in discrete table using binary search (O(log n))
	prFindNearestPitch { |yNorm, table|
		var sorted, lo, hi, mid, bestFreq;

		if(table.isNil or: { table.size < 1 }) {
			^yNorm.linexp(0, 1, 20, 7500)
		};

		// Use cached sorted table
		sorted = frequencyTableSortedByY ?? { table.copy.sort { |a, b| a.y < b.y } };

		if(sorted.size == 1) { ^sorted[0].freq };

		// Binary search for closest y position
		lo = 0;
		hi = sorted.size - 1;

		while { (hi - lo) > 1 } {
			mid = (lo + hi) div: 2;
			if(sorted[mid].y <= yNorm) {
				lo = mid;
			} {
				hi = mid;
			};
		};

		// Compare lo and hi to find closest
		bestFreq = if((yNorm - sorted[lo].y).abs <= (sorted[hi].y - yNorm).abs) {
			sorted[lo].freq
		} {
			sorted[hi].freq
		};

		^bestFreq ?? { yNorm.linexp(0, 1, 20, 7500) }
	}

	// Interpolate frequency from continuous table using binary search (O(log n))
	prInterpolateTable { |yNorm, table|
		var sorted, lo, hi, mid, pt1, pt2, t, freqRatio;

		if(table.isNil or: { table.size < 1 }) {
			^yNorm.linexp(0, 1, 20, 7500)
		};

		if(table.size < 2) {
			^table[0].freq ?? { yNorm.linexp(0, 1, 20, 7500) }
		};

		// Use cached sorted table (much faster)
		sorted = frequencyTableSortedByY ?? { table.copy.sort { |a, b| a.y < b.y } };

		// Clamp yNorm to valid range
		yNorm = yNorm.clip(0, 1);

		// Find bracketing points
		if(yNorm <= sorted.first.y) { ^sorted.first.freq };
		if(yNorm >= sorted.last.y) { ^sorted.last.freq };

		// Binary search for bracketing interval (O(log n))
		lo = 0;
		hi = sorted.size - 1;

		while { (hi - lo) > 1 } {
			mid = (lo + hi) div: 2;
			if(sorted[mid].y <= yNorm) {
				lo = mid;
			} {
				hi = mid;
			};
		};

		pt1 = sorted[lo];
		pt2 = sorted[hi];

		// Safety check for same y position
		if((pt2.y - pt1.y).abs < 0.0001) { ^pt1.freq };

		// Interpolate in log space for perceptually correct results
		t = (yNorm - pt1.y) / (pt2.y - pt1.y);
		t = t.clip(0, 1);

		// Safety check for frequencies
		if(pt1.freq <= 0 or: { pt2.freq <= 0 }) { ^yNorm.linexp(0, 1, 20, 7500) };

		freqRatio = pt2.freq / pt1.freq;
		^(pt1.freq * (freqRatio ** t))
	}

	// Convert frequency back to Y position using table with binary search (O(log n))
	prFreqToYFromTable { |freq, table|
		var sorted, lo, hi, mid, pt1, pt2, t, logRange;

		if(table.isNil or: { table.size < 1 }) {
			^freq.explin(20, 7500, 0, 1)
		};

		if(table.size < 2) {
			^table[0].y ?? 0.5
		};

		// Safety check for frequency
		if(freq <= 0) { ^0 };

		// Use cached sorted table (much faster)
		sorted = frequencyTableSortedByFreq ?? { table.copy.sort { |a, b| a.freq < b.freq } };

		// Find bracketing points
		if(freq <= sorted.first.freq) { ^sorted.first.y };
		if(freq >= sorted.last.freq) { ^sorted.last.y };

		// Binary search for bracketing interval (O(log n))
		lo = 0;
		hi = sorted.size - 1;

		while { (hi - lo) > 1 } {
			mid = (lo + hi) div: 2;
			if(sorted[mid].freq <= freq) {
				lo = mid;
			} {
				hi = mid;
			};
		};

		pt1 = sorted[lo];
		pt2 = sorted[hi];

		// Safety checks
		if(pt1.freq <= 0 or: { pt2.freq <= 0 }) { ^0.5 };
		if((pt2.freq - pt1.freq).abs < 0.001) { ^pt1.y };

		// Interpolate in log space
		logRange = pt2.freq.log - pt1.freq.log;
		if(logRange.abs < 0.0001) { ^pt1.y };

		t = (freq.log - pt1.freq.log) / logRange;
		t = t.clip(0, 1);
		^(pt1.y + (t * (pt2.y - pt1.y)))
	}

	// Generate preset frequency tables
	*generateChromaticTable { |freqMin = 20, freqMax = 7500, refFreq = 440|
		// 12-TET chromatic scale
		var table = List.new;
		var freq = freqMin;
		var semitone = 2 ** (1/12);

		// Start from nearest semitone below freqMin
		freq = refFreq;
		while { freq > freqMin } { freq = freq / semitone };
		freq = freq * semitone;

		while { freq <= freqMax } {
			var yNorm = freq.explin(freqMin, freqMax, 0, 1);
			table.add((y: yNorm, freq: freq));
			freq = freq * semitone;
		};

		^table.asArray
	}

	*generate31TETTable { |freqMin = 20, freqMax = 7500, refFreq = 440|
		// 31-TET (Fokker) microtonal scale
		var table = List.new;
		var freq = freqMin;
		var step = 2 ** (1/31);

		freq = refFreq;
		while { freq > freqMin } { freq = freq / step };
		freq = freq * step;

		while { freq <= freqMax } {
			var yNorm = freq.explin(freqMin, freqMax, 0, 1);
			table.add((y: yNorm, freq: freq));
			freq = freq * step;
		};

		^table.asArray
	}

	*generate24TETTable { |freqMin = 20, freqMax = 7500, refFreq = 440|
		// 24-TET quarter tone scale
		var table = List.new;
		var freq = freqMin;
		var step = 2 ** (1/24);

		freq = refFreq;
		while { freq > freqMin } { freq = freq / step };
		freq = freq * step;

		while { freq <= freqMax } {
			var yNorm = freq.explin(freqMin, freqMax, 0, 1);
			table.add((y: yNorm, freq: freq));
			freq = freq * step;
		};

		^table.asArray
	}

	*generateJustIntonationTable { |freqMin = 20, freqMax = 7500, refFreq = 261.63|
		// Just intonation (major scale ratios)
		var table = List.new;
		var ratios = [1, 16/15, 9/8, 6/5, 5/4, 4/3, 45/32, 3/2, 8/5, 5/3, 9/5, 15/8];
		var baseFreq = refFreq;

		// Find lowest octave
		while { baseFreq > freqMin } { baseFreq = baseFreq / 2 };
		baseFreq = baseFreq * 2;

		while { baseFreq <= freqMax } {
			ratios.do { |ratio|
				var freq = baseFreq * ratio;
				if(freq >= freqMin and: { freq <= freqMax }) {
					var yNorm = freq.explin(freqMin, freqMax, 0, 1);
					table.add((y: yNorm, freq: freq));
				};
			};
			baseFreq = baseFreq * 2;
		};

		// Sort and remove duplicates
		table = table.sort { |a, b| a.freq < b.freq };
		^table.asArray
	}

	*generatePythagoreanTable { |freqMin = 20, freqMax = 7500, refFreq = 261.63|
		// Pythagorean tuning (3:2 based)
		var table = List.new;
		var ratios = [1, 256/243, 9/8, 32/27, 81/64, 4/3, 729/512, 3/2, 128/81, 27/16, 16/9, 243/128];
		var baseFreq = refFreq;

		while { baseFreq > freqMin } { baseFreq = baseFreq / 2 };
		baseFreq = baseFreq * 2;

		while { baseFreq <= freqMax } {
			ratios.do { |ratio|
				var freq = baseFreq * ratio;
				if(freq >= freqMin and: { freq <= freqMax }) {
					var yNorm = freq.explin(freqMin, freqMax, 0, 1);
					table.add((y: yNorm, freq: freq));
				};
			};
			baseFreq = baseFreq * 2;
		};

		table = table.sort { |a, b| a.freq < b.freq };
		^table.asArray
	}

	*generateHarmonicSeriesTable { |freqMin = 20, freqMax = 7500, fundamental = 55|
		// Harmonic series
		var table = List.new;
		var harmonic = 1;
		var freq = fundamental;

		while { freq <= freqMax } {
			if(freq >= freqMin) {
				var yNorm = freq.explin(freqMin, freqMax, 0, 1);
				table.add((y: yNorm, freq: freq));
			};
			harmonic = harmonic + 1;
			freq = fundamental * harmonic;
		};

		^table.asArray
	}

	*generateCustomEqualDivisions { |divisions = 19, freqMin = 20, freqMax = 7500, refFreq = 440|
		// Equal divisions of the octave (e.g., 19-TET, 53-TET)
		var table = List.new;
		var freq = freqMin;
		var step = 2 ** (1/divisions);

		freq = refFreq;
		while { freq > freqMin } { freq = freq / step };
		freq = freq * step;

		while { freq <= freqMax } {
			var yNorm = freq.explin(freqMin, freqMax, 0, 1);
			table.add((y: yNorm, freq: freq));
			freq = freq * step;
		};

		^table.asArray
	}

	// ========== Selection ==========

	select { |index|
		if(index >= 0 and: { index < arcs.size }) {
			selectedArcs.add(index);
			onSelectionChanged.value(selectedArcs);
		};
	}

	deselect { |index|
		selectedArcs.remove(index);
		onSelectionChanged.value(selectedArcs);
	}

	toggleSelect { |index|
		if(selectedArcs.includes(index)) {
			this.deselect(index);
		} {
			this.select(index);
		};
	}

	selectAll {
		selectedArcs = (0..(arcs.size - 1)).asSet;
		onSelectionChanged.value(selectedArcs);
	}

	deselectAll {
		selectedArcs.clear;
		onSelectionChanged.value(selectedArcs);
	}

	// Bulk select specific indices (only one callback at the end)
	selectIndices { |indices|
		indices.do { |idx|
			if(idx >= 0 and: { idx < arcs.size }) {
				selectedArcs.add(idx);
			};
		};
		onSelectionChanged.value(selectedArcs);
	}

	isSelected { |index|
		^selectedArcs.includes(index)
	}

	selectedCount { ^selectedArcs.size }

	getSelectedArcs {
		^selectedArcs.asArray.collect { |i| arcs[i] }
	}

	// ========== Clipboard ==========

	copy {
		if(selectedArcs.size > 0) {
			// Copy all arc data including envelopes, synth settings, etc.
			clipboard = selectedArcs.asArray.collect { |i|
				(
					arc: arcs[i].deepCopy,
					amplitudeEnvelope: if(amplitudeEnvelopes[i].notNil) { amplitudeEnvelopes[i].deepCopy } { nil },
					widthEnvelope: if(widthEnvelopes[i].notNil) { widthEnvelopes[i].deepCopy } { nil },
					spatialEnvelope: if(spatialEnvelopes[i].notNil) { spatialEnvelopes[i].deepCopy } { nil },
					synthDef: arcSynthDefs[i],
					wavetable: if(arcWavetables[i].notNil) { arcWavetables[i].copy } { nil },
					channelOffset: channelOffsets[i],
					amplitudeScaler: arcAmplitudeScalers[i] ?? 1.0
				)
			};
			^true
		};
		^false
	}

	paste { |offsetX = 0.02, offsetY = 0.02|
		if(clipboard.notNil and: { clipboard.size > 0 }) {
			var newIndices = List.new;

			this.prSaveUndoState;

			// Deselect all currently selected arcs
			selectedArcs.clear;

			clipboard.do { |data|
				var newArc = data.arc.collect { |pt|
					var y = pt[\y] ?? { this.freqToYWithTable(pt[\freq], frequencyRangeMin, frequencyRangeMax) };
					var newY = y + offsetY;
					var newFreq = this.yToFreqWithTable(newY, frequencyRangeMin, frequencyRangeMax);
					(x: pt.x + offsetX, y: newY, freq: newFreq)
				};
				var newIndex = arcs.size;

				arcs.add(List.newFrom(newArc));
				amplitudeEnvelopes.add(if(data.amplitudeEnvelope.notNil) { data.amplitudeEnvelope.deepCopy } { nil });
				widthEnvelopes.add(if(data.widthEnvelope.notNil) { data.widthEnvelope.deepCopy } { nil });
				spatialEnvelopes.add(if(data.spatialEnvelope.notNil) { data.spatialEnvelope.deepCopy } { nil });
				arcSynthDefs.add(data.synthDef ?? defaultSynthDef);
				arcWavetables.add(if(data.wavetable.notNil) { data.wavetable.copy } { nil });
				arcWavetableBuffers.add(nil);  // Buffer will be created on playback
				channelOffsets.add(data.channelOffset ?? 0);
				arcAmplitudeScalers.add(data.amplitudeScaler ?? 1.0);

				// Select the new arc
				newIndices.add(newIndex);
				selectedArcs.add(newIndex);
			};

			onDataChanged.value;
			onSelectionChanged.value(selectedArcs);
			^true
		};
		^false
	}

	// Paste to a specific page (used for pasting to playback page while browsing other pages)
	pasteToPage { |pageIndex, offsetX = 0.02, offsetY = 0.02|
		var page;

		if(clipboard.isNil or: { clipboard.size == 0 }) { ^false };
		if(pageIndex < 0 or: { pageIndex >= pages.size }) { ^false };

		page = pages[pageIndex];

		clipboard.do { |clipData|
			var newArc = clipData.arc.collect { |pt|
				(x: pt.x + offsetX, y: pt.y + offsetY, freq: pt.freq)
			};

			page.arcs.add(List.newFrom(newArc));
			page.amplitudeEnvelopes.add(if(clipData.amplitudeEnvelope.notNil) { clipData.amplitudeEnvelope.deepCopy } { nil });
			page.widthEnvelopes.add(if(clipData.widthEnvelope.notNil) { clipData.widthEnvelope.deepCopy } { nil });
			page.spatialEnvelopes.add(if(clipData.spatialEnvelope.notNil) { clipData.spatialEnvelope.deepCopy } { nil });
			page.arcSynthDefs.add(clipData.synthDef ?? defaultSynthDef);
			page.arcWavetables.add(if(clipData.wavetable.notNil) { clipData.wavetable.copy } { nil });
			page.arcWavetableBuffers.add(nil);
			page.channelOffsets.add(clipData.channelOffset ?? 0);
			page.arcAmplitudeScalers.add(clipData.amplitudeScaler ?? 1.0);
		};

		// If pasting to current page, sync the working arrays
		if(pageIndex == currentPageIndex) {
			this.prSyncToCurrentPage;
			onDataChanged.value;
		};

		^true
	}

	// ========== Transformations ==========

	reverseSelected {
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;

			selectedArcs.do { |index|
				var arc = arcs[index];
				var reversed = arc.reverse;
				var xVals = arc.collect { |pt| pt[\x] };
				var minX = xVals.minItem;
				var maxX = xVals.maxItem;
				// Swap x coordinates to maintain left-to-right order
				reversed = reversed.collect { |pt|
					(x: maxX - (pt[\x] - minX), y: this.prGetPointY(pt))
				};
				arcs[index] = List.newFrom(reversed);
			};

			onDataChanged.value;
		};
	}

	inverseSelected {
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;

			selectedArcs.do { |index|
				var arc = arcs[index];
				var yVals = arc.collect { |pt| this.prGetPointY(pt) };
				var minY = yVals.minItem;
				var maxY = yVals.maxItem;
				var centerY = (minY + maxY) / 2;  // Center Y position

				arcs[index] = List.newFrom(arc.collect { |pt|
					// Flip Y position around center (inverts pitch visually)
					var y = this.prGetPointY(pt);
					var newY = centerY + (centerY - y);
					(x: pt[\x], y: newY.clip(0, 1))
				});
			};

			onDataChanged.value;
		};
	}

	timeCompressSelected { |factor = 0.5|
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;

			selectedArcs.do { |index|
				var arc = arcs[index];
				var ampEnv = amplitudeEnvelopes[index];
				var xVals = arc.collect { |pt| pt[\x] };
				var minX = xVals.minItem;

				// Scale arc points (convert freq to y if needed)
				arcs[index] = List.newFrom(arc.collect { |pt|
					(x: minX + ((pt[\x] - minX) * factor), y: this.prGetPointY(pt))
				});

				// Scale amplitude envelope x coordinates too
				if(ampEnv.notNil and: { ampEnv.size > 0 }) {
					amplitudeEnvelopes[index] = List.newFrom(ampEnv.collect { |pt|
						(x: pt[\x] * factor, amp: pt[\amp])
					});
				};
			};

			onDataChanged.value;
		};
	}

	timeExpandSelected { |factor = 2.0|
		this.timeCompressSelected(factor);
	}

	freqCompressSelected { |factor = 0.5|
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;

			selectedArcs.do { |index|
				var arc = arcs[index];
				var yVals = arc.collect { |pt| this.prGetPointY(pt) };
				var minY = yVals.minItem;
				var maxY = yVals.maxItem;
				var centerY = (minY + maxY) / 2;  // Center Y position

				arcs[index] = List.newFrom(arc.collect { |pt|
					// Compress Y toward center (compresses pitch range visually)
					var y = this.prGetPointY(pt);
					var newY = centerY + ((y - centerY) * factor);
					(x: pt[\x], y: newY.clip(0, 1))
				});
			};

			onDataChanged.value;
		};
	}

	freqExpandSelected { |factor = 2.0|
		this.freqCompressSelected(factor);
	}

	moveSelected { |deltaX = 0, deltaY = 0|
		// deltaX is in normalized spatial units (0-1)
		// deltaY is in normalized Y units (0-1, positive = up)
		if(selectedArcs.size > 0) {
			this.prSaveUndoState;

			selectedArcs.do { |index|
				var arc = arcs[index];
				arcs[index] = List.newFrom(arc.collect { |pt|
					var y;
					// If freq is provided (>1 Hz), convert to normalized y first
					if(pt[\freq].notNil and: { pt[\freq] > 1 }) {
						y = pt[\freq].explin(20, 7500, 0, 1).clip(0, 1);
					} {
						y = pt[\y] ?? 0.5;
					};
					// Return point with updated y (drop freq since we've converted it)
					(x: pt[\x] + deltaX, y: (y + deltaY).clip(0, 1))
				});
			};

			onDataChanged.value;
		};
	}

	snapSelectedToGrid {
		// Snap selected arcs' Y positions to nearest scale pitches
		// Works with both frequency table and default exponential grid
		var fMin, fMax;

		if(selectedArcs.size == 0) { ^this };

		this.prSaveUndoState;

		fMin = this.getFrequencyRangeMin;
		fMax = this.getFrequencyRangeMax;

		selectedArcs.do { |index|
			var arc = arcs[index];

			// Save pre-snap state for this arc (for smooth/restore)
			preSnapStates[index] = arc.deepCopy;

			arcs[index] = List.newFrom(arc.collect { |pt|
				var y = this.prGetPointY(pt);
				var freq, snappedFreq, snappedY;

				// Convert current Y to frequency
				freq = this.yToFreqWithTable(y, fMin, fMax);

				// Snap frequency to nearest grid pitch
				snappedFreq = this.prSnapFreqToGrid(freq);

				// Convert snapped frequency back to Y
				snappedY = this.freqToYWithTable(snappedFreq, fMin, fMax);

				(x: pt[\x], y: snappedY.clip(0, 1))
			});
		};

		onDataChanged.value;
		"Snapped % arcs to grid".format(selectedArcs.size).postln;
	}

	smoothSelected {
		// Restore selected arcs to their pre-snap state
		// This is the opposite of snap - brings back the original continuous arc
		var restoredCount = 0;

		if(selectedArcs.size == 0) { ^this };

		this.prSaveUndoState;

		selectedArcs.do { |index|
			var savedArc = preSnapStates[index];
			if(savedArc.notNil) {
				arcs[index] = savedArc.deepCopy;
				preSnapStates.removeAt(index);
				restoredCount = restoredCount + 1;
			};
		};

		if(restoredCount > 0) {
			onDataChanged.value;
			"Restored % arcs to pre-snap state".format(restoredCount).postln;
		} {
			"No pre-snap state saved for selected arcs".postln;
		};
	}

	prSnapFreqToGrid { |freq|
		var gridFreqs, closest, minDist = inf;

		// Get grid frequencies based on current mode
		if(frequencyTableEnabled and: { frequencyTable.notNil } and: { frequencyTable.size > 0 }) {
			// Use frequencies from the scale table
			gridFreqs = frequencyTable.collect { |pt| pt.freq };
		} {
			// Use 12-TET frequencies
			gridFreqs = this.prGet12TETFrequencies;
		};

		// Find nearest frequency (in log space for perceptual accuracy)
		gridFreqs.do { |gFreq|
			var dist = (gFreq.log - freq.log).abs;
			if(dist < minDist) {
				minDist = dist;
				closest = gFreq;
			};
		};

		^closest ?? freq
	}

	prGet12TETFrequencies {
		// Generate 12-TET frequencies from A0 to C8
		var freqs = List.new;
		var a4 = 440;
		(-48..48).do { |semitones|
			var freq = a4 * (2 ** (semitones / 12));
			if(freq >= 20 and: { freq <= 20000 }) {
				freqs.add(freq);
			};
		};
		^freqs.asArray
	}

	// ========== Hit Testing ==========

	findArcAt { |x, y, radius = 15|
		// Find arc near point, return index or nil
		arcs.do { |arc, i|
			arc.do { |pt|
				var dist = ((pt.x - x).squared + (pt.y - y).squared).sqrt;
				if(dist < radius) {
					^i
				};
			};
		};
		^nil
	}

	findArcsInRect { |left, top, right, bottom|
		// Find all arcs with points inside rectangle
		var found = Set.new;

		arcs.do { |arc, i|
			arc.do { |pt|
				if(pt.x >= left and: { pt.x <= right } and: { pt.y >= top } and: { pt.y <= bottom }) {
					found.add(i);
				};
			};
		};

		^found
	}

	// Helper: Get normalized Y from point (converts freq to y if needed)
	prGetPointY { |pt|
		if(pt[\freq].notNil and: { pt[\freq] > 1 }) {
			^pt[\freq].explin(20, 7500, 0, 1).clip(0, 1)
		} {
			^(pt[\y] ?? 0.5)
		}
	}

	// ========== Undo/Redo ==========

	prSaveUndoState {
		var state = (
			arcs: arcs.deepCopy,
			amplitudeEnvelopes: amplitudeEnvelopes.deepCopy,
			widthEnvelopes: widthEnvelopes.deepCopy,
			spatialEnvelopes: spatialEnvelopes.deepCopy,
			arcSynthDefs: arcSynthDefs.copy,
			selectedArcs: selectedArcs.copy
		);

		undoStack.add(state);
		if(undoStack.size > maxUndoSteps) {
			undoStack.removeAt(0);
		};

		// Clear redo stack when new action is performed
		redoStack.clear;
	}

	undo {
		if(undoStack.size > 0) {
			var currentState, state;

			currentState = (
				arcs: arcs.deepCopy,
				amplitudeEnvelopes: amplitudeEnvelopes.deepCopy,
				widthEnvelopes: widthEnvelopes.deepCopy,
				spatialEnvelopes: spatialEnvelopes.deepCopy,
				arcSynthDefs: arcSynthDefs.copy,
				selectedArcs: selectedArcs.copy
			);
			redoStack.add(currentState);

			state = undoStack.pop;
			arcs.clear.addAll(state.arcs);
			amplitudeEnvelopes.clear.addAll(state.amplitudeEnvelopes);
			widthEnvelopes.clear.addAll(state.widthEnvelopes ? List.new);
			spatialEnvelopes.clear.addAll(state.spatialEnvelopes);
			arcSynthDefs.clear.addAll(state.arcSynthDefs);
			selectedArcs = state.selectedArcs;

			// Update page reference
			pages[currentPageIndex].arcs = arcs;
			pages[currentPageIndex].amplitudeEnvelopes = amplitudeEnvelopes;
			pages[currentPageIndex].widthEnvelopes = widthEnvelopes;
			pages[currentPageIndex].spatialEnvelopes = spatialEnvelopes;
			pages[currentPageIndex].arcSynthDefs = arcSynthDefs;
			pages[currentPageIndex].selectedArcs = selectedArcs;

			onDataChanged.value;
			onSelectionChanged.value(selectedArcs);
			^true
		};
		^false
	}

	redo {
		if(redoStack.size > 0) {
			var currentState, state;

			currentState = (
				arcs: arcs.deepCopy,
				amplitudeEnvelopes: amplitudeEnvelopes.deepCopy,
				widthEnvelopes: widthEnvelopes.deepCopy,
				spatialEnvelopes: spatialEnvelopes.deepCopy,
				arcSynthDefs: arcSynthDefs.copy,
				selectedArcs: selectedArcs.copy
			);
			undoStack.add(currentState);

			state = redoStack.pop;
			arcs.clear.addAll(state.arcs);
			amplitudeEnvelopes.clear.addAll(state.amplitudeEnvelopes);
			widthEnvelopes.clear.addAll(state.widthEnvelopes ? List.new);
			spatialEnvelopes.clear.addAll(state.spatialEnvelopes);
			arcSynthDefs.clear.addAll(state.arcSynthDefs);
			selectedArcs = state.selectedArcs;

			// Update page reference
			pages[currentPageIndex].arcs = arcs;
			pages[currentPageIndex].amplitudeEnvelopes = amplitudeEnvelopes;
			pages[currentPageIndex].widthEnvelopes = widthEnvelopes;
			pages[currentPageIndex].spatialEnvelopes = spatialEnvelopes;
			pages[currentPageIndex].arcSynthDefs = arcSynthDefs;
			pages[currentPageIndex].selectedArcs = selectedArcs;

			onDataChanged.value;
			onSelectionChanged.value(selectedArcs);
			^true
		};
		^false
	}

	canUndo { ^undoStack.size > 0 }
	canRedo { ^redoStack.size > 0 }

	// ========== Preset Save/Load ==========

	*presetDirectory {
		var dir = Platform.userAppSupportDir +/+ "nUPIC_Presets";
		if(File.exists(dir).not) {
			File.mkdir(dir);
		};
		^dir
	}

	listPresets {
		var dir = NUPICData.presetDirectory;
		var files = PathName(dir).files.select { |f| f.extension == "preset" };
		^files.collect { |f| f.fileNameWithoutExtension }.sort.reverse
	}

	savePreset { |name, duration|
		var dir = NUPICData.presetDirectory;
		var page = this.currentPage;
		var timestamp = Date.localtime.stamp.replace(":", "-");
		var presetName, path, data, file;

		// Generate name: pageName_timestamp
		presetName = if(name.notNil) { name } {
			page.label.asString ++ "_" ++ timestamp
		};

		path = dir +/+ presetName ++ ".preset";

		// Collect data to save
		data = (
			version: 2,  // Incremented version for duration support
			name: presetName,
			savedAt: Date.localtime.asString,
			pageLabel: page.label,
			playDuration: duration ?? 10,  // Store page duration, default 10 sec
			arcs: arcs.collect { |arc| arc.asArray }.asArray,
			amplitudeEnvelopes: amplitudeEnvelopes.collect { |env|
				if(env.notNil) { env.asArray } { nil }
			}.asArray,
			widthEnvelopes: widthEnvelopes.collect { |env|
				if(env.notNil) { env.asArray } { nil }
			}.asArray,
			spatialEnvelopes: spatialEnvelopes.collect { |env|
				if(env.notNil) { env.asArray } { nil }
			}.asArray,
			arcSynthDefs: arcSynthDefs.asArray,
			channelOffsets: channelOffsets.asArray,
			arcWavetables: arcWavetables.collect { |wt|
				if(wt.notNil) { wt.asArray } { nil }
			}.asArray
		);

		// Write to file
		file = File(path, "w");
		if(file.isOpen) {
			file.write(data.asCompileString);
			file.close;
			("Preset saved: " ++ presetName).postln;
			^presetName
		} {
			("Error: Could not save preset to " ++ path).postln;
			^nil
		};
	}

	loadPreset { |presetName, server|
		var dir = NUPICData.presetDirectory;
		var path = dir +/+ presetName ++ ".preset";
		var file, content, data;

		server = server ?? { Server.default };

		if(File.exists(path).not) {
			("Preset not found: " ++ presetName).postln;
			^(success: false, duration: nil)
		};

		file = File(path, "r");
		if(file.isOpen) {
			content = file.readAllString;
			file.close;

			data = content.interpret;

			if(data.notNil and: { data.isKindOf(Event) }) {
				var loadedDuration;

				this.prSaveUndoState;

				// Clear current data
				this.clearAll;

				// Get duration (version 2+ presets have playDuration)
				loadedDuration = data.playDuration ?? 10;

				// Load arcs
				data.arcs.do { |arcData, i|
					var arc = List.newFrom(arcData.collect { |pt|
						(x: pt.x, y: pt.y, freq: pt.freq)
					});
					arcs.add(arc);

					// Load amplitude envelope
					if(data.amplitudeEnvelopes[i].notNil) {
						amplitudeEnvelopes.add(List.newFrom(
							data.amplitudeEnvelopes[i].collect { |pt|
								(x: pt.x, amp: pt.amp)
							}
						));
					} {
						amplitudeEnvelopes.add(nil);
					};

					// Load width envelope
					if(data.widthEnvelopes.notNil and: { data.widthEnvelopes[i].notNil }) {
						widthEnvelopes.add(List.newFrom(
							data.widthEnvelopes[i].collect { |pt|
								(x: pt.x, width: pt.width)
							}
						));
					} {
						widthEnvelopes.add(nil);
					};

					// Load spatial envelope
					if(data.spatialEnvelopes[i].notNil) {
						spatialEnvelopes.add(List.newFrom(
							data.spatialEnvelopes[i].collect { |pt|
								(x: pt.x, channel: pt.channel)
							}
						));
					} {
						spatialEnvelopes.add(nil);
					};

					// Load synthdef
					arcSynthDefs.add(data.arcSynthDefs[i] ?? defaultSynthDef);

					// Load channel offset
					channelOffsets.add(data.channelOffsets[i] ?? 0);

					// Load wavetable
					if(data.arcWavetables[i].notNil) {
						arcWavetables.add(data.arcWavetables[i]);
					} {
						arcWavetables.add(nil);
					};

					arcWavetableBuffers.add(nil);
				};

				// Create buffers for loaded wavetables
				this.prLoadWavetableBuffers(server);

				onDataChanged.value;
				("Preset loaded: " ++ presetName).postln;
				^(success: true, duration: loadedDuration)
			} {
				"Error: Invalid preset data".postln;
				^(success: false, duration: nil)
			};
		} {
			("Error: Could not open preset file: " ++ path).postln;
			^(success: false, duration: nil)
		};
	}

	rewritePreset { |presetName, duration|
		var dir = NUPICData.presetDirectory;
		var path = dir +/+ presetName ++ ".preset";

		if(File.exists(path)) {
			File.delete(path);
		};

		^this.savePreset(presetName, duration)
	}

	deletePreset { |presetName|
		var dir = NUPICData.presetDirectory;
		var path = dir +/+ presetName ++ ".preset";

		if(File.exists(path)) {
			File.delete(path);
			("Preset deleted: " ++ presetName).postln;
			^true
		} {
			("Preset not found: " ++ presetName).postln;
			^false
		};
	}
}
