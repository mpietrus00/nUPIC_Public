/*
 * nUPIC - Arc Synthesis System
 *
 * A SuperCollider implementation inspired by Iannis Xenakis's UPIC system.
 * Draw arcs to create frequency trajectories with customizable synthesis.
 *
 * INSTALLATION:
 * 1. Copy the nUPIC 'Classes' folder to your Extensions folder:
 *    - macOS: ~/Library/Application Support/SuperCollider/Extensions/
 *    - Linux: ~/.local/share/SuperCollider/Extensions/
 *    - Windows: %APPDATA%\SuperCollider\Extensions\
 * 2. Recompile the class library (Cmd+Shift+L on macOS, Ctrl+Shift+L on others)
 * 3. Run this file to start nUPIC
 *
 * OPTIONAL: For higher quality oscillators, install OversamplingOscillators:
 * Download from: https://github.com/spluta/OversamplingOscillators/releases
 * Place the folder in your SuperCollider Extensions folder.
 *
 * USAGE:
 * Execute one of the lines below to start the application:
 */

// STANDARD MODE - No extensions required (uses built-in SC oscillators)
//NUPICApplication().start;

// OVERSAMPLING MODE - Requires OversamplingOscillators extension
~app = NUPICApplication(oversampling: true).start;

/*
 * CONTROLS:
 * ---------
 * Drawing:
 *   Mouse drag     - Draw arcs (frequency trajectories)
 *   _select        - Enter selection mode (marquee select)
 *   _eraser        - Enter erase mode
 *
 * Playback:
 *   Spacebar       - Play/Stop
 *   Direction menu - FWD, REV, PAL (palindrome), LOOP
 *   Duration       - Set playback duration in seconds
 *
 * Selection & Editing:
 *   Click arc      - Select single arc
 *   Shift+Click    - Add to selection
 *   Cmd+A          - Select all
 *   Escape         - Deselect all
 *   Delete         - Remove selected arcs
 *   Arrow keys     - Move selected (left/right = time, up/down = pitch)
 *
 * Transform:
 *   ><  <>         - Time compress/expand
 *   ^v  v^         - Frequency compress/expand
 *   _rev           - Reverse selected arcs
 *   _inv           - Invert (flip vertically)
 *   _cp _ps        - Copy/Paste
 *
 * Undo/Redo:
 *   Cmd+Z          - Undo
 *   Cmd+Shift+Z    - Redo
 *
 * Editors (select arc(s) first):
 *   _edit WT       - Wavetable editor (draw custom waveforms)
 *   _edit amps     - Amplitude envelope editor
 *   _edit spatial  - Spatial/panning envelope editor
 *   _edit width    - Pulse width editor (VarSaw synths only)
 *
 * Synth Types:
 *   - UPIC Wavetable: Custom drawn waveforms (Mono, 2ch, 3ch, 4ch, 8ch, 12ch, 15ch, 24ch)
 *   - SawBL: Band-limited sawtooth (Mono, 2ch, 8ch)
 *   - VarSaw: Variable-width sawtooth with width control (Mono, 2ch, 8ch)
 *
 * Multi-Arc Selection:
 *   When 2+ arcs are selected, a popup window appears allowing you to:
 *   - View/change synth for each arc
 *   - Access individual arc editors
 *   - Apply settings to all selected arcs
 *
 * File Operations:
 *   _save / _load  - Save/load compositions
 *
 * Pages:
 *   Page buttons   - Switch between multiple drawing pages
 */
