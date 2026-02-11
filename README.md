# nUPIC - Arc Synthesis System

A SuperCollider implementation inspired by Iannis Xenakis's UPIC system. Draw arcs on a canvas to create frequency trajectories, then play them back with customizable synthesis.

## Features

- **Arc Drawing**: Draw frequency trajectories with mouse gestures
- **Multiple Synth Types**: Wavetable, SawBL, VarSaw with various channel configurations (mono to 24ch)
- **Custom Wavetables**: Draw your own waveforms in the wavetable editor
- **Envelope Editors**: Per-arc amplitude, spatial panning, and pulse width envelopes
- **Multi-Arc Editing**: Select multiple arcs and edit them individually or apply settings to all
- **Transform Tools**: Time/frequency compress/expand, reverse, invert
- **Multiple Pages**: Organize compositions across multiple pages
- **Save/Load**: Save and load your compositions

## Installation

1. **Copy the Classes folder** to your SuperCollider Extensions directory:
   - **macOS**: `~/Library/Application Support/SuperCollider/Extensions/`
   - **Linux**: `~/.local/share/SuperCollider/Extensions/`
   - **Windows**: `%APPDATA%\SuperCollider\Extensions\`

2. **Recompile the class library**:
   - macOS: `Cmd+Shift+L`
   - Linux/Windows: `Ctrl+Shift+L`

3. **Start nUPIC** by running in SuperCollider:
   ```supercollider
   // Standard mode (no extensions required)
   NUPICApplication().start;

   // Oversampling mode (requires OversamplingOscillators)
   NUPICApplication(oversampling: true).start;
   ```

## Quick Start

1. Start the application with `NUPICApplication().start;`
2. Draw arcs on the canvas by clicking and dragging
3. Press Spacebar to play
4. Select arcs by clicking on them or using the `_select` tool
5. Change synth type using the synth dropdown menu
6. Edit envelopes using the editor buttons (`_edit amps`, `_edit spatial`, etc.)

## Controls

### Drawing
| Action | Description |
|--------|-------------|
| Mouse drag | Draw arcs |
| `_select` button | Enter selection mode |
| `_eraser` button | Enter erase mode |

### Playback
| Action | Description |
|--------|-------------|
| Spacebar | Play/Stop |
| Direction menu | FWD, REV, PAL, LOOP |
| Duration field | Set playback duration |

### Selection
| Action | Description |
|--------|-------------|
| Click arc | Select single arc |
| Shift+Click | Add to selection |
| Cmd/Ctrl+A | Select all |
| Escape | Deselect all |
| Delete/Backspace | Remove selected |
| Arrow keys | Move selected arcs |

### Editors
| Button | Description |
|--------|-------------|
| `_edit WT` | Wavetable editor (custom waveforms) |
| `_edit amps` | Amplitude envelope |
| `_edit spatial` | Spatial/panning envelope |
| `_edit width` | Pulse width (VarSaw only) |

### Transform
| Button | Description |
|--------|-------------|
| `><` / `<>` | Time compress/expand |
| `^v` / `v^` | Frequency compress/expand |
| `_rev` | Reverse |
| `_inv` | Invert |

## Synth Types

- **UPIC Wavetable**: Custom drawn waveforms (uses Osc in standard mode, OscOS in oversampling mode)
- **Saw**: Sawtooth oscillator (uses Saw in standard mode, SawBL in oversampling mode)
- **VarSaw**: Variable-width sawtooth with pulse width control (uses VarSaw in standard mode, VarSawOS in oversampling mode)

Each synth is available in multiple channel configurations for spatial audio (mono, 2ch, 3ch, 4ch, 8ch, 12ch, 15ch, 24ch).

## Custom Wavetables

Place audio files in the `TABLES/` folder to use as wavetable presets:

- **Supported formats**: WAV, AIFF
- **Recommended**: Single-cycle waveforms (one complete wave cycle)
- Files are automatically resampled to 2048 samples
- Filename appears in the wavetable editor preset menu

See `TABLES/README.txt` for more details.

## Files

```
nUPIC/
├── Classes/
│   ├── NUPICApplication.sc    # Main application class
│   ├── NUPICData.sc           # Data model (arcs, envelopes, pages)
│   ├── NUPICGUI.sc            # Main GUI window
│   ├── NUPICPlayback.sc       # Audio playback engine & SynthDefs
│   ├── NUPICAmplitudeEditor.sc
│   ├── NUPICWidthEditor.sc
│   ├── NUPICSpatialEditor.sc
│   ├── NUPICWavetableEditor.sc
│   └── NUPICMultiArcEditor.sc # Multi-arc selection popup
├── TABLES/                    # Custom wavetable audio files (optional)
│   └── README.txt
├── nUPIC_Start.scd            # Startup file with documentation
├── LICENSE
└── README.md
```

## Requirements

- SuperCollider 3.11 or later

### Optional: OversamplingOscillators Extension

For higher quality oscillators with reduced aliasing, you can optionally install the OversamplingOscillators extension:

1. Download from: https://github.com/spluta/OversamplingOscillators/releases
2. Place the folder in your SuperCollider Extensions folder
3. Start nUPIC with: `NUPICApplication(oversampling: true).start;`

Without this extension, nUPIC uses standard SuperCollider oscillators which work great for most purposes.

## License

MIT License - See LICENSE file

## Credits

Inspired by UPIC (Unité Polyagogique Informatique du CEMAMu) created by Iannis Xenakis.
