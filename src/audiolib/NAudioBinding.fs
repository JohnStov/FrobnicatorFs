namespace AudioLib

open NAudio.Midi
open MIDI

module NAudioBinding =
    let mapMidiEvent (evt : NAudio.Midi.MidiEvent) =
        match evt.CommandCode with
        | MidiCommandCode.NoteOn ->
            let note = evt :?> NoteEvent
            if evt |> MidiEvent.IsNoteOff 
            then NoteOff {channel = note.Channel; note = note.NoteNumber; velocity = note.Velocity}
            else NoteOn {channel = note.Channel; note = note.NoteNumber; velocity = note.Velocity}
        | MidiCommandCode.NoteOff ->
            let note = evt :?> NoteEvent
            NoteOff {channel = note.Channel; note = note.NoteNumber; velocity = note.Velocity}
        | MidiCommandCode.KeyAfterTouch ->
            let note = evt :?> NoteEvent
            Aftertouch {channel = note.Channel; note = note.NoteNumber; velocity = note.Velocity}
        | MidiCommandCode.ControlChange ->
            let change = evt :?> ControlChangeEvent
            ControlChange {channel = change.Channel; control = (int)change.Controller; value = change.ControllerValue}
        | MidiCommandCode.PatchChange ->
            let patch = evt :?> PatchChangeEvent
            ProgramChange {channel = patch.Channel; program = patch.Patch}
        | MidiCommandCode.ChannelAfterTouch ->
            let touch = evt :?> ChannelAfterTouchEvent
            ChannelPressure {channel = touch.Channel; velocity = touch.AfterTouchPressure}
        | MidiCommandCode.PitchWheelChange ->
            let bend = evt :?> PitchWheelChangeEvent
            PitchBend {channel = bend.Channel; bend = bend.Pitch}
        | MidiCommandCode.Sysex -> 
            let sysex = evt :?> SysexEvent
            SystemExclusive (sysex.ToString ())
        | MidiCommandCode.TimingClock -> TimingClock
        | MidiCommandCode.StartSequence -> Start
        | MidiCommandCode.ContinueSequence -> Continue
        | MidiCommandCode.StopSequence -> Stop
        | MidiCommandCode.AutoSensing -> ActiveSensing
