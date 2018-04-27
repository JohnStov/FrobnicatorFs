namespace AudioLib

module MIDI =
    type Channel = int
    type NoteNumber = int
    type Velocity = int
    type ControlId = int
    type ControlValue = int
    type ProgramValue = int
    type BendValue =  int
    type MIDITimeCodeQuarterFrame = int
    type SongPositionPointer = int
    type SongId =  int
    type ChannelCount = int

    type ManufacturerId = byte array
    type SysexData = byte array
    
    // Channel Voice messages
    type Note = {channel : Channel; note: NoteNumber; velocity: Velocity}
    type KeyPressure = {channel : Channel; note: NoteNumber; velocity: Velocity}
    type ControlChange = {channel : Channel; control: ControlId; value: ControlValue}
    type ProgramChange = {channel : Channel; program: ProgramValue}
    type ChannelPressure = {channel : Channel; velocity: Velocity}
    type PitchBend = {channel : Channel; bend: BendValue}

    // Channel Mode messages
    type NoteOffType =
    | AllOff
    | OmniOff
    | OmniOn
    | MonoOn of ChannelCount
    | MonoOff

    // System Common messages 
    
    type Message =
        // Channel Voice
        | NoteOff of Note
        | NoteOn of Note
        | Aftertouch of KeyPressure
        | ControlChange of ControlChange
        | ProgramChange of ProgramChange
        | ChannelPressure of ChannelPressure
        | PitchBend of PitchBend
        // Channel Mode
        | AllSoundOff
        | ResetAllControllers
        | LocalControl of bool
        | AllNotesOff of NoteOffType
        // System Common
        | SystemExclusive of string
        | MIDITimeCodeQuarterFrame of MIDITimeCodeQuarterFrame
        | SongPositionPointer of SongPositionPointer
        | SongSelect of SongId
        | TuneRequest
        // System Real-Time
        | TimingClock
        | Start
        | Continue
        | Stop
        | ActiveSensing
        | Reset
