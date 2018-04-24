namespace AudioLib

open System

type AudioStream = float seq

module Generator =
    let TWOPI = Math.PI * 2.0 

    let generate fn sampleRate (frequency : AudioStream) : AudioStream = 
        let enumerator = frequency.GetEnumerator()
        let gen theta = 
            let f = if enumerator.MoveNext() then enumerator.Current else 0.0
            let delta = TWOPI * f / sampleRate
            Some (fn theta, (theta + delta) % TWOPI)
        Seq.unfold gen 0.0

    let sine : float -> AudioStream -> AudioStream = 
        generate Math.Sin

    let square : float -> AudioStream -> AudioStream = 
        generate (fun theta -> if theta <= Math.PI then 1.0 else -1.0)

    let sawtooth : float -> AudioStream -> AudioStream =
        generate (fun theta -> (theta / Math.PI) - 1.0)

    let triangle : float -> AudioStream -> AudioStream = 
        let triFunc theta =
            if theta < Math.PI then
                (2.0 * (theta / Math.PI)) - 1.0
            else
                (2.0 * (2.0 - (theta / Math.PI))) - 1.0
        generate triFunc

    let noise : AudioStream =
        let rand = Random()
        Seq.unfold (fun _ -> Some((rand.NextDouble() * 2.0) - 1.0, () )) ()

    let constant (value : float) : AudioStream =
        Seq.unfold (fun _ -> Some (value, ())) ()
