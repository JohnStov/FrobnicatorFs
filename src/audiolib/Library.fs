namespace AudioLib

open System

type AudioStream = float seq

module Generator =
    let TWOPI = Math.PI * 2.0 

    let generate fn sampleRate frequency : AudioStream = 
        let gen theta = 
            let delta = TWOPI * frequency / sampleRate
            Some (fn theta, (theta + delta) % TWOPI)
        Seq.unfold gen 0.0

    let sine = generate Math.Sin

    let square = generate (fun theta -> if theta <= Math.PI then 1.0 else -1.0)

    let sawtooth = generate (fun theta -> (theta / Math.PI) - 1.0)

    let triangle = 
        let triFunc theta =
            if theta < Math.PI then
                (2.0 * (theta / Math.PI)) - 1.0
            else
                (2.0 * (2.0 - (theta / Math.PI))) - 1.0
        generate triFunc
                