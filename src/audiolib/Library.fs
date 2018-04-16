namespace AudioLib

open System

type AudioStream = float seq

module Generator =
    let TWOPI = Math.PI * 2.0 

    let generate fn sampleRate frequency : AudioStream = 
        let gen theta = 
            let delta = TWOPI * frequency / float sampleRate
            Some (fn theta, (theta + delta) % TWOPI)
        Seq.unfold gen 0.0

    let sine = generate Math.Sin
