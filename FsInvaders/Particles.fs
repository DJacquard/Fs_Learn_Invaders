﻿module Particles

module Particle =
    [<Struct>]
    type Particle = {X: single; Y: single; SpeedX: single; SpeedY: single }

    let update particle = { particle with X = particle.X + particle.SpeedX; Y = particle.Y + particle.SpeedY } : Particle

module ParticleCloud =
    open Particle

    [<Struct>]
    type ParticleCloud = ParticleCloud of Particle list

    let apply f (ParticleCloud p) = f p

    let map f (ParticleCloud p) = p |> List.map f

    let mapAndUpdate f = map (update << f) 

    let iterate f (ParticleCloud p) = p |> List.iter f

module Explosion =
    open ParticleCloud
    open Particle

    let inline create count x y maxSpeed random =
        let nextRandomF = Random.nextInRangeF random
        let r() = nextRandomF -10.0 10.0
        let f() = {X = (single x + r()); Y = (single y + r()); SpeedX = nextRandomF -maxSpeed maxSpeed; SpeedY = nextRandomF -maxSpeed maxSpeed}
        ParticleCloud [for _ in 1..count -> f() ]

    let top exp =
        exp |> apply (List.minBy (fun p -> p.Y))

    let update explosion gravity =
        let f p = {p with SpeedY = p.SpeedY + gravity }
        
        explosion |> mapAndUpdate f |> ParticleCloud
