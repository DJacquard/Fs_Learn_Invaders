module Random

type RandomSource = {
    NextInRange: int->int->int
    NextInRangeF: float->float->float32
    }

let nextInRange source min max = source.NextInRange min max

let nextInRangeF source min max = source.NextInRangeF min max