[<AutoOpen>]
module Geometry

[<Struct>]
type Point = {X: int; Y: int}

[<Struct>]
type Size = {Width: int; Height: int}

[<Struct>]
type Rectangle = Rectangle of Point * Size


module Point =
    let create x y = {X = x; Y = y}

    let moveX deltaX {X = x; Y = y} = create (x + deltaX) y

    let moveY deltaY {X = x; Y = y} = create x (y + deltaY)


module Size =
    let create x y = {Width = x; Height = y}

    let width {Width = w} = w
    let height {Height = h} = h


module Rectangle =
    let create p s = Rectangle(p, s)

    let apply f (Rectangle (p, s)) = f (p, s)

    let map f = Rectangle << apply f

    let location = apply fst

    let Width = apply (snd >> Size.width)
    
    let Height = apply (snd >> Size.height)

    let leftEdge = apply (fst >> (fun p -> p.X))

    let rightEdge = apply (fun (p, s) -> p.X + s.Width)

    let intersect (Rectangle (p1, s1)) (Rectangle (p2, s2)) = 
        if p1.X > p2.X + s2.Width || p2.X > p1.X + s1.Width then
            false
        else if p1.Y > p2.Y + s2.Height || p2.Y > p1.Y + s1.Height then 
            false
        else 
            true

    let moveX deltaX = map (fun (p, s) -> (p |> Point.moveX deltaX, s))
        
    let moveY deltaY = map (fun (p, s) -> (p |> Point.moveY deltaY, s))


