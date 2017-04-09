open System

module HLL =
    type T<'a when 'a : equality> = {
        b : byte;
        M : Map<uint64, uint64>;
        hash : 'a -> uint64
        alpha : float
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="x"></param>
    let private GetLeftMostBit x =
        if 0UL = x then 0uy
        else
            let mutable r = 1uy
            let mutable y = x
            if y &&& 0xffffffff00000000UL <> 0UL then y <- y >>> 32; r <- r + 32uy
            if y &&& 0x00000000ffff0000UL <> 0UL then y <- y >>> 16; r <- r + 16uy
            if y &&& 0x000000000000ff00UL <> 0UL then y <- y >>> 8; r <- r + 8uy
            if y &&& 0x00000000000000f0UL <> 0UL then y <- y >>> 4; r <- r + 4uy
            if y &&& 0x000000000000000cUL <> 0UL then y <- y >>> 2; r <- r + 2uy
            if y &&& 0x0000000000000002UL <> 0UL then r <- r + 1uy;
            r

    /// <summary>
    /// Returns the number of leading zeros after indexing part of the hash is taken out.
    /// </summary>
    /// <param name="bitsForIndexing"></param>
    /// <param name="hash">Stream hash</param>
    let Sigma bitsForIndexing hash =
        let substreamBits = 64uy - bitsForIndexing
        let leftMostBit = hash |> GetLeftMostBit
        let sigma = 
            if leftMostBit <= substreamBits then
                substreamBits - leftMostBit
            else
                substreamBits
        sigma + 1uy |> uint64

    let private Alpha m = 
        match m with
            | 16.0 -> 0.673
            | 32.0 -> 0.697
            | 64.0 -> 0.709
            | _  -> 0.7213/(1.0 + 1.079/m)

    let CreateEstimator<'a when 'a : equality> b (hash : 'a -> uint64) =
        let m = 2.0 ** (b |> float)
        { 
            b = b; 
            M = Map.empty;
            hash = hash;
            alpha = Alpha m
        }

    let AddElement<'a when 'a : equality> estimator (element : 'a) =
        let bitsForIndexing = 64uy - estimator.b
        let x = element |> estimator.hash
        let idx = x >>> (bitsForIndexing |> int)
        let sigma = Sigma bitsForIndexing x
        let mj = if Map.containsKey idx estimator.M then
                        max estimator.M.[idx] sigma
                    else
                        sigma
        {
            b = estimator.b;
            alpha = estimator.alpha;
            hash = estimator.hash;
            M = Map.add idx  mj  estimator.M
        }
    let private LinearCounting m V =
        m * log(m / V) 

    let private BiasCorrection E m count=
        if E <= 2.5 * m then
            // small range correction
            let V = m - (count |> float) // Number of registers equals to 0
            if V <> 0.0 then 
                LinearCounting m V
            else 
                E
        // intermediate range - no correction
        else if E <= 143165576.5333 then 
            E
        else 
        // large range correction
            -(UInt32.MaxValue |> float) * log(1.0 - E / (UInt32.MaxValue |> float))

    let Count<'a when 'a : equality> estimator =
        let m = 2.0 ** (estimator.b |> float)
        let zInv = Map.fold 
                    (fun state _ value ->state + 2.0 ** -(value |> float))
                    0.0
                    estimator.M
        let E = estimator.alpha * m * m / zInv
        BiasCorrection E m estimator.M.Count