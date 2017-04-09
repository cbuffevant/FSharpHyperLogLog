namespace HyperLogLog.Test
open NUnit.Framework

module HyperLogLogTest = 
    open Main
    open TestUtils

    [<Test>]
    let TestSigma() =
        let b = 14uy
        let sigmaForB = b |> HLL.Sigma

        Assert.That(sigmaForB 0UL, Is.EqualTo(51))
        Assert.That(sigmaForB 1UL, Is.EqualTo(50))
        Assert.That(sigmaForB 8UL, Is.EqualTo(47))
        let value = (2.0 ** (float)(64uy - b) - 1.0) |> uint64
        Assert.That(sigmaForB value, Is.EqualTo(1))
        let value = (2.0 ** (float)(64uy - b) + 1.0) |> uint64
        Assert.That(sigmaForB value, Is.EqualTo(51))

    // [<Test>]
    let TestDifferentAccuracies() =
        let stdError4Bits = 0.26
        RunTest stdError4Bits 1000000

        let stdError12Bits = 0.01625
        RunTest stdError12Bits 1000000

        let stdError14Bits = 0.008125
        RunTest stdError14Bits 1000000

        let stdError16Bits = 0.0040625
        RunTest stdError16Bits 1000000