package floatparsing

import (
	"fmt"
	"math"
	"math/bits"
)

/*
 * REFS:
 * https://internals.rust-lang.org/t/implementing-a-fast-correct-float-parser/14670
 * https://www.h-schmidt.net/FloatConverter/IEEE754.html
 * go's source code implementation
 * https://nigeltao.github.io/blog/2020/eisel-lemire.html
 */

func Parse(s string) float64 {
	mantissa, exponent, negative, truncated := extract(s)

	fmt.Println("mantissa: ", mantissa)
	fmt.Println("exponent: ", exponent)
	fmt.Println("negative: ", negative)
	fmt.Println("truncated: ", truncated)
	fmt.Println()

	// fast-path
	if !truncated {
		float, ok := exact(mantissa, exponent, negative)
		fmt.Println("exact ok: ", ok)

		if ok {
			fmt.Println("float: ", float)
			fmt.Println()
			return float
		}

		float, ok = eiselLemire(mantissa, exponent, negative)

		fmt.Println("Eisel-Lemire ok: ", ok)
	}

	return 0
}

func extract(s string) (mantissa uint64, exponent int, negative, truncated bool) {
	if len(s) == 0 {
		return
	}

	i := 0

	if s[i] == '-' {
		negative = true
		i++
	}

	var decimalPoint, mantissaChars int
	var hasDecimal bool
	maxMantissaChars := 19 // largest amount of digits in 64 bits

	for ; i < len(s); i++ {
		if s[i] == '.' {
			if hasDecimal {
				panic("invalid")
			}

			decimalPoint = i
			hasDecimal = true
			continue
		}

		if '0' <= s[i] && s[i] <= '9' {
			if mantissaChars < maxMantissaChars {
				mantissa = mantissa*10 + uint64(s[i]-'0')
				mantissaChars++
			} else if s[i] != 0 { // dont say truncated because of trailing 0
				truncated = true
			}

			continue
		}

		break
	}

	if !hasDecimal {
		decimalPoint = mantissaChars
	}

	exponent = decimalPoint - mantissaChars

	if i < len(s) {
		if s[i] != 'e' {
			panic("syntax error")
		}

		i++

		eExponentSign := 1
		var eExponent int

		if i >= len(s) {
			panic("syntax error")
		} else if s[i] == '-' {
			eExponentSign = -1
			i++
		}

		for ; i < len(s); i++ {
			if '0' <= s[i] && s[i] <= '9' {
				eExponent = eExponent*10 + int(s[i]-'0')
			} else {
				panic("syntax error")
			}
		}

		exponent += eExponentSign * eExponent
	}

	if mantissa == 0 {
		exponent = 0
	}

	return
}

var POWERS_OF_TEN = []float64{
	1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
	1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
}

// if digits fit into float mantissa, just put it there
func exact(mantissa uint64, exponent int, negative bool) (float64, bool) {
	if mantissa>>53 != 0 { // digits cannot fit into mantissa
		return 0, false
	}

	float := float64(mantissa) // put digits straight into mantissa
	if negative {
		float = -float
	}

	if exponent == 0 {
		return float, true
	} else if 0 < exponent && exponent <= 22 { // fits
		return float * POWERS_OF_TEN[exponent], true
	} else if -22 <= exponent && exponent < 0 {
		return float / POWERS_OF_TEN[-exponent], true
	} else if 22 < exponent && exponent <= 15+22 { // 15 for regular int 22 for power of 10
		float *= POWERS_OF_TEN[exponent-22]

		if float < 1e-15 || float > 1e15 { // mantissa doesn't fit (10^15 fits in mantissa)
			return 0, false
		}

		return float * POWERS_OF_TEN[22], true // rest of the exponent
	}

	return 0, false
}

func eiselLemire(mantissa uint64, exponent10 int, negative bool) (float64, bool) {
	if mantissa == 0 {
		// should be unreachable, but still mentionded in blog post
		if negative {
			return math.Float64frombits(0x8000000000000000), true
		}
		return 0, true
	}

	if exponent10 < WidePowersOf10MinExp10 || exponent10 > WidePowersOf10MaxExp10 {
		return 0, false
	}

	// Normalization
	clz := bits.LeadingZeros64(mantissa)
	mantissa <<= clz
	// 217706 ≈ log_2(10)<<16
	// 217706>>16 ≈ log_2(10)
	// n*217706>>16 ≈ n*log_2(10)
	_ = uint64((217706*exponent10)>>16 + 1150) - uint64(clz)

	// Multiplication
	_, _ = bits.Mul64(mantissa, WidePowersOf10[-WidePowersOf10MinExp10][1])

	return 0, false
}
