# Round Numeric Values While Preserving a Target Sum

Applies the Hamilton (largest remainder or Hare-Niemeyer or Vinton)
rounding method (Balinski and Young 2001) to a numeric vector so that
the rounded values sum to a specified target.

## Usage

``` r
round_preserve_sum(x, target = round(sum(x)))
```

## Arguments

- x:

  A numeric vector to round.

- target:

  A numeric scalar giving the desired sum of the rounded values.
  Defaults to `round(sum(x))`.

## Value

An numeric vector of the same length as `x`, where the elements sum to
`target`.

## Details

Values are first rounded down using
[`floor()`](https://rdrr.io/r/base/Round.html), and the remaining
deficit is allocated to elements with the largest fractional parts.

## References

Balinski ML, Young HP (2001). *Fair Representation: Meeting the Ideal of
One Man, One Vote*. Brookings Institution Press. ISBN 978-0-8157-0111-8.

## Examples

``` r
round_preserve_sum(c(1.2, 2.7, 3.5))
#> [1] 1 3 3

round_preserve_sum(c(10.4, 10.4, 10.2), target = 32)
#> [1] 11 11 10
```
