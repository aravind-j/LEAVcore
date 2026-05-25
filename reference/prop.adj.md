# Relative Frequency Adjustments

Compute and transform relative frequencies for a qualitative trait in a
germplasm collection by the following methods (Balakrishnan and Suresh
2001) :

- Square root-proportion

- Log-frequency

## Usage

``` r
prop.adj(x, method = c("none", "log", "sqrt"), size.count = NULL)
```

## Arguments

- x:

  Data of a qualitative trait for accessions in a collection as a vector
  of type factor.

- method:

  The method for transformation. Either `"none"` for no transformation
  or `"log"` for log-frequency transformation or `"sqrt"` for square
  root-proportion transformation.

- size.count:

  A positive integer specifying the target size of the core collection.
  The sum of frequencies allocated across levels of each qualitative
  trait will not exceed this value, and serves as the upper bound for
  iterative proportion clamping when `size.count` is supplied. If
  `NULL`, no clamping is performed and the adjusted proportions are
  returned as-is.

## Value

The relative frequencies as a named numeric vector.

## Details

If \\p\_{i}\\ is the relative frequency of the \\i\\th descriptive state
for a qualitative trait in a collection, then the square root-proportion
transformed relative \\q\_{i}\\ is computed as

\\q\_{i} = \frac{\sqrt{p\_{i}}}{\sum\_{i=1}^{s}\sqrt{p\_{i}}}\\

Where \\s\\ is the number of possible descriptor states for the
qualitative trait in the collection.

Similarly, the log-frequency transformed relative \\q\_{i}\\ is computed
as

\\q\_{i} = \frac{\log(F\_{i} + k)}{\sum\_{i=1}^{s}\log(F\_{i} + k)}\\

Where \\F\_{i}\\ is the absolute frequency of the \\i\\th descriptive
state for a qualitative trait in a collection. It is incremented by a
constant \\k = 0.000001\\ prior to log transformation. This ensures that
singleton descriptor states (where \\F\_{i} = 1\\) yield a small but
non-zero proportion rather than being assigned a zero proportion due to
\\\log(1) = 0\\, which would otherwise exclude all accessions of that
descriptor state from core selection irrespective of `size.count`.

When `size.count` is supplied, the transformed proportions \\q\_{i}\\
are subject to iterative clamping to ensure that the implied frequency
\\q\_{i} \times n\\ for any descriptor state \\i\\ does not exceed its
actual count in the collection, where \\n\\ is `size.count`. Excess
proportion from clamped states is redistributed proportionally among
unclamped states and the process repeats until no state exceeds its
maximum allowable proportion \\F\_{i} / n\\.

## References

Balakrishnan R, Suresh KK (2001). “Strategies for developing core
collections of safflower (*Carthamus tinctorius* L.) germplasm-part II.
Using an information measure for obtaining a core sample with
pre-determined diversity levels for several descriptors simultaneously.”
*Indian Journal of Plant Genetic Resources*, **14**(1), 32–42.

## Examples

``` r
suppressPackageStartupMessages(library(EvaluateCore))

library(EvaluateCore)

# Get data from EvaluateCore

data("cassava_EC", package = "EvaluateCore")

# Data of 'Colour of unexpanded apical leaves' qualitative trait
CUAL <- as.factor(cassava_EC$CUAL)

# Raw relative frequencies
prop.adj(CUAL, method = "none")
#>   Dark green        Green Green purple  Light green       Purple 
#>  0.190617577  0.001187648  0.527909739  0.028503563  0.251781473 

# Square root-proportion transformed relative frequencies
prop.adj(CUAL, method = "sqrt")
#>   Dark green        Green Green purple  Light green       Purple 
#>   0.23369439   0.01844636   0.38890779   0.09036836   0.26858311 

# Square log-frequency transformed relative frequencies
prop.adj(CUAL, method = "log")
#>   Dark green        Green Green purple  Light green       Purple 
#>   0.24903071   0.02990848   0.29298448   0.16703764   0.26103868 
```
