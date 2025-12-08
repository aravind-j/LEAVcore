# Relative Frequency Adjustments

Compute and transform relative frequencies or accessions for a
qualitative trait by the following methods (Balakrishnan and Suresh
2001) :

- Square root-proportion

- Log-frequency

## Usage

``` r
prop.adj(x, method = c("none", "log", "sqrt"))
```

## Arguments

- x:

  Data of a qualitative trait for accessions in a collection as a vector
  of type factor.

- method:

  The method for transformation. Either `"none"` for no transformation
  or `"log"` for log-frequency transformation or `"sqrt"` for square
  root-proportion transformation.

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
as \\q\_{i} = \frac{\log{F\_{i}}}{\sum\_{i=1}^{s}\log{F\_{i}}}\\

Where \\F\_{i}\\ is the absolute frequency of the \\i\\th descriptive
state for a qualitative trait in a collection.

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
#>   0.24903071   0.02990846   0.29298449   0.16703765   0.26103869 
```
