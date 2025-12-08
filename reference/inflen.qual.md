# Compute Information Length for Qualitative Traits

The function `inflen.qual` computes the length of information code that
can indicate the possession of a descriptor state of a qualitative trait
(Wallace and Boulton 1968; Balakrishnan and Suresh 2001; Balakrishnan
and Suresh 2001; Balakrishnan and Nair 2003)

.

## Usage

``` r
inflen.qual(x, freq, adj = TRUE)
```

## Arguments

- x:

  Data of a qualitative trait for accessions in a collection as a vector
  of type factor.

- freq:

  The target absolute frequencies of the descriptor states of the
  qualitative trait `x` in the subset of all accessions.

- adj:

  logical. If `TRUE`, the proportion estimates are slightly biased to
  include zero frequency descriptor states in the computation (See
  **Details**). Default is `TRUE`.

## Value

A data frame with 2 columns:

- x:

  The qualitative trait data

- inflen:

  Information length computed

## Details

For each qualitative trait/descriptor \\d\\ the probability of
occurrence of a descriptor state \\m\\ in the in a subset \\t\\ is
estimated as

\\p\_{m,d,t} = \frac{n\_{m,d,t} + 1}{n\_{d,t} + M\_{d}}\\

Where, \\n\_{m,d,t}\\ is the number of accessions with \\m\\ state of
trait \\d\\ in subset \\t\\, \\n\_{d,t}\\ is the number of accessions
with any known state of trait \\d\\ in subset \\t\\,i.e. the number of
accessions in subset \\t\\ and \\M\_{d}\\ is the number of descriptor
states of trait \\d\\.

This is a slightly biased estimate to include zero frequency descriptor
states in the computation. The actual estimate is

\\p\_{m,d,t} = \frac{n\_{m,d,t}}{n\_{d,t}}\\

Now the length of the information code that can optimally indicate the
possession of descriptor state \\m\\ of trait \\d\\ in the subset \\t\\
is computed as

\\c\_{m,d,t} = -\ln p\_{m,d,t}\\

## References

Balakrishnan R, Nair NV (2003). “Strategies for developing core
collections of sugarcane (*Saccharum officinarum* L.)
germplasm-comparison of sampling from diversity groups constituted by
three different methods.” *Plant Genetic Resources Newsletter*, 33–41.  
  
Balakrishnan R, Suresh KK (2001). “Strategies for developing core
collections of safflower (*Carthamus tinctorius* L.) germplasm-part II.
Using an information measure for obtaining a core sample with
pre-determined diversity levels for several descriptors simultaneously.”
*Indian Journal of Plant Genetic Resources*, **14**(1), 32–42.  
  
Balakrishnan R, Suresh KK (2001). “Strategies for developing core
collections of safflower (*Carthamus tinctorius* L.) germplasm-part III.
Obtaining diversity groups based on an information measure.” *Indian
Journal of Plant Genetic Resources*, **14**(3), 342–349.  
  
Wallace CS, Boulton DM (1968). “An information measure for
classification.” *The Computer Journal*, **11**(2), 185–194.

## See also

[`inflen.quant`](https://aravind-j.github.io/LEAVcore/reference/inflen.quant.md)

## Examples

``` r
suppressPackageStartupMessages(library(EvaluateCore))

library(EvaluateCore)

# Get data from EvaluateCore

data("cassava_EC", package = "EvaluateCore")

# Data of 'Colour of unexpanded apical leaves' qualitative trait
CUAL <- as.factor(cassava_EC$CUAL)

# Get frequencies based on sample size
prop <-  prop.adj(CUAL, method = "sqrt")
size.prop <- 0.2
size.count <- ceiling(size.prop * length(CUAL))
CUALfreq <- round(prop * size.count)

# Compute information length
CUALinflen <- inflen.qual(x = CUAL, freq = CUALfreq, adj = TRUE)

head(CUALinflen)
#>             x   inflen
#> 1  Dark green 1.452784
#> 2 Light green 2.400824
#> 3  Dark green 1.452784
#> 4  Dark green 1.452784
#> 5  Dark green 1.452784
#> 6  Dark green 1.452784
```
