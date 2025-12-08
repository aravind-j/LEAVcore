# Compute Information Length for Quantitative Traits

The function `inflen.quant` computes the length of information code that
can indicate the possession of a specific value by a quantitative trait
(Wallace and Boulton 1968; Balakrishnan and Suresh 2001; Balakrishnan
and Suresh 2001; Balakrishnan and Nair 2003)

.

## Usage

``` r
inflen.quant(x, mean, sd, e = 1)
```

## Arguments

- x:

  Data of a quantitative trait for accessions in a collection as a
  numeric vector.

- mean:

  The target mean.

- sd:

  The target standard deviation

- e:

  The least count of measurement for the quantitative trait.

## Value

A data frame with 2 columns:

- x:

  The quantitative trait data

- inflen:

  Information length computed

## Details

For each quantitative trait \\d\\, it is assumed that it is normally
distributed within subset \\t\\ with mean \\\mu\_{d,t}\\ and the
standard deviation \\\sigma\_{d,t}\\ estimated as below.

\\\mu\_{d,t} = \frac{\sum x\_{d,s}}{n\_{d,t}}\\

\\\sigma\_{d,t} = \sqrt{\frac{\sum(x\_{d,s} - \mu\_{d,t})^2}{n\_{d,t} -
1}}\\

From this, a distribution normalizing constant \\g\_{d,t}\\ can be
estimated as

\\g\_{d,t} = \ln \left ( \frac{\sigma\_{d,t}}{K \cdot \varepsilon\_{d}}
\right )\\

Where \\K = \frac{1}{\sqrt{2\Pi}}\\, \\\varepsilon\_{d}\\ is the least
count of measurement of the descriptor \\d\\. i.e. \\x\\ is measured to
an accuracy of \\\pm\varepsilon\_{d}\\.

The probability of getting a measurement \\x\\ from a distribution of
mean \\\mu\\ and variance \\\sigma\\ is approximately as follows.

\\K \cdot \frac{\varepsilon\_{d}}{\sigma\_{d,t}} \cdot
e^{\frac{-(x\_{d,s} - \mu\_{d,t})^2}{2 \sigma\_{d,t}^{2}}}\\

Now the length of the information code that can optimally indicate the
possession of a value \\x\\ by the trait \\d\\ is computed as follows:

\\c\_{x,d,t} = g\_{d,t} + \frac{(x\_{d,s} - \mu\_{d,t})^2}{2
\sigma\_{d,t}^{2}}\\

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

[`inflen.qual`](https://aravind-j.github.io/LEAVcore/reference/inflen.qual.md)

## Examples

``` r
suppressPackageStartupMessages(library(EvaluateCore))

library(EvaluateCore)

# Get data from EvaluateCore

data("cassava_EC", package = "EvaluateCore")

# Data of 'Average plant weight' quantitative trait
AVPW <- cassava_EC$AVPW

# Compute information length
AVPWinflen <- inflen.quant(x = AVPW, mean = 4, sd = 3.25, e = 1)

head(AVPWinflen)
#>           x   inflen
#> 1 3.0000000 2.144931
#> 2 2.5000000 2.204102
#> 3 0.4000000 2.711085
#> 4 0.3333333 2.734017
#> 5 7.5000000 2.677475
#> 6 5.6000000 2.218777
```
