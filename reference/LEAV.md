# Compute the Length of Encoded Attribute Values

For accessions in a collection compute the Length of Encoded Attribute
Values (LEAV) information measure from qualitative and quantitative
trait data (Wallace and Boulton 1968; Balakrishnan and Suresh 2001;
Balakrishnan and Suresh 2001; Balakrishnan and Nair 2003)

.

## Usage

``` r
LEAV(
  data,
  names,
  quantitative = NULL,
  qualitative = NULL,
  freq,
  adj = TRUE,
  mean,
  sd,
  e
)
```

## Arguments

- data:

  The data as a data frame object. The data frame should possess one row
  per individual and columns with the individual names and multiple
  trait/character data.

- names:

  Name of column with the individual names as a character string.

- quantitative:

  Name of columns with the quantitative traits as a character vector.

- qualitative:

  Name of columns with the qualitative traits as a character vector.

- freq:

  A named list with the target absolute frequencies of the descriptor
  states for each qualitative trait specified in `qualitative`. The list
  names should be same as `qualitative`.

- adj:

  logical. If `TRUE`, the proportion estimates are slightly biased to
  include zero frequency descriptor states in the computation (See
  **Details**). Default is `TRUE`.

- mean:

  A named numeric vector of target means for each quantitative trait
  specified in `quantitative`. The list names should be same as
  `quantitative`.

- sd:

  A named numeric vector of target standard deviation for each
  quantitative trait specified in `quantitative`. The list names should
  be same as `quantitative`.

- e:

  A named numeric vector of least count of measurement for each
  quantitative trait specified in `quantitative`. The list names should
  be same as `quantitative`.

## Value

A data frame with

## Details

For each accession \\s\\ in the collection, the message length
\\F\_{s}\\ to optimally encode all the \\d\\ traits/descriptors is
computed as follows using the joint density distribution of the whole
collection.

\\F\_{s} = l\_{t} + \sum\_{i=1}^{p} c\_{m\_{s},d\_{i},t} +
\sum\_{j=1}^{q} c\_{x\_{s},d\_{j},t}\\

Here, the first expression \\l\_{t}\\ is the message length for the
subset \\t\\ to which an accession belongs when there are \\N\\
accessions in the whole collection and \\n\_{t}\\ accessions in the
subset \\t\\.

\\l\_{t} = l\_{t} = - \ln{\left ( \frac{N}{n\_{t}} \right )}\\

Similarly \\\sum\_{i=1}^{p} c\_{m\_{s},d\_{i},t}\\ is sum of the optimum
message length for \\p\\ qualitative traits, \\\sum\_{j=1}^{q}
c\_{x\_{s},d\_{j},t}\\ is sum of the optimum optimum message length for
\\q\\ quantitative traits. See
[`inflen.qual`](https://aravind-j.github.io/LEAVcore/reference/inflen.qual.md)
and
[`inflen.quant`](https://aravind-j.github.io/LEAVcore/reference/inflen.quant.md)
for more details.

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

[`inflen.qual`](https://aravind-j.github.io/LEAVcore/reference/inflen.qual.md),
[`inflen.quant`](https://aravind-j.github.io/LEAVcore/reference/inflen.quant.md)

## Examples

``` r
suppressPackageStartupMessages(library(EvaluateCore))

library(EvaluateCore)

# Get data from EvaluateCore
data("cassava_EC", package = "EvaluateCore")

cassava_EC <- cbind(genotypes = rownames(cassava_EC), cassava_EC)


quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
                  "ARSR", "SRDM")
qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
                 "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
                 "PSTR")

cassava_EC[, qual] <- lapply(cassava_EC[, qual], as.factor)

size <- 0.2

freq_list <- lapply(qual, function(x) {
  prop <-  prop.adj(cassava_EC[, x], method = "sqrt")
  size.count <- ceiling(size * length(x))
  round.to.target(prop * size.count)
})
#> Error in round.to.target(prop * size.count): could not find function "round.to.target"
names(freq_list) <- qual
#> Error: object 'freq_list' not found

mean_vec <- sapply(cassava_EC[, quant],
                   function(x) {
                     floor(mean(x))
                   })
names(mean_vec) <- quant

sd_vec <- sapply(cassava_EC[quant],
             function(x) {
               round(sd(x), 1)
             })
names(sd_vec) <- quant

e_vec <- rep(1, length(quant))
names(e_vec) <- quant


# Compute LEAV
LEAV_cassava <- LEAV(data = cassava_EC, names = "genotypes",
                     quantitative = quant, qualitative = qual,
                     freq = freq_list, adj = TRUE,
                     mean = mean_vec, sd = sd_vec, e = e_vec)
#> Error: object 'freq_list' not found

LEAV_cassava
#> Error: object 'LEAV_cassava' not found
```
