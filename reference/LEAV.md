# Compute the Length of Encoded Attribute Values

For accessions in a collection compute the Length of Encoded Attribute
Values (LEAV) information measure from qualitative and quantitative
trait data (Wallace and Boulton 1968; Balakrishnan and Suresh 2001;
Balakrishnan and Suresh 2001; Balakrishnan and Nair 2003) .

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

A data frame with one row per accession in `data` and the following
columns:

- `names`:

  Accession identifiers, as specified by the `names` argument.

- `lt`:

  The log-ratio message length term, \\\log(N / n)\\, where \\N\\ is the
  total number of accessions in `data` and \\n\\ is the sum of
  frequencies in `freq`.

- `<qualitative traits>`:

  One column per trait specified in `qualitative`, giving the
  information length \\-\log(p_k)\\ for the level \\k\\ of that trait
  observed for each accession.

- `<quantitative traits>`:

  One column per trait specified in `quantitative`, giving the Gaussian
  information length \\\log(\sigma / c \varepsilon) + (x - \mu)^2 /
  2\sigma^2\\ for each accession, where \\c = 1/\sqrt{2\pi}\\.

- `LEAV`:

  The total information length for each accession, equal to the row sum
  of `lt` and all trait information length columns.

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
three different methods.” *Plant Genetic Resources Newsletter*, **134**,
33–41.  
  
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

# Get data from EvaluateCore
data("cassava_EC", package = "EvaluateCore")

cassava_EC <- cassava_EC[sample(1:nrow(cassava_EC), 500), ]

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
  round_preserve_sum(prop * size.count)
})
names(freq_list) <- qual

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

head(LEAV_cassava)
#>   genotypes       lt      CUAL      LNGS     PTLC     DSTA      LFRT   LBTEF
#> 1  TMe-1005 6.214608 0.9162907 0.6931472 1.098612 1.098612 0.9162907 1.94591
#> 2  TMe-1006 6.214608 0.9162907 0.6931472 1.791759 1.791759 1.6094379 1.94591
#> 3  TMe-1007 6.214608 0.9162907 0.6931472 1.098612 1.098612 1.6094379 1.94591
#> 4  TMe-1010 6.214608 0.9162907 0.6931472 1.791759 1.791759 0.9162907 1.94591
#> 5  TMe-1016 6.214608 0.9162907 0.6931472 1.791759 1.791759 0.9162907 1.94591
#> 6  TMe-1025 6.214608 0.9162907 0.6931472 1.098612 1.098612 1.6094379 1.94591
#>        CBTR     NMLB      ANGB   CUAL9M    LVC9M   TNPR9M      PL9M      STRP
#> 1 1.3862944 2.079442 1.6094379 1.098612 1.791759 1.791759 1.3862944 1.6094379
#> 2 1.3862944 2.079442 1.6094379 1.791759 1.098612 1.791759 0.6931472 0.9162907
#> 3 0.6931472 2.079442 0.9162907 1.098612 1.791759 1.791759 0.6931472 1.6094379
#> 4 1.3862944 2.079442 0.9162907 1.098612 1.098612 1.791759 0.6931472 1.6094379
#> 5 0.6931472 2.079442 1.6094379 1.098612 1.098612 1.791759 1.3862944 1.6094379
#> 6 0.6931472 2.079442 0.9162907 1.098612 1.791759 1.791759 1.3862944 1.6094379
#>        STRC      PSTR     NMSR     TTRN    TFWSR     TTRW    TFWSS     TTSW
#> 1 1.0986123 1.0986123 3.375758 1.699297 2.657660 1.449567 2.708255 1.775477
#> 2 0.4054651 1.0986123 3.375758 1.945526 2.718152 1.487245 2.923710 1.919880
#> 3 0.4054651 1.0986123 3.104555 2.114809 3.035732 2.391512 4.495672 3.069218
#> 4 1.0986123 1.0986123 3.268920 2.545711 2.468624 1.468790 2.823164 1.846366
#> 5 1.0986123 0.4054651 3.047027 1.699297 2.559362 2.619117 2.751346 2.129919
#> 6 0.4054651 1.0986123 3.178519 1.699297 3.186016 1.920588 3.707393 1.993814
#>       TTPW     AVPW     ARSR     SRDM     LEAV
#> 1 3.293098 2.262596 1.846366 3.168110 52.06992
#> 2 3.490833 2.525408 1.846366 4.245367 54.31198
#> 3 4.550316 3.142828 1.751848 2.592589 56.00337
#> 4 3.231879 2.257787 1.751848 3.807867 52.61154
#> 5 3.301464 2.702693 1.846366 2.491027 52.28820
#> 6 4.211371 2.429234 1.846366 2.682867 53.30290
```
