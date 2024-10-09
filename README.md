
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zerozarr

<!-- badges: start -->
<!-- badges: end -->

Here we cut a 3D array into 12 pieces, as a start at illustrating how
Zarr works.

Note that everything here will be wrong!! For a little while, I need to
step forward and backwards and look at everything carefully, but I want
it visible. Reach out if you’re interested, I don’t really think I can
write a Zarr implementation, but I can write a toy version with enough
chops to show what’s happening to folks who know R.

For now I’m sticking with 3D only because it’s easy to think about and
show easy illustrations, little chunks of RGB array so I know I have
something sensible and not just one of those sliding tile games.

You can watch me slowly increment this to something useful, I’m obsessed
with how cool Zarr is how it is composed of very general pieces. (And, I
know about {Rarr} and {pizarr} and GDAL multidim and stars and terra,
not wanting to undercut those this is just a learning exercise for now).

## TODO

- n-d tiling (generalize {grout})
- create json metadata
- create parquet metadata
- make an example that avoids degenerate coordinate referencing …
- encoding of chunks and read/write

Also maybe we can do the kerchunk referencing thing with some example
NetCDF files.

``` r
f <- system.file("img", "Rlogo.png", package="png", mustWork = TRUE)

## we have this massive array
a <- fastpng::read_png(f)
dim(a)
#> [1]  76 100   4
```

``` r

## let's transpose and flip
a <- aperm(a, c(2, 1, 3))
## we have a tiling logic, let's go for 26x25x4

## hypertidy/grout on github
tiling <- grout::grout(dim(a)[1:2], blocksize = c(25, 26))
idx <- grout::tile_index(tiling)
idx
#> # A tibble: 12 × 11
#>     tile offset_x offset_y tile_col tile_row  ncol  nrow  xmin  xmax  ymin  ymax
#>    <int>    <dbl>    <dbl>    <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1        0        0        1        1    25    26     0    25    50    76
#>  2     2       25        0        2        1    25    26    25    50    50    76
#>  3     3       50        0        3        1    25    26    50    75    50    76
#>  4     4       75        0        4        1    25    26    75   100    50    76
#>  5     5        0       26        1        2    25    26     0    25    24    50
#>  6     6       25       26        2        2    25    26    25    50    24    50
#>  7     7       50       26        3        2    25    26    50    75    24    50
#>  8     8       75       26        4        2    25    26    75   100    24    50
#>  9     9        0       52        1        3    25    24     0    25     0    24
#> 10    10       25       52        2        3    25    24    25    50     0    24
#> 11    11       50       52        3        3    25    24    50    75     0    24
#> 12    12       75       52        4        3    25    24    75   100     0    24
```

``` r

## we necessarily have some dangle, but we minimized it by choosing 26 not 25 for x
plot(tiling)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

subset_3d_array <- function(x, offset, size) {
  ## offsets are zero-based
  l <- apply(rbind(offset + 1, size), 2, \(.x) seq(.x[1], length.out = .x[2]))
  x[l[[1]], l[[2]], l[[3]]]
}
## offset and size are the values in idx
str(subset_3d_array(a, c(0, 0, 0), c(26, 25, 4)))
#>  num [1:26, 1:25, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
```

``` r
## our table idx describes the blocks, and their sizes (some are 26x25x4, some are 24x25x4)
split_array_blocks <- function(x, index, n3 = 4) {
  index$offset_z <- 0
  index$nz <- n3
  offsets <- lapply(purrr::transpose(index[c("offset_x", "offset_y", "offset_z")]), unlist)
  sizes <- lapply(purrr::transpose(index[c("ncol", "nrow", "nz")]), unlist)
  
  l <- vector("list", nrow(index))
  for (i in seq_along(offsets)) {
    l[[i]] <- subset_3d_array(x, offsets[[i]], sizes[[i]])
  }
  l
}

## this is the list of 0.0.0 0.1.0 1.0.0 1.1.0 ... 2.3.0 
## blocks, there are 3 tile columns, and 4 tile rows, and 1 tile in z
## so we go up as far as 2.3.0
l <- split_array_blocks(a, idx)

## ok so we're not n-dimension yet
chunk <- apply(cbind(idx$tile_row - 1, idx$tile_col - 1, 0), 1, paste0, collapse = ".")
```

That indexing for the chunks 0.0.0 –\> 2.3.0 will hold the implicit
arrangement of the tiles.

We also have an xmin/xmax/ymin/ymax for each chunk, which is extremely
convenient.

``` r
par(bg = "grey35")
plot(NA, xlab = "", ylab = "", asp = 1,
     xlim = c(0, max(idx$xmax)), ylim = c(0, max(idx$ymax)))
for (i in seq_along(l)) {
  ex <- unlist(idx[i, c("xmin", "xmax", "ymin", "ymax")])
  ximage::ximage(aperm(l[[i]], c(2, 1, 3)), ex, add = TRUE)
  p <- c(mean(ex[1:2]), mean(ex[3:4]))
  vaster::plot_extent(ex, add = TRUE)
  text(p[1], p[2], lab = chunk[i], cex = 2, col = "hotpink")

}
```

<img src="man/figures/README-plot-1.png" width="100%" />
