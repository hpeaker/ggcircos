# ggcircos

Note that the package and examples in this repository are a work in progress.

To install:

```{r}
devtools::install_github("https://github.com/hpeaker/ggcircos")
```

# Intro
This package is attempting to replicate some of the function of ggbio (namely the circos plots) but using ggvis instead of ggplot2.

The ggbio package uses the GRanges and IRanges packages in order to formalise the idea of each chromosome being a sequence of defined length and together forming the whole genome.
They do this by defining using S4 classes.

This package currently just uses S3 to get things going quicker but it may make sense to formalise things further at some point.

# Setting up your sequence
We need to define two objects which will be used to define the sequences and their layout.

```{r}
radians <- create_radians()
seq_df <- create_seq_df(radians)
```

By default `create_radians()` will use the chromosomes and their lengths as arguments but any names and lengths can be used

```{r}
radians2 <- create_radians(letters[1:5], c(10, 20, 40, 30, 20), total_gap = 0.1)
seq_df2 <- create_seq_df(radians2)
```

The `total_gap` argument defines how much space will be left between each sequence on the plot. You can also define each gap individually with the `ind_gaps` argument

It may also be useful to define a `track_radians` object which creates smooth interpolations between `radians` which can be passed to some of the plotting functions to create tracks or circles.

```{r}
track_radians <- create_track_radians(radians)
```

It has arguments `approx_points` and `points_per_track` to define how many points you want the interpolation to create. The different options are there to give some flexiblilty in how objects in a circos plot will change when resized etc.

We can now create the beginnings of a circos plot from these objects.

# Plotting

```{r}
ggvis() %>%
    add_track(track_radians, outer = 1, inner = 0.9, fill = ~group, stroke = ~group) %>%
    add_track(track_radians, outer = 0.85, inner = 0.6) %>%
    add_circles(track_radians, r = c(0.65, 0.7, 0.75, 0.8), opacity := 0.2) %>%
    hide_axis("x") %>%
    hide_axis("y")
```

# Further Examples

Additional examples are shown in `inst/example_apps` for either static plots or reactive outputs for shiny applications.

## Note on ggvis
This package extends [ggvis](https://github.com/rstudio/ggvis) functionality which is currently dormant.

