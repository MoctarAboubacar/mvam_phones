# Phone Ownership and Food Security Outcomes — Nepal

Whether households that own phones differ systematically from those that do not on food security outcomes, and what that means for interpreting remote survey data.

Remote data collection reaches only households with phones. If phone ownership is correlated with the outcomes being measured, phone survey estimates are biased for the population as a whole. The direction is predictable (phone owners tend to be better off), but the size of the bias is an empirical question. It determines whether remote monitoring can stand as a population estimate or only as a trend among a reachable subpopulation.

The analysis uses a WFP mVAM panel containing both groups to compare food consumption outcomes between phone owners and non-owners, with survey weights and disaggregation by the characteristics most likely to drive the difference.

The sampling design that this bias question applies to is in [Covid_sampling](https://github.com/MoctarAboubacar/Covid_sampling).

## Methods

Survey-weighted comparison of means, Food Consumption Score construction and classification, disaggregated group comparison.

## Files

| File | Purpose |
|---|---|
| `Phones comparison.R` | Builds FCS categories and compares outcomes across phone ownership groups |
| `Comparison facet graph.png` | Faceted comparison of outcomes by group |

## Data

The analysis uses a WFP mVAM household panel, which is not redistributed here. To run the code, place the panel in `data/`:

| File | Description |
|---|---|
| `mVAM_panel.csv` | mVAM household panel with food security outcomes and phone ownership |

## Reproducing

Paths resolve from the repository root through the `here` package.

```r
install.packages(c("dplyr", "stringr", "foreign", "ggplot2", "survey",
                   "purrr", "ggmap", "here"))

source("Phones comparison.R")
```

## License

MIT — see [LICENSE](LICENSE).
