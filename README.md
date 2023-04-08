
<!-- README.md is generated from README.Rmd. Please edit that file -->

# untrader

<!-- badges: start -->

[![R CMD
Check](https://github.com/datapumpernickel/untrader/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/datapumpernickel/untrader/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of untrader is to provide a simple wrapper function for the new
[Comtrade API of the UN](https://comtradeplus.un.org/).

## 🚧 Under development 🚧

## 🤗 Looking for help 🤗

### Let me know, if you would like to be part of the developing process and help maintain the package!

## Installation

You can install the development version of untrader like so:

``` r
devtools::install_github('datapumpernickel/untrader')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(untrader)

## you need to set your API key first
# set_primary_comtrade_key()

exports <- get_comtrade_data(freq = 'A',
                   clCode = 'HS',
                   cmdCode = c('2204','2203'),
                   flowCode = 'export',
                   reporterCode = c("ARG","GBR"),
                   partnerCode = 'world',
                   period = "2018:2021",
                  process = T)

ggplot2::ggplot(exports) +
  ggplot2::geom_col(ggplot2::aes(
    x = period,
    y = primaryValue / 1000000,
    fill = stringr::str_wrap(cmd_description,30)
  ),
  position = 'dodge') +
  ggplot2::facet_wrap(. ~ reporter_description) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_manual(name = "HS Code", values = c('#F3B562','#F06060'))+
  ggplot2::ylab("Exports in Million USD") +
  ggplot2::xlab("Year") +
  ggplot2::labs(title = 'Exports of Wine and Beer from Great Britain and Argentina')
```

<img src="man/figures/README-example-1.png" width="100%" />

## Next steps

- [ ] Implement the different modes of transportation

- [ ] Include some more links to the UN Comtrade API FAQ.
