
# R package to download data from the World Inequality Database (WID.world)

This package downloads data from the online World Inequality
Database (WID.world) directly into R. The World Inequality Database is an
extensive source on the historical evolution of the
distribution of income and wealth both within and between countries.
It relies on the combined effort of an international network of over a
hundred researchers covering more than seventy countries from all continents.

## Installation

Install the CRAN release with:

```r
install.packages("wid")
```

To install the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("world-inequality-database/wid-r-tool")
```

## Data source

Data are retrieved through the webservice
<https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/>.

## Usage

The package exports a single function `download_wid(...)`. See `?download_wid` for help.

## Demo

See `vignette("wid-demo")`, or [click here](https://github.com/world-inequality-database/wid-r-tool/blob/master/vignettes/wid-demo.pdf) for a demonstration
of the package.
