``` r
# Load required libraries
library(xts)
#> Warning: package 'xts' was built under R version 4.3.3
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
library(PortfolioAnalytics)
#> Warning: package 'PortfolioAnalytics' was built under R version 4.3.3
#> Loading required package: foreach
#> Loading required package: PerformanceAnalytics
#> 
#> Attaching package: 'PerformanceAnalytics'
#> The following object is masked from 'package:graphics':
#> 
#>     legend
library(quantmod)
#> Warning: package 'quantmod' was built under R version 4.3.3
#> Loading required package: TTR
#> Warning: package 'TTR' was built under R version 4.3.3
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(ROI)
#> Warning: package 'ROI' was built under R version 4.3.3
#> Registered S3 method overwritten by 'ROI':
#>   method           from              
#>   print.constraint PortfolioAnalytics
#> ROI: R Optimization Infrastructure
#> Registered solver plugins: nlminb, glpk, quadprog.
#> Default solver: auto.
#> 
#> Attaching package: 'ROI'
#> The following objects are masked from 'package:PortfolioAnalytics':
#> 
#>     is.constraint, objective
require(ROI.plugin.glpk)
#> Loading required package: ROI.plugin.glpk
#> Warning: package 'ROI.plugin.glpk' was built under R version 4.3.3
require(ROI.plugin.quadprog)
#> Loading required package: ROI.plugin.quadprog
#> Warning: package 'ROI.plugin.quadprog' was built under R version 4.3.3

# Define a list of asset symbols
symbols <- c("AAPL", "MSFT", "GOOG", "AMZN", "META")

# Download historical stock prices
getSymbols(symbols, from = "2020-01-01", to = Sys.Date(), src = "yahoo")
#> [1] "AAPL" "MSFT" "GOOG" "AMZN" "META"

# Extract adjusted closing prices
prices <- list()
for (sym in symbols) {
  prices[[sym]] <- Ad(get(sym))
}

# Combine prices into a single xts object
prices_xts <- do.call(merge, prices)

# Calculate daily returns
returns <- Return.calculate(prices_xts)

# Define the portfolio spec
port_spec <- portfolio.spec(assets = colnames(returns))

# Define constraints
port_spec <- add.constraint(port_spec, type = "weight_sum", min_sum = 1, max_sum = 1)
port_spec <- add.constraint(port_spec, type = "box", min = 0, max = 0.5)

# Minimum variance portfolio
minvar_port <- add.objective(port_spec, type = "risk", name = "StdDev")
minvar_port <- add.objective(minvar_port, type = "return", name = "mean")

# Markowitz portfolio
markowitz_port <- add.objective(port_spec, type = "return", name = "mean")
markowitz_port <- add.objective(markowitz_port, type = "risk", name = "StdDev")

# Optimize portfolios
minvar_opt <- optimize.portfolio(returns, minvar_port, optimize_method = "ROI", trace = TRUE)
markowitz_opt <- optimize.portfolio(returns, markowitz_port, optimize_method = "ROI", trace = TRUE)

# Extract portfolio statistics
minvar_stats <- extractStats(minvar_opt)
markowitz_stats <- extractStats(markowitz_opt)

# Convert minvar_stats to dataframe
minvar_df <- data.frame(
  mean = minvar_stats["mean"],
  StdDev = minvar_stats["StdDev"],
  out = minvar_stats["out"],
  w.AAPL = minvar_stats["w.AAPL"],
  w.MSFT = minvar_stats["w.MSFT"],
  w.GOOG = minvar_stats["w.GOOG"],
  w.AMZN = minvar_stats["w.AMZN"],
  w.META = minvar_stats["w.META"]
)

# Convert markowitz_stats to dataframe
markowitz_df <- data.frame(
  mean = markowitz_stats["mean"],
  StdDev = markowitz_stats["StdDev"],
  out = markowitz_stats["out"],
  w.AAPL = markowitz_stats["w.AAPL"],
  w.MSFT = markowitz_stats["w.MSFT"],
  w.GOOG = markowitz_stats["w.GOOG"],
  w.AMZN = markowitz_stats["w.AMZN"],
  w.META = markowitz_stats["w.META"]
)

# Add Portfolio column to each dataframe
minvar_df$Portfolio <- "Minvar"
markowitz_df$Portfolio <- "Markowitz"

# Bind the dataframes together
comparison_df <- rbind(minvar_df, markowitz_df)

# Print the comparison dataframe
print(comparison_df)
#>              mean     StdDev           out w.AAPL w.MSFT w.GOOG w.AMZN w.META
#> mean  0.001136931 0.02006537 -0.0007343121     NA     NA     NA     NA     NA
#> mean1 0.001136931 0.02006537 -0.0007343121     NA     NA     NA     NA     NA
#>       Portfolio
#> mean     Minvar
#> mean1 Markowitz
```

<sup>Created on 2024-03-10 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
