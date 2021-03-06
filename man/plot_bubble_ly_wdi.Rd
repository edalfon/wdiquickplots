% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quickplots.R
\name{plot_bubble_ly_wdi}
\alias{plot_bubble_ly_wdi}
\title{Bubble plot, interactive (powered by plotly)}
\usage{
plot_bubble_ly_wdi(
  x_indicator = "SH.XPD.GHED.GD.ZS",
  y_indicator = "SH.SGR.CRSK.ZS",
  size_indicator = "SP.POP.TOTL",
  highlight_countries = c("Colombia", "Germany"),
  start = lubridate::year(Sys.Date()) - 10,
  end = lubridate::year(Sys.Date() - months(18)),
  country = "all",
  regions = default_regions(),
  income_groups = default_income_groups()
)
}
\arguments{
\item{x_indicator}{code of indicator for the x axis}

\item{y_indicator}{code of indicator for the y axis}

\item{size_indicator}{code of indicator for the size of the markers}

\item{highlight_countries}{character vector with country names to highlight}

\item{start}{first year to download data}

\item{end}{last year to download data}

\item{country}{subset of countries to download data from. Default to "all".
This argument is passed as is to WDI::WDI and hence, requires
'ISO-2 character codes, e.g. "BR", "US", "CA"'.}

\item{regions}{character vector to filter the data only to specific regions}

\item{income_groups}{character vector to filter the data only to specific
income groups}
}
\value{
a plotly object
}
\description{
Scatter plot that let's you map the size of the markers to an indicator
}
\examples{
\dontrun{
plot_bubble_ly_wdi()
}
}
