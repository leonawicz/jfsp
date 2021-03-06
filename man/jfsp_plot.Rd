% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{jfsp_plot}
\alias{jfsp_plot}
\title{Create plots based on various JFSP data sets}
\usage{
jfsp_plot(type = NULL, years = NULL, by_rcp = TRUE, by_tx = TRUE,
  col = NULL, file = NULL, base_size = 14, text_size = 18,
  pt_size = 2, ...)
}
\arguments{
\item{type}{character, the type of plot to make, based on a particular package data set. See details.}

\item{years}{numeric, vector of consecutive years. The maximum range is \code{1950:2099}. See details.}

\item{by_rcp}{logical, condition on RCP, defaults to \code{TRUE}. Otherwise marginalize over RCP. This applied to RCP-driven years, 2014 - 2099.}

\item{by_tx}{logical, if \code{FALSE}, then treatments are dropped and only status quo (control) data are plotted.}

\item{col}{optional vector of colors to override the defaults built into \code{jfsp_plot}.}

\item{file}{character, if provided, the plot is saved to disk. Otherwise, it is plotted in the R session graphics device. See details.}

\item{base_size}{base size passed to theme. See details.}

\item{text_size}{text size passed to theme.}

\item{pt_size}{override default size for points in applicable plot types.}

\item{...}{additional arguments. See details.}
}
\value{
a ggplot object. If saving a png file to disk, nothing is returned.
}
\description{
Create plots for each of the data sets included in the package using one convenient wrapper function.
}
\details{
Data sets included in the package can be explicitly plotted however desired,
but for convenience the package offers this wrapper function that creates stock plots associated with package data sets.
A plot \code{type} calls the applicable data set for that plot internally.
Stock plots use standardized formatting similar to the main \code{snapplot} package plot theme.

Available stock plots include:

\describe{
  \item{\code{ba_sd}}{n-year moving average FMO zone burn area annual time series. Optional arguments: \code{continuous}, \code{alaska = TRUE}, \code{breaks}, \code{fmo}, \code{n}, \code{obs = TRUE}.}
  \item{\code{ba_box}}{FMO zone burn area aggregate period box plots. Optional arguments: \code{alaska = TRUE}, \code{log = TRUE}, \code{fmo}.}
  \item{\code{cba}}{FMO zone cumulative burn area annual time series. Optional arguments: \code{alaska = TRUE}, \code{breaks}, \code{fmo}.}
  \item{\code{cost}}{Alaska fire management annual costs time series. Optional arguments: \code{breaks}, \code{obs = TRUE}.}
  \item{\code{cost_dec}}{Alaska fire management decadal projected costs time series. Optional arguments: \code{obs = TRUE}}
  \item{\code{cdratio}}{Alaska coniferous:deciduous ratios annual time series. Optional arguments: \code{breaks}.}
  \item{\code{cdba}}{Alaska coniferous and deciduous annual burn area time series. Optional arguments: \code{breaks}.}
  \item{\code{pfire}}{Probability of fire near Fairbanks as a function of radial buffer distance. The \code{years} argument is ignored for this plot.}
  \item{\code{fs_box}}{Alaska fire size distribution decadal box plots. Optional arguments: \code{log}.}
}

Additional arguments can be provided. General arguments include \code{family} (font family).
Arguments related to specific plot types are ignored when not applicable.
\code{alaska = TRUE} performs statewide aggregation over all FMO zones. \code{log = TRUE} applies a log scale transformation.
\code{continuous = TRUE} avoids a break in the time series where historical meets RCPs by triplicating the historical data and merging with each RCP series, only for \code{ba_sd}.
\code{n} is an integer for the number of years in the moving average window for \code{ba_sd}. Defaults to 30.
\code{breaks} is a vector of breaks applicable to time series plots with years along the x-axis.
\code{fmo} allows for subsetting the FMO zones available in the \code{fmoba} data set; not applicable when \code{alaska = TRUE}.
If not provided, it defaults to \code{fmo = c("Full", "Critical")} since these are the most important zones for work encapsulated by the package.
\code{obs = TRUE} will overlay a representation of historical observed data on a plot, such as a horizontal line showing the historical average. This applies to \code{ba_sd}, \code{cost} and \code{cost_dec}.

If the \code{showtext} is loaded, it may be necessary to significantly increase \code{base_size} and/or \code{text_size} for the 300 dpi image saved when \code{file} is not \code{NULL}.
Google fonts and even base R fonts may shrink significantly relative to the plot under these circumstances with showtext in effect (e.g., after calling \code{showtext_auto}).
If this happens, try 20 and 60 instead of 14 and 18, respectively.
This will be done for you automatically if you elect to name your font with the special name, \code{"gfont"}, and pass it as \code{family = "gfont"}.

When saving a plot directly from \code{jfsp_plot} by passing a filename to \code{file},
\code{jfsp_plot} will internally adjust plot formatting settings in order to save a 300 dpi resolution image to disk while maintaining consistent and appropriate sizing of text and other plot elements.
}
\examples{
jfsp_plot("ba_box", 1950:2013, log = TRUE)
}
