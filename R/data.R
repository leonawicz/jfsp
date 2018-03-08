#' Empirical fire management costs.
#'
#' A data frame of empirical historical fire management costs in dollars per acre.
#'
#' @format A data frame with 7 rows of annual cost observations for each of four columns of FMO zones.
"basecost"

#' Historical and projected estimated fire management costs.
#'
#' A data frame of estimated fire management costs in millions of dollars based on MC sampling of empirical costs and ALFRESCO simulated landscape fire scar model output.
#'
#' @format A data frame with 1,740 rows and 5 columns pertaining to ALFRESCO simulation FMO treatment, RCP, year, statistic and value.
"cost"

#' Projected estimated fire management costs.
#'
#' A data frame of estimated fire management costs in millions of dollars based on MC sampling of empirical costs and ALFRESCO simulated landscape fire scar model output.
#' Decadal averages beginning with the 2020s of annual mean, 5th and 95th percentile costs are provided in this simplified summary of the \code{cost} data set.
#'
#' @format A data frame with 48 rows of decadal averages of annual cost statistics pertaining to ALFRESCO simulation set, FMO treatment, RCP, and decade.
"costSummary"

#' Annual burn area.
#'
#' A data frame of empirical historical annual burn area and ALFRESCO model burn area outputs for historical and projected periods, by FMO zone.
#' ID columns include ALFRESCO model set, FMO treatment, RCP, FMO zone, year, and the statistics: burn area (BA), cumulative burn area (CBA) and a 10-year moving average of inter-annual standard deviation in BA (BA_sd_ma10).
#'
#'
#' @format A data frame with 3,864 rows and 8 columns.
"fmoba"

#' Burn area summary statistics.
#'
#' A data frame of burn area summary statistics by FMO zone. This data set is provided as a convenient summary of the \code{fmoba} data set.
#' It summarizes \code{fmoba} over time, providing the following statistics: minimum, first quartile, median, mean, third quartile and maximum burn area.
#'
#' @format A data frame with 48 rows and 10 columns.
"fmobaSummary"

#' Coniferous:deciduous ratios.
#'
#' A data frame of annual Alaska coniferous:deciduous landscape cover area ratios computed from ALFRESCO model output, by ALFRESCO FMO treatment and RCP.
#'
#' @format A data frame with 580 rows and 4 columns.
"cdratio"

#' Coniferous:deciduous burn area.
#'
#' A data frame of annual Alaska coniferous and deciduous burn area total computed from ALFRESCO model output, by ALFRESCO FMO treatment and RCP.
#'
#' @format A data frame with 1,160 rows and 5 columns.
"cdba"

#' Probability of fire near Fairbanks, Alaska.
#'
#' A data frame of the probability of fire near Fairbanks, Alaska within each of a sequence of radial buffers of Fairbanks, based on ALFRESCO simulation output.
#' Probabilities as a function of radius are broken out by FMO treatment, RCP, and different periods of time.
#'
#' @format A data frame with 1,900 rows and 5 columns.
"fbxfire"

#' Alaska fire sizes.
#'
#' A data frame of the decadal distribution of fire sizes based on ALFRESCO simulation output, by FMO treatment and RCP.
#' The FS column gives the fire size in acres and the Freq column gives the frequency of fires that size over a decade and multiple ALFRESCO simulation replicates.
#' Frequencies should be treated as simple weights and can be rescaled if desired. Freq is an average frequency across GCMs during the projected period, 2014 - 2099, so may contain decimals.
#'
#' @format A data frame with 63,200 rows and 5 columns.
"firesize"
