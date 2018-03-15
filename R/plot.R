.jfsp_palette <- function(years, by_rcp, by_tx){
  hist_col <- c("black", "orange", "purple")
  if(by_rcp){
    all_col <- c("black", "cornflowerblue", "orange", "firebrick1")
  } else if(by_tx){
    all_col <- hist_col
  } else {
    all_col <- "black"
  }
  proj_col <- if(by_rcp) all_col[-1] else all_col
  if(all(years <= 2013)) hist_col else if(all(years > 2013)) proj_col else all_col
}

.redo_masd <- function(x, n = 30){
  if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]])
  dplyr::group_by(x, .data[["Set"]], .data[["Tx"]], .data[["FMO"]], add = TRUE) %>%
    dplyr::mutate(BA_sd_ma = RcppRoll::roll_sd(.data[["BA"]], n, fill = NA)) %>%
    dplyr::ungroup()
}

.fmo_combine <- function(x, n = 30){
  if("Set" %in% names(x)) x <- dplyr::group_by(x, .data[["Set"]], add = TRUE)
  if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]], add = TRUE)
  dplyr::group_by(x, .data[["Tx"]], .data[["Year"]], add = TRUE) %>%
    dplyr::summarise(BA = sum(.data[["BA"]])) %>%
    dplyr::mutate(CBA = cumsum(.data[["BA"]]), BA_sd_ma = RcppRoll::roll_sd(.data[["BA"]], n, fill = NA)) %>%
    dplyr::ungroup()
}

.rcp_combine <- function(x, type, n = 30){
  if(type %in% c("ba_sd", "ba_box", "cba")) x <- dplyr::ungroup(x) %>%
      dplyr::group_by(.data[["Set"]], .data[["Tx"]], .data[["FMO"]], .data[["Year"]]) %>%
      dplyr::summarise(BA = mean(.data[["BA"]])) %>%
      dplyr::mutate(CBA = cumsum(.data[["BA"]]), BA_sd_ma = RcppRoll::roll_sd(.data[["BA"]], n, fill = NA)) %>%
      dplyr::ungroup()
  if(type == "cost") x <- dplyr::ungroup(x) %>%
      dplyr::group_by(.data[["Tx"]], .data[["Year"]], .data[["cost"]]) %>%
      dplyr::summarise(value = mean(.data[["value"]])) %>% dplyr:: ungroup()
  if(type == "cost_dec"){
    x <- dplyr::ungroup(x)
    if("Set" %in% names(x)) x <- dplyr::group_by(x, .data[["Set"]], add = TRUE)
    if("FMO" %in% names(x)) x <- dplyr::group_by(x, .data[["FMO"]], add = TRUE)
    x <- dplyr::group_by(x, .data[["Tx"]], .data[["Decade"]], add = TRUE) %>%
      dplyr::summarise(`5th percentile` = mean(.data[["5th percentile"]]),
                       Mean = mean(.data[["Mean"]]),
                       `95th percentile` = mean(.data[["95th percentile"]])) %>% dplyr:: ungroup()
  }
  if(type == "cdratio" | type == "cdba"){
    x <- dplyr::ungroup(x) %>%
      dplyr::group_by(.data[["Tx"]], .data[["Year"]])
    if(type == "cdba") x <- dplyr::group_by(x, .data[["Vegetation"]], add = TRUE)
    x <- dplyr::summarise(x, value = mean(.data[["value"]])) %>% dplyr::ungroup()
  }
  if(type == "pfire") x <- dplyr::ungroup(x) %>%
      dplyr::group_by(.data[["Tx"]], .data[["Years"]], .data[["buffer"]]) %>%
      dplyr::summarise(value = mean(.data[["value"]])) %>% dplyr:: ungroup()
  if(type == "fs_box") x <- dplyr::ungroup(x) %>%
      dplyr::group_by(.data[["Tx"]], .data[["Decade"]], .data[["FS"]]) %>%
      dplyr::summarise(Freq = mean(.data[["Freq"]])) %>% dplyr:: ungroup()
  x
}

.rcp_triplicate <- function(x, type, n){
  if(type != "ba_sd") return(x)
  x <- dplyr::ungroup(x) %>% dplyr::filter(.data[["Set"]] != "Observed")
  rcp <- c("RCP 4.5", "RCP 6.0", "RCP 8.5")
  x0 <- purrr::map(factor(rcp, levels = c("Historical", rcp)),
                   ~dplyr::filter(x, .data[["RCP"]] == "Historical") %>% dplyr::mutate(RCP = .x)) %>%
    dplyr::bind_rows()
  dplyr::bind_rows(x0, x) %>% dplyr::filter(.data[["RCP"]] != "Historical") %>%
    dplyr::group_by(.data[["Set"]], .data[["Tx"]], .data[["RCP"]], .data[["FMO"]]) %>%
    dplyr::arrange(.data[["Set"]], .data[["Tx"]], .data[["RCP"]], .data[["FMO"]], .data[["Year"]]) %>%
    dplyr::mutate(BA_sd_ma = RcppRoll::roll_sd(.data[["BA"]], n, fill = NA)) %>% dplyr::ungroup()
}

.prep_fs <- function(x, n_samples = 100, interp = FALSE, n_interp = 1e5, fit_density = FALSE, density_args = list()){
  if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]])
  x <- dplyr::group_by(x, .data[["Tx"]], .data[["Decade"]], add = TRUE)
  if(interp) x <- dplyr::do(x, tibble::data_frame(
    FS = stats::approx(.[["FS"]], .[["Freq"]],
                      xout = seq(min(.[["FS"]]), max(.[["FS"]]), length.out = n_interp))$x,
    Freq = stats::approx(.[["FS"]], .[["Freq"]],
                     xout = seq(min(.[["FS"]]), max(.[["FS"]]), length.out = n_interp))$y))
  x <- dplyr::ungroup(x)
  if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]])
  x <- dplyr::group_by(x, .data[["Tx"]], .data[["Decade"]], add = TRUE) %>%
    dplyr::do(tibble::data_frame(FS = sample(.[["FS"]], n_samples, replace = TRUE, prob = .[["Freq"]]))) %>%
    dplyr::ungroup()
  if(fit_density){
    if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]])
    x <- dplyr::group_by(x, .data[["Tx"]], .data[["Decade"]])
    x <- dplyr::do(x, tibble::data_frame(
      FS = do.call(stats::density, c(list(x = .[["FS"]]), density_args))$x,
      Prob = do.call(stats::density, c(list(x = .[["FS"]]), density_args))$y))
    x <- dplyr::ungroup(x)
    if("RCP" %in% names(x)) x <- dplyr::group_by(x, .data[["RCP"]])
    x <- dplyr::group_by(x, .data[["Tx"]], .data[["Decade"]]) %>%
      dplyr::do(tibble::data_frame(FS = sample(.[["FS"]], n_samples, replace = TRUE, prob = .[["Prob"]]))) %>%
      dplyr::ungroup()
  }
  x
}

# nolint start

#' Create plots based on various JFSP data sets
#'
#' Create plots for each of the data sets included in the package using one convenient wrapper function.
#'
#' Data sets included in the package can be explicitly plotted however desired,
#' but for convenience the package offers this wrapper function that creates stock plots associated with package data sets.
#' A plot \code{type} calls the applicable data set for that plot internally.
#' Stock plots use standardized formatting similar to the main \code{snapplot} package plot theme.
#'
#' Available stock plots include:
#'
#' \describe{
#'   \item{\code{ba_sd}}{n-year moving average FMO zone burn area annual time series. Optional arguments: \code{continuous}, \code{alaska = TRUE}, \code{breaks}, \code{fmo}, \code{n}, \code{obs = TRUE}.}
#'   \item{\code{ba_box}}{FMO zone burn area aggregate period box plots. Optional arguments: \code{alaska = TRUE}, \code{log = TRUE}, \code{fmo}.}
#'   \item{\code{cba}}{FMO zone cumulative burn area annual time series. Optional arguments: \code{alaska = TRUE}, \code{breaks}, \code{fmo}.}
#'   \item{\code{cost}}{Alaska fire management annual costs time series. Optional arguments: \code{breaks}, \code{obs = TRUE}.}
#'   \item{\code{cost_dec}}{Alaska fire management decadal projected costs time series. Optional arguments: \code{obs = TRUE}}
#'   \item{\code{cdratio}}{Alaska coniferous:deciduous ratios annual time series. Optional arguments: \code{breaks}.}
#'   \item{\code{cdba}}{Alaska coniferous and deciduous annual burn area time series. Optional arguments: \code{breaks}.}
#'   \item{\code{pfire}}{Probability of fire near Fairbanks as a function of radial buffer distance. The \code{years} argument is ignored for this plot.}
#'   \item{\code{fs_box}}{Alaska fire size distribution decadal box plots. Optional arguments: \code{log}.}
#' }
#'
#' Additional arguments can be provided. General arguments include \code{family} (font family).
#' Arguments related to specific plot types are ignored when not applicable.
#' \code{alaska = TRUE} performs statewide aggregation over all FMO zones. \code{log = TRUE} applies a log scale transformation.
#' \code{continuous = TRUE} avoids a break in the time series where historical meets RCPs by triplicating the historical data and merging with each RCP series, only for \code{ba_sd}.
#' \code{n} is an integer for the number of years in the moving average window for \code{ba_sd}. Defaults to 30.
#' \code{breaks} is a vector of breaks applicable to time series plots with years along the x-axis.
#' \code{fmo} allows for subsetting the FMO zones available in the \code{fmoba} data set; not applicable when \code{alaska = TRUE}.
#' \code{obs = TRUE} will overlay a representation of historical observed data on a plot, such as a horizontal line showing the historical average. This applies to \code{ba_sd}, \code{cost} and \code{cost_dec}.
#' If not provided, it defaults to \code{fmo = c("Full", "Critical")} since these are the most important zones for work encapsulated by the package.
#'
#' If the \code{showtext} is loaded, it may be necessary to significantly increase \code{base_size} and/or \code{text_size} for the 300 dpi image saved when \code{file} is not \code{NULL}.
#' Google fonts and even base R fonts may shrink significantly relative to the plot under these circumstances with showtext in effect (e.g., after calling \code{showtext_auto}).
#' If this happens, try 20 and 60 instead of 14 and 18, respectively.
#' This will be done for you automatically if you elect to name your font with the special name, \code{"gfont"}, and pass it as \code{family = "gfont"}.
#'
#' When saving a plot directly from \code{jfsp_plot} by passing a filename to \code{file},
#' \code{jfsp_plot} will internally adjust plot formatting settings in order to save a 300 dpi resolution image to disk while maintaining consistent and appropriate sizing of text and other plot elements.
#'
#' @param type character, the type of plot to make, based on a particular package data set. See details.
#' @param years numeric, vector of consecutive years. The maximum range is \code{1950:2099}. See details.
#' @param by_rcp logical, condition on RCP, defaults to \code{TRUE}. Otherwise marginalize over RCP. This applied to RCP-driven years, 2014 - 2099.
#' @param by_tx logical, if \code{FALSE}, then treatments are dropped and only status quo (control) data are plotted.
#' @param col optional vector of colors to override the defaults built into \code{jfsp_plot}.
#' @param file character, if provided, the plot is saved to disk. Otherwise, it is plotted in the R session graphics device. See details.
#' @param base_size base size passed to theme. See details.
#' @param text_size text size passed to theme.
#' @param pt_size override default size for points in applicable plot types.
#' @param ... additional arguments. See details.
#'
#' @return a ggplot object. If saving a png file to disk, nothing is returned.
#' @export
#'
#' @examples
#' jfsp_plot("ba_box", 1950:2013, log = TRUE)
jfsp_plot <- function(type = NULL, years = NULL, by_rcp = TRUE, by_tx = TRUE, col = NULL,
                      file = NULL, base_size = 14, text_size = 18, pt_size = 2, ...){
  o <- list(...)
  if(!is.null(o$x)) x <- o$x else x <- switch(type,
    "ba_sd" = jfsp::fmoba, "ba_box" = jfsp::fmoba, "cba" = jfsp::fmoba,
    "cost" = dplyr::filter(jfsp::cost, .data[["cost"]] != "5th percentile"),
    "cost_dec" = jfsp::costSummary, "cdratio" = jfsp::cdratio, "cdba" = jfsp::cdba,
    "pfire" = jfsp::fbxfire, "fs_box" = jfsp::firesize)
  if(!by_tx) x <- dplyr::filter(x, .data[["Tx"]] == "Status quo")
  size <- pt_size
  obs <- if(!is.null(o$obs) && o$obs) TRUE else FALSE
  alaska <- if(!is.null(o$alaska) && o$alaska) TRUE else FALSE
  continuous <- ifelse(!is.null(o$continuous) && o$continuous, TRUE, FALSE)
  n <- ifelse(!is.null(o$n), o$n, 30)
  if(continuous) x <- .rcp_triplicate(x, type, n)
  if(!by_rcp) x <- .rcp_combine(x, type, n)
  family <- if(is.null(o$family)) "sans" else o$family
  tsize <- text_size
  bsize <- base_size
  if(!is.null(file) && "package:showtext" %in% search() && family == "gfont"){
     tsize <- 60
     bsize <- 20
  }
  if(is.null(years)) years <- 1950:2099
  if(any(years < 1950 | years > 2099)) stop("Years must be in 1950:2099 for JFSP data.")
  if(type == "cost_dec" & any(years < 2020))
    stop("Projected decadal cost estimates summary data set, `costSummary`, covers 2020 - 2099.")
  if(is.null(col)){
    col <- .jfsp_palette(years, by_rcp, by_tx)
    if(type == "ba_box") col <- c("black", "orange")
    if(type == "ba_sd" & continuous & by_rcp) col <- c("cornflowerblue", "orange", "firebrick1")
  }
  scm <- ggplot2::scale_color_manual(values = col)
  sfm <- ggplot2::scale_fill_manual(values = col)
  slm <- ggplot2::scale_linetype_manual(values = 1:2)
  thm <- .thm(base_size = bsize, base_family = family)
  if(!by_rcp & !by_tx){
    gde <- ggplot2::guides(colour = ggplot2::guide_legend(order = 1))
  } else {
    gde <- ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2, override.aes = list(linetype = c("solid", "22"))))
  }
  if("Year" %in% names(x)) x <- dplyr::filter(x, .data[["Year"]] %in% years)
  if("Decade" %in% names(x)) x <- dplyr::filter(x, .data[["Decade"]] %in% years)
  is_hist <- all(years <= 2013)
  is_proj <- all(years > 2013)
  breaks <- if(is.null(o$breaks)) ggplot2::waiver() else o$breaks
  if(type %in% c("ba_sd", "cba", "ba_box")){
    x <- .redo_masd(x, n)
    if(alaska){
      x <- .fmo_combine(x, n)
    } else {
      fmo <- if(is.null(o$fmo)) c("Full", "Critical") else o$fmo
      x <- dplyr::filter(x, .data[["FMO"]] %in% fmo)
    }
  }
  if(is_hist){
    clr_var <- "Set"
    grp_var <- "interaction(Set, Tx)"
  } else {
    clr_var <- ifelse(by_rcp, "RCP", ifelse(by_tx, "Tx", "none"))
    if(clr_var == "none") clr_var <- NULL
    lty_var <- if(by_rcp & by_tx) "Tx" else NULL
    if("Set" %in% names(x)) x <- dplyr::filter(x, .data[["Set"]] != "Observed")
  }

  if(type == "ba_sd"){
    print(x)
    y_var <- "BA_sd_ma"
    y_lab <- paste0(n, "-year MA burn area SD")
    subtitle <- paste0(n, "-year moving average")
    subtitle <- ifelse(is_hist, subtitle,
                       ifelse(by_rcp & by_tx, paste(subtitle, "by treatment and RCP"),
                              ifelse(by_rcp, paste(subtitle, "by RCP"),
                                     ifelse(by_tx, paste(subtitle, "by treatment"),
                                            subtitle))))
    if(is_hist) p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", y_var, color = clr_var,
                                                   fill = clr_var, group = grp_var))
    if(!is_hist) p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", y_var, color = clr_var,
                                                    fill = clr_var, linetype = lty_var))
    p <- p + ggplot2::geom_line(size = 1)
    if(!alaska) p <- p + ggplot2::facet_wrap(stats::as.formula("~FMO"), scales = "free_y")
    lmt <- if(is.numeric(breaks)) range(breaks) else range(x$Year[!is.na(x$BA_sd_ma)])
    p <- p + scm + sfm + thm + .thm_adj("topright", text_size = tsize) + gde +
      ggplot2::scale_x_continuous(limits = lmt, expand = c(0, 0), breaks = breaks) +
      ggplot2::labs(title = paste(min(years), "-", max(years), "inter-annual variability in burn area"),
           subtitle = subtitle, y = y_lab)
    if(obs){
      xo <- dplyr::filter(jfsp::fmoba, .data[["Set"]] == "Observed") %>% .redo_masd(n)
      if(alaska) xo <- .fmo_combine(xo, n)
      v <- mean(xo$BA_sd_ma, na.rm = TRUE)
      p <- p + ggplot2::geom_hline(yintercept = v, linetype = "dashed", color = "gray", size = 1)
    }
  } else if(type == "cba"){
    cba_lab <- "Cumulative burn area (acres)"
    subtitle <- ifelse(is_hist, "Historical observed and modeled",
                       ifelse(is_proj, "Projected 5-model average",
                              "Historical modeled and projected 5-model average"))
    if(is_hist) p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", "CBA", colour = clr_var, group = grp_var))
    if(!is_hist) p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", "CBA", colour = clr_var, linetype = lty_var))
    p <- p + ggplot2::geom_step()
    if(!alaska) p <- p + ggplot2::facet_wrap(stats::as.formula("~FMO"), scales = "free_y")
    p <- p + scm + slm + thm + .thm_adj("topleft", text_size = tsize) + gde +
      ggplot2::scale_x_continuous(limits = range(years), breaks = breaks) +
      ggplot2::labs(title = paste(min(years), "-", max(years), "cumulative annual burn area"),
                    subtitle = subtitle, y = cba_lab)
  } else if(type == "ba_box"){
    subtitle <- ifelse(is_hist, "Historical observed and modeled",
                       ifelse(is_proj, "Projected 5-model average",
                              "Historical modeled and projected 5-model average"))
    if(!is.null(o$log) && o$log == TRUE){
      y_var <- "log(BA + 1)"
      ba_lab <- "Burn area (Log acres)"
      subtitle <- paste0(subtitle, ". Log scale.")
    } else {
      y_var <- "BA"
      ba_lab <- "Burn area (acres)"
    }
    if(length(unique(x[["Set"]])) > 1 & is_hist){
      clr_var <- "Set"
    } else {
      clr_var <- ifelse(is.null(clr_var), "none", ifelse(clr_var == "RCP", ifelse(by_tx, "Tx", "none"), clr_var))
      if(clr_var == "none") clr_var <- NULL
    }
    if(is.null(clr_var)){
      pos <- ggplot2::position_jitter(0.2)
    } else {
      pos <- ggplot2::position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
    }
    x_var <- ifelse(by_rcp, "RCP", "Tx")
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x_var, y_var, colour = clr_var)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes_string(fill = clr_var), size = size, pch = 21, alpha = 0.5, position = pos)
    if(!alaska) p <- p + ggplot2::facet_wrap(stats::as.formula("~FMO"), scales = "free_y")
    p <- p + sfm + scm + thm + .thm_adj("topright", text_size = tsize) +
      ggplot2::labs(title = paste(min(years), "-", max(years), "annual burn area"),
                    subtitle = subtitle, y = ba_lab)
  } else if(type == "cost"){
    cost_lab <- "Cost (Millions of $)"
    subtitle <- ifelse(is_hist, "Mean and 95th percentile",
                       ifelse(by_rcp & by_tx, "Mean and 95th percentile by treatment and RCP",
                              ifelse(by_rcp, "Mean and 95th percentile by RCP",
                                     ifelse(by_tx, "Mean and 95th percentile by treatment",
                                            "Mean and 95th percentile"))))
    if(!is_hist & ((by_rcp & !by_tx) | (!by_rcp & by_tx))){
      lty_var <- "cost"
    } else if(!is_hist & !by_rcp & !by_tx){
      lty_var <- NULL
      clr_var <- "cost"
      if(length(col) < 2){
        col <- c("black", "orange")
        scm <- ggplot2::scale_color_manual(values = col)
        sfm <- ggplot2::scale_fill_manual(values = col)
      }
    }
    if(is_hist) p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", "value", linetype = "cost")) +
      ggplot2::geom_line() + ggplot2::geom_point(size = size)
    if(!is_hist)
      p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", "value", colour = clr_var, linetype = lty_var)) +
      ggplot2::geom_line() + ggplot2::geom_point(size = size, shape = 21)
    if(!is_hist & by_rcp & by_tx) p <- p + ggplot2::facet_wrap(stats::as.formula("~cost"))
    p <- p + thm + sfm + scm + slm + .thm_adj("topleft", text_size = tsize) + gde +
      ggplot2::scale_x_continuous(limits = range(years), breaks = breaks) +
      ggplot2::labs(title = paste(min(years), "-", max(years), "fire management cost"),
                    subtitle = subtitle, y = cost_lab)
    if(obs) p <- p + ggplot2::geom_hline(yintercept = 45, linetype = "dashed", color = "gray", size = 1)
  } else if(type == "cost_dec"){
    cost_lab <- "Cost (Millions of $)"
    title <- ifelse(is_hist, "Historical annual fire management costs",
                    ifelse(is_proj, "Annual fire management costs",
                           "Historical and projected annual fire management costs"))
    subtitle <- ifelse(by_rcp & by_tx, "Mean and 90% confidence interval by decade, treatment and RCP",
                       ifelse(by_rcp, "Mean and 90% confidence interval by decade and RCP",
                              ifelse(by_tx, "Mean and 90% confidence interval by decade and treatment",
                                     "Mean and 90% confidence interval by decade")))
    p <- ggplot2::ggplot(x, ggplot2::aes_string("factor(Decade)", "Mean", colour = clr_var,
                                       fill = clr_var, linetype = lty_var)) +
      ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "`5th percentile`", ymax = "`95th percentile`"),
                    size = 1, position = ggplot2::position_dodge(width = 0.5), width = 0.4) +
      ggplot2::geom_point(shape = 21, size = size, colour = "black",
                          position = ggplot2::position_dodge(width = 0.5)) +
      sfm + scm + slm + thm + .thm_adj("topright", text_size = tsize) + gde +
      ggplot2::scale_x_discrete(labels = paste0(unique(x[["Decade"]]), "s")) +
      ggplot2::labs(title = title, subtitle = subtitle, x = "Decade", y = cost_lab)
    if("FMO" %in% names(x))
      p <- p + ggplot2::facet_wrap(stats::as.formula("~FMO"), scales = "free_y", ncol = 2)
    if(obs) p <- p + ggplot2::geom_hline(yintercept = 45, linetype = "dashed", color = "gray", size = 1)
  } else if(type == "cdratio" | type == "cdba"){
    if(is_hist){
      lty_var <- NULL
      clr_var <- NULL
      gde <- ggplot2::guides(colour = ggplot2::guide_legend(order = 1))
    }
    subtitle <- ifelse(is_hist, "Alaska historical modeled outputs",
                       ifelse(is_proj, "Alaska 5-model average projected outputs",
                              "Alaska historical modeled and 5-model average projected outputs"))
    a <- ifelse(type == "cdratio", "coniferous:deciduous ratio", "coniferous and deciduous burn area")
    b <- ifelse(type == "cdratio", "Ratio", "Burn area (acres)")
    p <- ggplot2::ggplot(x, ggplot2::aes_string("Year", "value", colour = clr_var, linetype = lty_var)) +
      ggplot2::geom_line(size = 1)
    if(!is_hist) p <- p + slm
    p <- p + scm + thm + .thm_adj("topright", "vertical", tsize) + gde +
      ggplot2::scale_x_continuous(limits = range(years), expand = c(0, 0), breaks = breaks) +
      ggplot2::labs(title = paste(min(years), "-", max(years), a),
                    subtitle = subtitle, y = b)
    if(type == "cdba")
      p <- p + ggplot2::facet_wrap(stats::as.formula("~Vegetation"), scales = "free_y")
  } else if(type == "pfire"){
    p <- ggplot2::ggplot(x, ggplot2::aes_string("buffer", "value", colour = clr_var, linetype = lty_var)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::labs(title = "Simulated fire over time around Fairbanks",
                    subtitle = "Over multiple time periods", x = "Distance from center (km)", y = "P(Fire)") +
      scm + slm + thm + .thm_adj("topleft", text_size = tsize) + gde +
      ggplot2::scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
      ggplot2::facet_wrap(stats::as.formula("~Years"))
  } else if(type == "fs_box"){
    x <- .prep_fs(x)
    pos <- ggplot2::position_jitter(0.2)
    size <- if(is.null(pt_size)) 1 else pt_size
    if(is_hist){
      lty_var <- NULL
      clr_var <- NULL
      gde <- ggplot2::guides(colour = ggplot2::guide_legend(order = 1))
    }
    if(!is.null(clr_var)) pos <- ggplot2::position_jitterdodge(0.2)
    subtitle <- ifelse(is_hist, "none",
                       ifelse(by_rcp & by_tx, "By decade, treatment and RCP",
                              ifelse(by_rcp, "By decade and RCP",
                                     ifelse(by_tx, "By decade and treatment", "none"))))
    if(subtitle == "none") subtitle <- NULL
    if(!is.null(o$log) && o$log == TRUE){
      y_var <- "log(FS)"
      y_lab <- "Fire size (Log acres)"
      subtitle <- ifelse(is.null(subtitle), "Log scale", paste0(subtitle, ". Log scale."))
    } else {
      y_var <- "FS"
      y_lab <- "Fire size (acres)"
    }
    title <- ifelse(is_hist, "Historical fire size distributions",
                    ifelse(is_proj, "Projected fire size distributions",
                           "Historical and projected fire size distributions"))
    p <- ggplot2::ggplot(x, ggplot2::aes_string("factor(Decade)", y_var, colour = clr_var)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes_string(fill = clr_var), size = size, pch = 21, alpha = 0.5, position = pos)
    p <- p + sfm + scm + thm + gde +
      ggplot2::scale_x_discrete(labels = paste0(unique(x[["Decade"]]), "s")) +
      ggplot2::labs(title = title, subtitle = subtitle, x = "Decade", y = y_lab)
    if(by_rcp & by_tx){
      p <- p + ggplot2::facet_wrap(stats::as.formula("~Tx"), scales = "fixed") +
        ggplot2::coord_flip() + .thm_adj("bottomright", "vertical", text_size = tsize)
    } else {
      p <- p + .thm_adj("topright", text_size = tsize)
    }
  }
  if(is.null(file)) return(p)
  grDevices::png(file, width = 3000, height = 2000, res = 300, type = "cairo")
  print(p)
  grDevices::dev.off()
  invisible()
}

# nolint end

.thm <- function(base_size = 14, base_family = "", base_col = "black",
                       base_fill = "white", grid_col = "#F0F0F0") {
  .thm_prep(base_size = base_size, base_family = base_family,
              base_col = base_col, base_fill = base_fill) +
    ggplot2::theme(panel.background = ggplot2::element_rect(colour = NA),
                   plot.background = ggplot2::element_rect(colour = NA),
                   panel.border = ggplot2::element_rect(colour = NA),
                   axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
                   axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
                   axis.title.x = ggplot2::element_text(vjust = -0.2),
                   axis.line = ggplot2::element_line(colour = base_col),
                   panel.grid.major = ggplot2::element_line(colour = grid_col),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = NA),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_text(face = "italic"),
                   strip.background = ggplot2::element_rect(colour = base_col, fill = grid_col),
                   strip.text = ggplot2::element_text(face = "bold")
    )
}

.thm_prep <- function(base_size = 14, base_family = "", base_col = "black", base_fill = "white"){
  x <- ggplot2::theme_gray(base_size = base_size, base_family = base_family)
  for (i in names(x)) {
    if ("colour" %in% names(x[[i]])) x[[i]]["colour"] <- list(NULL)
    if ("fill" %in% names(x[[i]])) x[[i]]["fill"] <- list(NULL)
  }
  x + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                     legend.background = ggplot2::element_rect(colour = NA),
                     line = ggplot2::element_line(colour = base_col),
                     rect = ggplot2::element_rect(fill = base_fill, colour = base_col),
                     text = ggplot2::element_text(colour = base_col))
}

.thm_adj <- function(position, direction = "horizontal", text_size = 14){
  pos <- switch(position, topright = c(1, 1), topleft = c(0, 1), bottomright = c(1, 0))
  just <- switch(position, topright = "right", topleft = "left", bottomright = "right")
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 1.5, 0.5, 0.5), "lines"),
                 text = ggplot2::element_text(size = text_size),
                 legend.position = pos, legend.box.just = just, legend.direction = direction,
                 legend.justification = pos, legend.background = ggplot2::element_rect(fill = "#CCCCCC80"))
}
