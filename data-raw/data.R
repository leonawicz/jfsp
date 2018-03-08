library(dplyr)

f <- function(x) switch(x, historical = "Historical",
                        rcp45 = "RCP 4.5", rcp60 = "RCP 6.0", rcp85 = "RCP 8.5")
lev <- c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")

basecost <- tibble::data_frame(
  limited = c(13.9, 0.3, 7.6, 92.0, 10.6, 8.1, 12.0),
  modified = c(16, 22, 164, 356, 31, 56, 534),
  critical = c(2459, 473, 12868, 11160, 198, 6935, 8439),
  full = c(351, 257, 456, 76, 89, 312, 120)
)

# Alaska fire size distributions
firesize <- readRDS("C:/github/jfsp-archive/data-raw/fs.rds") %>% select(-Set) %>%
  mutate(Decade = Year - Year %% 10) %>% group_by(Tx, RCP, Decade, Val, Model) %>%
  summarise(Freq = sum(Freq)) %>% summarise(Freq = mean(Freq)) %>% ungroup %>%
  mutate(Tx = ifelse(Tx == "tx0", "Status quo", ifelse(Tx == "tx1", "Treatment 1", "Treatment 2")),
         Val = as.integer(round(247.105 * Val))) %>% rename(FS = Val)

# Alaska confierous:deciduous ratios
cdratio <- readRDS("C:/github/jfsp-archive/data-raw/conif_decid_area.rds") %>% select(-FMO) %>%
  group_by(Tx, RCP, Year, Vegetation) %>% summarise(Val = mean(Val)) %>%
  summarise(Val = Val[1] / Val[2]) %>% ungroup %>% rename(value = Val) %>%
  mutate(Tx = ifelse(Tx == "tx0", "Status quo", ifelse(Tx == "tx1", "Treatment 1", "Treatment 2")))

# Alaska confierous:deciduous burn area
cdba <- readRDS("C:/github/jfsp-archive/data-raw/conif_decid_ba.rds") %>% select(-FMO) %>%
  group_by(Tx, RCP, Year, Vegetation) %>% summarise(Val = mean(Val)) %>%
  ungroup %>% rename(value = Val) %>%
  mutate(value = as.integer(round(247.105 * value)),
         Tx = ifelse(Tx == "tx0", "Status quo", ifelse(Tx == "tx1", "Treatment 1", "Treatment 2")))

# Robust annual P(Fire) near Fairbanks, Alaska
fbxfire <- readRDS("C:/github/jfsp-archive/data-raw/fire_prob_fbks_simplified.rds") %>%
  mutate(RCP = factor(sapply(as.character(RCP), f), levels = lev))

gcms <- c("CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
fmoba <- readRDS("C:/github/jfsp-archive/data-raw/ba_fmo/ba_fmo2_sw_alaska.rds")
sets <- strsplit(fmoba$Set, "\\.")
fmoba <- mutate(fmoba, Set = sapply(sets, "[", 1), RCP = sapply(sets, "[", 2),
                Model = gsub("CRU32", "CRU 3.2", sapply(sets, "[", 3))) %>%
  filter(Tx != "none") %>% mutate(RCP = factor(sapply(as.character(RCP), f), levels = lev)) %>%
  mutate(Tx = ifelse(is.na(Tx) & Set == "fmo99s95i", "Status quo",
                     ifelse(is.na(Tx), "No management",
                            ifelse(Tx == "tx0", "Status quo",
                                   ifelse(Tx == "tx1", "Treatment 1",
                                          ifelse(Tx == "tx2", "Treatment 2",
                                                 ifelse(Tx == "none", "No management", Tx)))))))
fmoba2 <- mutate(readRDS("C:/github/jfsp-archive/data-raw/ba_fmo/ba_fmo_sw_alaska.rds"),
             Set = substr(Set, 1, 9), RCP = factor("Historical", levels = lev),
             Model = "Observed", Tx = "Status quo")
fmoba <- bind_rows(fmoba, fmoba2) %>%
  mutate(Set = factor(Set, levels = c("Observed", "fmo99s95i")),
         Model = factor(Model, levels = c("Observed", "CRU 3.2", gcms)),
         Tx = factor(Tx, levels = c("No management", "Status quo", "Treatment 1", "Treatment 2"))) %>%
  select(c(1, 2, 10:11, 3:9)) %>% mutate_at(6:11, ~round(247.105 * .x)) %>%
  arrange(Set, Tx, RCP, Model, Year)

mgmtcost <- filter(fmoba, Set == "fmo99s95i")
fmoba <- tidyr::gather(fmoba, "FMO", "BA", -c(1:5)) %>%
  mutate(FMO = factor(FMO, levels = c("Unmanaged", "Limited", "Modified", "Full", "Critical", "Other")))
fmoba <- group_by(fmoba, Set, RCP, Model, Tx, FMO) %>%
  arrange(Set, Tx, RCP, Model, FMO, Year) %>% mutate(CBA = cumsum(BA)) %>% ungroup

# Summary statistics for FMO zone-specific burn area over time
fmobaSummary <- group_by(fmoba, Set, Tx, RCP, FMO) %>%
  summarise(min = min(BA), q1 = quantile(BA, 0.25), median = median(BA),
            mean = mean(BA), q3 = quantile(BA, 0.75), max = max(BA)) %>% ungroup

# FMO zone-specific annual burn area, plus cumulative and 10-year MA SD
fmoba <- group_by(fmoba, Set, Tx, RCP, FMO, Year) %>%
  summarise(BA = mean(BA), CBA = mean(CBA)) %>%
  mutate(BA_sd_ma10 = RcppRoll::roll_sd(BA, 10, fill = NA)) %>% ungroup

set.seed(1)
x <- replicate(1000,
  rowSums(mgmtcost[, 6:11]*cbind(0, sapply(basecost, sample, nrow(mgmtcost), replace = TRUE), 0))) / 1e6
cost <- mutate(mgmtcost, `5th percentile` = apply(x, 1, quantile, prob = 0.05),
               Mean = rowMeans(x), `95th percentile` = apply(x, 1, quantile, prob = 0.95)) %>%
  tidyr::gather("cost", "value", 12:14, factor_key = TRUE) %>%
  group_by(Tx, RCP, Year, cost) %>% summarise(value = mean(value))
costSummary <- mutate(mgmtcost, Decade = Year - Year %% 10) %>%
  bind_cols(as_data_frame(x)) %>% filter(Year >= 2020) %>% tidyr::gather("cost", "value", 13:1012) %>%
  group_by(Set, Tx, RCP, Decade) %>%
  summarise(`5th percentile` = quantile(value, prob = 0.05),
            Mean = mean(value), `95th percentile` = quantile(value, prob = 0.95)) %>% ungroup %>%
  select(-Set)

# set variable labels and attributes
set_atts <- function(data, lab, desc){
  x <- names(data)
  for(i in 1:ncol(data)){
    if(!is.na(lab[i])) attr(data[[x[i]]], "labels") <- lab[i]
    if(!is.na(desc[i])) attr(data[[x[i]]], "shortDescription") <- desc[i]
  }
  data
}

set <- c("Observed/ALFRESCO run", "Historical observations or ALFRESCO output based on various run parameterizations")
tx <- c("Control/Treatment run", "The control group is 'status quo' fire management and other values are different fire management treatments")
rcp <- c("Representative concentration pathways", "RCPs based on different greenhouse gas emissions scenarios")
fmo <- c("Fire management options", "Fire management options refer to regions of Alaska where a particular management strategy is applied")
yr <- c("Year", "")
dec <- c("Decade", "")
ba <- c("Burn area", "The annual area burned")
cba <- c("Cumulative burn area", "The cumulative annual area burned")
basd <- c("10-yr MA burn area SD", "The inter-annual variability in area burned given by the 10-year moving average of the standard deviation in burn area")

lab <- c(set[1], tx[1], rcp[1], fmo[1], yr[1], ba[1], cba[1], basd[1])
desc <- c(set[2], tx[2], rcp[2], fmo[2], yr[2], ba[2], cba[2], basd[2])
fmoba <- set_atts(fmoba, lab, desc)

lab <- c(set[1], tx[1], rcp[1], fmo[1], "Minimum", "First quartile", "Median", "Mean", "Third quartile", "Maximum")
desc <- c(set[2], tx[2], rcp[2], fmo[2], rep("Summary statistic: all years", 6))
fmobaSummary <- set_atts(fmobaSummary, lab, desc)

lab <- c(tx[1], rcp[1], yr[1], "Management cost statistic", "Management cost")
desc <- c(tx[2], rcp[2], yr[2], "5th percentile, mean, and 95th percentile statistics for the distribution of MC cost samples", "Fire management cost in millions of dollars")
cost <- set_atts(cost, lab, desc)

lab <- c(tx[1], rcp[1], dec[1], "Lower bound cost", "Mean cost", "Upper bound cost")
desc <- c(tx[2], rcp[2], dec[2], "5th percentile fire management cost in millions of dollars", "Mean fire management cost in millions of dollars", "95th percentile fire management cost in millions of dollars")
costSummary <- set_atts(costSummary, lab, desc)

lab <- c(tx[1], rcp[1], "Time period", "Spatial buffer", "P(Fire)")
desc <- c(tx[2], rcp[2], "Aggregate time period in years", "Spatial buffer radius in km", "The probability of fire near Fairbanks, Alaska in ALFRESCO output")
fbxfire <- set_atts(fbxfire, lab, desc)

lab <- c(tx[1], rcp[1], yr[1], "Coniferous:Deciduous ratio")
desc <- c(tx[2], rcp[2], yr[2], "The ratio of coniferous to deciduous species on the Alaska landscape through time in ALFRESCO output")
cdratio <- set_atts(cdratio, lab, desc)

lab <- c(tx[1], rcp[1], yr[1], "Coniferous:Deciduous burn area")
desc <- c(tx[2], rcp[2], yr[2], "TConiferous and deciduous species burn area on the Alaska landscape through time in ALFRESCO output")
cdba <- set_atts(cdba, lab, desc)

lab <- c(tx[1], rcp[1], dec[1], "Fire size", "Frequency")
desc <- c(tx[2], rcp[2], dec[2], "Fir size distributions per year in acres with a minimum of resolution of 247 acres after conversion to acres from native 1-km resolution ALFRESCO output", "Frequency of fires of a given size per year in ALFRESCO output")
firesize <- set_atts(firesize, lab, desc)

lab <- c("Limited management", "Modified management", "Critical management", "Full management")
desc <- c("Historical annual empirical fire management costs per unit area (dollars/acre) for the years 2001 - 2017")
basecost <- set_atts(select(basecost, c(1, 2, 4, 3)), lab, desc)

usethis::use_data(basecost, cdratio, fbxfire, fmoba, fmobaSummary, cost, costSummary, firesize)
