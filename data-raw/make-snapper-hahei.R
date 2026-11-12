# Builds data/snapper-hahei.csv from the supplements of Smith et al. (2012, 2014).
#
# The counts are the 348 baited underwater video deployments at Te Whanganui-A-Hei
# (Hahei) Marine Reserve in the R and OpenBUGS supplement to Smith et al. (2012),
# Ecological Archives E093-237-S1. The same deployments are the Hahei subset of the
# supplement to Smith et al. (2014), and the script checks that the two agree before
# writing anything.
#
# Run from the repository root. The supplements are read from literature/, which is
# not part of the published repository.

supp_2012 <- file.path(
  "literature",
  "Smith et al 2012 Supp Mat",
  "3554706.zip"
)
supp_2014 <- file.path(
  "literature",
  "Smith et al MEPS Supplement Bayesian models of marine reserve effects",
  "Snapper data.R"
)
stopifnot(
  file.exists(supp_2012),
  file.exists(supp_2014)
)

# The 2012 data are a structure() call inside the supplement script.
txt <- readLines(
  unz(supp_2012, "Linked_zero-inflated_model_for_snapper.R")
)
first <- grep("indat <- structure", txt, fixed = TRUE)
last  <- grep("reserve.in.area\")", txt, fixed = TRUE)[1]
raw   <- eval(parse(text = sub("^indat <- ", "", paste(txt[first:last], collapse = "\n"))))

# Year codes 1 to 9 are 1997 to 2001, 2003, 2004, 2006 and 2010 (Smith et al. 2012,
# Appendix A). Season -1 is spring and +1 is autumn. Areas 2, 3 and 4 are inside
# the reserve.
years <- c(1997, 1998, 1999, 2000, 2001, 2003, 2004, 2006, 2010)

snapper <- data.frame(
  count   = as.integer(raw$sna),
  reserve = factor(
    ifelse(raw$reserve.in.area[raw$area] == 2, "inside", "outside"),
    levels = c("outside", "inside")
  ),
  area    = factor(raw$area),
  year    = factor(years[raw$year]),
  season  = factor(
    ifelse(raw$season == 1, "autumn", "spring"),
    levels = c("spring", "autumn")
  )
)

stopifnot(
  nrow(snapper) == 348,
  sum(snapper$count == 0) == 191,
  nlevels(snapper$area) == 6,
  nlevels(snapper$year) == 9,
  setequal(unique(as.character(snapper$area[snapper$reserve == "inside"])), c("2", "3", "4")),
  all(tapply(as.integer(snapper$reserve), snapper$area, function(x) length(unique(x))) == 1),
  !anyNA(snapper)
)

# Cross-check against the Hahei subset of the 2014 supplement: same counts and
# seasons in the same order, and the same years once both are mapped to calendar
# years.
d14 <- source(supp_2014)$value
h14 <- d14$location == 3
years_2014 <- c(1997:2007, 2010)
stopifnot(
  sum(h14) == nrow(snapper),
  identical(as.integer(d14$legsna[h14]), snapper$count),
  identical(as.numeric(d14$season[h14]), ifelse(snapper$season == "autumn", 1, -1)),
  identical(years_2014[d14$year[h14]], as.numeric(as.character(snapper$year))),
  identical(d14$status[h14] == 1, snapper$reserve == "inside")
)

dir.create("data", showWarnings = FALSE)
write.csv(
  snapper,
  file.path("data", "snapper-hahei.csv"),
  row.names = FALSE
)
