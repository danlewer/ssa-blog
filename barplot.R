library(data.table)
library(viridisLite)
library(RColorBrewer)
library(extrafont)
loadfonts(device = 'win')

# treatment
# https://www.gov.uk/government/statistics/substance-misuse-treatment-for-adults-statistics-2020-to-2021

trt <- read.csv(url('https://raw.githubusercontent.com/danlewer/ssa-blog/main/adult_treatment_2021.csv'))

# deaths
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable

drd <- read.csv(url('https://raw.githubusercontent.com/danlewer/ssa-blog/main/drds_reg_2020.csv'))

# irid

ird <- read.csv(url('https://raw.githubusercontent.com/danlewer/ssa-blog/main/irid_age_group_by_year_5june2022.csv'))

# uam

uam <- read.csv('https://raw.githubusercontent.com/danlewer/ssa-blog/main/uam_age_groups_6june2022.csv')
setnames(uam, 'age_group', 'age_group2')
setDT(trt); setDT(drd); setDT(ird); setDT(uam)

# ============================
# make same age groups & years
# ----------------------------

# treatment
trt <- melt(trt, id.vars = c('Year', 'Substance'), variable.name = 'age', value.name = 'p')
trt <- trt[Substance %in% c('Opiate (not crack cocaine)', 'Both opiate and crack cocaine', 'Crack cocaine (not opiate)')]
trt <- trt[, age_group := factor(age, 
                                 c('X18.19', 'X20.24', 'X25.29', 'X30.34', 'X35.39', 'X40.44', 'X45.49', 'X50.54', 'X55.59', 'X60.64', 'X65.'),
                                 c('U20', '20-29', '20-29', '30-39', '30-39', '40-49', '40-49', '50+', '50+', '50+', '50+'))]
trt[, year := as.integer(substr(Year, 0, 4))]
trt <- trt[, .(p = sum(p)), c('year', 'age_group')]
trt <- trt[order(year, age_group)]

# drds
drd <- melt(drd, id.vars = c('year', 'substance'), variable.name = 'age', value.name = 'p')
drd <- drd[substance == 'Heroin or morphine']
drd[, age_group := factor(age,
                          c('Under.20', 'X20.29', 'X30.39', 'X40.49', 'X50.69', 'X70.and.over'),
                          c('U20', '20-29', '30-39', '40-49', '50+', '50+'))]
drd <- drd[, .(p = sum(p)), c('year', 'age_group')]
drd <- drd[order(year, age_group)]

# irid
ird[, age_group := factor(age_group2, 
                          c('0', '18', '20', '25', '30', '35', '40', '45', '50', '55', '60', '65'),
                          c('U20', 'U20', '20-29', '20-29', '30-39', '30-39', '40-49', '40-49', '50+', '50+', '50+', '50+'))]
ird <- ird[, .(p = sum(N)), c('year', 'age_group')]
ird <- ird[order(year, age_group)]

# uam
uam[, age_group := factor(age_group2, 
                          c('0', '18', '20', '25', '30', '35', '40', '45', '50', '55', '60', '65', '70'),
                          c('U20', 'U20', '20-29', '20-29', '30-39', '30-39', '40-49', '40-49', '50+', '50+', '50+', '50+', '50+'))]
uam <- uam[, .(p = sum(N)), c('year', 'age_group')]
uam <- uam[order(year, age_group)]
uam <- uam[year > 1992]

# ==========================
# make stacked bar variables
# --------------------------

cols <- magma(5)

sbv <- function (x) {
  y <- split(x, f = x$year)
  y <- lapply(y, function (z) {
    cbind(z, yt = cumsum(z$p) / sum(z$p), yb = shift(cumsum(z$p), fill = 0) / sum(z$p), cl = cols)
  })
  do.call(rbind, y)
}

trt <- sbv(trt)
drd <- sbv(drd)
ird <- sbv(ird)
uam <- sbv(uam)

# =====
# plots
# -----

pt <- function (z, xax = T, yax = T, TITLE = NA) {
  plot(1, type = 'n', xlim = c(1993, 2022), ylim = c(0, 1), axes = F, xlab = NA, ylab = NA)
  rect(1993, 0, 2022, 1, col = 'grey95')
  with(z, {
    rect(year, yb, year + 1, yt, col = cl, lwd = 1)
  })
  if (xax) {
    axis(1, 1993:2022, labels = F, pos = 0)
    text(1995 + 0:5 * 5 + 0.5, - 0.07, 1995 + 0:5 * 5, srt = 90, adj = 1)}
  if (yax) axis(2, 0:5/5, paste0(0:5 * 20, '%'), pos = 1993, las = 2)
  text(2007.5, 1.05, TITLE, adj = c(0.5, 0))
}

png('age_barplots.png', height = 5, width = 6, units = 'in', res = 300, family = 'Franklin Gothic Book')

layout(mat = matrix(c(1:5, 5), ncol = 3), widths = c(2, 2, 1))
par(xpd = NA, mar = c(0, 0, 3, 0), oma = c(4, 4, 0, 0))
pt(trt, xax = F, TITLE = 'People in treatment for\nheroin or crack cocaine [1]')
pt(drd, TITLE = 'Deaths due to\nheroin or morphine [2]')
pt(ird, xax = F, yax = F, TITLE = 'Injecting-related\nbacterial infections [3]')
pt(uam, yax = F, TITLE = 'Participants in the Unlinked Anonymous Monitoring\nSurvey of People who Inject Drugs [4]')
plot(1, type = 'n', xlim = c(0, 10), ylim = c(0, 10), xlab = NA, ylab = NA, axes = F)
ys <- seq(3, 7, length.out = 6)
rect(2, ys[-length(ys)], 3, ys[-1], col = cols)
text(3.5, ys[-length(ys)] + diff(ys)/2, c('Under 20', '20-29', '30-39', '40-49', '50+'), adj = 0)
text(2, max(ys) + diff(ys)[1] / 2, 'Age', adj = 0)

dev.off()
