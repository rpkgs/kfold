source("scripts/Wuhan_Tland/main_pkgs.R")
library(data.table)
library(ggplot2)
library(Ipaper)
library(broom)

load("//kong-nas/GitHub/rpkgs/hydroTools.R/scripts/Wuhan_Tair/Wuhan_met54_Inputs-[195101-202003].rda")
sites = st$site %>% as.character() %>% set_names(., .)

i = 1
sitename = sites[i]
# sitename = "57381"
lat = st[site==sitename, lat]
df = map(lst, function(d) {
    dplyr::select(d, -starts_with("QC")) %>% tidy_input() %>%
        .[!is.na(TG + Tair)]
}) %>% do.call(rbind, .)
# d = lst[1]
d = df[site == sitename]

# d_calib = d[date <= "2010-12-31"]
# d_valid = d[date > "2010-12-31"]

vars_x = c("Rn", "Tair", "U2", "prcp", "RH")#[2]
vars_y = "deltaT"
# vars_y = "TG"
formula = sprintf("%s ~ %s", vars_y, paste(vars_x, collapse = " + "))
vars = c("date", 'site', vars_x, vars_y, "TG") %>% unique()


res = foreach(sitename = sites, i = icount()) %do% {
    runningId(i)
    d = df[site == sitename]
    dat = d[, ..vars] %>% na.omit()
    ## The module of select variables
    l = lm(formula, dat) %>% MASS::stepAIC(trace = 0)
    dat$ysim = predict(l, dat, na.rm = FALSE)
    list(gof = glance(l), coef = tidy(l), data = dat)
}
gof = map(res, "gof") %>% melt_list("site")
data = map(res, "data") %>% melt_list("site")

summary(gof$adj.r.squared)

library(gg.layers)
# summary(gof$adj.r.squared)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.3799  0.5067  0.5482  0.5425  0.5800  0.6693

d_coef = map(res, "coef") %>% melt_list("site")
r = dcast(d_coef, site ~ term, value.var = "estimate") %>% melt("site")
p = ggplot(r, aes(variable, value)) +
    geom_boxplot2() +
    facet_wrap(~variable, scales = "free", nrow = 1) +
    theme(axis.text.x = element_blank())
write_fig(p, "Figure1.pdf")

X = dat[, ..vars_x]
Y = dat[, ..vars_y]

r_lm = kfold_lm(X, Y)
r_rf = kfold_rf(X, Y)

## 方案1
d2 = d %>%
    select(date, deltaT, TG, Tair, Rn, RH, U2, Pa) %>%
    mutate(Ts = cal_Ts(Rn, Tair, D = cal_es(Tair) * (100 - RH)/100, U2, Pa, method="full"))
# d[, .(cal_Ts(Rn, Tair, D = cal_es(Tair) * (100 - RH)/100, U2, Pa))]
d2[, GOF(TG, Ts)]
d2[, GOF(deltaT, Ts - Tair)]

## 方案2
l = lm(TG ~ Rn + Tair + RH + U2, d)
summary(l)
glance(l)
d[, .(deltaT, TG, Tair, deltaT_sim = TG_ysim - Tair)][, GOF(deltaT, deltaT_sim)]
# d$TG_ysim = predict(l, d, na.rm = FALSE)



cal_anorm <- function(x, year, ref = c(1961, 1990)) {
    ind_ref = which(year >= ref[1] & year <= ref[2])
    x = x - mean(x[ind_ref], na.rm = TRUE)
    data.table(year, anorm = x)
}

d = fread(f)
d2 = d[, cal_anorm(Ta, year), site][order(site, year)]
ggplot(d2[year >= 1961], aes(year, anorm, color = as.factor(site))) +
    geom_line() +
    geom_smooth()
