source("scripts/Wuhan_Tland/main_pkgs.R")
library(data.table)
library(ggplot2)
library(Ipaper)

load("I:/GitHub/rpkgs/hydroTools.R/scripts/Wuhan_Tair/Wuhan_met54_Inputs-[195101-202003].rda")
sites = st$site %>% as.character() %>% set_names(., .)

i = 1
sitename = sites[i]
# sitename = "57381"
lat = st[site==sitename, lat]
df = map(lst, function(d) {
    dplyr::select(d, -starts_with("QC")) %>% tidy_input() %>%
        .[!is.na(TG + Tair)]
}) %>% do.call(rbind, .)
d = d[site == sitename]

# d_calib = d[date <= "2010-12-31"]
# d_valid = d[date > "2010-12-31"]

vars_x = c("Rn", "Tair", "U2", "prcp", "RH")#[-1]
vars_y = "deltaT"
formula = sprintf("%s ~ %s", vars_y, paste(vars_x, collapse = " + "))
vars = c(vars_x, vars_y)

res = foreach(sitename = sites, i = icount()) %do% {
    runningId(i)
    d = df[site == sitename]
    dat = d[, ..vars] %>% na.omit()
    ## The module of select variables
    l = lm(formula, dat) %>% MASS::stepAIC(trace = 0)
    list(gof = glance(l), coef = tidy(l))
}

d_coef = map(res, "coef") %>% melt_list("site")
r = dcast(d_coef, site ~ term, value.var = "estimate") %>% melt("site")
ggplot(r, aes(variable, value)) +
    geom_boxplot2() +
    facet_wrap(~variable, scales = "free", nrow = 1) +
    theme(axis.text.x = element_blank())

X = dat[, ..vars_x]
Y = dat[, ..vars_y]

r_lm = kford_lm(X, Y)
r_rf = kford_rf(X, Y)
