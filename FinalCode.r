# Always start with a clean slate
rm(list=ls(all=TRUE))

# YOU WILL HAVE TO CHANGE THIS FOR YOUR COMPUTER
setwd("C:/Users/tui53272/Desktop/OneDrive - Temple University/PolStats/FinalPaper")

# Load needed libraries & Check to see if you have them installed
packages = c(
  "tidyverse",
  "plm",
  "broom",
  "magrittr",
  "stargazer"
  )

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Load the data
vdem <- readRDS("V-Dem-CY-Full-v12.rds")

######################Final Paper######################################

d <- vdem %>%
  dplyr::filter(year>1950 & year<2020)

# Select variables
f <- d %>%
  dplyr::select(year,
                country_name,
                country_id,
                # Democracy
                v2x_partipdem,
                v2xel_frefair,
                # Education
                v2clacfree,
                v2peedueq,
                # Economics
                e_gdppc,
                # Civil Liberties 
                e_fh_cl,
                v2x_freexp,
                #Demographics
                e_peinfmor,
                # Region
                e_regionpol
  )
stargazer(f,type="text", single.row= FALSE)
##################################Graphing###############################


yearly_means <- f %>% 
  group_by(year) %>%
  dplyr::summarize(PDI = mean(v2x_partipdem, na.rm=TRUE))
ggplot(data=yearly_means, mapping = aes(x = year, y = PDI)) +
  geom_line()

yrreg_means <- f %>% 
  group_by(year,e_regionpol) %>%
  dplyr::summarize(PDI = mean(v2x_partipdem, na.rm=TRUE))
ggplot(yrreg_means, mapping = aes(x = year, y = PDI, group=e_regionpol)) +
  geom_line()
ggplot(yrreg_means, mapping = aes(x = year, y = PDI, group=e_regionpol)) +
  geom_line() +
  facet_wrap(facets = vars(e_regionpol))



africa <- f %>%
dplyr::filter(e_regionpol >= 3 & e_regionpol <= 4)

asia <- f %>%
dplyr::filter(e_regionpol >= 6 & e_regionpol <= 8)
########################################


f %>% 
  group_by(year) %>% 
  select(-year, -country_name) %>%
  summarise_all(sd)

f %>% 
  group_by(country_name) %>% 
  select(-year, -country_id, -country_name) %>% 
  summarise_all(sd)


mf_plm_fe_lag <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + factor(year) + factor(country_id), 
                     data = f,
                     index = c("country_id", "year"), 
                     model = "within", 
                     effect = "twoways")
summary(mf_plm_fe_lag)
stargazer(mf_plm_fe_lag,type="text", single.row= FALSE)

mf_plm_fe_lag_wo_equal <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + factor(year) + factor(country_id), 
                     data = f,
                     index = c("country_id", "year"), 
                     model = "within", 
                     effect = "twoways")
summary(mf_plm_fe_lag_wo_equal)

mf_plm_fe_lag_w_controls <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                             data = f,
                             index = c("country_id", "year"), 
                             model = "within", 
                             effect = "twoways")
summary(mf_plm_fe_lag_w_controls)
stargazer(mf_plm_fe_lag_w_controls,type="text", single.row= FALSE)

stargazer(type="text", single.row= FALSE)

stargazer(mf_plm_fe_lag, mf_plm_fe_lag_wo_equal,mf_plm_fe_lag_w_controls, type="text", single.row= FALSE )

###########################Africa Models########################################
africa_mf_plm_fe_lag <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + factor(year) + factor(country_id), 
                     data = africa,
                     index = c("country_id", "year"), 
                     model = "within", 
                     effects= "twoways")
summary(africa_mf_plm_fe_lag)
africa_mf_plm_fe_lag_wo_equal <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + factor(year) + factor(country_id), 
                              data = africa,
                              index = c("country_id", "year"), 
                              model = "within", 
                              effect = "twoways")
summary(africa_mf_plm_fe_lag_w_controls)
africa_mf_plm_fe_lag_w_controls <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                                data = africa,
                                index = c("country_id", "year"), 
                                model = "within", 
                                effect = "twoways")
summary(africa_mf_plm_fe_lag_wo_equal)
stargazer(africa_mf_plm_fe_lag, africa_mf_plm_fe_lag_wo_equal,africa_mf_plm_fe_lag_w_controls, omit="year", type="text", single.row= FALSE)


africa_yearly_means <- africa %>% 
  group_by(year) %>%
  dplyr::summarize(PDI = mean(v2x_partipdem, na.rm=TRUE))
ggplot(data=africa_yearly_means, mapping = aes(x = year, y = PDI)) +
  geom_line()

#########################Asia Models############################################
asia_mf_plm_fe_lag <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + factor(year) + factor(country_id), 
                            data = asia,
                            index = c("country_id", "year"), 
                            model = "within", 
                            effects= "twoways")
summary(asia_mf_plm_fe_lag)
asia_mf_plm_fe_lag_wo_equal <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + factor(year) + factor(country_id), 
                                     data = asia,
                                     index = c("country_id", "year"), 
                                     model = "within", 
                                     effect = "twoways")
summary(africa_mf_plm_fe_lag_wo_equal)
asia_mf_plm_fe_lag_w_controls <- plm(v2x_partipdem ~ lag(v2x_partipdem) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                                       data = asia,
                                       index = c("country_id", "year"), 
                                       model = "within", 
                                       effect = "twoways")
summary(asia_mf_plm_fe_lag_wo_equal)
stargazer(asia_mf_plm_fe_lag, asia_mf_plm_fe_lag_wo_equal,asia_mf_plm_fe_lag_w_controls,omit = "year", type="text", single.row= FALSE)

asia_yearly_means <- asia %>% 
  group_by(year) %>%
  dplyr::summarize(PDI = mean(v2x_partipdem, na.rm=TRUE))
ggplot(data=asia_yearly_means, mapping = aes(x = year, y = PDI)) +
  geom_line()

####################Robustness Checks####################################
robust_mf_plm_fe_lag = plm(v2xel_frefair ~ lag(v2xel_frefair) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                           data = f,
                           index = c("country_id", "year"), 
                           model = "within", 
                           effect = "twoways") 

africa_robust_mf_plm_fe_lag = plm(v2xel_frefair ~ lag(v2xel_frefair) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                           data = africa,
                           index = c("country_id", "year"), 
                           model = "within", 
                           effect = "twoways") 

asia_robust_mf_plm_fe_lag = plm(v2xel_frefair ~ lag(v2xel_frefair) + v2clacfree + v2peedueq + e_gdppc + e_peinfmor + factor(year) + factor(country_id), 
                           data = asia,
                           index = c("country_id", "year"), 
                           model = "within", 
                           effect = "twoways") 

stargazer(robust_mf_plm_fe_lag, africa_robust_mf_plm_fe_lag, asia_robust_mf_plm_fe_lag, omit = "year", type="text", single.row= FALSE)
