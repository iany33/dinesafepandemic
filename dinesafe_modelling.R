
pacman::p_load(rio,          # File import
               here,         # File locator
               brms,
               Matrix,
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               janitor,
               lubridate,
               tidybayes, 
               bayesplot,
               marginaleffects,
               emmeans,
               modelr)


# Prep data for analysis - add 0 counts to missing weeks

ts_data <- ts_data %>% fill_gaps(.full = TRUE) %>% 
  replace(is.na(.), 0)

ts_data <- ts_data %>% 
  mutate(time = row_number(week), 
         pandemic = as.factor(case_when(week >= covid_start ~ "Pandemic Period",
                                        TRUE ~ "Pre-Pandemic")), 
         time_post = pmax(0, week - covid_start + 1)) %>% 
  mutate(pandemic = fct_relevel(pandemic, "Pre-Pandemic", "Pandemic Period"))

ts_data  %>% tabyl(time_post)


# Remove weeks with 0 to only a few inspections in 2020

ts_model <- ts_data %>% filter(inspections > 3)
ts_model <- ts_model %>% mutate(month = as.factor(month(week)))

ts_model %>% tabyl(pandemic)


### Segmented regression approach using BRMS ###

#### Model pass rate

get_prior(pass | trials(inspections) ~ 0 + Intercept + time*pandemic + month,
          data = ts_model, family = binomial)

prior <- set_prior("normal(0,1)", class = "b")

m1 <- brm(pass | trials(inspections) ~ 0 + Intercept + time*pandemic + month, 
          data = ts_model, family = binomial, 
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(m1)
plot(m1)
pp_check(m1, ndraws=100)
pp_check(m1, type = "loo_pit")
mcmc_acf(m1, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
mcmc_plot(m1)
conditional_effects(m1)

m2 <- brm(pass | trials(inspections) ~ 0 + Intercept + time*pandemic + month + ar(p = 1), 
          data = ts_model, family = binomial, 
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "loo_pit")
mcmc_acf(m2, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
mcmc_plot(m2)
conditional_effects(m2)

loo(m1, m2)

# Auto-regressive term model fits worse - Check model with month as varying vs. fixed effect

get_prior(pass | trials(inspections) ~ 0 + Intercept + time*pandemic + (1 | month),
          data = ts_model, family = binomial)

m3 <- brm(pass | trials(inspections) ~ 0 + Intercept + time*pandemic + (1 | month), 
          data = ts_model, family = binomial, 
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(m3)
plot(m3)
pp_check(m3, ndraws=100)
pp_check(m3, type = "loo_pit")
mcmc_acf(m3, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
mcmc_plot(m3)
conditional_effects(m3, effects = "pandemic")

loo(m1, m3)

# Compare adding auto-regressive term to varying effect model

m4 <- update(m3, formula. = ~ . + ar(p = 1), backend = "cmdstanr", 
             stan_model_args = list(stanc_options = list("O1")))

summary(m4)
plot(m4)
pp_check(m4, ndraws=100)
pp_check(m4, type = "loo_pit")
mcmc_acf(m4, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
mcmc_plot(m4)
conditional_effects(m4, effects = "pandemic")

loo(m3, m4)



# Summarize model results

print(m3, digits = 5)

exp(fixef(m3)[,-2])

get_variables(m3)

m3 %>% gather_draws(r_month[month,]) %>%  mean_qi()

# Plot posterior predictive distributions for expected values/means

# First effect of pandemic

pred1 <- predictions(m3, type = "response", 
                     newdata = datagrid(model = m3, pandemic = "Pre-Pandemic", 
                                        time = seq(1, 168, by = 2), inspections = 1)) %>% posteriordraws()
head(pred1)

pred2 <- predictions(m3, type = "response", 
                     newdata = datagrid(model = m3, pandemic = "Pandemic Period", 
                                        time = seq(169, 277, by = 2), inspections = 1)) %>% posteriordraws()

pred <- rbind(pred1, pred2)
remove(pred1, pred2)

p1 <- ggplot(pred, aes(x = draw, fill = pandemic)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Pass Rate", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom")   

mfx <- marginaleffects(m3, type = "response", variables = "pandemic", 
                       newdata = datagrid(time = seq(1, 277, by = 2), inspections = 1)) %>% 
  posteriordraws()

p2 <- ggplot(mfx, aes(x = draw)) +
  stat_halfeye(slab_alpha = .5, fill = "#440154")  +
  labs(x = "Effect of Pandemic on Pass Rate", y = "",
       subtitle = "Marginal Effect of Pandemic") +
  theme_minimal() 

cowplot::plot_grid(p1, p2, nrow=1, ncol=2)

remove(p1, p2)

# Alternative plot comparing effect of different time periods

mfx <- marginaleffects(m3, type = "response", variables = "pandemic", 
                       newdata = datagrid(time = c(mean(169:261), mean(262:277)), 
                                          inspections = 1)) %>% posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = factor(time))) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Pandemic on Pass Rate", y = "",
       subtitle = "Marginal Effect of Pandemic") +
  theme_minimal() 

remove(p1, p2)

# Summarize marginal effects/contrast of pandemic of whole time period, over initial 2 years,
# and over 2022 since inspection numbers resumed to 200+ in week 17 (Apr. 25, 2022)

marginaleffects(m3, type = "response", variables = "pandemic", 
                newdata = datagrid(time = seq(1, 277, by = 2), inspections = 1)) %>% summary()

marginaleffects(m3, type = "response", variables = "pandemic", 
                newdata = datagrid(time = seq(1, 261, by = 2), inspections = 1)) %>% summary()

marginaleffects(m3, type = "response", variables = "pandemic", 
                newdata = datagrid(time = seq(262, 277, by = 2), inspections = 1)) %>% summary()

# Now plot effects of time

pred %>% ggplot(aes(x = time, y = draw, colour = pandemic)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 280, by = 50)) +
  labs(x = "Week", y = "Predicted Pass Rate",
       fill = "Credible Interval", colour = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom") 

# Plot effects of month 

pred1 <- predictions(m3, type = "response", re_formula = NULL,
                     newdata = datagrid(model = m3, pandemic = "Pre-Pandemic", 
                                        month = ts_model$month,
                                        time = seq(1, 168, by = 2), inspections = 1)) %>% posteriordraws()
head(pred1)

pred2 <- predictions(m3, type = "response", re_formula = NULL,
                     newdata = datagrid(model = m3, pandemic = "Pandemic Period", 
                                        month = ts_model$month,
                                        time = seq(169, 277, by = 2), inspections = 1)) %>% posteriordraws()

pred <- rbind(pred1, pred2)
remove(pred1, pred2)

pred <- pred %>% mutate(month2 = month.abb[as.numeric(month)]) %>% 
  mutate(month2 = as.factor(month2))  %>% 
  mutate(month2 = fct_relevel(month2, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec"))
  
pred %>% ggplot(aes(x = draw, y = month2, fill = pandemic)) +
  stat_halfeye(slab_alpha = .5) +
  scale_y_discrete(limits = rev) +
  labs(x = "Predicted Pass Rate", y = "", fill = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom") 

remove(pred)


#### Model infraction rate

get_prior(infractions | rate(inspections) ~ 0 + Intercept + time*pandemic + month,
          data = ts_model, family = negbinomial)

prior <- set_prior("normal(0,1)", class = "b")

r1.1 <- brm(infractions | rate(inspections) ~ 0 + Intercept + time*pandemic + month,
          data = ts_model, prior = prior, family = negbinomial,
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, sample_prior = TRUE, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

plot(r1.1)
pp_check(r1.1, type = "stat", stat = "mean", prefix = "ppd")
pp_check(r1.1)
pp_check(r1.1, ndraws=100, prefix = "ppd")
pp_check(r1.1, type = "loo_pit")

r1 <- brm(infractions | rate(inspections) ~ 0 + Intercept + time*pandemic + month,
          data = ts_model, prior = prior, family = negbinomial,
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r1)
plot(r1)
pp_check(r1, ndraws=100)
pp_check(r1, type = "loo_pit")
mcmc_plot(r1)
mcmc_acf(r1, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
conditional_effects(r1)


r2 <- brm(infractions  | rate(inspections) ~ 0 + Intercept + time*pandemic + month + ar(p = 1),
          data = ts_model, prior = prior, family = negbinomial,
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r2)
plot(r2)
pp_check(r2, ndraws=100)
pp_check(r2, type = "loo_pit")
mcmc_plot(r2)
mcmc_acf(r2, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
conditional_effects(r2)

loo(r1, r2)

# Compare with multi-level approach for month

r3 <- brm(infractions  | rate(inspections) ~ 0 + Intercept + time*pandemic + (1 | month),
          data = ts_model, prior = prior, family = negbinomial,
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r3)
plot(r3)
pp_check(r3, ndraws=100)
pp_check(r3, type = "loo_pit")
mcmc_plot(r3)
mcmc_acf(r3, pars = vars(contains("Pandemic"), contains("time")), lags = 10)
conditional_effects(r3)

loo(r1, r3)

# Compare to multi-level model with auto-regressive term

r4 <- brm(infractions  | rate(inspections) ~ 0 + Intercept + time*pandemic + (1 | month) + ar(p = 1),
          data = ts_model, prior = prior, family = negbinomial,
          iter = 4000, chains = 4, cores = 4, warmup = 1000, seed = 9, backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

loo(r3, r4)


# Summarize results of best fitting model

print(r3, digits = 5)

exp(fixef(r3)[,-2])

get_variables(r3)

r3 %>% gather_draws(r_month[month,]) %>%  mean_qi()


# First effect of pandemic

pred <- predictions(r3, type = "response", re_formula = NA,
                    newdata = datagrid(pandemic = c("Pre-Pandemic", "Pandemic Period"),
                                       inspections = 1)) %>% posteriordraws()


pred1 <- predictions(r3, type = "response",
                     newdata = datagrid(model = r3, pandemic = "Pre-Pandemic", 
                                        time = seq(1, 168, by = 2), inspections = 1)) %>% posteriordraws()
head(pred1)

pred2 <- predictions(r3, type = "response",
                     newdata = datagrid(model = r3, pandemic = "Pandemic Period", 
                                        time = seq(169, 277, by = 2), inspections = 1)) %>% posteriordraws()

pred <- rbind(pred1, pred2)
remove(pred1, pred2)

p1 <- ggplot(pred, aes(x = draw, fill = pandemic)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Infraction Rate", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom")   

mfx <- marginaleffects(r3, type = "response", variables = "pandemic", 
                       newdata = datagrid(time = seq(1, 277, by = 2), inspections = 1)) %>% 
  posteriordraws()

p2 <- ggplot(mfx, aes(x = draw)) +
  stat_halfeye(slab_alpha = .5, fill = "#440154")  +
  labs(x = "Effect of Pandemic on Infraction Rate", y = "",
       subtitle = "Marginal Effect of Pandemic") +
  theme_minimal() 

cowplot::plot_grid(p1, p2, nrow=1, ncol=2)

remove(p1, p2)

# Summarize marginal effects/contrast of pandemic of whole time period, over initial 2 years,
# and over 2022 since inspection numbers resumed to 200+ in week 17 (Apr. 25, 2022)

marginaleffects(r3, type = "response", variables = "pandemic",
                        newdata = datagrid(time = seq(1, 277, by = 2), 
                                           inspections = 1)) %>% summary()

marginaleffects(r3, type = "response", variables = "pandemic", 
                newdata = datagrid(time = seq(1, 261, by = 2), inspections = 1)) %>% summary()

marginaleffects(r3, type = "response", variables = "pandemic", 
                newdata = datagrid(time = seq(262, 277, by = 2), inspections = 1)) %>% summary()


# Now plot effects of time 

pred %>% ggplot(aes(x = time, y = draw, colour = pandemic)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 280, by = 50)) +
  labs(x = "Week", y = "Predicted Infraction Rate",
       fill = "Credible Interval", colour = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Plot effects of month (grouping variable)

pred1 <- predictions(r3, type = "response", re_formula = NULL,
                     newdata = datagrid(model = r3, pandemic = "Pre-Pandemic", 
                                        month = ts_model$month,
                                        time = seq(1, 168, by = 2), inspections = 1)) %>% posteriordraws()
head(pred1)

pred2 <- predictions(r3, type = "response", re_formula = NULL,
                     newdata = datagrid(model = r3, pandemic = "Pandemic Period", 
                                        month = ts_model$month,
                                        time = seq(169, 277, by = 2), inspections = 1)) %>% posteriordraws()

pred <- rbind(pred1, pred2)
remove(pred1, pred2)

pred <- pred %>% mutate(month2 = month.abb[as.numeric(month)]) %>% 
  mutate(month2 = as.factor(month2))  %>% 
  mutate(month2 = fct_relevel(month2, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec"))

pred %>% ggplot(aes(x = draw, y = month2, fill = pandemic)) +
  stat_halfeye(slab_alpha = .5) +
  scale_y_discrete(limits = rev) +
  labs(x = "Predicted Infraction Rate", y = "", fill = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom") 

remove(pred)


## Sensitivity analysis

prior_s1 <- set_prior("normal(0,2)", class = "b")
prior_s2 <- set_prior("normal(0,0.5)", class = "b")

m3.1 <- update(m3, prior = prior_s1, backend = "cmdstanr", 
             stan_model_args = list(stanc_options = list("O1")))

m3.2 <- update(m3, prior = prior_s2, backend = "cmdstanr", 
               stan_model_args = list(stanc_options = list("O1")))

print(m3.1, digits = 5)
print(m3.2, digits = 5)
print(m3, digits = 5)


r3.1 <- update(r3, prior = prior_s1, backend = "cmdstanr", 
               stan_model_args = list(stanc_options = list("O1")))

r3.2 <- update(r3, prior = prior_s2, backend = "cmdstanr", 
               stan_model_args = list(stanc_options = list("O1")))

print(r3.1, digits = 5)
print(r3.2, digits = 5)
print(r3, digits = 5)

