# Gelman et al. (2021) Regression and Other Stories
# Chapter 1
# ---------------------------------------------------------------------------- #

# Setup
# ---------------------------------------------------------------------------- #
if (!require("xfun")) install.packages("xfun")

# Load/install packages
pkg_attach2("tidyverse", "rio", "rstanarm", "patchwork", "janitor", "lubridate")

# Plots for figure 1.1 "Predicting elections from the economy"
# ---------------------------------------------------------------------------- #
# Load data
election.df <- import("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat")

# Scatterplot of election results (Fig. 1.1a)
fig1.1 <- ggplot(election.df, aes(x = growth, y = vote, label = year)) +
  geom_text() +
  geom_hline(yintercept = 50, linetype = "dashed") +
  labs(x = "Average recent growth in personal income",
       y = "Incumbent party's vote share",
       title = "Forecasting the election from the economy") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12)

# brms regression
election.mod <- stan_glm(vote ~ growth, data = election.df)

# Create fig 1.1b
fig1.2 <- ggplot(election.df, aes(x = growth, y = vote)) +
  geom_point() +
  geom_abline(intercept = coef(election.mod)[[1]], 
              slope = coef(election.mod)[[2]], 
              linetype = "dashed") +
  annotate("text", x = 3.5, y = 55, 
           label = paste0("y = ", round(coef(election.mod)[[1]], 1), " + ", round(coef(election.mod)[[2]], 1), "x"),
           hjust = .5, vjust = .5, size = 5) +
  labs(x = "Average recent growth in personal income",
       y = "Incumbent party's vote share",
       title = "Data and linear fit") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12)

# Combine using patchwork
fig1.1 + fig1.2

# Plots for figure 1.2 "A randomized experiment on the effect of an educational
#                       TV program"
# ---------------------------------------------------------------------------- #
# Load data
tv.df <- import("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectricCompany/data/electric.csv") %>%
  select(-V1) %>%
  mutate(treatment = ifelse(treatment == 1, "Treated\nclasses", "Control\nclasses"),
         grade = paste0("Grade ", grade)) %>%
  group_by(grade, treatment) %>%
  mutate(mean = mean(post_test)) %>%
  ungroup()

# Create fig 1.2
ggplot(tv.df, aes(x = post_test)) +
  geom_histogram(binwidth = 4) +
  geom_vline(aes(xintercept = mean), size = 1.5) +
  scale_y_continuous(breaks = seq(0,8, 2), labels = rep("", 5)) +
  scale_x_continuous(breaks = c(50, 75, 100, 125)) +
  facet_grid(treatment ~ grade, switch = "y") +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "")

# Plots for figure 1.3 "United Nations peacekeeping"
# ---------------------------------------------------------------------------- #
# Notes: Difficult example due to an array of variables that are used without 
#        much commenting. 

# Load data
un.df <- import("https://github.com/avehtari/ROS-Examples/blob/master/Peacekeeping/data/pk&pkept_old.dta?raw=true")

# Dates
un.df <- un.df %>% 
  mutate(censored     = morewar == 0,
         badness      = log(hazard1),  # Aki made this, but it isn't needed for this plot
         peacekeepers = pk_dum == 1) %>% 
  mutate(faildate = ifelse(is.na(faildate) & !is.na(cfdate), as.Date("2004-12-31"), faildate) %>% as.Date(., origin = "1970-01-01")) %>% 
  mutate(delay = as.numeric(faildate - cfdate) / 365.24) %>% 
  mutate(ok = pcw == 1 & !is.na(delay))

# Create figure 1.3
fig1.3 <- un.df %>% 
  filter(ok == T, censored == F) %>% 
  # to make the facet labels pretty
  mutate(peacekeepers = factor(peacekeepers,
                               levels = c(T, F),
                               labels = c("With peacekeeping: 56% of countries stayed at peace.\nFor others, histogram of time until civil war returned:",
                                          "Without peackeeping: 34% stayed at peace.\nFor others, histogram of time until civil war returned:"))) %>% 
  
  # plot!
  ggplot(aes(x = delay)) +
  geom_histogram(boundary = 0, binwidth = 0.5) +
  scale_x_continuous("Years until return of war", limits = c(0, 8)) +
  facet_wrap(~peacekeepers, scales = "free_y") +
  labs(y = "") +
  theme_minimal(base_size = 12)

# Figure 1.4
# Additional variables
un.df <- un.df %>% 
  mutate(ok2 = ifelse(ok == T & !is.na(badness), T, F),
         badness2 = badness / 2 + 8) 

# Data cleaning
fig1.4 <- un.df %>% 
  filter(ok2 == T) %>% 
  mutate(peacekeepers = factor(peacekeepers,
                               levels = c(F, T),
                               labels = c("Without U.N. peacekeeping",
                                          "With U.N. peacekeeping"))) %>%
  mutate(peacekeepers = fct_rev(peacekeepers),
         censored = factor(censored,
                           levels = c(T, F),
                           labels = c("censored", "not censored"))) %>% 
  ggplot(aes(x = badness2, y = delay)) + 
  geom_point(aes(shape = censored)) +
  scale_shape_manual(NULL, values = c(1, 19)) +
  scale_x_continuous("Pre???treatment measure of problems with the country",
                     breaks = quantile(filter(un.df, ok2 == T) %>% pull(badness2), probs = c(.05, .95)),
                     labels = c("not so bad", "really bad")) +
  ylab("Delay (in years) before return of conflict\n(open circles where conflict did not return)") +
  facet_wrap(~peacekeepers) +
  theme_minimal(base_size = 12) 
