library(tidyverse)
library(tidybayes)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = "-march=corei7 -mtune=corei7")



d_eng  <- tibble::tribble(
	~discipline, ~year, ~n_paper, ~n_cited,
	"Engineering", 2000L, 3L, 10L,
	"Engineering", 2001L, 4L, 38L,
	"Engineering", 2002L, 5L, 84L,
	"Engineering", 2003L, 10L, 175L,
	"Engineering", 2004L, 5L, 96L,
	"Engineering", 2005L, 12L, 76L,
	"Engineering", 2006L, 7L, 53L,
	"Engineering", 2007L, 11L, 79L,
	"Engineering", 2008L, 7L, 105L,
	"Engineering", 2009L, 11L, 109L,
	"Engineering", 2010L, 15L, 143L,
	"Engineering", 2011L, 14L, 207L,
	"Engineering", 2012L, 16L, 93L,
	"Engineering", 2013L, 15L, 95L,
	"Engineering", 2014L, 15L, 102L,
	"Engineering", 2015L, 13L, 208L,
	"Engineering", 2016L, 13L, 103L,
	"Engineering", 2017L, 20L, 431L,
	"Engineering", 2018L, 49L, 1166L,
	"Engineering", 2019L, 56L, 231L
)


brms::make_stancode(
  n_cited ~ 1 + year,
	data = d_eng, 
  family = "negbinomial"
	)


# preparation
# https://vasishth-statistics.blogspot.com/2017/03/fitting-bayesian-linear-mixed-models.html
X <- unname(model.matrix(~ 1 + year, d_eng))
attr(X, "assign") <- NULL
ncol(X)

new_X <- matrix(c(2020), ncol = 1)
new_X

# put into a list
stan_dat <- list(
	   N = nrow(d_eng),
	   Y = d_eng$n_cited,
	   K = ncol(X),
     X = X,  # population-level design matrix
	   M = 1,
	   new_X = new_X
)
stan_dat


########################################################
fit <- stan(
      file = "complex2.stan", 
			data = stan_dat,
			iter = 4000, 
			chains = 4, 
			warmup = 2000, 
			cores = 4,
			control = list(
				adapt_delta = 0.99,
				max_treedepth = 13
			))

fit
########################################################



# trace 
stan_hist(fit)

stan_trace(fit, pars = c("beta[1]", "alpha", "b_alpha", "phi"))
stan_trace(fit, pars = c("new_y"))

stan_hist(fit, pars = c("beta[1]", "alpha", "b_alpha", "phi"))
stan_hist(fit, pars = c("new_y"))





# Posterior Predictive Checks
library(bayesplot)
y_rep <- as.matrix(fit, pars = "y_rep")
y_rep

ppc_dens_overlay(y = d_eng$n_cited, y_rep[1:200,])

get_posterior_mean(fit)




# Posterior sample
post_draw <- fit %>% tidybayes::spread_draws(new_y[condition]) 
post_draw

post_draw %>% 
  ggplot(aes(x = new_y, y= condition) ) +
  stat_halfeyeh() 




# add cumsum 
prob_pred <- post_draw %>% 
	mutate(new_y = new_y + 2636) %>% 
	group_by(condition) %>% 
	summarise(
		pred_mean = mean(new_y, na.rm = T),
		prob_above_line = mean(new_y >= 2843)
	)
prob_pred


post_draw %>% 
  mutate(new_y = new_y + 2636) %>% 
	ggplot(aes(x = new_y, 
	           y= condition, #y = factor(condition), 
			   fill = stat(x > 2843)
			   )
		   ) +
	stat_halfeyeh() +
	geom_vline(xintercept = 2843, linetype = "dashed", size = 2,
			   color = "red") +
	scale_fill_manual(values = c("gray80", "skyblue"))
