predict_prob_above_threshold <- function(.data) {
  library(dplyr)

  d_eng <- .data

  brms_eng <- brms::brm(
    data = d_eng,
    family = "negbinomial",
    n_cited ~ 1 + year + (1 || year),
    prior = c(
      prior(normal(0, 100),  class = Intercept),
      prior(normal(0, 2), lb = 0, class = b),
      prior(cauchy(0, 2), class = shape)
    ),
    iter = 4000, warmup = 2000, chains = 4, cores = 4,
    seed = 1024,
    control = list(
      adapt_delta = 0.99,
      max_treedepth = 13
    )
  )


  exist_nine_year_cum_cited <- d_eng %>%
    dplyr::filter(between(year, 2011, 2019)) %>%
    summarise(
      exist_cum_cited = sum(n_cited)
    ) %>%
    pull()

  
  result <- tibble::tibble( year = c(2020)) %>% 
    tidybayes::add_predicted_draws(brms_eng, allow_new_levels = TRUE) %>%
    mutate(
      .prediction = .prediction + exist_nine_year_cum_cited
    ) %>%
    group_by(year) %>%
    summarise(
      pred_mean = mean(.prediction),
      quantile2.5 = quantile(.prediction, probs = 0.025),
      quantile97.5 = quantile(.prediction, probs = 0.975),
      prob_above_line = mean(.prediction >= 2843)
    ) 

  return(result)
}
