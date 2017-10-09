# Q: Where should I eat?
# https://www.yelp.com/search?cflt=restaurants&find_loc=Columbia%2C+SC

## setup ----
# libraries
library(tidyverse)
library(ggridges)

# grid
N <- 1e4
p_grid <- seq(0.001, 0.999, len = N)

# priors
uniform_prior <- rep(1, N)
weak_prior <- dbeta(p_grid, 2, 2)
normalizing_prior <- dbeta(p_grid, 36, 16)

par(mfrow = c(3, 1))
plot(p_grid, uniform_prior, main = 'Uniform')
plot(p_grid, weak_prior, main = 'Weak')
plot(p_grid, normalizing_prior, main = 'Normalizing')
par(mfrow = c(1, 1))

# grid approximation
grid_approx <- function(mu, n, prior, ...) {
  a <- mu * n
  b <- (1-mu) * n
  likelihood <- dbeta(p_grid, round(a) + 1, round(b) + 1)
  unstd.posterior <- prior * likelihood
  posterior <- unstd.posterior %>% {. / sum(.)}
  samples <- sample(p_grid, prob = posterior, size = N, replace = T)
  return(samples)
}

## data ----
df <- tibble::frame_data(
    ~rank, ~restaurant, ~mu, ~n,
    1, "southern belly",  .9, 440,
    2, "bourbon",         .8, 259,
    3, "duck donuts",     .9,  43,
    4, "goat\'s",         .9,  45,
    5, "midwood smoke",   .9, 220,
    6, "the war mouth",   .8,  68,
    7, "the oak table",   .8, 163,
    8, "urban cookhouse", .8,  10,
    9, "poke (columbia)", .9,  51,
   10, "mr friendlys",    .9, 197)

## viz ----
df %>% 
  group_by(restaurant) %>%
  # do(data.frame(samples = grid_approx(.$mu, .$n, uniform_prior))) %>%
  # do(data.frame(samples = grid_approx(.$mu, .$n, weak_prior))) %>%
  do(data.frame(samples = grid_approx(.$mu, .$n, normalizing_prior))) %>%
  ungroup %>%
  inner_join(df %>% select(rank, restaurant), .) %>%
  ggplot(aes(x = samples, y = reorder(restaurant, -rank))) +  
  geom_density_ridges(scale = 4) + theme_ridges() + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = 'Density of samples from posterior', y = 'Restaurant') +
  ggtitle('Bayesian Estimate of Yelp Ratings for Columbia, SC',
          subtitle = 'Prior of beta(36, 16)')
