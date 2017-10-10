# Q: Where should I eat?
# https://www.yelp.com/search?cflt=restaurants&find_loc=Columbia%2C+SC

## setup ----
# libraries
library(tidyverse)
library(ggridges)
`%<>%` <- magrittr::`%<>%`

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
df %<>% 
  mutate(samples = map2(.$mu, .$n, ~ grid_approx(.x, .y, prior = normalizing_prior)))
df %<>% mutate(mean_sample = map_dbl(.$samples, mean),
               median_sample = map_dbl(.$samples, median)) %>%
  arrange(desc(mean_sample), desc(median_sample))

p <- df %>% 
  unnest %>%
  ggplot(aes(x = samples, y = reorder(restaurant, mean_sample))) +  
  geom_density_ridges(scale = 4, alpha = 0.7) + theme_ridges() + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = 'density of samples from posterior', y = 'restaurant') +
  ggtitle('Bayesian Estimates of Restaurant Quality',
          subtitle = 'Beta(36, 16) prior') + 
  labs(caption = 'Data from Google reviews')
  
ggsave('restaurants.pdf', p)