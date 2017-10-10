# Q: Where should I see Blade Runner 2049?
# https://www.google.com/search?q=blade+runner+2049+columbia+sc&oq=blade+runner+2049+columbia+sc&gs_l

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
normalizing_prior <- dbeta(p_grid, 30, 22)

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
    ~rank, ~br, ~theater, ~mu, ~n,
    1, F, "nick",                     .94, 116,
    2, T, "regal cinema 7",           .78,  85,
    3, T, "amc dutch square 14",      .84, 160,
    4, F, "amc classic columbia 10",  .80, 162,
    5, F, "regal grande 14",          .88, 318,
    6, F, "spotlight st andrews",     .90, 267,
    7, T, "amc harbison 14",          .78, 159,
    8, F, "carver",                  1.00,   1,
    9, F, "fort jackson",             .78,  59,
   10, T, "regal sandhill 16",        .80, 306
)

## viz ----
df %<>%
  mutate(samples = map2(.$mu, .$n, ~ grid_approx(.x, .y, prior = uniform_prior)))
df %<>% mutate(mean_sample = map_dbl(.$samples, mean),
               median_sample = map_dbl(.$samples, median)) %>%
  arrange(desc(mean_sample), desc(median_sample))

best_two <- df %>% 
  select(theater, samples) %>%
  filter(theater %in% c('amc dutch square 14', 'regal sandhill 16')) %>% 
  unnest %>%
  group_by(theater) %>%
  mutate(row = row_number()) %>%
  spread(theater, samples) %>%
  mutate(diff = `amc dutch square 14` - `regal sandhill 16`)

mean(best_two$diff > 0)
hist(best_two$diff, 30, main = '(dutch square 14) - (regal sandhill 16)')
abline(v = 0, lty = 2, lwd = 3, col = 'red')

p <- df %>% 
  filter(br) %>%
  unnest %>%
  ggplot(aes(x = samples, y = reorder(theater, mean_sample))) +  
  geom_density_ridges(scale = 4, alpha = 0.7) + theme_ridges() + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = 'density of samples from posterior', y = 'theater') +
  ggtitle('Bayesian Estimates of Movie Theater Quality',
          subtitle = 'uniform prior') + 
  labs(caption = 'Data from Google reviews, filtered to only show theaters playing Blade Runner 2049.')

ggsave('theaters.pdf', p)
