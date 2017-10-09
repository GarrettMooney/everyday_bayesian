# Q: Where should I see Blade Runner 2049?
# https://www.google.com/search?q=blade+runner+2049+columbia+sc&oq=blade+runner+2049+columbia+sc&gs_l

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
    2, T, "regal columbia cinema 7",  .78,  85,
    3, T, "amc dutch square 14",      .84, 160,
    4, F, "amc classic columbia 10",  .80, 162,
    5, F, "regal columbia grande 14", .88, 318,
    6, F, "spotlight st andrews",     .90, 267,
    7, T, "amc harbison 14",          .78, 159,
    8, F, "carver",                  1.00,   1,
    9, F, "fort jackson",             .78,  59,
   10, T, "regal sandhill 16",        .80, 306
)

## viz ----
s <- df %>% 
  group_by(theater) %>%
  # do(data.frame(samples = grid_approx(.$mu, .$n, uniform_prior))) %>%
  # do(data.frame(samples = grid_approx(.$mu, .$n, weak_prior))) %>%
  do(data.frame(samples = grid_approx(.$mu, .$n, normalizing_prior))) %>%
  ungroup %>%
  inner_join(df %>% select(rank, br, theater), .) %>%
  filter(br) 

with(s, mean(samples[theater == 'amc dutch square 14'] >
             samples[theater == 'regal sandhill 16']))

with(s, hist(samples[theater == 'amc dutch square 14'] - 
             samples[theater == 'regal sandhill 16'], xlab = '', 
             main = '(dutch square 14) - (regal sandhill 16)'))
abline(v = 0, lty = 2, lwd = 3, col = 'red')

ggplot(s, aes(x = samples, y = reorder(theater, -rank))) +  
  geom_density_ridges(scale = 4, alpha = 0.7) + theme_ridges() + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = 'Density of samples from posterior', y = 'theater') +
  ggtitle('Bayesian Estimate of Google Reviews for Movie Theaters in Columbia',
          subtitle = 'Uniform Prior')
