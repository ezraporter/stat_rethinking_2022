# 1
grid <- seq(0, 1, length.out = 1000)

prior <- rep(1 , 1000)
likelihood <- dbinom(4, size = 15, prob = grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(grid, posterior, type = "l")

# 2

prior <- ifelse(grid < .5, 0, 2)
likelihood <- dbinom(4, size = 6, prob = grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(grid, posterior, type = "l")

# 3

library(rethinking)

samples <- sample(grid, size=1e4, replace=TRUE, prob=posterior )

PI(samples, .89)
HPDI(samples, .89)

# 4
generate_data <- function(p, n = 1000) {
  x <- sample(c("W", "L"), n, replace = TRUE, prob = c(p, 1-p))
  x[x == "W"] <- sample(c("W", "L"), sum(x == "W"), replace = TRUE, prob = c(.8, .2))
}

x <- generate_data(.7)
sum(x == "W")

N <- 20

x <- generate_data(.7, N)
n_x <- sum(x == "W")

likelihood <- dbinom(n_x, size = N, prob = grid * .8)

prior <- dunif(grid, 0, 1)

post <- prior*likelihood

plot(grid, post, type = "l")
