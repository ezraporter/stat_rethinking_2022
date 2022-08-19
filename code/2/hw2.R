library(rethinking)

# 1

data("Howell1")

d <- Howell1[ Howell1$age >= 18 , ]

xbar <- mean(d$height)

# Plotting some prior predictions
N <- 100

a <- rnorm(N, 60, 10)
b <- rlnorm(N, 0 , .5 )


plot( NULL , xlim=range(d$height) , ylim=range(d$weight)*1.2 ,
      ylab="weight" , xlab="height" )


for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d$height) , to=max(d$height) , add=TRUE ,
                        col=col.alpha("black",0.2) )

# Fitting
fit <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*( height - xbar ) ,
    a ~ dnorm( 60 , 10 ) ,
    b ~ dlnorm( 0 , .5 ) ,
    sigma ~ dunif( 0 , 50 ) ),
  data=d )

# Posterior preds
heights <- c(140, 160, 175)

post <- sim(fit, list(height = heights, xbar = xbar))

apply(post, 2, mean)
apply(post, 2, PI)

# 2
d <- Howell1[ Howell1$age < 13 , ]

plot(d$age, d$w)

xbar <- mean(d$age)

# Simulating priors
N <- 100
a <- rnorm(N, 15, 5)
b <- rlnorm(N, 0, .75)
plot( NULL , xlim=c(0, 13) , ylim=c(0, 40) ,
      ylab="weight" , xlab="age" )

for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=0 , to=13 , add=TRUE ,
                        col=col.alpha("black",0.5) )

dat <- list(weight = d$weight, age = d$age, xbar = mean(d$age))

# Fitting
fit <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*( age - xbar ) ,
    a ~ dnorm(15, 5) ,
    b ~ dlnorm(0, .75) ,
    sigma ~ dunif( 0 , 50 ) ),
  data=dat )

# Posterior
post <- extract.samples(fit)
dens(post$b)
precis(fit)

# 3
dat$sex <- d$male + 1

fit <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a[sex] + b[sex]*( age - xbar ) ,
    a[sex] ~ dnorm(15, 5) ,
    b[sex] ~ dlnorm(0, .75) ,
    sigma ~ dunif( 0 , 50 ) ),
  data=dat )

precis(fit, depth = 2)

post <- extract.samples(fit)

b_contrast <- post$b[,2] - post$b[,1]

dens(b_contrast)
precis(b_contrast)
