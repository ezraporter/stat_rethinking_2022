curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

sample_mu <- rnorm( 1e4 , 178 , 100 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

library(rethinking)
data("Howell1")
d2 <- Howell1[ Howell1$age >= 18 , ]

mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )


sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

plot( d2$height ~ d2$weight )


xbar <- mean(d2$weight)
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 ) ),
  data=d2 )


plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )

mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

sim.height <- sim( m4.3 , data=list(weight=weight.seq) )

data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[ complete.cases(d$temp) , ]
num_knots <- 60
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

m4.7 <- quap(
  alist(
    T ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(6,10),
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=list( T=d2$temp , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

mu <- link( m4.7 )
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$temp , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )
                                       