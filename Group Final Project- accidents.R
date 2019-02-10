#Explore
pairs(~Injuries+ Wounded+Male + Age +bigcar + scooter + Weather, data = d)

#Gaussian model
s1 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Age,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.age <- map(
  s1 ,
  data=d)
precis(m.age)

s2 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Male,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.male <- map(
  s2 ,
  data=d)
precis(m.male)

#scooter
s3 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*scooter,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)


m.scooter <- map(
  s3 ,
  data=d)
precis(m.scooter)

#bigcar
s4 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*bigcar,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.bigcar <- map(
  s4 ,
  data=d)
precis(m.bigcar)

#Weather
s5 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Weather,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.weather <- map(
  s5 ,
  data=d)
precis(m.weather)


#Timing
s6 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*overnight,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.overnight <- map(
  s6 ,
  data=d)
precis(m.overnight)

#Timing
s7 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*commuting,
  a ~ dnorm( 1 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m.commuting <- map(
  s7 ,
  data=d)
precis(m.commuting)
compare(m.age,m.male,m.commuting,m.scooter,m.bigcar,m.weather,m.overnight)
y1 = coeftab(m.age,m.male,m.commuting,m.scooter,m.bigcar,m.weather,m.overnight)

f1 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Age + bM*Male + bB*bigcar + bW*Weather ,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f2 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*Age + bM*Male+ bB*bigcar + bW*Weather ,
  gamma <- bA + bAM*Male,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bAM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f3 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*Age + bM*Male  ,
  gamma <- bA + bAM*Male,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bAM ~ dnorm(0 ,1 ),
  sigma ~ dunif( 0 , 10 )
)

f4 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Age + bM*Male + bB*bigcar + bW*Weather + bN*neihu + bS*scooter ,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bN ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f5 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Age + bM*Male + bB*bigcar  + bS*scooter ,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f6 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + bA*Age + bM*Male + bB*bigcar + bS*scooter+bW*Weather+bC*commuting+bO*overnight,
  a ~ dnorm( 0 , 5 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  bO ~ dnorm(0, 1 ),
  bC ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f7 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*Age + bM*Male + bB*bigcar+bS*scooter+bW*Weather+bC*commuting+bO*overnight,
  a ~ dnorm( 0 , 5 ) ,
  gamma <- bA + bAM*Male,
  bAM ~ dnorm(0 ,1 ),
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  bO ~ dnorm(0, 1 ),
  bC ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f8 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*Age + bM*Male + bB*bigcar+bS*scooter+bW*Weather+bC*commuting+bO*overnight + bWO*Wounded,
  a ~ dnorm( 0 , 5 ) ,
  gamma <- bA + bAM*Male,
  bAM ~ dnorm(0 ,1 ),
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  bO ~ dnorm(0, 1 ),
  bC ~ dnorm(0, 1 ),
  bWO ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)

f9 =  alist(
  Injuries ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*Age + bM*Male + bB*bigcar+bS*scooter+bW*Weather+bC*commuting+bO*overnight + bWO*Wounded+bN*neihu,
  a ~ dnorm( 0 , 5 ) ,
  gamma <- bA + bAM*Male,
  bAM ~ dnorm(0 ,1 ),
  bA ~ dnorm( 0 , 1 ) ,
  bM ~ dnorm(0 ,1 ),
  bB ~ dnorm(0, 1 ),
  bS ~ dnorm(0, 1 ),
  bW ~ dnorm(0, 1 ),
  bO ~ dnorm(0, 1 ),
  bC ~ dnorm(0, 1 ),
  bWO ~ dnorm(0, 1 ),
  bN ~ dnorm(0, 1 ),
  sigma ~ dunif( 0 , 10 )
)



m1 <- map(
  f1 ,
  data=d)
precis(m1)

m1.2 <- map(
  f2 ,
  data=d)
precis(m1.2)

m1.3 = map(
  f3 ,
  data=d)
precis(m1.3)

m1.4 = map(
  f4 ,
  data=d)
precis(m1.4)

m1.5 = map(
  f5 ,
  data=d)
precis(m1.5)

m1.6 = map(
  f6 ,
  data=d)
precis(m1.6)

m1.7 = map(
  f7 ,
  data=d)
precis(m1.7)

m1.8 = map(
  f8 ,
  data=d)
precis(m1.8)


m1.9 = map(
  f9 ,
  data=d)
precis(m1.9)

c1 = compare(m1.9, m1.8,m1.7, m1.6,m1.5,m1.4,m1.3,m1.2,m1, func=WAIC ) 
c1.2 = compare(m1.8,m1.7, m1.6,m1.5,m1.4,m1.3,m1.2,m1, func=WAIC ) 
coeftab(m1.9)

summary(lm(Injuries ~Age + Male +bigcar+scooter+Weather+commuting+overnight+Wounded+neihu, data = d))
