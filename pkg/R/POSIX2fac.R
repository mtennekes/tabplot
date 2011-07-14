## test
dat <- data.frame(x = rnorm(10000,mean=10, sd=2), y=factor(round(runif(10000)*6)))

dat$df <- ISOdate(2011, 7, 14) + 500*86400*runif(100)
dat$df2 <- ISOdate(2011, 7, 14) + 10000*3600*runif(100)
dat$df3 <- ISOdate(2011, 7, 14) + 10000*60*runif(100)

plot(dat)






rng <- range(dat$df)
diff <- as.numeric(rng[2] - rng[1])



dtminmax <- as.numeric(range(dat$df))
dff <- sum(dtminmax * c(-1, 1))

pretty(dat$df)
range(dat$df)

levels(cut(dat$df, pretty.dates(dat$df, n=7)))



cut(dff, breaks=c(0, 
	breaks = c(0, 120, 60 * 120, 
	labels = c("sec", "min", "hour", "day", "week", "month", "quarter", "year")

# round to years


if (dff > (86400 * 365 * 2.5)


# round to quarters
if (dff > (86400 * 365)

# round to months
if (dff > (86400 * 365 / 4)

# round to weeks
if (dff > (86400 * 365 / 12)

# round to days
if (dff > (86400 * 2)



pretty(as.POSIXct(levels(cut(dat$df, "year"))))


mn <- dtminmax[1]

round(mn, "days")


difference.years <- sum(year(dtminmax)*c(-1, 1))


dtminmax <- range(dat$df3)
brks <- pretty(trunc(dat$df, "days"))



stepsize <- (brks[2] - brks[1])
if (dtminmax[1] < brks[1]) {
	brks[length(brks+1)] <- brks[1]-stepsize
	brks <- brks[c(length(brks), 1:(length(brks)-1))]
	
}
if (dtminmax[2] > brks[2]) {
	brks[length(brks+1)] <- brks[length(brks)]+stepsize
}

dat$temp <- cut(dat$df3, brks, right=FALSE)

range(dat$df3)
levels(dat$temp)

sum(is.na(dat$temp))

pretty(year(dtminmax))



if (difference.years) > 



difference.month <- sum(month(dtminmax)*c(-1, 1))




difference <- sum(as.numeric(dtminmax)*c(-1,1))
difference.years <- difference / 86400 * 


levels(cut(dtminmax, "year"))


cut(ISOdate(2001, 1, 1) + 70*86400*runif(100), 10)
cut(as.Date("2001/1/1") + 70*stats::runif(100), "weeks")


dat$dt <- POSIXct

tableplot(dat)

POSIX2fac <- function(p) {
	
}