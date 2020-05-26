nsim = 1.6e4
p = c(7/16, 1/4, 1/8, 3/16)


# Constructing qs ---------------------------------------------------------

# m = n - 1
m = length(p) - 1

qs = data.frame(v1 = integer(m), p1 = numeric(m),
					 v2 = integer(m), p2 = numeric(m))
p2 = p * m

for (i in 1 : m) {
	ordered_p = order(p2)
	qs[i,]$v1 = ordered_p[i]
	qs[i,]$p1 = p2[ordered_p[i]]
	p2[ordered_p[i]] = 0
	
	qs[i,]$v2 = ordered_p[m + 1]
	qs[i,]$p2 = 1 - qs[i,]$p1
	p2[ordered_p[m + 1]] = p2[ordered_p[m + 1]] - qs[i,]$p2
}


# simulating from distribution p ------------------------------------------

rvs = integer(nsim)
for (i in 1 : nsim){
	k = ceiling(m * runif(1))
	rvs[i] = ifelse(qs[k,]$p1 > runif(1), qs[k,]$v1, qs[k,]$v2)
}
hist(rvs)


rvs = integer(nsim)
k = ceiling(m * runif(nsim))
rvs = ifelse(qs[k,]$p1 > runif(nsim), qs[k,]$v1, qs[k,]$v2)
hist(rvs,seq(0.4,4.5, by = 1))
