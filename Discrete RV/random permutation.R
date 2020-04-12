n = 10^2
nsim = 10^4
matrix(rep(1:n,nsim), nrow = nsim, byrow = T)->m
for (i in 1 : nsim) {
	for (j in n : 2) {
		r = ceiling(runif(1) * j)
		b = m[i,r]
		m[i, r] = m[i, j]
		m[i, j] = b
	}
}

# via sort ----------------------------------------------------------------

x = runif(10)
order(x)
