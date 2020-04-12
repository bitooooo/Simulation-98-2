p = c(.21,.22,.19,.18,.2)
q = rep(.2, 5)
nsim = 10^6
c = 1.1
randnumbers = integer(nsim)
t1 = Sys.time()
for (i in 1 : nsim) {
	while (T) {
		x = ceiling(runif(1) * 5)
		if (runif(1)< p[x]/c/q[x]) {
			randnumbers[i] = x
			break
			}
	}
}
t2 = Sys.time()

hist(randnumbers)
nsim2 = ceiling(c * 1e6)
t3 = Sys.time()

randnumbers2 = ceiling(runif(nsim2) * 5)
u = runif(nsim2)
randnumbers2[p[randnumbers2]/c/q[randnumbers2]>u]->randnumbers2
length(randnumbers2)
t4 = Sys.time()

hist(randnumbers2)
c(t2-t1, t3 - t2)