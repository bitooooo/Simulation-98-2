v = 1:1000
p = rep(1/1000, 1000)
n = length(v)
cump = cumsum(p)
nsim = 10
x = integer(nsim)
for (j in 1 : nsim){
	u = runif(1)
	for (i in 1 : n) {
		if (cump[i] >= u) {
			x[j] = v[i]
			break
		}
	}
}
table(x)/nsim

# uniform integer ---------------------------------------------------------

n = 1000
nsim = 1000000
u = runif(nsim,max = n)
x = ceiling(u)
hist(x)

# geometric ---------------------------------------------------------------

p = .1
r = runif(1)
s = 0
count = 0
lastp = p
while (r >= s) { 
	s = s + lastp
	lastp = lastp * (1 - p)
	count = count + 1
}
count