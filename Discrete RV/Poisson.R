nsim = 10^4
lambda = 100

# Poisson -----------------------------------------------------------------



rvs = integer(nsim)
p_0 = exp(-lambda)

t = Sys.time()

for (i in 1 : nsim){
	u = runif(1)
	cump = p_0
	p = exp(-lambda)
	while(u > cump) {
		rvs[i] = rvs[i] + 1;
		p = p * lambda / rvs[i] 
		cump = cump + p
	}
}
t1 = Sys.time() - t
m1 = mean(rvs)
	

# Poisson starting from mean ----------------------------------------------

int_lambda = floor(lambda)

cump_lambda = 0
p = exp(-lambda)

for (i in 0 : int_lambda) {
	cump_lambda = cump_lambda + p
	p = p * lambda / (i + 1)
}


p_lambda = p / lambda * (i + 1)

rvs = integer(nsim) + int_lambda

t = Sys.time()

for (i in 1 : nsim){
	u = runif(1)
	cump = cump_lambda
	p = p_lambda
	if (u > cump_lambda) {
		while(u > cump) {
			rvs[i] = rvs[i] + 1;
			p = p * lambda / rvs[i] 
			cump = cump + p
		} 
	} else {
		while(u <= cump) {
			cump = cump - p
			p = p / lambda * rvs[i] 
			rvs[i] = rvs[i] - 1
		}
		rvs[i] = rvs[i] + 1
		
	}
}

t2 = Sys.time() - t
m2 = mean(rvs)
c(t1, t2)
c(m1, m2)