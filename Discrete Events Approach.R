
# Single server -----------------------------------------------------------

T = 8
lambda = 12
mu = 1/5
sigma = 1/80
t = 0
na = 0
nd = 0
ss = 0
ta = rexp(1, rate = lambda)
td = Inf
n = 0
arrivals = numeric(2 * ceiling (T * lambda))
departures = numeric(2 * ceiling (T * lambda))
next_dep = function (){
	max(0, rnorm(1, mean = mu, sd = sigma))
	}
while (min(ta, td) < T) {
	if (ta <= td) {
		t = ta
		na = na + 1
		n = n + 1
		ta = ta + rexp(1, rate = lambda)
		if (n == 1) {
			td = t + next_dep()
		}
		if (na > length(arrivals)) {
			arrivals = c(arrivals, numeric(100))
			departures = c(departures, numeric(100))
		}
		arrivals[na] = t
	} else {
		t = td
		n = n - 1
		nd = nd + 1
		td = ifelse (nd == 0, Inf, td + next_dep())
		departures[nd] = t
	}
}
while (n > 0) {
	t = td
	n = n - 1
	nd = nd + 1
	td = td + next_dep()
	departures[nd] = t
}
tp = max (t- T, 0)
arrivals = arrivals[1:na]
departures = departures[1:nd]



# Servers in series -------------------------------------------------------

maxn = 10000
lambda = 12 
mu1 = 1/6
mu2 = 1/3
sigma = 1/80

t = 0
n1 = 0
n2 = 0
na = 0
nd = 0

ta = rexp(1, lambda)
t1 = Inf
t2 = Inf

arrivals1 = numeric(maxn)
arrivals2 = numeric(maxn)
departures = numeric(maxn)

next_dep = function (mu){
	max(0, rnorm(1, mean = mu, sd = sigma))
}

while (na < maxn) {
	if (ta <= t1 & ta <= t2) {
		t = ta
		na = na + 1
		n1 = n1 + 1
		ta = ta + rexp(1, lambda)
		if (n1 == 1) {
			t1 = t + next_dep(mu1)
		}
		arrivals1[na] = t
	} else if (t1 < ta & t1 <= t2) {
		t = t1
		n1 = n1 - 1
		n2 = n2 + 1
		t1 = ifelse (n1 == 0, Inf, t + next_dep(mu1)) 
		if (n2 == 1) {
			t2 = t + next_dep(mu2)
		}
		arrivals2[na - n1] = t
	} else {
		t = t2
		nd = nd + 1
		n2 = n2 - 1
		t2 = ifelse (n2 == 0, Inf, t + next_dep(mu2))
		departures[nd] = t
	}
}
arrivals2 = arrivals2 [1:(na - n1)]
delartures = departures[1:nd]
(na-n1)/na
nd/na



# Inventory Model ---------------------------------------------------------

lambda = 20
users_demand = function () {sample(1:5, 1)}
L = 3
h = 100
c = function (y) {2e3 * y + 2e4}
r = 3e3
maxT = 10^4
s = 180
S = 700

R = 0
C = 0
H = 0

t = 0
x = 1000
y = 0
t0 = rexp(1, lambda)
t1 = Inf

while (min(t0, t1) < maxT) {
	if (t0 < t1) {
		H = H + (t0 - t) * x * h
		t = t0
		w = min (x , users_demand())
		R = R + w * r
		x = x - w
		if (x < s & y == 0) {
			y = S - x
			t1 = t + L
		}
		t0 = t0 + rexp(1, lambda)
	} else {
		H = H + (t1 - t0) * x * h
		t = t1
		C = C + c(y)
		x = x + y
		y = 0
		t1 = Inf
	}
}
(R - C - H) / maxT


# An Insurance Risk Model -------------------------------------------------

nu
lambda
mu
c

