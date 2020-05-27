
# 0-1 sequence ------------------------------------------------------------


n = 10
nruns = 5e2
nsims = 1e3

result = matrix(F, nrow = nsims, ncol = n + 2)

for (simnumber in 1 : nsims) {
	for (run in 1 : nruns) {
		j = sample(2:(n+1), 1)
		if (!(result[simnumber, j - 1] | result[simnumber, j + 1])){
			result[simnumber, j] = !result[simnumber, j]
		}
	}
}
colSums(result)


# random permutation ------------------------------------------------------

n = 30
threshold = 8000
success_count = 0
nruns = 1e3
sigma = 1:n
for (i in 1 : nruns) {
	transposition = sample(n, 2)
	candidate = sigma
	candidate[transposition] = sigma[transposition[2:1]]
	if (sum(candidate*(1:n)) > threshold) {
		sigma = candidate
		success_count = success_count + 1
	}
}
success_count