## Probability

S <- expand.grid(roll1 = 1:6, roll2 = 1:6, roll3 = 1:6)
S
minthree <- apply(S, 1, min)
table(minthree)
