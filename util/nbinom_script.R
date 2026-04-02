# pnbinom(q, ...) returns P(X <= q)
# So pnbinom(2, ...) = P(X = 0) + P(X = 1) + P(X = 2)

# For a line of 2.5:
# -----------------
# UNDER 2.5 = need 0, 1, or 2  → P(X <= 2) → pnbinom(2, ...)
# OVER 2.5  = need 3, 4, 5...  → P(X >= 3) → 1 - pnbinom(2, ...)

# The trick: always use floor(line) for the pnbinom call
# Then ask: am I wanting AT MOST that (under) or MORE than that (over)?

mu <- 3.7
sd <- 1.95
variance <- sd^2
r <- mu^2 / (variance - mu)

line <- 2.5

# Under 2.5: need to stay at or below 2
p_under <- pnbinom(floor(line), size = r, mu = mu)  # P(X <= 2)

# Over 2.5: need 3 or more  
p_over <- 1 - pnbinom(floor(line), size = r, mu = mu)  # 1 - P(X <= 2) = P(X >= 3)

# Sanity check: they sum to 1
p_under + p_over  # Should be 1