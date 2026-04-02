# Parameters
mu_law <- 17.8; sd_law <- 4.575
mu_nix <- 24.6; sd_nix <- 5.3

# Difference distribution
mu_diff <- mu_law - mu_nix
sd_diff <- sqrt(sd_law^2 + sd_nix^2)

# P(Lawrence > Nix) = P(Diff > 0)
p_lawrence_wins <- 1 - pnorm(0, mean = mu_diff, sd = sd_diff)
# Or equivalently: pnorm(0, mean = mu_diff, sd = sd_diff, lower.tail = FALSE)

p_lawrence_wins
# ~0.319

# Convert to American odds
prob_to_odds <- function(p, name = "") {
  # American odds
  american <- if (p >= 0.5) round(-100 * p / (1 - p)) else round(100 * (1 - p) / p)
  
  # Decimal odds (what you multiply your stake by)
  decimal <- 1 / p
  
  # Fractional odds (profit per unit staked)
  fractional <- (1 - p) / p
  
  # Log odds (logit)
  log_odds <- log(p / (1 - p))
  
  cat(name, "\n")
  cat("  Probability:   ", round(p, 4), "\n")
  cat("  American:      ", ifelse(american > 0, paste0("+", american), american), "\n")
  cat("  Decimal:       ", round(decimal, 3), "\n")
  cat("  Fractional:    ", round(fractional, 2), "/1\n")
  cat("  Log odds:      ", round(log_odds, 3), "\n\n")
  
  invisible(list(prob = p, american = american, decimal = decimal, 
                 fractional = fractional, log_odds = log_odds))
}

prob_to_odds(p_lawrence_wins, "Lawrence > Nix")
prob_to_odds(1 - p_lawrence_wins, "Nix > Lawrence")


