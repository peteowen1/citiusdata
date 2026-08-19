# The World Athletics combined-events scoring tables, in one place.
#
# Sourced by build_combined_components.R and build_combined_simulation.R. Kept
# in a single file deliberately: a duplicated constant table that drifts between
# two scripts is the exact failure mode that has bitten this repo before, and
# these constants are validated by reconstruction in build_combined_components.R
# (97-99% of performances recompute to the stored total exactly, and Kevin
# Mayer's 9126 world record rebuilds with residual 0).
suppressMessages(library(data.table))

# --- the scoring tables ------------------------------------------------------
# Track:  P = A * (B - T)^C   with T in seconds
# Field:  P = A * (M - B)^C   with M in cm for jumps, metres for throws
# Constants are the published World Athletics combined-events tables. They are
# NOT trusted on my say-so - the reconstruction below is what confirms them.
CE_TABLE <- rbindlist(list(
  # men's decathlon
  data.table(ce = "AT-Decathlon-M", event_id = "AT-100Metres-M",        kind = "track", A = 25.4347,  B = 18.00,  C = 1.81),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-LongJump-M",         kind = "cm",    A = 0.14354,  B = 220,    C = 1.40),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-ShotPut-M",          kind = "m",     A = 51.39,    B = 1.50,   C = 1.05),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-HighJump-M",         kind = "cm",    A = 0.8465,   B = 75,     C = 1.42),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-400Metres-M",        kind = "track", A = 1.53775,  B = 82.00,  C = 1.81),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-110MetresHurdles-M", kind = "track", A = 5.74352,  B = 28.50,  C = 1.92),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-DiscusThrow-M",      kind = "m",     A = 12.91,    B = 4.00,   C = 1.10),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-PoleVault-M",        kind = "cm",    A = 0.2797,   B = 100,    C = 1.35),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-JavelinThrow-M",     kind = "m",     A = 10.14,    B = 7.00,   C = 1.08),
  data.table(ce = "AT-Decathlon-M", event_id = "AT-1500Metres-M",       kind = "track", A = 0.03768,  B = 480.00, C = 1.85),
  # women's heptathlon
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-100MetresHurdles-W", kind = "track", A = 9.23076,  B = 26.70,  C = 1.835),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-HighJump-W",         kind = "cm",    A = 1.84523,  B = 75.00,  C = 1.348),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-ShotPut-W",          kind = "m",     A = 56.0211,  B = 1.50,   C = 1.05),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-200Metres-W",        kind = "track", A = 4.99087,  B = 42.50,  C = 1.81),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-LongJump-W",         kind = "cm",    A = 0.188807, B = 210,    C = 1.41),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-JavelinThrow-W",     kind = "m",     A = 15.9803,  B = 3.80,   C = 1.04),
  data.table(ce = "AT-Heptathlon-W", event_id = "AT-800Metres-W",        kind = "track", A = 0.11193,  B = 254.00, C = 1.88),
  # women's pentathlon (indoor): 60mH, HJ, SP, LJ, 800m - shares heptathlon constants
  data.table(ce = "AT-Pentathlon-W", event_id = "AT-60MetresHurdles-W",  kind = "track", A = 20.0479,  B = 17.00,  C = 1.835),
  data.table(ce = "AT-Pentathlon-W", event_id = "AT-HighJump-W",         kind = "cm",    A = 1.84523,  B = 75.00,  C = 1.348),
  data.table(ce = "AT-Pentathlon-W", event_id = "AT-ShotPut-W",          kind = "m",     A = 56.0211,  B = 1.50,   C = 1.05),
  data.table(ce = "AT-Pentathlon-W", event_id = "AT-LongJump-W",         kind = "cm",    A = 0.188807, B = 210,    C = 1.41),
  data.table(ce = "AT-Pentathlon-W", event_id = "AT-800Metres-W",        kind = "track", A = 0.11193,  B = 254.00, C = 1.88),
  # men's heptathlon (indoor): 60m, LJ, SP, HJ, 60mH, PV, 1000m
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-60Metres-M",         kind = "track", A = 58.0150,  B = 11.50,  C = 1.81),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-LongJump-M",         kind = "cm",    A = 0.14354,  B = 220,    C = 1.40),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-ShotPut-M",          kind = "m",     A = 51.39,    B = 1.50,   C = 1.05),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-HighJump-M",         kind = "cm",    A = 0.8465,   B = 75,     C = 1.42),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-60MetresHurdles-M",  kind = "track", A = 20.5173,  B = 15.50,  C = 1.92),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-PoleVault-M",        kind = "cm",    A = 0.2797,   B = 100,    C = 1.35),
  data.table(ce = "AT-Heptathlon-M", event_id = "AT-1000Metres-M",       kind = "track", A = 0.08713,  B = 305.50, C = 1.85)
))
CE_EVENTS <- unique(CE_TABLE$ce)

# marks are stored in seconds for track and metres for field, so jumps are
# converted to cm here rather than in the constants
ce_points <- function(mark, kind, A, B, C) {
  x <- fifelse(kind == "track", B - mark,
       fifelse(kind == "cm",    mark * 100 - B,
                                mark - B))
  fifelse(is.finite(x) & x > 0, floor(A * x^C), 0)
}

