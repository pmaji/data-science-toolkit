# The actual plots that go along with Part 2 Hands on 1

simple_lm_values %>%
  ggplot(aes(x = 10 ^ .fitted, y = 10 ^ `log10(Sale_Price)`)) +
  geom_point(alpha = .3) +
  geom_abline(col = "green", alpha = .5) +
  geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)

simple_lm_values %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = .3) +
  geom_hline(col = "green", alpha = .5, yintercept = 0) +
  geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)

# Non linear relationship w/ longitude and latitude
simple_lm_values %>%
  ggplot(aes(x = Longitude, y = .std.resid)) +
  geom_point(alpha = .3) +
  geom_hline(col = "green", alpha = .5, yintercept = 0) +
  geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)


simple_lm_values %>%
  ggplot(aes(x = Latitude, y = .std.resid)) +
  geom_point(alpha = .3) +
  geom_hline(col = "green", alpha = .5, yintercept = 0) +
  geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)

# Partial residual plot
add_partial_resid <- function(model, pred_df) {
  
  coefs <- coef(model)
  int <- coefs["(Intercept)"]
  lat <- coefs["Latitude"]
  lon <- coefs["Longitude"]
  
  mutate(
    pred_df, 
    partial_resid_latitude  = `log10(Sale_Price)` - (int + lon * Longitude),
    partial_resid_longitude = `log10(Sale_Price)` - (int + lat * Latitude)
  )
}

# Non linear relationship persists _after_ controlling for 
# other features
add_partial_resid(simple_lm, simple_lm_values) %>%
  ggplot(aes(x = Longitude, y = partial_resid_longitude)) +
  geom_point()

add_partial_resid(simple_lm, simple_lm_values) %>%
  ggplot(aes(x = Latitude, y = partial_resid_latitude)) +
  geom_point()
