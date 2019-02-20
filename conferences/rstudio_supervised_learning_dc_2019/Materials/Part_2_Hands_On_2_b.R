holdout_results %>%
  ggplot(aes(x = Year_Built, y = .resid)) +
  geom_point() +
  geom_smooth()

holdout_results %>%
  ggplot(aes(x = Garage_Area, y = .resid)) +
  geom_point() +
  geom_smooth()

holdout_results %>%
  ggplot(aes(x = Central_Air, y = .resid)) +
  geom_boxplot()

holdout_results %>%
  ggplot(aes(x = Electrical, y = .resid)) +
  geom_boxplot()

holdout_results %>%
  ggplot(aes(x = Heating, y = .resid)) +
  geom_boxplot()
