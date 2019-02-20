# Slides for Applied Machine Learning workshop 

# Part_1_Getting_Started.R

# Slide 7 --------------------------------------------------------

library(tidymodels)

# Slide 12 -------------------------------------------------------

## contains("Sepal")
## 
## # instead of
## 
## c("Sepal.Width", "Sepal.Length")

## merged <- inner_join(a, b)
## 
## # is equal to
## 
## merged <- a %>%
##   inner_join(b)

# Slide 13 -------------------------------------------------------

library(tidyverse)

ames_prices <- "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t") %>%
  rename_at(vars(contains(' ')), funs(gsub(' ', '_', .))) %>%
  rename(Sale_Price = SalePrice) %>%
  filter(!is.na(Electrical)) %>%
  select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )

# Slide 14 -------------------------------------------------------

library(ggplot2)

ggplot(ames_prices, 
       aes(x = Garage_Type,
           y = Sale_Price)) + 
  geom_violin() + 
  coord_trans(y = "log10") + 
  xlab("Garage Type") + 
  ylab("Sale Price") 

# Slide 15 -------------------------------------------------------

library(purrr)

mini_ames <- ames_prices %>%
  select(Alley, Sale_Price, Yr_Sold) %>%
  filter(!is.na(Alley))

head(mini_ames, n = 5)

by_alley <- split(mini_ames, mini_ames$Alley)
map(by_alley, head, n = 2)

# Slide 16 -------------------------------------------------------

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

# Slide 17 -------------------------------------------------------

ames_lst_col <- nest(mini_ames, -Alley)
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~max(.x$Sale_Price))
  )

# Slide 18 -------------------------------------------------------

ames_lst_col

unnest(ames_lst_col, data)


# Slide 19 -------------------------------------------------------

mtcars %>% select(mpg, wt, hp) %>% slice(1:2)

# Slide 20 -------------------------------------------------------

cols <- c("mpg", "wt", "hp")
mtcars %>% select(!!!cols) %>% names()

value <- 5
mtcars %>% select(!!!cols) %>% mutate(x = !!value) %>% slice(1:2)

# Slide 21 -------------------------------------------------------

library(AmesHousing)
ames <- make_ames()

