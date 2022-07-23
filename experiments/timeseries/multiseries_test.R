# multiseries test

df <- tsibbledata::aus_livestock %>%
  dplyr::group_by(State, Animal) %>%
  dplyr::arrange(Month)

train <- df %>%
  dplyr::slice_head(prop = 0.8) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(key = c("State", "Animal"), index = "Month")

test <- df %>%
  dplyr::slice_tail(prop = 0.2) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(key = c("State", "Animal"), index = "Month")


