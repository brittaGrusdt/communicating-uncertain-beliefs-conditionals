#' @import dplyr
#' @import tibble
clean_train_data = function(train_data, df.out) {
  anti_by = c("prolific_id")
  # only filter out data from participants from whom all data is excluded
  df.ids = df.out %>% group_by(prolific_id) %>% dplyr::count() %>%
    filter(n == 13) %>% dplyr::select(prolific_id)

  df.slider_choice = anti_join(train_data$train.slider_choice, df.ids,
                               by="prolific_id") %>%
    group_by(prolific_id)
  df.attention = anti_join(train_data$train.attention, df.ids, by="prolific_id") %>%
    group_by(prolific_id)
  df.pe = anti_join(train_data$train.pe, df.ids, by = "prolific_id")

  return(list(train.slider_choice = df.slider_choice,
              train.pe = df.pe,
              train.attention = df.attention))
}
