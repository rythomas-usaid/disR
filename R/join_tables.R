#### Replicating from Extract Database
#' @export
join_tables <- function(x) {
  x %>% #distinct(udn)

    # join values
    left_join(x = ., y = values
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>%

    # join activities details
    left_join(activities
              , by = join_by(id_ac == id)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)
}

