load_clean_csv = function(filename) {
  return(read.csv(filename) |>
    mutate(
      id = tools::file_path_sans_ext(basename(filename)),
      .before = 1
    ) |> 
    pivot_longer(
      week_1:week_8,
      names_prefix = 'week_',
      names_to = 'week',
      values_to = 'observation'
    ))
}
