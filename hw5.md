Homework 5
================
Ian O’Keeffe
2026-11-17

## Problem 1

``` r
birthday_results_df = expand_grid(
  number_of_people = 2:50,
  iter = 1:10000
) |> 
  mutate(
    repeated_birthdays = map_lgl(number_of_people, create_birthdays)
  )

group_df = birthday_results_df |>
  group_by(number_of_people) |> 
  summarize(repeat_probability = mean(repeated_birthdays))

group_df |> 
  ggplot(aes(x = number_of_people, y = repeat_probability)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  annotate(
    "label", 
    x = 25, y = 0.45, 
    label = "23 people = ~50.7% chance", 
    hjust = 0, size = 3.5, fill = "white"
  ) +
  geom_point(color = 'blue') +
  geom_line() +
  labs(
    x = "Number of People in the Room",
    y = "Probability of a Shared Birthday")
```

<img src="hw5_files/figure-gfm/Problem 1-1.png" alt="" width="85%" style="display: block; margin: auto;" />
Once there are at least 23 people in the room, there is a greater than
50% chance that two people share a birthday. With 50 people in the room,
there is a 96.98% chance of two people sharing a birthday.

## Problem 2

``` r
file_names = list.files("./data", full.names = TRUE)

study_df = tibble(data = purrr::map(file_names, load_clean_csv)) |> 
  tidyr::unnest(cols = data)

exp_ids = sort(unique(study_df$id[startsWith(study_df$id, "exp")]))
con_ids = sort(unique(study_df$id[startsWith(study_df$id, "con")]))

subject_colors = c(
  setNames(colorRampPalette(c("lightblue", "darkblue"))(length(exp_ids)), exp_ids),
  setNames(colorRampPalette(c("salmon", "darkred"))(length(con_ids)), con_ids)
)
study_df |> 
  ggplot(aes(x = week, y = observation, group = id, color = id)) +
  geom_line() +
  scale_color_manual(values = subject_colors) +
  labs(
    x = "Week",
    y = "Observation")
```

<img src="hw5_files/figure-gfm/Problem 2-1.png" alt="" width="85%" style="display: block; margin: auto;" />
The control group has generally lower observation numbers than the
experiment group. There doesn’t seem to be much pattern to the control
group, but the observation values for the experiment group are higher at
the end of the 8 weeks.

## Problem 3
