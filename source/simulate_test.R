simulate_test = function(mu) {
  
  sample_data = rnorm(n = 30, mean = mu, sd = 5)
  
  broom::tidy(t.test(sample_data, mu = 0)) |>
    transmute(
      mu_hat = estimate,
      p_value = p.value
    )
}
