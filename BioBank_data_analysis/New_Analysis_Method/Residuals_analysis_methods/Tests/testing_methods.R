mock_data <- tibble(value = rexp(1e3))
test_normalization <- normalizeResiduales(mock_data)
hist(test_normalization$value)
