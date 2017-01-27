### cutpointr 0.0.9002
### oc_youden mit stats::ecdf

# > microbenchmark::microbenchmark(suppressMessages(cutpointr(elas, elas, status)))
# Unit: milliseconds
# expr      min       lq     mean   median   uq
# suppressMessages(cutpointr(elas, elas, status)) 3.077935 3.136192 3.268425 3.203876 3.28
# max neval
# 5.684583   100

# temp <- data.frame(x = rnorm(1e3),
#                    y = sample(0:1, size = 1e3, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
# expr      min       lq     mean  median       uq
# suppressMessages(cutpointr(temp, x, y)) 3.714339 3.820265 4.078542 3.93885 4.165214
# max neval
# 6.537062   100

# temp <- data.frame(x = rnorm(1e5),
#                    y = sample(0:1, size = 1e5, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
# expr      min       lq     mean  median       uq
# suppressMessages(cutpointr(temp, x, y)) 101.0155 104.1958 113.8081 106.114 108.4907
# max neval
# 197.3222   100


# temp <- data.frame(x = rnorm(1e6),
#                    y = sample(0:1, size = 1e6, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)), times = 5)
# Unit: seconds
# expr      min       lq     mean   median       uq
# suppressMessages(cutpointr(temp, x, y)) 1.969716 2.002865 2.143466 2.062898 2.112437
# max neval
# 2.569414     5

