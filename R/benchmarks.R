### cutpointr 0.0.0.9002
### oc_youden mit stats::ecdf

# > microbenchmark::microbenchmark(suppressMessages(cutpointr(elas, elas, status)))
# Unit: milliseconds
#                                            expr      min       lq     mean   median   uq
# suppressMessages(cutpointr(elas, elas, status)) 3.077935 3.136192 3.268425 3.203876 3.28
# max neval
# 5.684583   100

# temp <- data.frame(x = rnorm(1e3),
#                    y = sample(0:1, size = 1e3, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
#                                    expr      min       lq     mean  median       uq
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


#
### cutpointr 0.0.1.9000
# Etwas schneller, wenn die Daten "groß" sind
#
### oc_youden mit stats::ecdf

# microbenchmark::microbenchmark(suppressMessages(cutpointr(elas, elas, status)))
# Unit: milliseconds
#                                            expr      min       lq     mean   median      uq
# suppressMessages(cutpointr(elas, elas, status)) 3.348286 3.442267 3.690816 3.539508 3.60152
# max neval
# 11.06698   100

# temp <- data.frame(x = rnorm(1e3),
#                    y = sample(0:1, size = 1e3, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
#                                    expr      min       lq    mean   median       uq
# suppressMessages(cutpointr(temp, x, y)) 3.837542 3.904194 4.16407 3.960816 4.053559
# max neval
# 6.535849   100

# temp <- data.frame(x = rnorm(1e5),
#                    y = sample(0:1, size = 1e5, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
#                                    expr      min       lq     mean   median      uq
# suppressMessages(cutpointr(temp, x, y)) 84.22623 88.67384 139.2307 91.97892 214.937
# max neval
# 225.7992   100


#
### cutpointr 0.0.1.9000
# Im Bootstrap auf roc() umgestellt. youden auf tp, fp, tn, fn umgestellt
# Neue auc() Funktion, die keinen Overflow ab 1e6 Beobachtungen mehr erzeugt.
# Mein etwas beschleunigter Fork der auc-Funktion aus dem AUC-Paket
#

# microbenchmark::microbenchmark(suppressMessages(cutpointr(elas, elas, status)))
# Unit: milliseconds
#                                            expr      min       lq     mean   median
# suppressMessages(cutpointr(elas, elas, status)) 4.032198 4.117853 4.334191 4.191602
# uq     max neval
# 4.327507 6.47891   100

# microbenchmark::microbenchmark(suppressMessages(cutpointr(elas, elas, status, boot_runs = 100)))
# Unit: milliseconds
#                                                             expr      min       lq     mean
# suppressMessages(cutpointr(elas, elas, status, boot_runs = 100)) 362.6863 376.1693 382.3008
#   median       uq      max neval
# 378.8858 382.2828 507.6127   100

# temp <- data.frame(x = rnorm(1e3),
#                    y = sample(0:1, size = 1e3, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
#                                    expr      min       lq     mean   median       uq
# suppressMessages(cutpointr(temp, x, y)) 4.380787 4.463188 4.719387 4.578738 4.766332
# max neval
# 8.104159   100

# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y, boot_runs = 100)), times = 10)
# Unit: milliseconds
#                                                     expr      min       lq     mean
# suppressMessages(cutpointr(temp, x, y, boot_runs = 100)) 406.4553 409.3364 418.1336
#   median       uq      max neval
# 410.9794 418.9098 467.9604    10

# temp <- data.frame(x = rnorm(1e5),
#                    y = sample(0:1, size = 1e5, replace = T))
# microbenchmark::microbenchmark(suppressMessages(cutpointr(temp, x, y)))
# Unit: milliseconds
#                                    expr      min       lq     mean   median       uq
# suppressMessages(cutpointr(temp, x, y)) 52.28016 53.92546 59.83342 55.07977 56.32792
# max neval
# 168.3551   100