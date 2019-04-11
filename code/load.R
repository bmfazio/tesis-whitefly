# imm= number of immature whiteflies
# week= the week in which the measurement was taken
# rep= block (experiment was a randomized complete block design with 
# repeated measures over several weeks)
# trt = the treatment received
#       (I believe 1-4 are increasing levels of pesticide via
#       subirrigation, 5= control and 6= hand watering)
# bindenom = the number of live adult insects placed on the plant
# nlive = the number of adult insects surviving (out of bindenom)
# plantid = an identifier for the plant on which the measurement was taken.

wf <- read_delim(
  "in/whitefly.txt",
  delim = " ", skip = 10
  ) %>%
  mutate(
    trt =
      as.factor(ifelse(trt == 5, 0, trt)),
    rep = as.factor(rep)
    )