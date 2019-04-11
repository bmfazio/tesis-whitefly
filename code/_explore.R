wf %>%
  ggplot(aes(x = nlive/bindenom)) +
  geom_bar() +
  facet_wrap(vars(week))
