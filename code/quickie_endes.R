library(drake)
library(tidyverse)
library(bayesplot)
library(rstan)
library(loo)

putlabel <- function(x) {
  if (is.null(attr(x, "labels"))) {
    stop("No 'labels' attribute")
  } else {
    return(as.character(factor(
      x,
      levels = attr(x, "labels"),
      labels = names(attr(x, "labels"))
    )))
  }
}

endesdir <- "D:/datasets/inei/endes/"

CSALUD01 <-
  file.path(endesdir, "2017", "Modulo414", "CSALUD01.sav") %>%
  haven::read_sav(encoding = "latin1") %>%
  select(
    !!!c(
      id.household = "HHID",
      id.hh.person = "QSNUMERO",
      survey.weight = "PESO15_AMAS",
      sex = "QSSEXO",
      age = "QS23",
      month = "QSINTM",
      had.fruit  = "QS213U",
      days.fruit = "QS213C",
      had.juice  = "QS215U",
      days.juice = "QS215C",
      had.fsalad  = "QS217U",
      days.fsalad = "QS217C",
      had.vsalad  = "QS219U",
      days.vsalad = "QS219C"
    )
  )
  
REC0111 <-
  file.path(endesdir, "2017", "Modulo66", "REC0111.SAV") %>%
  haven::read_sav(encoding = "latin1") %>%
  select(
    !!!c(
      id.household = "hhid",
      id.hh.person = "V003",
      residence.childhood = "V103",
      residence.time      = "V104",
      residence.last      = "V105"
    )
  )

RECH1 <-
  file.path(endesdir, "2017", "Modulo64", "RECH1.SAV") %>%
  haven::read_sav(encoding = "latin1") %>%
  select(!!!c(
    id.household = "HHID",
    id.hh.person = "HVIDX",
    education = "HV106"
  ))

RECH0 <-
  file.path(endesdir, "2017", "Modulo64", "RECH0.SAV") %>%
  haven::read_sav(encoding = "latin1") %>%
  select(!!!c(
    id.household = "HHID",
    loc.region = "HV023",
    stratum.area = "HV022",
    psu = "HV021"
  ))

RECH23 <-
  file.path(endesdir, "2017", "Modulo65", "RECH23.SAV") %>%
  haven::read_sav(encoding = "latin1") %>%
  select(
    !!!c(
      id.household = "HHID",
      loc.natural = "SHREGION",
      loc.province = "SHPROVIN",
      loc.district = "SHDISTRI",
      wealth.quintile = "HV270",
      wealth.index = "HV271"
    )
  )

endes.merged <-
  CSALUD01 %>%
  left_join(REC0111, by = c("id.household", "id.hh.person")) %>%
  left_join(RECH1, by = c("id.household", "id.hh.person")) %>%
  left_join(RECH0, by = "id.household") %>%
  left_join(RECH23, by = "id.household") %>%
  mutate(
    survey.weight = replace(survey.weight, TRUE, survey.weight / 10 ** 6),
    days.fruit = replace(days.fruit, had.fruit == 3, 0),
    days.juice = replace(days.juice, had.juice == 3, 0),
    days.fsalad = replace(days.fsalad, had.fsalad == 3, 0),
    days.vsalad = replace(days.vsalad, had.vsalad == 3, 0)
  ) %>%
  select(-c(had.fruit, had.juice, had.fsalad, had.vsalad))

endes.ready <- endes.merged %>%
  subset(!(is.na(days.vsalad)|education==8))

endes.ready$bindenom <- 7
endes.ready$sex <- putlabel(endes.ready$sex)
endes.ready$education <- putlabel(endes.ready$education)
endes.ready$education <- putlabel(endes.ready$loc.region)
endes.ready$wealth.quintile <- putlabel(endes.ready$wealth.quintile)
endes.ready$month <- putlabel(endes.ready$month)
endes.ready$age2 <- endes.ready$age**2

save_fit("zib_endes", fit_zib,
         bf(days.vsalad|trials(bindenom) ~ sex + education + loc.region + wealth.quintile + month + age + age2,
            zi ~ sex + education + loc.region + wealth.quintile + month + age + age2))

save_fit("eibSM_endes", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt))

save_fit("eibLN_endes", fit_eiBinomialLN,
         bf(nlive ~ trt + week + rep,
            muL ~ trt))