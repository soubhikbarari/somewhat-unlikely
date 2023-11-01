
readRenviron("../.Renviron")

if (!file.exists("data/shor_mccarty_11_agg.rds")) {
  shor_mccarty_11_agg <- dataverse::get_dataframe_by_name("shor mccarty 2011 state aggregate data.tab",
                                                                dataset = "10.7910/DVN/RCMM6E",
                                                                .f = readr::read_tsv)
  saveRDS(shor_mccarty_11_agg, file = "data/shor_mccarty_11_agg.rds")
} else {
  shor_mccarty_11_agg <- readRDS("data/shor_mccarty_11_agg.rds")
}

if (!file.exists("data/shor_mccarty_11_ind.rds")) {
  shor_mccarty_11_ind <- dataverse::get_dataframe_by_name("shor mccarty 1993-2020 individual legislator data April 2023 release.tab",
                                                          dataset = "10.7910/DVN/NWSYOS",
                                                          .f = readr::read_tsv)
  saveRDS(shor_mccarty_11_ind, file = "data/shor_mccarty_11_ind.rds")
} else {
  shor_mccarty_11_ind <- readRDS("data/shor_mccarty_11_ind.rds")
}

#TODO: merge with party competition data