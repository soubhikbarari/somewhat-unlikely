if (!file.exists("GSS_stata.zip")) {
  download.file("https://gss.norc.org/documents/stata/GSS_stata.zip",
                destfile = "GSS_stata")
  unzip("GSS_stata.zip")
}

if (!file.exists("https://gss.norc.org/Documents/stata/GSS_2020_panel_stata_1a")) {
  download.file("https://gss.norc.org/documents/stata/GSS_2020_panel_stata_1a.zip",
                destfile = "GSS_2020_panel_stata_1a.zip")
  unzip("GSS_2020_panel_stata_1a.zip")
}

if (!file.exists("https://gss.norc.org/Documents/other/Paradata2021.zip")) {
  download.file("https://gss.norc.org/Documents/other/Paradata2021.zip",
                destfile = "Paradata2021.zip")
  unzip("Paradata2021.zip")
}