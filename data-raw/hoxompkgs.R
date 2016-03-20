dev.member <-
  c(
    "dichika",
    "gghatano",
    "hoxo-m",
    "teramonagi",
    "uribo",
    "yamano357",
    "yutannihilation",
    "sinhrks",
    "kos59125"
  )

pkgs <- pforeach::npforeach(i = 1:length(dev.member))({
  githubinstall::list_github_packages(dev.member[i])
})

hoxompkgs <- pforeach::npforeach(i = 1:length(dev.member), .c = rbind)({
  repo <- gh::gh("/users/:username/repos", username = dev.member[i], .limit = 100)
  repo %>% lambdaR::Map_(x:list(
    full_name     = x$full_name,
    description   = x$description
  )) %>%
    lambdaR::Reduce_(rbind)
}) %>%
  dplyr::as_data_frame() %>% as.data.frame() %>%
  dplyr::filter(full_name %in% pkgs) %>%
  dplyr::mutate(description = ifelse(description == "", NA, description)) %>% 
  dplyr::mutate(description = gsub("\"", "\'", description)) %>% 
  dplyr::mutate(full_name = unlist(full_name))

hoxompkgs %>%
  assertr::verify(nrow(.) >= 34) %>% readr::write_csv(path = "data-raw/hoxompkgs.csv")
devtools::use_data(hoxompkgs, overwrite = TRUE)
