#' Industrial Production Indices in manufacturing in the European Union 
#'
#' A dataset containing on monthly industrial production indices in manufacturing in the European Union (from sts_inpr_m dataset of Eurostat). Data are based 100 in 2015 and are unadjusted, i.e. neither seasonally adjusted nor calendar adjusted.
#' 
#' The dataset contains 37 time series corresponding to the following geographical area
#' \tabular{cl}{
#' EU28      \tab European Union (current composition)            \cr
#' EU27_2019 \tab European Union (without United Kingdom)         \cr
#' EA19      \tab Euro area (19 countries)                        \cr
#' BE        \tab Belgium                                         \cr
#' BG        \tab Bulgaria                                        \cr
#' CZ        \tab Czechia                                         \cr
#' DK        \tab Denmark                                         \cr
#' DE        \tab Germany (until 1990 former territory of the FRG)\cr
#' EE        \tab Estonia                                         \cr
#' IE        \tab Ireland                                         \cr
#' EL        \tab Greece                                          \cr
#' ES        \tab Spain                                           \cr
#' FR        \tab France                                          \cr
#' HR        \tab Croatia                                         \cr
#' IT        \tab Italy                                           \cr
#' CY        \tab Cyprus                                          \cr
#' LV        \tab Latvia                                          \cr
#' LT        \tab Lithuania                                       \cr
#' LU        \tab Luxembourg                                      \cr
#' HU        \tab Hungary                                         \cr
#' MT        \tab Malta                                           \cr
#' NL        \tab Netherlands                                     \cr
#' AT        \tab Austria                                         \cr
#' PL        \tab Poland                                          \cr
#' PT        \tab Portugal                                        \cr
#' RO        \tab Romania                                         \cr
#' SI        \tab Slovenia                                        \cr
#' SK        \tab Slovakia                                        \cr
#' FI        \tab Finland                                         \cr
#' SE        \tab Sweden                                          \cr
#' UK        \tab United Kingdom                                  \cr
#' NO        \tab Norway                                          \cr
#' ME        \tab Montenegro                                      \cr
#' MK        \tab Former Yugoslav Republic of Macedonia, the      \cr
#' RS        \tab Serbia                                          \cr
#' TR        \tab Turkey                                          \cr
#' BA        \tab Bosnia and Herzegovina                          
#' }
#' @docType data
#' @format A monthly \code{ts} object from january 1990 to december 2017 with 37 variables.
#' @source \url{http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/sts_inpr_m?nace_r2=C&precision=1&sinceTimePeriod=1980M01&unit=I15&s_adj=NSA}
"ipi_c_eu"

# # To update data:
# ipi_c_eu <- eurostat::get_eurostat("sts_inpr_m",select_time = "M",
#                                    filters = list(nace_r2="C",
#                                                   unit = "I15", s_adj = "CA",
#                                                   sinceTimePeriod = "1990M01"))
# ipi_c_eu <- reshape2::dcast(ipi_c_eu, time ~ geo,  value.var = "values")
# ipi_c_eu <- ts(ipi_c_eu[, c("EU28", "EU27_2019", "EA19", "BE", "BG", "CZ", "DK", "DE", 
#                             "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", 
#                             "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE", 
#                             "UK", "NO", "ME", "MK", "RS", "TR", "BA")],
#                start = c(1990, 1), frequency = 12)
# # Last date is removed due to NA:
# ipi_c_eu <- window(ipi_c_eu, end = tail(time(ipi_c_eu),1) - 1/12) 
