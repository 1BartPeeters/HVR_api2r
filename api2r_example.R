
## Using R to downloading data from the Hjorteviltregisteret Rapportgenerator API v1

# Documentation:
# https://hvr-utv.non-prod.landbruksdirektoratet.no/swagger/index.html?urls.primaryName=Rapportgenerator


# set local encoding to work with Norwegian letters (maybe not necessary on Linux server)
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

# install.packages(c("httr2", "jsonlite"))
library(httr2)
library(jsonlite)

## LOGIN ####
# credentials are stored in .Renviron
brukernavn <- Sys.getenv("MY_HVR_API_USERNAME")
passord <- Sys.getenv("MY_HVR_API_PASSWORD")

#base_url <- "https://hvr-utv.non-prod.landbruksdirektoratet.no" # deprecated: test version
base_url <-  "https://www.hjorteviltregisteret.no/"


# 1) Login and get tokens
login_req <- request(base_url) |>
  req_url_path_append("api/v1/brukerauth/login") |>
  req_headers(
    Accept = "application/json",
    `Content-Type` = "application/json"
  ) |>
  req_body_json(list(
    Brukernavn = brukernavn,
    Passord = passord
  ))

# send login request
login_resp <- login_req |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()

resp_status(login_resp)
#resp_headers(login_resp)
login_data <- fromJSON(resp_body_string(login_resp), flatten = TRUE)
# Inspect returned fields
print(login_data) # if error on login


# 2) Extract tokens
access_token <- login_data$AccessToken
refresh_token <- login_data$RefreshToken


# 3) Create authenticated base request
base_req <- request(base_url) |>
  req_headers(
    Authorization = paste("Bearer", access_token),
    Accept = "application/json"
  )


# Simple example query:
resp <- base_req |>
  req_url_path_append("api/v1/rapportgenerator/individ") |> # legg til path til endpoint
  req_url_query(Art="Villrein",page=1,pageSize=100) |>     # legg til query parameters
  req_perform() |>                                          # utfør request
  resp_body_string() |>                                 # hent response body som string
  fromJSON(flatten = TRUE)          # parse JSON til R-liste/dataframe

str(resp,1)
head(resp)

# to remove object
# rm(resp)

resp <- base_req |>
  req_url_path_append("api/v1/rapportgenerator/individ") |> # legg til path til endpoint
  req_url_query(Art="Villrein", Sesong=2024,
                Villreinområde = "2222", page=1,pageSize=100) |>     # legg til query parameters
  req_perform() |>                                          # utfør request
  resp_body_string() |>                                 # hent response body som string
  fromJSON(flatten = TRUE)          # parse JSON til R-liste/dataframe

head(resp)




# Rapporter ####
# Load function helpers that simplifies query building and response parsing
source("HVR_API_helpers.R")
args(get_individrapport)

# Build query
# (a) Define parameters based on the report type, see documentation
# https://hvr-utv.non-prod.landbruksdirektoratet.no/swagger/index.html?urls.primaryName=Rapportgenerator

# (b) Call get_individrapport() with the appropriate parameters and report type.
# The function will handle pagination and return a combined dataframe.

# . Individ  ----

# valid paramters for the report type:
valid_rapport_params("individ")

# create list of parameters
params_individ <- list(
 Art = "Villrein",
 Sesong = c(2024,2025),
 Villreinområde = c("Knutshø","Rondane")
)

# Or, create parameter list template to modify
params_individ <- rapport_template("individ")
params_individ$Art <- "Villrein"
params_individ$Villreinområde <- "2222"
params_individ$Sesong <- seq(2020, 2024)
params_individ$Godkjent <- "Ja"

# call function:
res_individ <- get_individrapport(
 base_req = base_req,
 params = params_individ,
 rapport = "individ",
 all_pages = TRUE # Set to TRUE if you want to retrieve all pages of results
)

# inspect
#str(res_individ, 1)
head(res_individ)

# save report:
# preferred format is .parquet (much smaller and safer for Norwegian letters), but .xlsx and .csv are also possible

save_rapport(res_individ,
             "individ_villrein_2222_godkjent.parquet", # use either xlsx or csv extension.
             overwrite = TRUE) # default FALSE, will not overwrite existing file

# read:
# res_individ <- arrow::read_parquet("individ_villrein_2222_godkjent.parquet")

# or save as rds
#saveRDS(res_individ, "individ_villrein_knutsho-rondane_2024-2025.rds")




#..........................................
#. Fallvilt ----
params_fallvilt <- list(
 Fylke = "18,50",
 FraDato = "2024-01-01",
 TilDato = "2024-12-31",
 Art = "Hjort",
 page = 1
)
params_fallvilt$Fylke <- "Nordland"

res_fallvilt <- get_individrapport(
 base_req = base_req,
 params = params_fallvilt,
 rapport = "fallvilt"
)

head(res_fallvilt)

#..........................................
# . Jaktdag ----

# et eksempel med feil parameter
params_jaktdag <- list(
 Art = "Elg",
 Sesong = "2024",
 Terreng = "Utmark",
 Godkjent = "true",
 pageSize = 100,
 page = 1
)

res_jaktdag <- get_individrapport(
 base_req = base_req,
 params = params_jaktdag,
 rapport = "jaktdag"
)

# Juster
params_jaktdag$Terreng <- NULL

params_jaktdag$Kommune = "Trondheim"

res_jaktdag <- get_individrapport(
  base_req = base_req,
  params = params_jaktdag,
  rapport = "jaktdag"
)

head(res_jaktdag)

#..........................................
# . Telledata ----
params_telledata <- list(
  villreinomrade = "Forollhogna",
  fraAr = 2020,
  tilAr = 2025,
  pageSize = 100,
  page = 1
)

res_telledata <- get_individrapport(
  base_req = base_req,
  params = params_telledata,
  rapport = "telledata"
)
head(res_telledata)



#..........................................
# . Område / vald ----
params_omrade_vald <- list(
 Art = "Hjort",
 Fylke = "Troms",
 pageSize = 100,
 page = 1
)

res_omrade_vald <- get_individrapport(
 base_req = base_req,
 params = params_omrade_vald,
 rapport = "omrade",
 omrade = "vald"
)

head(res_omrade_vald)


#..........................................
# . Område / villreinomrade ----
args(rapport_template)
params_om_rein <- rapport_template("omrade", omrade = "villreinomrade")
params_om_rein$Villreinområde <- NULL

res_omrade_vald <- get_individrapport(
  base_req = base_req,
  params = params_om_rein,
  rapport = "omrade",
  omrade = "villreinomrade"
)

head(res_omrade_vald)

