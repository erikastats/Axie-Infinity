

# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(googlesheets4)


# Autenticacoes -----------------------------------------------------------

# designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()

gs4_auth(
  cache = ".secrets",
  email = "erikanima3dk@gmail.com"
)

