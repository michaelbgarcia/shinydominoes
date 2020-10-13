# Project Delete
library(shinydominoes)

project_delete(owner_name = Sys.getenv("DOMINO_PROJECT_OWNER"),
               project_name = Sys.getenv("DOMINO_PROJECT_NAME"),
               api_key = Sys.getenv("DOMINO_USER_API_KEY"),
               host = "https://opendatasciencelab.jnj.com")
