# Copy files from Domino directory to /repos
dir_from = list.files("/mnt/github/shinydominoes", full.names = TRUE)
dir_to = "/repos/shinydominoes"
file.copy(from = dir_from, to = dir_to, recursive = TRUE, overwrite = TRUE)


dir_to2 = "/repos/shinydominoes_git"
file.copy(from = dir_from, to = dir_to2, recursive = TRUE, overwrite = TRUE)
