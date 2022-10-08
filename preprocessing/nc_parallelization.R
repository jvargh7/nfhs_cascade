# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
# https://furrr.futureverse.org/articles/progress.html

require(furrr)
require(progressr)
options(future.globals.maxSize= (6*1024*1024)^3) #6GB
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
plan(multisession, workers = 3)
