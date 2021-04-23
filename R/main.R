###############################################################################
# The main program

library(magrittr)
source("R/gfx.R")
source("R/cactz.R")
source("R/titles.R")
source("R/utils.R")
source("R/sounds.R")

launch <- function() {
  init_sound()

  check_windows_ansi()
  cat("\033[2J\033[;H")

  G <- list(cursor = draw_tv_screen(60, 23),
            tv_width = 60, tv_height = 23)

  cursor_off()

  while (TRUE) {
    G %<>% main_title()
    G %<>% cactz_title()
  }
}
