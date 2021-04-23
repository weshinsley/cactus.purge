###############################################################################
# The main program

library(magrittr)
source("R/gfx.R")
source("R/cactz.R")
source("R/titles.R")
source("R/utils.R")
source("R/sounds.R")

close <- function() {
  set_colour(15)
  cursor_on()
  keypress::restore_term_status()
}

launch <- function() {
  check_windows_ansi()
  keypress::save_term_status()
  keypress::set_term_echo(FALSE)

  on.exit({
    close()
  })

  cat("\033[2J\033[;H")

  G <- list(cursor = draw_tv_screen(60, 23),
            tv_width = 60, tv_height = 23)

  cursor_off()

  while (TRUE) {
    G %<>% main_title()
    if (G$main_menu_result == "CACTZ") {
      G %<>% cactz_title()
    } else {
      break
    }
  }
}
