###############################################################################
# The main program

library(magrittr)

#' End the therapeutic entertainment
#'
#' Gets called on exit, and hopefully will restore the terminal to the state
#' it was in before the therapeutic entertainment commenced.
#'

close <- function() {
  set_colour(15)
  cursor_on()
}

#' Begin the therapeutic entertainment
#'
#' This needs to be called from a script in a terminal or command prompt,
#' not in RStudio or Rgui. See the `run` script in the root of the repo.
#'
#' @export
#'

launch <- function() {
  if (!check_windows_ansi()) {
    quit(save = "no", status = 0)
  }

  on.exit({
    close()
  })

  game <- function() {
    cat("\033[2J\033[;H")

    G <- list(cursor = draw_tv_screen(60, 23),
              tv_width = 60, tv_height = 23)

    cursor_off()

    if (nrow(audio::audio.drivers()) == 0) {
      G %<>% show_pic(pkg_file("gfx/nosound.txt"), pattern = "down")
      k <- keypress::keypress(block = TRUE)
      G %<>% show_pic(pkg_file("gfx/empty.txt"), pattern = "up")
    } else if (!check_sound_card()) {
      G %<>% show_pic(pkg_file("gfx/nosoundcard.txt"), pattern = "down")
      k <- keypress::keypress(block = TRUE)
      G %<>% show_pic(pkg_file("gfx/empty.txt"), pattern = "up")
    }

    while (TRUE) {
      G %<>% main_title()
      if (G$main_menu_result == "CACTZ") {
        G %<>% cactz_title()
      } else {
        break
      }
    }
  }

  keypress::with_no_echo(game())
}
