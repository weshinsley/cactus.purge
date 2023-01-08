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

    TV_WIDTH <- 60
    TV_HEIGHT <- 23
    cursor <- draw_tv_screen(TV_WIDTH, TV_HEIGHT)

    cursor_off()

    if (nrow(audio::audio.drivers()) == 0) {
      cursor <- show_pic(cursor, pkg_file("gfx/nosound.txt"), pattern = "down")
      k <- keypress::keypress(block = TRUE)
      cursor <- show_pic(cursor, pkg_file("gfx/empty.txt"), pattern = "up")
    } else if (!check_sound_card()) {
      cursor <- show_pic(cursor, pkg_file("gfx/nosoundcard.txt"), pattern = "down")
      k <- keypress::keypress(block = TRUE)
      cursor <- show_pic(cursor, pkg_file("gfx/empty.txt"), pattern = "up")
    }

    while (TRUE) {
      res <- main_title(cursor)
      cursor <- res$cursor
      config <- res$config
      if (res$res == "CACTZ") {
        cursor <- cactz_title(cursor, config)
      } else if (res$res == "CACTUSKI") {
        cursor <- cactuski_title(cursor, config)
      } else {
        break
      }
    }
  }

  keypress::without_echo(game())
}
