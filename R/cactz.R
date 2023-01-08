###############################################################################
cactz <- function(cursor, config, TV_HEIGHT = 23, TV_WIDTH = 60) {

  #########################################
  # Cactz Internal State

  G <- new.env(parent = emptyenv())
  G$cursor <- cursor
  G$config <- config
  G$planex <- (-4L)
  G$planey <- 0L
  G$land <- NA
  G$bombx <- NA
  G$bomby <- NA
  G$blast <- NA
  G$cactus_bombed <- NA
  G$frame <- 1L
  G$end_game <- NA
  G$VICTORY <- 1L
  G$DEATH <- 2L
  G$score <- 0L
  G$level <- 1L
  G$tv_height <- TV_HEIGHT
  G$tv_width <- TV_WIDTH
  G$au_drop = load_sound(pkg_file("audio/cactz-drop.wav"), config)
  G$au_splat = load_sound(pkg_file("audio/cactz-splat.wav"), config)
  G$au_boom = load_sound(pkg_file("audio/cactz-boom.wav"), config)
  G$au_victory = load_sound(pkg_file("audio/cactz-victory.wav"), config)
  G$cactus <- paste0(get_colour(64), " %",
                     get_colour(82), "<>",
                     get_colour(64), "% ")

  cactz_init_screen <- function() {
    RUNWAY_1 <- 248
    RUNWAY_2 <- 252
    SOIL_1 <- 137
    SOIL_2 <- 178
    SOIL_3 <- 94
    SOIL_4 <- 130

    # Initialise cactus heights
    cac_min <- min(15, round(5 + (G$level / 2)))
    cac_max <- min(17, round(10 + (G$level / 2)))
    G$frame_delay <- max(0, 0.05 - (G$level * 0.003))

    G$cactii <- round((stats::runif(10) * (cac_max - cac_min)) + cac_min)
    G$cac_count <- sum(G$cactii)

    # Draw cactii, top to bottom

    runway <- G$tv_height - 6
    starty <- runway - max(G$cactii)
    for (y in starty:runway) {
      G$cursor <- pos_at(G$cursor, 0, y)
      for (x in 1:10) {
        if ((runway - G$cactii[x]) < y) {
          G$cursor <- write(G$cursor, G$cactus)
        } else {
          G$cursor <- write(G$cursor, "      ")
        }
      }
    }

    # Draw runway, some soil, and a divider

    G$cursor <- pos_at(G$cursor, 0, runway + 1)
    for (x in seq_len(G$tv_width)) {
      G$cursor <- write(G$cursor, "=", ifelse(stats::runif(1) < 0.5, RUNWAY_1, RUNWAY_2))
    }

    G$cursor <- pos_at(G$cursor, 0, runway + 2)
    for (x in seq_len(G$tv_width)) {
      G$cursor <- write(G$cursor, if (stats::runif(1) < 0.5) "%" else "#",
                                  if (stats::runif(1) < 0.5) SOIL_1 else SOIL_2)
    }

    G$cursor <- pos_at(G$cursor, 0, runway + 3)
    for (x in seq_len(G$tv_width)) {
      G$cursor <-write(G$cursor, if (stats::runif(1) < 0.5) "%" else "#",
                                 if (stats::runif(1) < 0.5) SOIL_3 else SOIL_4)
    }

    G$cursor <- pos_at(G$cursor, 0, runway + 4)
    for (x in seq_len(G$tv_width)) {
      G$cursor <- write(G$cursor,"-",
                        if (stats::runif(1) < 0.5) RUNWAY_1 else RUNWAY_2)
    }

    G$cursor <- write_at(G$cursor, 1,22,
      "SCORE:              BOMBS: +Inf     Fuel: NA     Level: 01", 123)

    G$cursor <- write_at(G$cursor, 28, 22, "+Inf", 202)
    G$cursor <- write_at(G$cursor, 43, 22, "NA", 196)
    G$cursor <- write_at(G$cursor, 57, 22,
                         formatC(G$level, width = 2, flag = "0"), 194)

    invisible()
  }

###############################################################################

  cactz_burst_into_flames <- function() {
    if (G$config$audio) {
      G$au_drop <- stop_sound(G$au_drop)
      G$au_boom <- play_sound(G$au_boom)
    }
    x <- G$cursor[1]
    y <- G$cursor[2]

    set_colour(15)
    flame <- c(226,220,214,208,202,196,160,124,0)

    rings_x <- list(0,
                  c(-1, 1, 0, 0),
                  c(-3,-2,-1,1,2,3,1,-1),
                  c(-4,-3,-2,-1,0,1,2,3,4,3,2,1,0,-1,-2,-3),
                  c(-5,-4,-3,-2,-1,0,1,2,3,4,5,4,3,2,1,0,-1,-2,-3,-4),
                  c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,5,4,3,2,1,0,-1,-2,-3,-4,-5),
                  c(-7,-6,-5,-4,-2,2,4,5,6,7,6,5,4,2,-2,-4,-5,-6),
                  c(-7,-6,-5,-3,3,5,6,7,7,6,5,3,-3,-5,-6,-7))


    rings_y <- list(0,
                  c(0,0,-1,1),
                  c(0,0,-1,-1,0,0,1,1),
                  c(0,-1,-1,-2,-2,-2,-1,-1,0,1,1,2,2,2,1,1),
                  c(0,-1,-2,-2,-3,-3,-3,-2,-2,-1,0,1,2,2,3,3,3,2,2,1),
                  c(0,-1,-2,-3,-3,-4,-4,-4,-3,-3,-2,-1,0,1,2,3,3,4,4,4,3,3,2,1),
                  c(0,-1,-2,-3,-4,-4,-3,-2,-1,0,1,2,3,4,4,3,2,1),
                  c(-1,-2,-3,-4,-4,-3,-2,-1,1,2,3,4,4,3,2,1))

    draw_ring <- function(xs, ys, col) {
      for (i in seq_along(xs)) {

        if ((x + xs[i] >= 0) && (x + xs[i] < G$tv_width) &&
            (y + ys[i] >= 0) && (y + ys[i] < (G$tv_height - 2))) {

          G$cursor <- pos_at(G$cursor, x + xs[i], y + ys[i])
          set_colour(col)
          r <- 1 + as.integer(5 * stats::runif(1))
          chs <- c("@","o","O","&","#")
          G$cursor <- write(G$cursor, chs[r])
        }
      }
    }

    next_frame <- waitr::waitr_timestamp() + 50
    for (i in 1:8) {
      draw_ring(rings_x[[i]], rings_y[[i]], flame[i])
      next_frame <- waitr::wait_until(next_frame) + 50
    }

    for (i in 2:16) {
      if (i<=8)
        draw_ring(rings_x[[i]], rings_y[[i]], flame[1])
      fprev <- i - 1
      fnext <- i + 1

      for (fno in 2:9) {
        if (fno <= 9) {
          if ((fprev >= 1) && (fprev <= 8))
            draw_ring(rings_x[[fprev]], rings_y[[fprev]], flame[fno])
          if (fnext <= 8)
            draw_ring(rings_x[[fnext]], rings_y[[fnext]], flame[fno])
        }
        fprev <- fprev - 1
        fnext <- fnext + 1
      }
      next_frame <- waitr::wait_until(next_frame) + 50
    }

    set_colour(15)
    G$cursor <- pos_at(G$cursor, 0, G$tv_height + 1)
  }

###############################################################################

  cactz_move_plane <- function() {

    PLANE_COL <- 159

    # Animate propeller

    G$frame <- (1 - G$frame)
    plane <- ifelse(G$frame == 1, " L-|x", " L-|+")

    # Add some arbitrary points for even a partial landing.

    if (G$planey == G$tv_height - 6) {
      G$score <- G$score + G$level
    }

    # If no more cactus, speed up landing...

    if ((G$cac_count == 0) && (G$planey < (G$tv_height - 7)) &&
        (G$planex %in% c(20,30,40))) {
      G$cursor <- pos_at(G$cursor, max(G$planex, 0), G$planey)
      G$cursor <- write(G$cursor, "     ")
      G$planey <- G$planey + 1
    }

    # Plane partially off left of screen

    if (G$planex < 0) {
      plane <- substring(plane, 5 - (G$planex+4))


    # Plane partially off right of screen
    # And check for landing

    } else if (G$planex > G$tv_width - 5) {
      if (G$planey == G$tv_height - 6) {
        G$end_game <- G$VICTORY
        return()
      }
      plane <- substring(plane, 1, G$tv_width - G$planex)
    }

    # Draw plane

    G$cursor <- pos_at(G$cursor, max(G$planex, 0), G$planey)
    G$cursor <- write(G$cursor, plane, PLANE_COL)

    # Check for cactus smash
    # Special case on leftmost

    if ((G$planex == -4) && (G$planey == ((G$tv_height - 5) - G$cactii[1]))) {
      G$cursor <- pos_at(G$cursor, G$planex + 3, G$planey)
      cactz_burst_into_flames()
      G$end_game <- G$DEATH
      return()
    }

    if ((G$planex >= 2) && (G$planex < G$tv_width - 5)) {
      if (((G$planex - 2) %% 6 == 0) &&
          (G$planey == ((G$tv_height - 5) - G$cactii[2 + (G$planex %/% 6)]))) {
        G$cursor <- pos_at(G$cursor, G$planex + 3, G$planey)
        cactz_burst_into_flames()
        G$end_game <- G$DEATH
        return()
      }
    }

    # Update co-ords, and check for wrapping x

    G$planex <- G$planex + 1
    if (G$planex == G$tv_width) {
      G$planey <- G$planey + 1
      G$planex <- (-4)
    }

    invisible()
  }

###############################################################################

  cactz_move_bomb <- function() {
    BOMB_WARHEAD <- 196
    BOMB_TRAIL <- 178

    # Check for nothing to do

    if ((is.na(G$bombx)) || (G$frame == 1) || (!is.na(G$end_game)) ||
        (G$planey == G$tv_height - 6)) {
      return()
    }


    # Check for cactus damage

    if (!is.na(G$cactus_bombed)) {
      if (G$bomby > (G$tv_height - 6) - G$cactii[G$cactus_bombed]) {

      # Score 2 for bombing middle of cactus, 1 for a passing blow.

        G$au_splat <- play_sound(G$au_splat)
        G$score <- G$score + (G$level * ifelse(G$bombx %% 6 %in% 1:3, 2, 1))
        G$cac_count <- G$cac_count - 1
        G$cursor <- pos_at(G$cursor, 1 + ((G$cactus_bombed - 1) * 6), G$bomby)
        G$cursor <- write(G$cursor, "    ")
        G$blast <- G$blast - 1
        G$cactii[G$cactus_bombed] <- G$cactii[G$cactus_bombed] - 1
        if ((G$blast == 0) || (G$cactii[G$cactus_bombed] == 0)) {
          G$blast <- NA
          G$cactus_bombed <- NA
          G$au_drop <- stop_sound(G$au_drop)
        }
      }
    }

    G$bomby <- G$bomby + 1

    # First plot - don't let bomb erase plane

    if (((G$bomby - 2) > G$planey) || (G$bombx >= 58)) {
      G$cursor <- pos_at(G$cursor, G$bombx, G$bomby - 2)
      G$cursor <- write(G$cursor, "  ")
    }

    # If bomb hasn't hit the ground yet...

    if ((G$bomby < (G$tv_height - 5)) && (!is.na(G$blast))) {
      G$cursor <- pos_at(G$cursor, G$bombx, G$bomby - 1)
      G$cursor <- write(G$cursor, "%%", BOMB_TRAIL)
      G$cursor <- pos_at(G$cursor, G$bombx, G$bomby)
      G$cursor <- write(G$cursor, "VV", BOMB_WARHEAD)
      return()
    }

    # Bomb is done - erase
    G$au_drop <- stop_sound(G$au_drop)
    G$cursor <- pos_at(G$cursor, G$bombx, G$bomby - 1)
    G$cursor <- write(G$cursor, "  ")
    G$bombx <- NA
    invisible()
  }

###############################################################################

  cactz_drop_bomb <- function() {
    if (G$cac_count == 0) {
      return()
    }

    # Drop bomb out of middle of plane.
    G$au_drop <- play_sound(G$au_drop)
    G$bombx <- G$planex + 2
    G$bomby <- G$planey + 1

    # Work out which cactus (if any) will get trashed

    if ((G$bombx + 1) %% 6 == 0) {
      G$cactus_bombed <- NA
    } else {
      G$cactus_bombed <- 1 + floor(G$bombx / 6)
    }

    # Number of blasts of cactus damage.

    G$blast <- 3
    invisible()
  }

###############################################################################

  cactz_update_score <- function() {
    G$score <- min(G$score, 99999999)
    score <- formatC(G$score, width = 8, format = "d", flag = "0")
    G$cursor <- write_at(G$cursor, 8, 22, score, 226)
    invisible()
  }

###############################################################################

  cactz_check_keys <- function() {

    # Check for dropping a bomb, if we're not already
    # dropping one, and if plane is reasonable on screen

    kp <- keypress::keypress(block = FALSE)

    if (is.na(kp)) {
      return()
    }

    if ((kp == " ") && (is.na(G$bombx)) &&
        (G$planex >= -2) && (G$planex <= G$tv_width - 4)) {

      cactz_drop_bomb()
    }

    # Check for landing...

    if (kp == "down") {
      G$land <- 1
    }

    invisible()
  }

###############################################################################
# Back to top level function - main game loop.


  while (TRUE) {
    cactz_init_screen()

    while (is.na(G$end_game)) {
      next_frame <- waitr::waitr_timestamp() + (1000 * G$frame_delay)
      cactz_move_plane()
      cactz_move_bomb()
      cactz_check_keys()
      cactz_update_score()

      if (is.na(G$land)) {
        waitr::wait_until(next_frame)
      }
      G$land <- NA
    }

    if (G$end_game == G$VICTORY) {
      G$au_victory <- play_sound(G$au_victory)
      G$cursor <- show_pic(G$cursor, pkg_file(sprintf("gfx/cactz-win%d.txt",
                                              1 + (G$level %% 2))))
      G$cursor <- fade_text(G$cursor, 30, 22,
        " %<>% %<>% %<>% WELL DONE! LET'S DO IT AGAIN %<>% %<>% %<>% ", UNICORN,
                       FADE_IN_OUT, delay = 3000)
      G$cursor <- clear_pic(G$cursor, 23)
      G$level <- min(99, G$level + 1)
      G$end_game <- NA
      G$planex <- -4
      G$planey <- 0

    } else {
      waitr::wait_for(500)
      G$cursor <- show_pic(G$cursor, pkg_file("gfx/cactz-go.txt"))
      waitr::wait_for(2000)
      if (good_score(user_file("cactz-hs.csv.xz"), G$score)) {
        G$cursor <- fade_text(G$cursor, 30, 16, "GREAT SCORE! TYPE YOUR NAME",
                              UNICORN, FADE_IN, triple = TRUE)
        G$cursor <- fade_text(G$cursor, 25, 19, "..........",
                              GREY_SCALE, FADE_IN, align = LEFT)
        res <- get_input(G$cursor, 25, 19, 10)
        G$cursor <- res$cursor
        insert_score(user_file("cactz-hs.csv.xz"), res$res, G$score)
      }
      G$cursor <- clear_pic(G$cursor, 23)
      break
    }
  }
  return(G$cursor)
}

###############################################################################
