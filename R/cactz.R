#' @importFrom magrittr %<>%

###############################################################################
# Some colours...

CACTUS_SKIN <- 64
CACTUS_FLESH <- 82
RUNWAY_1 <- 248
RUNWAY_2 <- 252
SOIL_1 <- 137
SOIL_2 <- 178
SOIL_3 <- 94
SOIL_4 <- 130
PLANE_COL <- 159
BOMB_WARHEAD <- 196
BOMB_TRAIL <- 178

###############################################################################

init_screen <- function(G) {

  # Initialise cactus heights
  cac_min <- min(15, round(5 + (G$level / 2)))
  cac_max <- min(17, round(10 + (G$level / 2)))
  G$frame_delay <- max(0, 0.05 - (G$level * 0.003))

  G$cactii <- round((stats::runif(10) * (cac_max - cac_min)) + cac_min)

  # Draw cactii, top to bottom

  runway <- G$tv_height - 6
  starty <- runway - max(G$cactii)
  for (y in starty:runway) {
    G$cursor %<>% pos_at(0, y)
    for (x in 1:10) {
      if ((runway - G$cactii[x]) < y) {
        G$cursor %<>% write(" %", CACTUS_SKIN)
        G$cursor %<>% write("<>", CACTUS_FLESH)
        G$cursor %<>% write("% ", CACTUS_SKIN)
      } else {
        G$cursor %<>% write("      ")
      }
    }
  }

  # Draw runway, some soil, and a divider

  G$cursor %<>% pos_at(0, runway + 1)
  for (x in seq_len(G$tv_width)) {
    G$cursor %<>% write("=", ifelse(stats::runif(1) < 0.5, RUNWAY_1, RUNWAY_2))
  }

  G$cursor %<>% pos_at(0, runway + 2)
  for (x in seq_len(G$tv_width)) {
    G$cursor %<>% write(ifelse(stats::runif(1) < 0.5, "%", "#"),
                        ifelse(stats::runif(1) < 0.5, SOIL_1, SOIL_2))
  }

  G$cursor %<>% pos_at(0, runway + 3)
  for (x in seq_len(G$tv_width)) {
    G$cursor %<>% write(ifelse(stats::runif(1) < 0.5, "%", "#"),
                        ifelse(stats::runif(1) < 0.5, SOIL_3, SOIL_4))
  }

  G$cursor %<>% pos_at(0, runway + 4)
  for (x in seq_len(G$tv_width)) {
    G$cursor %<>% write("-", ifelse(stats::runif(1) < 0.5, RUNWAY_1, RUNWAY_2))
  }

  G$cursor %<>% write_at(1,22,
    "SCORE:              BOMBS: +Inf     Fuel: NA     Level: 01", 123)

  G$cursor %<>% write_at(28, 22, "+Inf", 202)
  G$cursor %<>% write_at(43, 22, "NA", 196)
  G$cursor %<>% write_at(57, 22, formatC(G$level, width = 2, flag = "0"), 194)

  G
}

###############################################################################

burst_into_flames <- function(G) {
  G$au_drop %<>% stop_sound()
  G$au_boom %<>% play_sound()
  x <- G$cursor[1]
  y <- G$cursor[2]
  set_colour(15)
  flame <- c(226,220,214,208,202,196,160,124, 0)

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

  draw_ring <- function(G, xs, ys, col) {
    for (i in seq_along(xs)) {

      if ((x + xs[i] >= 0) && (x + xs[i] < G$tv_width) &&
          (y + ys[i] >= 0) && (y + ys[i] < (G$tv_height - 2))) {

        G$cursor %<>% pos_at(x + xs[i], y + ys[i])
        set_colour(col)
        r <- 1 + as.integer(5 * stats::runif(1))
        chs <- c("@","o","O","&","#")
        G$cursor %<>% write(chs[r])
      }
    }
    G
  }
  for (i in 1:8) {
    G %<>% draw_ring(rings_x[[i]], rings_y[[i]], flame[i])
    Sys.sleep(0.05)
  }
  for (i in 2:16) {
    if (i<=8)
      G %<>% draw_ring(rings_x[[i]], rings_y[[i]], flame[1])
    fprev <- i - 1
    fnext <- i + 1
    for (fno in 2:9) {
      if (fno <= 9) {
        if ((fprev >= 1) && (fprev <= 8))
          G %<>% draw_ring(rings_x[[fprev]], rings_y[[fprev]], flame[fno])
        if (fnext <= 8)
          G %<>% draw_ring(rings_x[[fnext]], rings_y[[fnext]], flame[fno])
      }
      fprev <- fprev - 1
      fnext <- fnext + 1
    }
    Sys.sleep(0.05)
  }

  Sys.sleep(0.1)
  set_colour(15)
  G$cursor %<>% pos_at(0, G$tv_height + 1)
  G
}

###############################################################################

move_plane <- function(G) {

  # Animate propeller

  G$frame <- (1 - G$frame)
  plane <- ifelse(G$frame == 1, " L-|x", " L-|+")

  # Add some arbitrary points for even a partial landing.

  if (G$planey == G$tv_height - 6) {
    G$score <- G$score + G$level
  }

  # Plane partially off left of screen

  if (G$planex < 0) {
    plane <- substring(plane, 5 - (G$planex+4))


  # Plane partially off right of screen
  # And check for landing

  } else if (G$planex > G$tv_width - 5) {
    if (G$planey == G$tv_height - 6) {
      G$end_game <- G$VICTORY
      return(G)
    }
    plane <- substring(plane, 1, G$tv_width - G$planex)
  }

  # Draw plane

  G$cursor %<>% pos_at(max(G$planex, 0), G$planey)
  G$cursor %<>% write(plane, PLANE_COL)

  # Check for cactus smash
  # Special case on leftmost

  if ((G$planex == -4) && (G$planey == ((G$tv_height - 5) - G$cactii[1]))) {
    G$cursor %<>% pos_at(G$planex + 3, G$planey)
    G %<>% burst_into_flames
    G$end_game <- G$DEATH
    return(G)
  }
  if ((G$planex >= 2) && (G$planex < G$tv_width - 5)) {
    if (((G$planex - 2) %% 6 == 0) &&
        (G$planey == ((G$tv_height - 5) - G$cactii[2 + (G$planex %/% 6)]))) {
      G$cursor %<>% pos_at(G$planex + 3, G$planey)
      G %<>% burst_into_flames
      G$end_game <- G$DEATH
      return(G)
    }
  }

  # Update co-ords, and check for wrapping x

  G$planex <- G$planex + 1
  if (G$planex == G$tv_width) {
    G$planey <- G$planey + 1
    G$planex <- (-4)
  }

  G
}

###############################################################################

move_bomb <- function(G) {

  # Check for nothing to do

  if ((is.na(G$bombx)) || (G$frame == 1) || (!is.na(G$end_game))) {
    return(G)
  }

  # Check for cactus damage

  if (!is.na(G$cactus_bombed)) {
    if (G$bomby > (G$tv_height - 6) - G$cactii[G$cactus_bombed]) {

      # Score 2 for bombing middle of cactus, 1 for a passing blow.

      G$au_splat %<>% play_sound()
      G$score <- G$score + (G$level * ifelse(G$bombx %% 6 %in% 1:3, 2, 1))
      G$cursor %<>% pos_at(1 + ((G$cactus_bombed - 1) * 6), G$bomby)
      G$cursor %<>% write("    ")
      G$blast <- G$blast - 1
      G$cactii[G$cactus_bombed] <- G$cactii[G$cactus_bombed] - 1
      if ((G$blast == 0) || (G$cactii[G$cactus_bombed] == 0)) {
        G$blast <- NA
        G$cactus_bombed <- NA
        G$au_drop %<>% stop_sound()
      }
    }
  }

  G$bomby <- G$bomby + 1

  # First plot - don't let bomb erase plane

  if (((G$bomby - 2) > G$planey) || (G$bombx >= 58)) {
    G$cursor %<>% pos_at(G$bombx, G$bomby - 2)
    G$cursor %<>% write("  ")
  }

  # If bomb hasn't hit the ground yet...

  if ((G$bomby < (G$tv_height - 5)) && (!is.na(G$blast))) {
    G$cursor %<>% pos_at(G$bombx, G$bomby - 1)
    G$cursor %<>% write("%%", BOMB_TRAIL)
    G$cursor %<>% pos_at(G$bombx, G$bomby)
    G$cursor %<>% write("VV", BOMB_WARHEAD)
    return(G)
  }

  # Bomb is done - erase
  G$au_drop %<>% stop_sound()
  G$cursor %<>% pos_at(G$bombx, G$bomby - 1)
  G$cursor %<>% write("  ")
  G$bombx <- NA
  return(G)
}

###############################################################################

drop_bomb <- function(G) {

  # Drop bomb out of middle of plane.
  G$au_drop %<>% play_sound()
  G$bombx <- G$planex + 2
  G$bomby <- G$planey + 1
  G$pau_drop <- audio::play(G$au_drop)

  # Work out which cactus (if any) will get trashed

  if ((G$bombx + 1) %% 6 == 0) {
    G$cactus_bombed <- NA
  } else {
    G$cactus_bombed <- 1 + floor(G$bombx / 6)
  }

  # Number of blasts of cactus damage.

  G$blast <- 3
  G
}

###############################################################################

update_score <- function(G) {
  G$score <- min(G$score, 99999999)
  score <- formatC(G$score, width = 8, format = "d", flag = "0")
  G$cursor %<>% write_at(8, 22, score, 226)
  G
}

###############################################################################

check_keys <- function(G) {

  # Check for dropping a bomb, if we're not already
  # dropping one, and if plane is reasonable on screen

  kp <- keypress::keypress(block = FALSE)
  if ((kp == " ") && (is.na(G$bombx)) &&
      (G$planex >= -2) && (G$planex <= G$tv_width - 4)) {

    G %<>% drop_bomb
  }

  # Check for landing...

  if (kp == "down") {
    G$land <- 1
  }

  return(G)
}

###############################################################################

cactz <- function(G, level) {
  remember_G <- G

  G <- c(G, list(planex = -4, planey = 0, land = NA,
                 bombx = NA, bomby = NA,
                 blast = NA, cactus_bombed = NA,
                 frame = 1, end_game = NA,
                 VICTORY = 1, DEATH = 2,
                 score = 0, level = 1,
                 au_drop = load_sound("data/cactz-drop.wav"),
                 au_splat = load_sound("data/cactz-splat.wav"),
                 au_boom = load_sound("data/cactz-boom.wav"),
                 au_victory = load_sound("data/cactz-victory.wav")))

  while (TRUE) {
    G %<>% init_screen()

    while (is.na(G$end_game)) {
      G %<>% move_plane %<>% move_bomb %<>% check_keys %<>% update_score
      if (is.na(G$land)) {
        if (G$frame_delay > 0) Sys.sleep(G$frame_delay)
      }
      G$land <- NA
    }

    if (G$end_game == G$VICTORY) {
      G$au_victory %<>% play_sound()
      G %<>% show_pic(sprintf("data/cactz-win%d.txt", 1 + (G$level %% 2)))
      G %<>% fade_text(30, 22, " %> %> %> %> %> WELL DONE! LET'S DO IT AGAIN <% <% <% <% <% ", UNICORN,
                       FADE_IN_OUT, 3)
      G %<>% clear_pic(23)
      G$level <- min(99, G$level + 1)
      G$end_game <- NA
      G$planex <- -4
      G$planey <- 0

    } else {
      Sys.sleep(0.5)
      G %<>% show_pic("data/cactz-go.txt")
      Sys.sleep(2)
      if (good_score("data/cactz-hs.csv.xz", G$score)) {
        G %<>% fade_text(30, 16, "GREAT SCORE! TYPE YOUR NAME", UNICORN,
                         FADE_IN, triple = TRUE)
        G %<>% fade_text(25, 19, "..........", GREY_SCALE, FADE_IN, align = LEFT)
        G %<>% get_input(25, 19, 10)
        insert_score("data/cactz-hs.csv.xz", G$text_input, G$score)
      }
      G %<>% clear_pic(23)
      break
    }
  }
  remember_G$cursor <- G$cursor
  remember_G
}

###############################################################################
