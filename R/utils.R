##########################################################################
# ansi_regex copied from has_ansi.R in the crayon library.
# and raw_nchars is like strip_stype, but without the
# useBytes=TRUE, which fails on R 4.1.3 and earlier
# when the text was read with readLines(..., encoding="UTF-8")

ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]")

raw_nchar <- function(text) {
  nchar(gsub(ansi_regex, "", text, perl = TRUE))
}

data_frame <- function(...) {
  data.frame(stringsAsFactors = FALSE, ...)
}

load_hiscores <- function(f) {
  hs <- utils::read.csv(f, stringsAsFactors = FALSE)
  hs$name <- unlist(lapply(hs$name, RCurl::base64Decode))
  hs$score <- unlist(lapply(hs$score, RCurl::base64Decode))
  hs
}

################################################################################
# Make hi-score file utterly impenetrable, because we don't want CHEATING,
# (unless you have the source code of course)

save_hiscores <- function(hs, f) {
  hs$name <- unlist(lapply(hs$name, RCurl::base64Encode))
  hs$score <- unlist(lapply(hs$score, RCurl::base64Encode))
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(hs, tmp, row.names = FALSE)
  R.utils::gzip(tmp, overwrite = TRUE, ext = "xz", FUN = xzfile, compression = 9)
  tmp <- paste0(tmp, ".xz")
  if (file.exists(f)) {
    file.remove(f)
  }
  file.copy(tmp, f)
  file.remove(tmp)
  invisible()
}

good_score <- function(f, score) {
  as.numeric(score) > as.numeric(load_hiscores(f)$score[8])
}

insert_score <- function(f, name, score) {
  hs <- rbind(load_hiscores(f),
              data_frame(name = name, score = score))
  hs <- hs[order(as.numeric(hs$score), decreasing = TRUE), ]
  save_hiscores(hs[1:8, ], f)
  invisible()
}

pkg_file <- function(f) {
  system.file(f, package = "cactus.purge")
}

user_file <- function(f) {
  dir.create(tools::R_user_dir("cactus.purge"), showWarnings = FALSE,
             recursive = TRUE)
  file.path(tools::R_user_dir("cactus.purge"), f)
}

load_config <- function() {
  f <- user_file("config.csv")
  if (!file.exists(f)) {
    audio_drivers = nrow(audio::audio.drivers())
    df <- data.frame(stringsAsFactors = FALSE,
                     name = "audio",
                     value = audio_drivers > 0)
    write.csv(df, f, row.names = FALSE)
  } else {
    df <- read.csv(f, stringsAsFactors = FALSE)
  }
  split(df$value, df$name)
}

save_config <- function(cfg) {
  df <- data.frame(stringsAsFactors = FALSE,
                   name = names(cfg),
                   value = unlist(unname(cfg)))
  write.csv(df, user_file("config.csv"), row.names = FALSE)
}

################################################################################
