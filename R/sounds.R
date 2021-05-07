#' Prepare a sound for playing
#'
#' A convenience wrapper that calls audio::load.wave to load
#' a WAV file, returning a list of the sample, and a placeholder for the
#' player instance.
#'
#' @param wav Path to a wave file to load.
#' @keywords sound
#' @return A list of two elements, `wav` is the audioSample for playing, and
#'         `player` is initialised as NULL, but will later keep track of the
#'         audioInstance used to play the sample.

load_sound <- function(wav, config) {
  if (config$audio) {
    list(wav = audio::load.wave(wav), player = NULL, enable = TRUE)
  } else {
    list(enable = FALSE)
  }
}

#' Play a sound.
#'
#' Starts playing a sound, setting the player instance if it was not already
#' set.
#'
#' @param wavobj The list returned by `load_sound` when the sample is loaded.
#' @keywords sound
#' @return The updated list, in case the audio instance is no longer null.

play_sound <- function(wavobj) {
  if (wavobj$enable) {
    wavobj$player <- audio::play(wavobj$wav)
  }
  wavobj
}

#' Stop playing a sound.
#'
#' Stops playing, by pausing and rewinding the audio instance, if it
#' exists
#' @param wavobj The list returned by `load_sound` when the sample is loaded.
#' @return The updated list, as the audio instance will be NULL afterwards.
#' @keywords sound

stop_sound <- function(wavobj) {
  if (wavobj$enable) {
    if (!is.null(wavobj$player)) {
      audio::pause(wavobj$player)
      wavobj$player <- NULL
    }
  }
  wavobj
}

check_sound_driver <- function(config) {
  nd <- nrow(audio::audio.drivers())
  if ((nd == 0) & (config$audio != FALSE)) {
    config$audio <- FALSE
  }
  config
}

check_sound_card <- function() {

  # Try to play a sound. Not sure if there's another way to see if an
  # actual sound card is there. Have to do this as an external, to
  # prevent text going everywhere...

  rscript <- as.character(Sys.which("Rscript"))
  args <- paste('-e "',
    "fake_conf <- list(audio = TRUE);",
    "status <- 'NOT OK';",
    "wav <- cactus.purge:::load_sound(cactus.purge:::pkg_file('audio/empty.wav'), fake_conf);",
    "try({audio::play(wav$wav); status <- 'OK'}, silent = TRUE);",
    'message(status); "', sep = " ")

  system2(rscript, args, stdout = TRUE, stderr = TRUE) == "OK"
}

