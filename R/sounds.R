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
#' @export
#'

load_sound <- function(wav) {
  list(wav = audio::load.wave(wav),
       player = NULL)
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
  wavobj$player <- audio::play(wavobj$wav)
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
  if (!is.null(wavobj$player)) {
    audio::pause(wavobj$player)
    wavobj$player <- NULL
  }
  wavobj
}
