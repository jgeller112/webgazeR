#' Find Image Location in a Given Set of Locations
#'
#' @param locations A character vector of image names at each location.
#' @param image A character string: the image to locate.#'
#' @return A character string representing the location name or NA if not found.
#'
find_location <- function(locations, image) {
  if (is.na(image)) return(NA_character_)

  loc_names <- names(locations)

  if (is.null(loc_names)) {
    stop("locations must be a named vector or loc_names must be provided.")
  }

  match_index <- match(image, locations)

  if (!is.na(match_index)) {
    return(loc_names[match_index])
  } else {
    return(NA_character_)
  }
}
