#' Find Image Location in a Given Set of Locations
#'
#' This function determines the location of an image within a set of locations.
#' The function accepts a vector of locations (such as "TL", "TR", "BL", "BR") and an image identifier.
#' It returns the corresponding location name if the image is found, or `NA` if the image is not present or is `NA`.
#'
#' @param locations A character vector representing the possible locations (e.g., `c("TL", "TR", "BL", "BR")`).
#' @param image A character value representing the image to find in the locations.
#'
#' @return A character string representing the location of the image, or `NA` if the image is not found or is missing.
#'
#' @examples
#' # Example usage of the find_location function
#' locations <- c("apple", "banana", "cherry", "date")
#' find_location(locations, "banana")  # Returns "TR" if locations follow c("TL", "TR", "BL", "BR")
#' find_location(locations, "orange")  # Returns NA
#'
#' @export
find_location <- function(locations, image) {
  # Return NA if the image is missing
  if (is.na(image)) {
    return(NA)
  }

  # Define location names corresponding to positions in the 'locations' vector
  loc_names <- c("TL", "TR", "BL", "BR")

  # Find the index of the image in the locations vector
  match_index <- match(image, locations)

  # Return the corresponding location name if the image is found, otherwise return NA
  if (!is.na(match_index)) {
    return(loc_names[match_index])
  } else {
    return(NA)
  }
}
