#' Load CSV Files from a Directory
#'
#' This function loads all CSV files in a specified directory into the R environment.
#' Each CSV file is read into a separate data frame named after the file (without the file extension).
#'
#' @param directory A character string specifying the path to the directory containing the CSV files.
#' 
#' @return The function loads data frames into the environment, each named after its corresponding CSV file (without the extension).
#' 
#' @examples
#' # Example of how to use the function
#' load_csvs("path/to/your/directory")
#' 
#' @export
load_csvs <- function(directory) {
  
  # Check if the directory exists
  if (!dir.exists(directory)) {
    stop("The specified directory does not exist.")
  }
  
  # List all CSV files in the specified directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if there are CSV files in the directory
  if (length(csv_files) == 0) {
    message("No CSV files found in the directory.")
    return(NULL)
  }
  
  # Loop through each CSV file and load it into the environment with the file name (without extension)
  for (file in csv_files) {
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Read the CSV into a data frame and assign it to a variable with the file name
    assign(file_name, read.csv(file), envir = .GlobalEnv)
  }
  
  # Return a message indicating that the files have been loaded
  message("CSV files loaded successfully.")
}