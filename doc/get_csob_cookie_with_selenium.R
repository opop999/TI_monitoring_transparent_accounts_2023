get_csob_cookie <- function(host_port, container_port, address) {
  
# Loading the required R libraries
  
  # Package names
  packages <- c("RSelenium", "data.table")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
# Pull the necessary Docker image
system("docker pull selenium/standalone-firefox:latest")

# Start the Docker container
system(paste0("docker run --rm -d --name selenium_headless -p ",
              host_port,
              ":",
              container_port,
              " -e START_XVFB=false --shm-size='2g' selenium/standalone-firefox:latest"),
       wait = TRUE)
 
# Connect to a Docker instance of headless Firefox server
remDr <- remoteDriver(
  remoteServerAddr = address,
  port = host_port,
  browserName = "firefox",
  extraCapabilities = list("moz:firefoxOptions" = list(
    args = list("--headless")
  ))
)

Sys.sleep(5)

# Request to the server to instantiate browser
remDr$open(silent = TRUE)

# Request to selected url (can be any CSOB page, robots.txt is the most lightweight)
remDr$navigate("https://www.csob.cz/portal/robots.txt")

# Wait before page fully loaded
Sys.sleep(10)

# Save all of the cookies to an object-list
cookies_list <- remDr$getAllCookies()

# Close the headless browser
remDr$close()

# Stop the Docker container
system("docker stop selenium_headless")

# Merge the list to a dataframe
cookies_df <-  rbindlist(cookies_list, fill = TRUE)

print("CSOB cookie:")
# Subset the dataframe to get the cookie
return(cookies_df[name == "TSPD_101", value])

}

# Specify arguments of the function
address <- "localhost"
host_port <- 4445L
container_port <- 4444

# Run the function
get_csob_cookie(host_port = host_port,
                container_port = container_port,
                address = address)
