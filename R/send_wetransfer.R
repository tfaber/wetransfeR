#' Creating a file transfer using the wetransfer open API
#' 
#' @param files_dir Link to the directory where your files are stored
#' @param api_key The api key that you can get from https://developers.wetransfer.com
#' @param name The name of the transfer
#' @param description The description of the transfer (optional)
#' @return The shortened url of the transfer
#' @examples
#' send_wetransfer("path/to/files","somekey","my_first_transfer","some nice files")
#' @export send_wetransfer

send_wetransfer <- function(files_dir, api_key, name, description) {

  path_dir <- files_dir
  transfer_name <- name
  
  if(missing(description)) { transfer_description <- ""
  } else { transfer_description <- description }
  
  your_api_key <- api_key

  file_list <- file.info(list.files(path_dir,full.names = T)) # use full names to prevent NA

  # File checks (with warnings)
  if(any(file_list$isdir == T)) warning("your files contain a directory")
  if(any(file_list$file_size) > 2147483648) warning("the file size of on of the files exceeds the file size limit (2 Gb)")
  if(any(is.na(file_list$size))) warning("It is not possible to retrieve the required file info from the selected files")

  ########################################## Step 1: Authorization

  authorize <- function(your_api_key) {
    we_url <- "https://dev.wetransfer.com/v1/authorize"
    post_auth <- httr::POST(we_url, httr::add_headers("x-api-key" = your_api_key))

    if(post_auth$status_code != 200) stop(paste("The authorization process failed, http response code:", post_auth$status_code))
    return(content(post_auth)[[2]]) # returns JSON Web token (JWT) if succesful which is needed in further steps

  }

  jwt_token <- authorize(your_api_key)

  ########################################## Step 2: Create an empty transfer

  create_transfer <- function(your_api_key, jwt_token, name, description) {
    we_url <- "https://dev.wetransfer.com/v1/transfers"
    post_create <- httr::POST(we_url, httr::add_headers("x-api-key" = your_api_key,
                                                        "Authorization" = paste("Bearer", jwt_token)),
                              accept_json(), # check
                              body = jsonlite::toJSON(list(name = description), pretty=TRUE,auto_unbox = T))

    if(post_create$status_code != 202) stop(paste("Creating an empty transfer failed, http response code:", post_request$status_code))
    return(content(post_create)) # returns transfer id
  }

  empty_transfer_response <- create_transfer(your_api_key, jwt_token, transfer_name, transfer_description)
  transfer_id <- empty_transfer_response$id
  short_url <- empty_transfer_response$shortened_url

  ########################################## Step 3: Adding files to your transfer

  # create a table of files, filenames, identifiers
  files_set <- data.frame(local_identifier = paste("file",1:nrow(file_list)),
                          content_identifier = rep("file",nrow(file_list)),
                          filename = gsub("^.*/","",rownames(file_list)),
                          filesize = file_list$size, stringsAsFactors = FALSE)

  # change the table to JSON format
  files_list <- list()
  files_list$items <- files_set
  files_list <- jsonlite::toJSON(files_list)

  add_files <- function(your_api_key, jwt_token, files_list) {
    we_url <- paste0("https://dev.wetransfer.com/v1/transfers/",transfer_id,"/items")
    post_files <- httr::POST(we_url, httr::add_headers("x-api-key" = your_api_key,
                                                       "Authorization" = paste("Bearer", jwt_token)),
                             accept_json(), # check
                             body = files_list)
    if(post_files$status_code != 202) stop(paste("Adding files to your transfer failed, http response code:", post_files$status_code))
    return(content(post_files)) # returns posted files
  }

  post_files_response <- add_files(your_api_key, jwt_token, files_list) # the problem is the items format: multiple items should be multiple $ list items

  ########################################## Step 4: request an upload URL

  # the wetransfer open API works by splitting larger files into chunks or fileparts which are all uploaded to a specified pre-signed URL
  # in order to request a pre-signed URL you need to specify: /files/{file_id}/uploads/{part_number}/{multipart_upload_id}
  # here, we loop over all files and return pre-signed URLs for the nested fileparts

  get_uploadURL <- function(your_api_key, jwt_token, file_id, part_number, multipart_upload_id) {
    we_url <- paste0("https://dev.wetransfer.com/v1/files/",file_id,"/uploads/",part_number,"/",multipart_upload_id)
    post_request <- httr::GET(we_url, httr::add_headers("x-api-key" = your_api_key,
                                                      "Authorization" = paste("Bearer", jwt_token)))
    if(post_request$status_code != 200) stop(paste("Requesting an upload URL failed, http response code:", post_request$status_code))
    return(content(post_request)) # returns transfer id
  }

  files_count <- length(post_files_response) # number of files part of the upload
  upload_url_list <- list() # create empty list for response upload URLs

  for (i in 1:files_count) { # loop through files

    multipart_count <- post_files_response[[i]]$meta$multipart_parts # how many parts does each file have
    file_id <- post_files_response[[i]]$id
    upload_url_list[[i]] <- list()

    for(j in 1:multipart_count) { # loop through file_parts
      
      Sys.sleep(0.1) # since the api limit allows 1000 requests per day / 5 per second this slows down the transfer by some X amount
      part_number <- j # multiple multiparts per file?
      multipart_upload_id <- post_files_response[[i]]$meta$multipart_upload_id[1]
      upload_URL_response <- get_uploadURL(your_api_key,jwt_token,file_id,part_number,multipart_upload_id)
      upload_url_list[[i]][[j]] <- upload_URL_response$upload_url

    }
  }

  ########################################## Step 5: Actual file upload
  
  # Files are read in chunks using readBin()

  file_upload <- function(we_url, path_to_file) {
    upload_files <- httr::PUT(we_url, body = path_to_file)

    if(upload_files$status_code != 200) stop(paste("Something went wrong during the file upload process for the file:",path_to_file,"http response code:",upload_files$status_code))
    return(content(upload_files)) # returns transfer id
  }

  for(i in 1:files_count) { # loop through files

    multipart_count <- length(upload_url_list[[i]])
    conBinFile <- file(description = rownames(file_list)[i], open = "rb")
    
    for(j in 1:multipart_count) { # loop through file_parts
      
      # read files in chunks of 6 MB each; last multipart may be bigger/smaller
      
      if(j > 1 & j == max(multipart_count)) {
        part_size = 20000000L
      } else {
        part_size = 6000000L
      }
      
      we_url <- upload_url_list[[i]][[j]]
      path_to_file = readBin(conBinFile,"raw", size = NA_integer_,n = part_size)
      file_upload(we_url,path_to_file)
    }
    
    close(conBinFile)
    
  }

  ########################################## Step 6: Complete the upload

  complete_upload <- function(your_api_key, jwt_token, file_id) {
    we_url <- paste0("https://dev.wetransfer.com/v1/files/",file_id,"/uploads/complete")
    upload_complete <- httr::POST(we_url, httr::add_headers("x-api-key" = your_api_key,
                                                            "Authorization" = paste("Bearer", jwt_token)))
    if(upload_complete$status_code != 202) stop(paste("Something went wrong when completing the file upload process for the file:",file_id,"http response code:",upload_complete$status_code))
    return(content(upload_complete)) # returns transfer id
  }

  for(i in 1:files_count) {

    file_id <- post_files_response[[i]]$id
    complete_upload(your_api_key,jwt_token,file_id)

  }

  return(short_url) # returns the shortened url

}


