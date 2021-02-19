#' Function to mount S drive
#'
#' When working in idies/crunchr, this function provides a way to mount a specific
#' S drive project.
#' @param project_name string name of project (eg. PMAPMullany)
#' @param mount_location string path to mount this S drive project
#' @param username string username (i.e. jhed id)
#' @param showdir (default = FALSE); if set to TRUE will list dir contents after mounting
#' @return NULL
#' @export
#' @examples
#' mount_S_drive("PMAPMullany", "home/idies/workspace/SAFE/", username="<jhed>")
mount_S_drive <- function(project_name=NULL, mount_location=NULL, username=NULL, showdir=F) {

  if(is.null(project_name)) {
    stop("Project Name must be provided",call. = F)
    return(NULL)
  }
  if(is.null(username)) {
    stop("username must be provided",call. = F)
    return(NULL)
  }
  if(is.null(mount_location)) {
    stop("Mount location must be provided. Eg: '/home/idies/workspace/SAFE/'",call. = F)
    return(NULL)
  }

  #Get password
  password = getPass::getPass(paste0("Enter password for ",username,": "))

  #Check if /home/idies/workspace/SAFE/ exists; if not, create it.
  if(!dir.exists(mount_location)) {
    cat('Creating directory', mount_location, '\n')
    dir.create(mount_location)
  }

  # Create the user string
  dirname = paste0("//cloud.nas.jh.edu/sddesktop$/",project_name)
  userstring = paste0("username=",username,",workgroup=win,uid=idies,password=",password)
  syscmd = paste0(
    "sudo mount -t cifs ",
    dirname,
    paste0(" ", mount_location),
    " -o ",
    userstring
  )
  cat("Mounting project", project_name," at", mount_location, "\n")
  system(syscmd)

  if(showdir) {
    print(dir(mount_location, all.files = T))
  }

  invisible(NULL)

}
