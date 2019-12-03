# Secret Santa Algorithm

# Define Participant list
Participants <- c("Coworker1@work.com",
                  "Coworker2@work.com",
                  "Coworker3@work.com")

# Defining Function
SecretSanta <- function(names){

  # random assignment
  SantaDF <- cbind.data.frame(Gifter = names, Giftee = sample(names))

  # reshuffle if somebody has to gift themselves
  while (sum(SantaDF[,1] == SantaDF[,2]) > 0) {

    SantaDF <- cbind.data.frame(Gifter = names, Giftee = sample(names))

  }

  # return data frame
  return(SantaDF)

}

# Apply Function
SantaDF <- SecretSanta(Participants)


# COOL BONUS: Plot as network

# loading library
library(igraph)

# converting to graph object
graph <- graph_from_data_frame(SantaDF, directed = TRUE)

# plotting
plot(graph)





# COOL BONUS: Sending Secret Santa invites via mail

# install packages (Java needs to be installed and findable for R)
if ("mailR" %in% installed.packages() == FALSE) {
  install.packages("mailR")
}

# attach packages
library(mailR)


# Email Credentials: You need to allow Google the login with 3rd party apps
#   -> https://myaccount.google.com/u/1/lesssecureapps?pageId=none

# Enter the gmail address that you want to send the secret Santa invitations from
sender <- "YourEmail@gmail.com"

# Enter your Gmail password (DO NOT EVER SHARE THIS)
pwd <- "YOUR PASSWORD"


# Sending Emails
for (i in 1:dim(SantaDF)[2]){

  recipients <- as.character(SantaDF[i,1])

  send.mail(from = sender,
            to = recipients,
            subject = "Secret Santa",
            body = paste("HoHoHo, you are getting a gift for:",SantaDF[i,2]),
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = sender,
                        passwd = pwd, ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
}


