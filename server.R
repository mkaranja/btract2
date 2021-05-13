
source("tabs/server/overview_server.R", local=T)
source("tabs/server/data_server.R", local=T)
# source("tabs/server/status_server.R", local=T)
source("tabs/server/tc_server.R", local=T)
source("tabs/server/labels_server.R", local=T)


# Define server logic 
shinyServer(
  function(input, output, session) {
  
    env_serv = environment()
  
    overviewserver(env_serv)
    dataserver(env_serv)
    source("tabs/server/status.R", local=T)
    tc_server(env_serv)
    labels_server(env_serv)
  
})
