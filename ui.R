
shinyUI(
  
 navbarPage("",
            id = "inTabset",  
            theme = shinythemes::shinytheme("readable"), 
            fluid = T,
            windowTitle = "BTracT",
                         
            frontpage,
            overviewPage,
            dataPage,
            statusp,
            labels,
            about
        )
)
