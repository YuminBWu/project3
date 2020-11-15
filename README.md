install packages

    install.packages(c("dplyr","shiny","rtweet","ggplot2","plotly","shinydashboard","randomForest"),repos="https://cloud.r-project.org") 

    ## Installing packages into 'C:/Users/yumin/Documents/R/win-library/3.5'
    ## (as 'lib' is unspecified)

    ## 
    ##   There are binary versions available but the source versions are later:
    ##          binary source needs_compilation
    ## dplyr     0.8.5  1.0.2              TRUE
    ## shiny   1.4.0.2  1.5.0             FALSE
    ## ggplot2   3.3.0  3.3.2             FALSE
    ## 
    ## package 'rtweet' successfully unpacked and MD5 sums checked
    ## package 'plotly' successfully unpacked and MD5 sums checked
    ## package 'shinydashboard' successfully unpacked and MD5 sums checked
    ## package 'randomForest' successfully unpacked and MD5 sums checked

    ## Warning: cannot remove prior installation of package 'randomForest'

    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\yumin\AppData\Local\Temp\RtmpENqIwT\downloaded_packages

    ## installing the source packages 'dplyr', 'shiny', 'ggplot2'

Run my app

    shiny::runGitHub("project3", "YuminBWu", subdir = "ST558_final")
