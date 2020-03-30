# Blog Explorer

## Analysing 10+ years of R blogs

A Shiny app to browse the results of a topic model trained on 30,000+ blog articles about the statistical programming language R. 

![blog-explorer.gif](https://github.com/nz-stefan/blog-explorer/blob/master/blog-explorer.gif)

## Data

Blog data was scraped from r-bloggers.com from their public archives using the super convenient rvest package. The process is split into three phases to allow me later to change my mind about the particular processing of articles.

1. Obtain a list of links to articles published on R-Bloggers
2. Download each article from the link collection
3. Parse each document and combined them into a single dataframe

For each document meta information was collected such as blog title, publication date, author and URLs to the original blog. In total more than 30,000 articles were collected published as early as Jan 2010.

For the ongoing data refresh the R-Blogger RSS feeds are downloaded every day using a script running in a Docker container on AWS on a daily schedule. The RSS feed is a XML file that contains the latest few articles published on the website. At the end of each month a second script in a second container combines the articles in all collected feeds with the 30,000 scraped articles and refreshes the topic model behind this application. So every month something new should appear in this Shiny app.

## Deployment

The app is deployed through RStudio's webservice [shinyapps.io](https://nz-stefan.shinyapps.io/blog-explorer/). Additionally, the app is published on [RStudio Cloud](https://rstudio.cloud/project/1085074) which provides a complete development environment of the project.

## Setup development environment

The development environment of this project is encapsulated in a Docker container.

1. Install Docker. Follow the instructions on [https://docs.docker.com/install/](https://docs.docker.com/install/)
2. Make docker run without sudo
    ```
    sudo groupadd docker
    sudo usermod -aG docker $USER
    ```
    Log out and log back in so that your group membership is re-evaluated
3. Clone the GIT repository
    ```
    git clone https://github.com/nz-stefan/blog-explorer.git
    ```
4. Setup development Docker container
    ```
    cd blog-explorer
    bin/setup-environment.sh
    ```
    You should see lots of container build messages
5. Spin up the container
    ```
    bin/start_rstudio.sh
    ```
6. Open [http://localhost:8790](http://localhost:8790) in your browser to start a new RStudio session
7. Install R packages required for this app. Type the following instructions into the R session window of RStudio
    ```
    renv::restore()
    ```
    The installation will take a few minutes. The package library will be installed into the `renv/lib` directory of the project path.
8. Open the file `app/global.R` and hit the "Run app" button in the toolbar of the script editor (or type `shiny::runApp("app")` in the R session window). The Shiny app should open in a new window. You may need to instruct your browser to not block popup windows for this URL.

