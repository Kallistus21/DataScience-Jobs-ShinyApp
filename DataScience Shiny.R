Sys.setenv(LANG = "en")
library(rvest)
library(RSelenium)
library(purrr)
library(dplyr)
library(stringr)
library(shiny)
library(shinythemes)
library(xlsx)
#Start with opening the browser using RSelenium
#rD <- rsDriver(browser = "chrome", verbose = TRUE,port=sample(1000:9999,1))
rD <- rsDriver(browser = "firefox", verbose = TRUE,chromever=NULL,port=sample(1000:9999,1))
# The rsDriver function is only run once per session because it "reserves" a specific port.
# The reason for using `sample()` to choose a port is to avoid common errors, especially for 
# less tech-savvy users who might be unfamiliar with the process. For example, you could encounter 
# an error like this:
# 
# Error in wdman::selenium(port = port, verbose = verbose, version = version, : 
#   Selenium server signals port = 4567 is already in use.
#
# Essentially, the rsDriver function "reserves" the chosen port, and it can cause confusion 
# for beginners who are new to Selenium, as I experienced when I first started.
#
# While this solution isn't the most elegant, it is simple and effective. 
# Technically, there is a very small chance that the same port number could be randomly chosen 
# within the range of 1000 to 9999. However, if this happens, simply rerun the function since 
# the likelihood of the same port being selected again is extremely low.

# After several attempts, I found that Chrome does not work properly with this setup.
# Additionally, Firefox requires the 'chromever=NULL' argument to function correctly in this context.
# Therefore, we will proceed with using Firefox for our scraping task.
# Feel free to run the code above the line with browser="chrome" if Google Chrome is preferred
# but note that I cannot confirm if it will work. 

remDr <- rD[["client"]]
remDr$open()
# We will scrape job listings from justjoin.it. According to their robots.txt file,
# scraping is permitted on their site.
#remDr$navigate("http://justjoin.it/warszawa")
remDr$navigate("http://justjoin.it/warszawa/python")

# For this example, we will scrape job listings from the '/python/' section of justjoin.it,
# as it contains a smaller number of offers (approximately 100-200), which allows for a faster demonstration 
# and quicker data processing.
# If you'd like to scrape a different section, you can modify the URL in remDr$navigate 
# by removing '/python' or replacing it with another category by going to the site
# and checking the URL link



remDr$setWindowSize(1920, 1080)  # 1920x1080 is a very common screen resolution, 
# but you can set your own (e.g., 2560x1440) if needed.
# Unlike in Python, R's RSelenium doesn't have a WindowMaximize() function. 
# Therefore, we need to manually set the window size to a desired resolution.
# In the next steps, we will also handle cookie acceptance (depending on the website's behavior).

Sys.sleep(2)  # Wait for the website to load fully, as the cookie consent popup 
# may not appear immediately after page load.
# The 2-second delay is an approximation. 
# You can adjust this based on your connection speed or use a more dynamic wait
# By changing (2) to any other number, representing seconds.
# We will now find and click the 'Accept Cookies' button on the page:
accept_cookies <- remDr$findElement(using = "xpath", value = '//*[@id="cookiescript_accept"]') 
accept_cookies$clickElement()  # Clicks the 'Accept Cookies' button to dismiss the consent popup

# We are using both rvest and Selenium here for web scraping.
# Selenium is useful for interacting with dynamic elements (like the cookies popup), but we need rvest 
# to efficiently extract the static content once the page is fully loaded.
# Without rvest, Selenium alone wouldn't allow us to scrape the data efficiently.

# An empty data frame that will collect scraped data
job_data <- data.frame()

# A function that scrapes data from a page and scrolls down to load more content
# This function performs a scroll-loop to gather multiple job listings by continuously scrolling down.
# This is necessary due to the dynamic structure of the site.
scrape_and_scroll <- function(remDr, num_scrolls = 10, scroll_amount = 500) {
  for (i in 1:num_scrolls) {
    # Scroll down by the specified amount (in pixels). This simulates the user scrolling to load more jobs.
    remDr$executeScript(paste0("window.scrollBy(0, ", scroll_amount, ");"))#Rselenium will scroll by the amount we provide
    Sys.sleep(2) # Wait for content to load after scrolling down (adjust if the page loads slowly)
    
    # Get the page source to extract the updated HTML content after scrolling
    page_source <- remDr$getPageSource()[[1]]
    
    # Parse the page source using rvest's read_html function
    page <- read_html(page_source)
    
    #parse the data using rvest
    #NOW, the problem with those is that css selectors can change over time
    #therefore we have to make it dynamic
    #sadly i could only find a way to get the dynamic locator for titles and links.
    #it was not easy for salaries, companies or skills
    # Extract the job titles using the corresponding CSS selector.
    # 'h3' is commonly used for job titles allowing us to easily locate job titles
    titles<-page %>% html_nodes("h3") %>% html_text()
    # Extract job links by selecting anchor tags that start with "/job-offer/"
    # These links lead to individual job postings.
    links<-page %>% html_nodes('a[href^="/job-offer/"]')%>%html_attr("href")
    # NOW, the problem with these selectors is that CSS selectors can change over time
    # Therefore, we need to make the locators dynamic and adaptable to potential changes in the site's structure.
    # While I was able to easily identify dynamic locators for titles and links, 
    # it was more challenging to find reliable, dynamic selectors for the following elements:
    
    # - Salaries: The CSS selectors for salaries may vary and be less consistent, 
    #   so it was difficult to find a stable pattern for extracting salary data.
    # - Companies: Company information is often embedded in dynamic elements that change their structure, 
    #   making it tricky to reliably extract this data.
    # - Skills: The dynamic nature of skill data made it harder 
    # to identify stable locators for extracting this information.
    
    # In practice, we need to be prepared to update these selectors periodically 
    # to identify the elements reliably as the website changes.
    
    # Extract company names by selecting the CSS selector containing the company information.
    companies <- page %>% html_nodes("div.MuiBox-root.css-1mx97sn") %>% html_text()
    
    # Extract the required skills for each job listing.
    # The key difference here is the use of html_text2() instead of html_text().
    # While html_text() would return a single string (e.g., [1] SQLPythonAI),
    # html_text2() separates each skill by a newline character, resulting in a more structured format (e.g., [1] SQL\nPython\n).
    # This makes it much easier to later split the skills into individual columns.
    # For example:
    #   html_text() produces:
    #     [1] SQLPythonAI
    #   html_text2() produces:
    #     [1] SQL\nPython\nAI
    # This structure will come in handy when we need to process and organize the skills data later.
    skills <- page %>% html_nodes("div.MuiBox-root.css-vzlxkq") %>% html_text2()
    # Extract salary information for each job posting
    salaries <- page %>% html_nodes("div.MuiBox-root.css-18ypp16") %>% html_text()
    
    # Combine all the extracted data into a single data frame for this scroll
    new_data <- data.frame(
      Title = titles,
      Link = links,
      Company = companies,
      Skills = skills,
      Salary = salaries,
      stringsAsFactors = FALSE
    )
    
    # Append the newly scraped data (new_data) to the main data frame (job_data). 
    # The '<<-' operator is used to modify the global 'job_data' variable, ensuring that
    # the data is saved even after the loop completes. This is necessary because
    # job_data is defined outside of the function.
    job_data <<- bind_rows(job_data, new_data)
    
    # To track the progress of the scraping loop and show how many scrolls have been completed:
    message(paste("Completed scroll ", i, "of", num_scrolls))
  }
}

# The scraping process isn't perfect. The program doesn't automatically stop when it reaches the last job offer.
# Instead, it keeps scrolling until no more new content is loaded, and then continues scraping the same job offers.
# To handle this, we implemented a "brute-force" solution: 
# After scraping, we check if the last job offer (which is visible in the browser after the scroll) is already in the data frame.
# If it is, we remove the duplicates, ensuring that only unique job offers are retained.

# Run the scraping function, setting the number of scrolls and the scroll amount (in pixels)
# In here, for presentation purposes, our scraper scrolls 5 times, 500 pixels each time
scrape_and_scroll(remDr, num_scrolls = 5, scroll_amount = 500) 

# Display the top rows of the collected job data to check the results
head(job_data)

# Great, it's scrapped, now we have to clean it

# Clean the scraped data to remove duplicates and refine the structure
# (this is the bruteforce solution)
job_data<-distinct(job_data)

View(job_data)

#There are still three columns we have to take care of:
#1. Salary and Skills, we have to separate them 
#2. Links, we have to add a prefix so that we can create a full link if we wanted to copy paste it

######Skills:######
head(job_data$Skills)
#We split them using separator \n (html_text2() will easily allow us to do this
# using \n as a pattern)



split_skills <- str_split_fixed(job_data$Skills, "\n", 10)
# The number 10 is chosen as a defensive programming measure for future-proofing. 
# Currently, JustJoin.it displays up to 3 skills, but when accessing the offer via link, more skills are shown. 
# The code is set to handle up to 10 skills, anticipating potential changes in the platform's behavior.
# The code will remove any columns that are entirely blank. 
# Columns are initially named "V1", "V2", etc., due to the use of str_split_fixed, 
# so we rename them for clarity (e.g., "Skill 1", "Skill 2", etc.).
df_split_skills<-as.data.frame((str_split_fixed(job_data$Skills, "\n", 10)))
#I had to add as.data.frame as the loop below didn't work on matrix (which str_split_fixed makes)
for(i in 1:ncol(df_split_skills)){
  names(df_split_skills)[i]<-paste0("Skill ",i)
}

df_split_skills<-df_split_skills[ # Overwrite the current results by keeping only columns where at least one value is non-empty.
  ,
  # The condition colSums(df_split_skills != "") > 0 ensures that columns with all blank values are removed.
  colSums(df_split_skills!="")>0]
# IF AT LEAST one is NOT empty, that column is not removed, otherwise it is.
View(df_split_skills)

#####Salaries#####
head(job_data$Salary)
# Two issues to address:
# 1. The salary range is separated by a "-"
# 2. The currency "PLN" is included, but we need to remove it to convert the values into numeric form
df_split_salaries <- str_split_fixed(job_data$Salary, "-", 2)

# No need for defensive programming here. The salary data will either contain:
# 1. A salary range (min-max), as is currently the case.
# 2. A single salary value (e.g., "5000"), in which case both min and max would be the same value.
# 3. Undisclosed, which is the same as above.

# Convert the result into a data frame for further processing.
df_split_salaries<-as.data.frame(df_split_salaries,stringsAsFactors = FALSE)
View(df_split_salaries)

# Remove the "PLN" currency symbol (case-insensitive) to prepare the values for numeric conversion.
# The scraping process may return "PLN" or "pln", so we use a case-insensitive regex to handle both.

df_split_salaries<- df_split_salaries %>%
  mutate_all(~ str_replace(., "(?i)PLN", ""))


head(df_split_salaries)
# Now, in order to not leave any rows blank, we will do a not-so-perfect solution
# Undisclosed salary has no range, therefore we will paste undisclosed salary into
# both min and max salary if in that same row 
# undisclosed salary already appears in either min or max salary
fill_undisclosed<-apply(df_split_salaries#dataframe
                        ,1 #  1 - rows, 2 - columns
                        ,function(row){# a so called anonymous function, FUN. WE work on rows here so (row)
                          any(row=="Undisclosed Salary")}) # here, we see if any row has an undisclosed salary)
df_split_salaries[fill_undisclosed,]<-"Undisclosed Salary"
head(df_split_salaries)
# It works, now lets rename those V1 and V2 columns to Min and Max Salaries
df_split_salaries<-df_split_salaries%>%rename("Min_Salary"="V1","Max_Salary"="V2")
# Almost done, let's also convert salaries to numeric and get undisclosed salary to NA
df_split_salaries <- df_split_salaries %>%
  mutate(
    Min_Salary = as.numeric(gsub("[^0-9]", "", Min_Salary)),
    Max_Salary = as.numeric(gsub("[^0-9]", "", Max_Salary))
  )
head(df_split_salaries)

#Perfect, now we will add a justjoin.it prefix to our Links
#but since html is structured in such a way 
#that there is no justjoin.it in the html, we will add it using stringr

#add justjoin.it to every row in all_data
job_data$Link<-str_c("justjoin.it",job_data$Link,sep="")
print(job_data$Link)
# good. Now let's join those with our scraped dataframe and get rid of 
# unsplit salaries and skills

job_data<-job_data%>%
  select(.,-Salary,-Skills)%>% #we get rid of salary and skills since we're adding the split, cleaned versions
  bind_cols(df_split_salaries,df_split_skills)%>%
  #we use relocate so that they don't end up at the end of the dataframe
  relocate(starts_with("Skill"), .after = Company)%>% #we dont know how many Skills we will end up with
  #that's why we have a starts_with() argument
  relocate(contains("Salary"),.after=Company) #we have min and max at the start, therefore contains()

#we also relocate Link to be at the end of the dataframe
job_data<-job_data%>%
  relocate(Link,.after=last_col())
View(job_data)


#Let's now make an S3 object "job"

k<-list(Title="Data Scientist",Company="Allegro",Min_Salary=10,Max_Salary=100,Link="link",`Skill 1`="SQL",`Skill 2`="Python",`Skill3`="R")
class(k)<-"job"
#sadly, we didnt manage to print dynamic amount of skills
#so, change the amount of skills (like, add `Skill 4` if 4 skills or remove `Skill3` if only 2 skills)
#this is not an ideal solution but it's better than nothing and "if it works it works"
job <- function(Title, Company, Min_Salary, Max_Salary, Link,`Skill 1` = NULL, `Skill 2` = NULL, `Skill 3` = NULL ) {
  stopifnot(is.character(Title))
  stopifnot(is.character(Company))
  stopifnot(is.numeric(Min_Salary))
  stopifnot(is.numeric(Max_Salary))
  stopifnot(is.character(Link))
  
  
  new_object <- list(Title = Title,
                     Company = Company,
                     Min_Salary = Min_Salary, 
                     Max_Salary = Max_Salary,
                     `Skill 1` = `Skill 1`,
                     `Skill 2`=`Skill 2`,
                     `Skill 3`=`Skill 3`,
                     Link=Link)
  
  
  new_object <- structure(new_object, class = "job")
  
  return(new_object)
}


#make a custom print.job function to enable easier verification of jobs later

# A couple of words for the print.job function below.
# At first, I had my cat() function look something like this:
# cat("The position is called", x$Title, "at a Company", x$Company, "\n")
# which would give us an output of (for example):
# "The position is called Software Engineering, Site Reliability Engineering BS/MS Intern, 2025 at a Company Google"
# This looks definitely clean and neat but interferes with the export of the results to Excel at the end
# Therefore, I had to change it so that a job class when using print.job would only print the 
# result, for example company would be only "Google" - boring, but essential.
print.job <- function(x) {
  # Print the position and company
  cat(x$Title, "\n")
  cat(x$Company, "\n")
  # Check if both Min_Salary and Max_Salary are NA
  if (is.na(x$Min_Salary) & is.na(x$Max_Salary)) {
    cat("Not Available", "\n")
    cat("Not Available", "\n")
  } else {
    # As a defensive programming, if for some weird reason only one of the salaries, either max or min are 
    #NA, then:
    if (is.na(x$Min_Salary)) {
      cat("Not Available", "\n")
      cat(x$Max_Salary, "\n")
    } else if (is.na(x$Max_Salary)) {
      cat(x$Min_Salary, "\n")
      cat("Not Available", "\n")
    } else {
      #if both are available
      cat(x$Min_Salary, "\n")
      cat(x$Max_Salary, "\n")
    }
  }
  
  # If there are skills, print skills (Optional)
  if (is.null(x$`Skill 1`)||x$`Skill 1`=="") {
    cat("N/A", "\n")
  } else {
    cat(x$`Skill 1`, "\n")
  }
  if (is.null(x$`Skill 2`)||x$`Skill 2`=="") {
    cat("N/A", "\n")
  } else {
    cat(x$`Skill 2`, "\n")
  }
  if (is.null(x$`Skill 3`)||x$`Skill 3`=="") {
    cat("N/A", "\n")
  } else {
    cat(x$`Skill 3`, "\n")
  }
  
  
  # also print the link
  cat(x$Link, "\n")
}

#SHINY.
# The way our Shiny dashboard works is composed of several functions:
#1. A salary slider with a checkbox if we want to use it
#2. A company name filter
#3. A title filter
#4. A skill filter 
#5. A button that saves the result of our filters to a list of our s3 object job
#6. A checkbox of undisclodes(NA) values - if checked it will show jobs with no salaries given

# define UI
ui <- fluidPage(
  theme=shinytheme("cyborg"),
  titlePanel("Justjoin.it job offers"), 
  
  sidebarLayout(
    sidebarPanel(
      textInput("companyFilter", "Filter by Company Name:", ""), #company filter
      textInput("titleFilter", "Filter by Job title:", ""), #title filter
      textInput("skillFilter", "Filter by Skill:", ""), #skill filter
      checkboxInput("showUndisclosed", "Show Undisclosed Salaries", TRUE), 
      checkboxInput("applySalaryRange", "Filter by Salary Range", FALSE),
      uiOutput("salarySlider"), #  salary slider
      actionButton("saveJobs", "Save Filtered Jobs"),  # the button to save filtered jobs
      textOutput("saveStatus")  # text output we want to be shown when clicking the "save" button
    ),
    
    mainPanel(
      DT::DTOutput("jobTable")
    )
  )
)

# server function
server <- function(input, output, session) {
  
  # the reactive expression to filter data based on input
  filtered_data <- reactive({
    data <- job_data
    
    # filter by title
    if (input$titleFilter != "") {
      data <- data %>%
        filter(grepl(input$titleFilter, Title, 
                     ignore.case = TRUE))#allows for case insentivity
    }
    # filter by company name
    if (input$companyFilter != "") {
      data <- data %>%
        filter(grepl(input$companyFilter, Company, 
                     ignore.case = TRUE))#allows for case insentivity
    }
    
    # filter by skill
    if (input$skillFilter != "") {
      skill_columns <- grep("^Skill", names(data), value = TRUE)
      skill_regex <- paste0(".*", input$skillFilter, ".*")  #".*" this allows partial matching
      #without this we would not be able to, for example, type "SQ" and get results with SQL
      data <- data %>%
        filter(
          rowSums(sapply(skill_columns, function(col) grepl(skill_regex, data[[col]], ignore.case = TRUE))) > 0
        )
    }
    
    # filter out undisclosed salaries(NAs) if checkbox is unchecked
    if (!input$showUndisclosed) {
      data <- data %>%
        filter(!is.na(Min_Salary) & !is.na(Max_Salary))
    }
    
    # apply salary filter if checkbox is checked
    if (input$applySalaryRange) {
      data <- data %>%
        filter(
          (is.na(Min_Salary) & is.na(Max_Salary)) |  #this one line fixes the "both undisclosed salary and salary slider checked at the same time" problem.
            (Min_Salary >= input$salaryRange[1] & Max_Salary <= input$salaryRange[2])
        )
    }
    
    data
  })
  
  # salary slider based on the checkbox
  output$salarySlider <- renderUI({
    data <- job_data %>% filter(!is.na(Min_Salary) & !is.na(Max_Salary)) #I
    
    if (input$applySalaryRange && nrow(data) > 0) {
      sliderInput("salaryRange", "Select Salary Range:", 
                  min = min(data$Min_Salary, na.rm = TRUE), 
                  max = max(data$Max_Salary, na.rm = TRUE), 
                  value = c(min(data$Min_Salary, na.rm = TRUE), max(data$Max_Salary, na.rm = TRUE)),
                  step = 1000,
                  sep = "") #makes the slider choose only the values available in the dataset. so no 0 to, let's say, infinity
      
    }
  })
  
  # render our table
  output$jobTable <- DT::renderDT({
    filtered_data()
  }, escape = FALSE) 
  # a list to store job objects
  saved_jobs <- reactiveVal(list())
  # the clickable button to save jobs
  observeEvent(input$saveJobs, {
    data <- filtered_data()
    
    # convert each row to a job object and store it in a list using map from purrr package
    jobs <<- map(1:nrow(data), function(i) { # double arrow <<- allows us to get the results "out of" shiny
      #nothing would have been saved to jobs if we had just <-
      row <- data[i, ]
      job(
        Title = row$Title,
        Company = row$Company,
        Min_Salary = row$Min_Salary,
        Max_Salary = row$Max_Salary,
        `Skill 1` = row$`Skill 1`,
        `Skill 2` = row$`Skill 2`,
        `Skill 3` = row$`Skill 3`,
        Link = row$Link
      )
    })
    
    # update the saved jobs list
    saved_jobs(jobs)
    
    # display a message of successful save
    output$saveStatus <- renderText("Jobs saved successfully to ''jobs'', use print(jobs)!")
  })
}

# run shiny
shinyApp(ui = ui, server = server)

jobs[1:5]
#if we want a specific job, use print.job
print.job(jobs[[1]])

#Convert the list of lists into a dataframe
Final_Data_Frame<-as.data.frame(do.call(rbind,jobs))
#export the dataframe into an .xlsx
write.xlsx(Final_data_Frame, "Scraped Jobs.xlsx")

#problems:
#1. we couldn't get remote css working (roundabout solution: justjoin.it/warsawa/remote_yes)
#2. we couldn't get locations working  (roundabout solution: https://justjoin.it/warszawa, but if we care about other cities then it's a problem)
#3. in the s3 job class we couldnt get the dynamic skill function working (we have to put the amount of skills by ourselves)

