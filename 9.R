#-----------------DATA SCRAPING, CLEANING AND MANIPULATION-----------------

#install.packages(rvest)

# importing the rvest package
library(rvest)

# specify URL of the website to be scraped
base_url<-"https://www.jobstreet.com.my/jobs?page="

# initialize empty vectors to store job-related data
job_titles_list<- c() 
company_names_list<-c()
company_areas_list<-c()
salary_range_list<-c()
specialization_list<-c()
job_type_list<-c()


# using CSS selectors to scrape the web page
# extracting data from the website using html_nodes and html_text
# selecting page 6 to 7 to get 60 data objects, 30 per page
for (page in 2:3) {
  url <- paste0(base_url, page)
  
  # reading the HTML code from the website
  webpage <- read_html(url)
  print(url)
  path <- '/html/body/div[1]/div/div[5]/div/section/div[2]/div/div/div/div/div/div[1]/div/div/div[1]/div/div[3]/div/div/div'
  # Extract only the side of webpage which contains job cards
  side <- html_nodes(webpage, xpath=path)
  # Extract the job cards from the side section of the HTML code containing job information
  cards <- html_nodes(side,"._1wkzzau0 ._1wkzzau1")
  
  # Loop through each job entry in cards
  for (i in 1:length(cards)){
    
    # first variable to be scraped -> Job Titles data
    job_titles_node <- html_nodes(cards[i],".uo6mkd")
    # converting the job titles data to text
    if (length(job_titles_node)>0){
      job_titles <- html_text(job_titles_node)
    }
    # if job_titles is empty, then put as NA
    else{
      job_titles <- "NA"
    }
    
    # second variable to be scraped -> Company Names data
    company_names_node <- html_nodes(cards[i], "._842p0a1")
    if (length(company_names_node)>0){
      company_names <- html_text(company_names_node)
    }
    else{
      company_names <- "NA"
    }
    
    # third variable to be scraped -> Company Area/Location data
    company_areas_node <- html_nodes(cards[i],xpath="div[3]/div/div/div[2]/div/div[2]/div[1]/span/a[1]")
    
    
    # using a function to execute data pre-processing for company areas to remove any character symbols and take only the first location 
    convert_company_areas <- function(company_areas){
      company_areas <- gsub("/.*","",company_areas)
      company_areas <- gsub("-.*","",company_areas)
      company_areas <- gsub(",.*","",company_areas)
    }
    
    if (length(company_areas_node)>0){
      company_areas <- html_text(company_areas_node)
      company_areas <- lapply(company_areas, convert_company_areas)
      company_areas <- t(matrix(unlist(company_areas), ncol = length(company_areas), byrow = TRUE))
    }
    else{
      company_areas <- "NA"
    }
    
    # fourth variable to be scraped -> Salary data
    salary_range_node <-  html_nodes(cards[i],"._16v7pfz3")
    
    # converting the salary range into numeric values
    convert_salary_range <- function(salary_range) {
      # removing all characters from the string (e.g., "MYR 3.5K - 5,000 >>> 3.5K - 5000")
      numeric_values <- gsub("[^0-9K.–]+", "", salary_range)
      # converting the data into an array (e.g., "3.5K - 5000" >> ["3.5K", "5000"])
      numeric_values <- strsplit(numeric_values, "–")[[1]]
      # converting the values into numeric (e.g., ["3.5K", "5000"] >> [3500, 5000])
      numeric_values <- as.numeric(gsub("([0-9.]+)K", "\\1e3", numeric_values))
      # get the median salary
      salary_mean <- mean(numeric_values)
    }
    
    if (length(salary_range_node)>0){
      salary_range <- html_text(salary_range_node)
      salary_range <- lapply(salary_range, convert_salary_range)
    }
    
    else{
      salary_range <- "NA"
    }
    
    # fifth variable to be scraped -> Job Specialization data
    job_node <- html_nodes(cards[i],".szurmz2j")
    specialization_node <- html_nodes(job_node,xpath="div[2]")
    if (length(specialization_node)>0){
      specialization <- html_text(specialization_node)
      print(specialization)
    }
    else{
      specialization <- "NA"
    }
    
    # sixth variable to be scraped -> Job Type data
    job_type_node <- html_nodes(job_node,xpath="div[5]")
    if (length(job_type_node)>0){
      job_type <- html_text(job_type_node)
    }
    else{
      job_type <- "NA"
    }
    
    # append scraped information to the existing list
    job_titles_list<-c(job_titles_list,job_titles)
    company_names_list<-c(company_names_list,company_names)
    company_areas_list<-c(company_areas_list,company_areas)
    salary_range_list<-c(salary_range_list,salary_range)
    specialization_list<-c(specialization_list,specialization)
    job_type_list<-c(job_type_list,job_type)
  }
  
}

# check for missing data
is.na(job_titles_list)
is.na(company_names_list)
is.na(company_areas_list)
is.na(salary_range_list)
is.na(specialization_list)
is.na(job_type_list)

# print the lists
print(job_titles_list)
print(company_names_list)
print(company_areas_list)
print(salary_range_list)
print(specialization_list)
print(job_type_list)

# combining all the lists to form a data frame
jobs_df<-data.frame(Job_Title = job_titles_list, 
                    Company = company_names_list,
                    Location = sapply(company_areas_list, '[', 1), 
                    Salary = sapply(salary_range_list, '[', 1),
                    Specialization = specialization_list, 
                    Type = job_type_list)

# printing the structure of the data frame
print(str(jobs_df))

# saving the data frame into a csv file 
write.csv(jobs_df, "/Users/user/Desktop/JobStreet.csv", row.names=TRUE)

#-------------------------DATA VISUALIZATION-------------------------

# to visualize data
library(ggplot2)
library(dplyr)
library(viridisLite)



# creating a new data frame for plotting 
plot3_data <- jobs_df %>%
  select(Location) %>%
  filter(!grepl("NA", Location, fixed = TRUE))

# printing the data frame for the third plot
print(plot3_data)

# plotting the grouped bar chart showing the number of jobs per location
plot3 <- ggplot(plot3_data, aes(y = Location, fill = Location)) + 
  geom_bar(color = "gray") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate the x-axis labels for better readability
  labs(title = "Number of Jobs per Location", x = "Number of Jobs", y = "Location") +
  theme_minimal() +
  scale_fill_manual(values = viridis(length(unique(plot3_data$Location)), option = 'B'))

# printing the third plot to the console
print(plot3)

# saving the third plot as a PNG file
ggsave("Number of Jobs per Location.png", plot = plot3, device = "png")



