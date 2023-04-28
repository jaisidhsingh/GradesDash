library(shiny)
library(hash)
library(bslib)

find_rows_with_value <- function(value, df) {
  row_names <- df$course
  matching_rows <- row_names[apply(df, 1, function(x) any(x == value))]
  return(matching_rows)
}

creds <- read.csv("data/CREDITS.csv")
sems <- read.csv("data/SEMS.csv")
cwng <- read.csv("data/COURSEWISE_num_grades.csv")
cwlg <- read.csv("data/COURSEWISE_letter_grades.csv")
cwgc <- read.csv("data/COURSEWISE_grade_count.csv")

# print(find_rows_with_value("sem4", sems))

grade_levels <- c("A", "A-", "B", "B-", "C", "C-", "D", "E", "F")
grade_names <- list("F", "E", "D", "C-", "C", "B-", "B", "A-", "A")
grade_values <- list(0, 0, 4, 5, 6, 7, 8, 9, 10)
raw_grade_names <- list("A", "A.", "B", "B.", "C", "C.", "D", "E", "F")

course_names <- names(cwlg)
new_names <- names(cwlg)

for (i in 1:length(course_names)) {
  new_names[[i]] <- gsub("\\.", " ", course_names[[i]])
}

X_idx <- grep("X", new_names)
new_names <- new_names[-X_idx]
ids_idx <- grep("ids", new_names)
new_names <- new_names[-ids_idx]

X_idx <- grep("X", course_names)
course_names <- course_names[-X_idx]
ids_idx <- grep("ids", course_names)
course_names <- course_names[-ids_idx]

new2old_hash <- hash()

for (i in 1:length(course_names)) {
  new2old_hash[[ new_names[[i]] ]] <- course_names[[i]]
}

row2idx_hash <- hash()
for (i in 1:length(course_names)) {
  row2idx_hash[[ new_names[[i]] ]] <- i
}

sem_list0 <- c("sem1", "sem2","sem3","sem4","sem5","sem6")
sem_list <- c("Sem 1", "Sem 2","Sem 3","Sem 4","Sem 5","Sem 6")
sem_list2 <- c("Sem 1", "Sem 2","Sem 3","Sem 4","Sem 5","Sem 6", "All")

sem_hash <- hash()
for (i in 1:length(sem_list0)){
  sem_hash[[sem_list[[i]]]] <- sem_list0[[i]]
}

grade_hash <- hash()
for (i in 1:length(raw_grade_names)){
  grade_hash[[grade_levels[[ i ]] ]] <- raw_grade_names[[ i ]]
}

theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "	#F8F9FA", fg = "#202123",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#EA80FC", secondary = "#48DAC6",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  "input-border-color" = "#EA80FC"
)

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(theme=theme,navbarPage(
  "DV Project: Grades Dashboard",
  # grading curve tab
  tabPanel(
    title = "View Grading Curves",
    sidebarPanel(
      id="course_panel",
      selectInput(
        "course_name",
        label="Select Course",
        choices = new_names,
        selected = "Calculus"
      )
    ),
    mainPanel(
      splitLayout(cellWidths = c("80%", "60%"),
                  plotOutput("grade_bars"), plotOutput("grade_pie"))
    )
  ),
  #----------------------------------------------------
  tabPanel(
    title="Grade-Wise Analysis",
    sidebarPanel(
      id="grades_panel",
      selectInput(
        "sem_id",
        label="Select semester",
        choices = sem_list,
        selected = "Sem 4"
      ),
      selectInput(
        "grade_id",
        label="Select grade",
        choices = grade_levels,
        selected = "A"
      )
    ),
    mainPanel(splitLayout(cellWidths = c("95%", "55%"),
                          plotOutput("grade_count_bar"), plotOutput("grade_count_pie"))),
  ),
  #--------------------------------------------------------------------------------
  tabPanel(
    title="Student-Wise Analysis",
    sidebarPanel(
      id="stu_panel",
      selectInput(
        "sem_id2",
        label="Select semester",
        choices = sem_list2,
        selected = "Sem 1"
      ),
      selectInput(
        "stu_id",
        label="Select student",
        choices = cwng$ids,
        selected = "R001"
      ),
    ),
    mainPanel(fluidRow(align="center",
                       splitLayout(align="center", plotOutput("student_hist"))
    )),
  ),
))

server <- function(input, output) {
  output$grade_bars <- renderPlot({
    col_name <- new2old_hash[[input$course_name]]
    course_grades <- cwlg[, col_name, drop = FALSE]
    grades_melted <- reshape2::melt(course_grades)
    grade_counts <- table(grades_melted)
    barplot(grade_counts, main = "Grade Counts", xlab = "Grade", ylab = "Count", col = "steelblue")
  })
  output$grade_pie <- renderPlot({
    
    col_name <- new2old_hash[[input$course_name]]
    course_grades <- cwlg[, col_name, drop = FALSE]
    grades_melted <- reshape2::melt(course_grades)
    grade_counts <- table(grades_melted)
    pie(grade_counts, names(grade_counts))
  })
  output$grade_count_bar <- renderPlot({
    sn <- sem_hash[[input$sem_id]]
    gn <- grade_hash[[input$grade_id]]
    
    # courses <- unlist(list(sems[[sn]]))
    courses <- find_rows_with_value(sn, sems)
    
    grades_in_sem <- cwgc[cwgc$X %in% courses, ]
    
    lbls <- cwgc$X %in% courses
    
    count_of_grade <- grades_in_sem[[gn]]
    barplot(count_of_grade, names.arg = courses, main = paste("Counts and portions of grade", input$grade_id, "in semester", input$sem_id), 
            ylab = "Grade Count", col="steelblue")
  })
  output$grade_count_pie <- renderPlot({
    sn <- sem_hash[[input$sem_id]]
    gn <- grade_hash[[input$grade_id]]
    
    # courses <- unlist(list(sems[[sn]]))
    courses <- find_rows_with_value(sn, sems)
    grades_in_sem <- cwgc[cwgc$X %in% courses, ]
    
    lbls <- cwgc$X %in% courses
    
    count_of_grade <- grades_in_sem[[gn]]
    pie(count_of_grade, courses)
  })
  output$student_hist <- renderPlot({
    if (input$sem_id2 == "All"){
      cn <- names(cwng)
      nn <- cn
      for (i in 1:length(cn)) {
        nn[[i]] <- gsub("\\.", " ", cn[[i]])
      }
      
      tmp <- cwng
      names(tmp) <- nn
      tmp[is.na(tmp)] <- 7
      
      den <- 0
      
      for (i in 1:length(nn)){
        if (nn[[i]] != "X" && nn[[i]] != "ids"){
          
          tmp[[ nn[[i]] ]] <- tmp[[ nn[i] ]] * creds[[ new2old_hash[[nn[[i]]]] ]]
          den <- den + creds[[ new2old_hash[[nn[[i]]]] ]]
        }
      }
      
      tmp$cg <- rowSums(tmp[, new_names])
      tmp$cg <- tmp$cg / den
      x <- tmp$cg
      
      p <- which(tmp$ids == input$stu_id)
      p <- x[[p]]
      
      bins <- seq(min(x), max(x), length.out = 10)
      hist(x, breaks=bins, 
           main=paste("Histogram for CGPAs and highlight for student", input$stu_id) ,
           col="steelblue", xlab="Values")
      abline(v=p, col="red", lwd=2) 
    }
    
    if (input$sem_id2 != "All"){
      sn <- sem_hash[[input$sem_id2]]
      courses_in_sem <- find_rows_with_value(sn, sems) #<- unlist(list(sems[[sn]]))
      
      cn <- names(cwng)
      nn <- cn
      for (i in 1:length(cn)) {
        nn[[i]] <- gsub("\\.", " ", cn[[i]])
      }
      
      tmp <- cwng
      names(tmp) <- nn
      tmp[is.na(tmp)] <- 7
      
      den <- 0
      
      for (i in 1:length(courses_in_sem)){
        tmp[[ courses_in_sem[[i]] ]] <- tmp[[ courses_in_sem[i] ]] * creds[[ new2old_hash[[courses_in_sem[[i]]]] ]]
        den <- den + creds[[ new2old_hash[[courses_in_sem[[i]]]] ]]
      }
      
      tmp$sg <- rowSums(tmp[, courses_in_sem])
      tmp$sg <- tmp$sg / den
      x <- tmp$sg
      
      p <- which(tmp$ids == input$stu_id)
      p <- x[[p]]
      
      bins <- seq(min(x), max(x), length.out = 10)
      hist(x, breaks=bins, 
           main=paste("Histogram for SGPAs for the", input$sem_id2, "and highlight for student", input$stu_id), 
           col="steelblue", xlab="Values")
      abline(v=p, col="red", lwd=2)
    }
  })
}



shinyApp(ui = ui, server = server)