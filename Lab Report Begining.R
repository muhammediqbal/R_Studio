#Seting working directory
    curdir <- getwd()
#Reading in my prefered dataset(Calling it df for short)
  df=read.csv(paste(curdir,"/car.data",sep = ""),header=T,
              na.strings="?")

#1 Data Exploration -------------------------------------------------------

# 1.3  exploration of the data
#checking how many rows and colums in my dataset 
  nr <- nrow(df) # number of rows
  nc <- ncol(df) # number of columns
#delivering the data the way i want to present it 
  cat ("Cars Evaluation Dataset has: ", nr," Rows", " and ",nc, " Columns")

#Reciveing a breif description of the data set
  str(df)

#finding out what the coloum names are
  names(df)

#Changing the names of the columns names to make it more clearer for interpretation
  #1.Check what the names of the colums are.
    names(df)
  #2.Change the names of each column sequentially
    colnames(df)[which(names(df) == "vhigh")] <- "Price"
    colnames(df)[which(names(df) == "vhigh.1")] <- "Maint"
    colnames(df)[which(names(df) == "X2")] <- "Doors"
    colnames(df)[which(names(df) == "X2.1")] <- "Persons"
    colnames(df)[which(names(df) == "small")] <- "Boot"
    colnames(df)[which(names(df) == "low")] <- "Safety"
    colnames(df)[which(names(df) == "unacc")] <- "Evaluation"
  #3.Check the names of the colums to make sure all the names have been changed.
    names(df)

#Subsetting:showing 25 rows from the dataset(From 150 to 175)
  df[150:175,]

#Checking to see how many unique labels i have in the dataset  
  #ul = uniquelabel
    ul <- length(unique(df$Label))
  #displaying the amount of unique labels in a presentable way
    cat ("The number of Unique labels in the dataset: ", ul, ". ")
  #confirming that there is no unique labels
    unique(df$Label)
    
#1. Bar plots and pie charts ------------------------------------------------------------

    #Pie chart
    # Pie Chart from data frame with Appended Sample Sizes
    mytable <- table(df$Evaluation)
    lbls <- paste(names(mytable), "\n", mytable, sep="")
    pie(mytable, labels = lbls, 
        main="Pie Chart of car Evaluations\n (from data set)")

   
    # A way of showing   
    labelFreqs <- table(df$Maint)# frequency of labels
    barplot(labelFreqs,col = gray.colors(4),
            main="Maintanence Frequncy")
    labelFreqs <- table(df$Price)# frequency of labels
    barplot(labelFreqs,col = terrain.colors(4),
            main="Price Frequency")
    labelFreqs <- table(df$Doors)# frequency of labels
    barplot(labelFreqs,col = heat.colors(4),
            main="Number of Car Door Frequency")
    
    labelFreqs <- table(df$Persons)# frequency of labels
    barplot(labelFreqs,col = topo.colors(4),
            main="Peopple per car Frequency")
    labelFreqs <- table(df$Boot)# frequency of labels
    barplot(labelFreqs,col = cm.colors(4),
            main="boot size Frequency")
    labelFreqs <- table(df$Safety)# frequency of labels
    barplot(labelFreqs,col = cm.colors(4),
            main="Safety of the cars frequency")
    labelFreqs <- table(df$Evaluation)# frequency of labels
    barplot(labelFreqs,col = cm.colors(4),
            main="Evaluation of the cars frequency")
#1.4 Pre Proseccing ---------------------------------------------------------------------------------
    #Exploring missing values
      #declaring mv (mv = Missing Values) and working out if there are any missing values
      mv <- is.na(df)
      #displaying the output of missing values
      cat(mv)
    #more on missing values
      #Lab 3 for how this is done and also to comment on it 
      na_counts <- sapply(df,function(x) sum(is.na(x)))
      unique_vals<- sapply(df, function(x) length(unique(x)))
      na_counts
      unique_vals
      
#normailiseing and standardising data
#I think it just basically means cleaning your data, removing outliers
  #, changing values to a normalised range
      
  #talk about how i changed the details from the words to numbers
      #in order to get box plots and to confim that there are no outliers 
  

#1.5changing the values to numbers in order to obtain box plots and data corelation -------------------------------------------------------------------  
      #creating a new data set so that i have a back up of the old one 
       nd <- df


    colnames(nd)[which(names(nd) == "vhigh")] <- "Price"
  colnames(nd)[which(names(nd) == "vhigh.1")] <- "Maint"
  colnames(nd)[which(names(nd) == "X2")] <- "Doors"
  colnames(nd)[which(names(nd) == "X2.1")] <- "Persons"
  colnames(nd)[which(names(nd) == "small")] <- "Boot"
  colnames(nd)[which(names(nd) == "low")] <- "Safety"
  colnames(nd)[which(names(nd) == "unacc")] <- "Evaluation"
  
  
  nd$Price<-as.character(nd$Price)
  nd$Price[nd$Price=="vhigh"] <- "4"
  nd$Price[nd$Price=="high"] <- "3"
  nd$Price[nd$Price=="med"] <- "2"
  nd$Price[nd$Price=="low"] <- "1"
  nd$Price = as.numeric(nd$Price)
  
  nd$Maint<-as.character(nd$Maint)
  nd$Maint[nd$Maint=="vhigh"] <- "4"
  nd$Maint[nd$Maint=="high"] <- "3"
  nd$Maint[nd$Maint=="med"] <- "2"
  nd$Maint[nd$Maint=="low"] <- "1"
  nd$Maint = as.numeric(nd$Maint)
  
  nd$Doors<-as.character(nd$Doors)
  nd$Doors[nd$Doors=="5more"] <- "5"
  nd$Doors[nd$Doors=="4"] <- "4"
  nd$Doors[nd$Doors=="3"] <- "3"
  nd$Doors[nd$Doors=="2"] <- "2"
  nd$Doors[nd$Doors=="1"] <- "1"
  nd$Doors = as.numeric(nd$Doors)

  nd$Persons<-as.character(nd$Persons)
  nd$Persons[nd$Persons=="more"] <- "5"
  nd$Persons[nd$Persons=="4"] <- "4"
  nd$Persons[nd$Persons=="2"] <- "2"
  nd$Persons = as.numeric(nd$Persons)
  
  nd$Boot<-as.character(nd$Boot)
  nd$Boot[nd$Boot=="small"] <- "1"
  nd$Boot[nd$Boot=="med"] <- "2"
  nd$Boot[nd$Boot=="big"] <- "3"
  nd$Boot = as.numeric(nd$Boot)
  
  nd$Safety<-as.character(nd$Safety)
  nd$Safety[nd$Safety=="low"] <- "1"
  nd$Safety[nd$Safety=="med"] <- "2"
  nd$Safety[nd$Safety=="high"] <- "3"
  
  nd$Safety = as.numeric(nd$Safety)
  
  
  nd$Evaluation<-as.character(nd$Evaluation)
  nd$Evaluation[nd$Evaluation=="unacc"] <- "1"
  nd$Evaluation[nd$Evaluation=="acc"] <- "2"
  nd$Evaluation[nd$Evaluation=="good"] <- "3"
  nd$Evaluation[nd$Evaluation=="vgood"] <- "4"
  nd$Evaluation = as.numeric(nd$Evaluation)
  
  boxplot(Evaluation ~ Safety, data = nd, color = TRUE, main="Evaluation Safety", 
          xlab="Safety", ylab="Evaluation")
  readline()
  boxplot(Evaluation ~ Boot, data = nd, color = TRUE, main="Evaluation Boot", 
          xlab="Boot Space", ylab="Evaluation")
  readline()    
  boxplot(Evaluation ~ Price, data = nd, color = TRUE, main="Evaluation Price", 
          xlab="Price", ylab="Evaluation")

  boxplot(Evaluation ~ Persons, data = nd, color = TRUE, main="Evaluation Price", 
          xlab="Price", ylab="Evaluation")
    
boxplot(Safety ~ Maint, data = nd, color = TRUE, main= "Safety againsts Maintenace",
        xlab="Safety",ylab="Maintence")



#random fact.counter imnbalence data set for the model










#Dropping irrellevant feautures  ------------------------------------------------------------
# removing data that is not needed-----------------------------
df_clean <- nd
head(df_clean) 

#0000--------------------------------------------

model_table <- table(df_clean$Evaluation)
model_table <- prop.table(model_table)
round(model_table, digits = 1)

plot(x = df_clean$Maint, y = df_clean$Evaluation,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Maintenace  (1=low 2 =med 3= high 4=vhigh.)",
     ylab = "price (1=low, 2 =med 3= high 4=vhigh.))")
df_clean$Pred <- df_clean$Evaluation
head(df_clean)
#2 Modelling / Classification =-----------------------------------------------------

    
    
  
