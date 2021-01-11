setwd("C:\\Users\\Sushanth\\Desktop\\SimpliLearn\\R\\Project\\Telecom")
getwd()


# Install the following packages.

install.packages("stringi")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")


# Loading Libraries
library(ggplot2)
library(ggpubr)
library(stringi)
library(lubridate)
library(dplyr)

#Loading Data Set
comcast_data<- read.csv("Comcast Telecom Complaints data.csv",header = TRUE)
#Manipulating column names
names(comcast_data)<- stri_replace_all(regex =  "\\.",replacement = "",str =names(comcast_data))
head(comcast_data)

na_vector <- is.na(comcast_data)
length(na_vector[na_vector==T])
#This shows that there is no missing values in dataset,so now data is tidy and available to process further or do EDA based on requriment. •

#Processing Date 
comcast_data$Date<- dmy(comcast_data$Date)

#Extracting Monthly and Daily Ticket Count.
monthly_count<- summarise(group_by(comcast_data,Month =as.integer(month(Date))),Count = n())
daily_count<- summarise(group_by(comcast_data,Date),Count =n())
monthly_count<-arrange(monthly_count,Month)


#Comparing Monthly and Daily Complaints.

ggplot(data = monthly_count,aes(Month,Count,label = Count))+
  geom_line(colour ="Red")+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))

#As we can see that in the month of April,May the tickets are increses but in the month of June it increases drastically, so there might be some reason for which they received high amount of tickets.

ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+
  geom_line(colour = "Blue")+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

#And with the help of above daily chart of tickets we can observe that in second half of June month we recived more tickets with respect to normal days

# Complaint Type Processing
network_tickets<- contains(comcast_data$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(comcast_data$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcast_data$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(comcast_data$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(comcast_data$CustomerComplaint,match = 'charge',ignore.case = T)

comcast_data$ComplaintType[internet_tickets]<- "Internet"
comcast_data$ComplaintType[network_tickets]<- "Network"
comcast_data$ComplaintType[billing_tickets]<- "Billing"
comcast_data$ComplaintType[email_tickets]<- "Email"
comcast_data$ComplaintType[charges_ticket]<- "Charges"

comcast_data$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_ticket,email_tickets)]<- "Others"

table(comcast_data$ComplaintType)



#----
open_complaints<- (comcast_data$Status == "Open"| comcast_data$Status =="Pending")
closed_complaints<-(comcast_data$Status == "Closed"| comcast_data$Status =="Solved")
comcast_data$ComplaintStatus[ open_complaints]<-"Open" 
comcast_data$ComplaintStatus[closed_complaints]<- "Closed" 


#-------------

#Creating Stacked barchart for complaints based on State and Status.
comcast_data<- group_by(comcast_data,State,ComplaintStatus)
chart_data<- summarise(comcast_data,Count = n())

hp<-ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")

hp<-ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
        title = element_text(size = 16,colour = "#0073C2FF")+
        plot.title = element_text(hjust =  0.5),
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")
hp
hp+scale_fill_manual(values= c("#E69F00", "#56B4E9"))
hp+Scale_fill_manual(values= c("#E69F00","#56B4E9"))


chart_data%>%
  filter(ComplaintStatus == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]


resolved_data <- group_by(comcast_data,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(comcast_data,ReceivedVia,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)))

par(mfrow = c(1,2))
#mycols <- c("#EFC000FF", "#CD534CFF")
total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_manual()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank()
                          )
total <- total+Scale_fill_manual(values= c("#EFC000FF", "#CD534CFF"))

# Pie Chart for Category wise Ticket Status
category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y",start = 0)+
  scale_color_manual(colour = mycols)
  geom_text(aes(label = paste0(ReceivedVia,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_void()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank() +
                          scale_fill_manual(values = mycols)  )
ggarrange(total,category,nrow = 1, ncol = 2)

#"#0073C2FF", "#EFC000FF","#868686FF", "#CD534CFF"
mycols <- c("#EFC000FF", "#CD534CFF")
ggplot(Category_resloved, aes(x = 2, y = percentage, fill = ComplaintStatus)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = percentage, label = paste0(ReceivedVia,"-",round(percentage*100),"%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)

