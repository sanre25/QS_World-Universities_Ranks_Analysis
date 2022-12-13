library(fmsb)

#########################
# Make sure all datasets 
# are loaded in R session 
#########################


#########################
# Radar chart for
# IIT KANPUR
#########################

uni = unique(c(QS_Rank_2020$University , QS_Rank_2021$University , QS_Rank_2022$University , QS_Rank_2023$University))
mat = matrix(c(rep(100 , 1 , 7) , rep(0 , 1 , 7)) , ncol = 7 , nrow = 2 , byrow = T)
mat = rbind(mat ,as.numeric(QS_Rank_2020[which(QS_Rank_2020$University=="Indian Institute of Technology Kanpur (IITK)" ),c(3,7,8,9,10,11,12)]),as.numeric(QS_Rank_2021[which(QS_Rank_2021$University=="Indian Institute of Technology Kanpur (IITK)"),c(3,7,8,9,10,11,12)]) , as.numeric(QS_Rank_2022[which(QS_Rank_2022$University=="Indian Institute of Technology Kanpur (IITK)"),c(3,7,8,9,10,11,12)]) , as.numeric(QS_Rank_2023[which(QS_Rank_2023$University=="Indian Institute of Technology Kanpur (IITK)"),c(3,7,8,9,10,11,12)]))
colnames(mat) = c("overall score" , "International Student Ratio" , "International Faculty Ratio" , "Faculty Student Ratio" , "Citetion per facalty" , "Academic reputation" , "Employee Reputation")


create_beautiful_radarchart <- function(data, color = c("red" , "blue" , "green" , "black"), 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
    
  )
}



op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(as.data.frame(mat), caxislabels = c(0, 25, 50, 75, 100) , title = "Indian Institute of Technology Kanpur (IITK)")
par(op)

legend("bottomleft" , col = c("red" , "blue" , "green" , "black") , legend = c("2020" , "2021" , "2022" , "2023") , pch = 1 , cex = 1 , lwd = 2)


