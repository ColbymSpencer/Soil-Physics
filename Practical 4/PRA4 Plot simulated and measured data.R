# This script is part of the practical exercise of the course Soil Physics. 
# This script is only to be used for the exercise in this course.
rm(list=ls()) 

# Land use. 1 = cropland, 2 = forest, 3 = grass
LU = 3

## Load all data
# Hydrus simulation output file 
input_sim = read.csv("Practical 4/Hydrus Simulation/Prac4_Grassland/Obs_Node.out",
                     skip=10,
                     sep=" ")
# Measured data 
measured =read.csv("Practical 4/Measured Data/Grassland.csv")

##In order to compare to the correct measured data you need to insert the starting day of your simulation.
# Update the field between "". Change to the start moment of your simulation. 
# Only change the date, do not adapt the hours:minutes:seconds. 
# Keep an identical format!
s = "01/05/2021 00:00"

##Variables related to the plotting 
#Limits of the y axis
SMC_min = 0.1 #Soil moisture content (m³/m³)
SMC_max = 0.55
w_min = 0 #soil water storage (mm)
w_max = 400

##Place where the output will be stored
# Change the path to the location where the output csv file can be stored. 
# The last letters make the name of the csv file. 
# In order to distinguish between the land uses you add a "C" for cropland, "F" for forest or "G" for grassland. 
# Depending on which land use you simulated. 
path = "Practical 4/G.csv"

#### Don't change anything in this script underneath this line ####

#Obtain the dataset of the simulation data where all necessary columns are removed
input_sim=input_sim[,2:ncol(input_sim)]
m = 3 #only adapt this value if you would have more/less then 3 nodes
data = matrix(ncol=(m*3+1))
colnames(data)=c(1:ncol(data))
for (i in 1:(nrow(input_sim)-1)){
  t = input_sim[i,which(is.na(input_sim[i,])==FALSE)]
  colnames(t)=c(1:ncol(t))
  data= rbind(data,t)
}
#Only change the column names if you have more or less nodes than the standard 3
colnames(data)=c("time (hours)","N1 matric potential (hPa)","N1 theta (m³/m³)","N1 flux (cm/h)","N2 matric potential (hPa)","N2 theta (m³/m³)","N2 flux (cm/h)","N3 matric potential (hPa)","N3 theta (m³/m³)","N3 flux (cm/h)")
#your final dataset
Simulation = data[2:nrow(data),]

#select a part from the data, same time period as your simulation
measured = measured[3:nrow(measured),]
start = as.numeric(which(measured[,1]==s))
end = as.numeric(round(Simulation[nrow(Simulation),1]))
time = seq(1:end)

#Calculate the soil water storage
dl1 = 250 #Depth of the first layer (inserted in Hydrus) in mm
dl2 = 200 #Depth second layer
dl3 = 350 #Depth third layer

w = matrix(ncol = 2, nrow=nrow(Simulation))
w[,1] = Simulation[,1]
w[,2] = dl1*Simulation[,3] + dl2*Simulation[,6] + dl3*Simulation[,9]


#Make a figure where the simulated soil moisture content is compared to the measured soil moisture content
c1 = "darkgrey"
c2 = "darkmagenta"

dev.new()
par(mfrow=c(2,2))
plot(time,measured[start:(end+start-1),2],main="10 cm depth",col=c1,pch=19,ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=c(SMC_min,SMC_max))
legend("topleft",legend=c("Measured","Simulated"),pch=c(19, 19),col=c(c1,c2),bty="n")
points(Simulation[,1],Simulation[,3],col=c2,pch=19)
plot(time,measured[start:(end+start-1),3],main="40 cm depth",col=c1,pch=19,ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=c(SMC_min,SMC_max))
points(Simulation[,1],Simulation[,6],col=c2,pch=19)
plot(time,measured[start:(end+start-1),4],main="80 cm depth",col=c1,pch=19,ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=c(SMC_min,SMC_max))
points(Simulation[,1],Simulation[,9],col=c2,pch=19)

#Make a figure where the simulated soil water storage is plotted over time
if (LU == 1){
  t = "cropland"
  th_AC = 293.3
  th_FC = 253.5
  th_CR = 173.7
  th_PWP = 59.3
}
if (LU == 2){
  t = "forest"
  th_AC = 324
  th_FC = 249.9
  th_CR = 183.8
  th_PWP = 85.7
}
if (LU == 3){
  t = "grass"
  th_AC = 305
  th_FC = 255.4
  th_CR = 161.3
  th_PWP = 87.3
}

AC = matrix(nrow=nrow(Simulation),ncol=2)
AC[,1]=Simulation[,1]
AC[,2] = rep(th_AC,each=nrow(Simulation))
FC = matrix(nrow=nrow(Simulation),ncol=2)
FC[,1]=Simulation[,1]
FC[,2] = rep(th_FC,each=nrow(Simulation))
CR = matrix(nrow=nrow(Simulation),ncol=2)
CR[,1]=Simulation[,1]
CR[,2] = rep(th_CR,each=nrow(Simulation))
PWP = matrix(nrow=nrow(Simulation),ncol=2)
PWP[,1]=Simulation[,1]
PWP[,2] = rep(th_PWP,each=nrow(Simulation))

dev.new()
plot(w[,1],w[,2],main=paste("Soil water storage until 80 cm depth",t),col="black",pch=19,ylab="Soil water storage [mm]",xlab="Time since start of the simulation [hours]",ylim=c(w_min,w_max))
legend("bottomleft",legend=c("Soil water storage","Air capacity","Field capacity","Critical water content","Permanent wilting point"),pch=c(19,20,20,20,20),col=c("black","blue","green","orange","red"),bty="n")
lines(AC[,1],AC[,2],lty=2,col="blue")
lines(FC[,1],FC[,2],lty=2,col="green")
lines(CR[,1],CR[,2],lty=2,col="orange")
lines(PWP[,1],PWP[,2],lty=2,col="red")

#Write your output to a csv which can be easily shared with your group
write.csv(Simulation,path)
