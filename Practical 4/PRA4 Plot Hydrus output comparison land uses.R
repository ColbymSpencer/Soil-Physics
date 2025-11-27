#This script is part of the practical exercise of the course Soil Physics. This script is only to be used for the exercise in this course.
rm(list=ls()) 

##Read the input data, adapt the path to the location where the Hydrus file is saved

#Inputs
Cropland = read.csv("Practical 4/Script Outputs/Cropland.csv")
Forest = read.csv("Practical 4/Script Outputs/Forest.csv")
Grass = read.csv("Practical 4/Script Outputs/Grassland.csv")

#Plotting of the output. You can adapt here limits of the X and Y axes
SMC_min = 0.2
SMC_max = 0.5
MP_min = -500
MP_max = 0
w_min = 0
w_max = 400

### Don't change anything in this script underneath this line ###

#Retain the correct columns in the dataset and give appropriate column names
Cropland=Cropland[,2:ncol(Cropland)]
colnames(Cropland)=c("time (hours)","N1 matric potential (hPa)","N1 theta (m³/m³)","N1 flux (cm/h)","N2 matric potential (hPa)","N2 theta (m³/m³)","N2 flux (cm/h)","N3 matric potential (hPa)","N3 theta (m³/m³)","N3 flux (cm/h)")
Forest=Forest[,2:ncol(Forest)]
colnames(Forest)=c("time (hours)","N1 matric potential (hPa)","N1 theta (m³/m³)","N1 flux (cm/h)","N2 matric potential (hPa)","N2 theta (m³/m³)","N2 flux (cm/h)","N3 matric potential (hPa)","N3 theta (m³/m³)","N3 flux (cm/h)")
Grass=Grass[,2:ncol(Grass)]
colnames(Grass)=c("time (hours)","N1 matric potential (hPa)","N1 theta (m³/m³)","N1 flux (cm/h)","N2 matric potential (hPa)","N2 theta (m³/m³)","N2 flux (cm/h)","N3 matric potential (hPa)","N3 theta (m³/m³)","N3 flux (cm/h)")

#Calculate the soil water storage for each land use
dl1 = 250 #Depth of the first layer (inserted in Hydrus) in mm
dl2 = 200 #Depth second layer
dl3 = 350 #Depth third layer

w_Cropland = matrix(ncol = 2, nrow=nrow(Cropland))
w_Cropland[,1] = Cropland[,1]
w_Cropland[,2] = dl1*Cropland[,3] + dl2*Cropland[,6] + dl3*Cropland[,9]

w_Forest = matrix(ncol = 2, nrow=nrow(Forest))
w_Forest[,1] = Forest[,1]
w_Forest[,2] = dl1*Forest[,3] + dl2*Forest[,6] + dl3*Forest[,9]

w_Grass = matrix(ncol = 2, nrow=nrow(Grass))
w_Grass[,1] = Grass[,1]
w_Grass[,2] = dl1*Grass[,3] + dl2*Grass[,6] + dl3*Grass[,9]

#Make a figure where both the soil moisture content of the three land uses can be compared
X1 = Cropland
Y1= X1
X2 = Forest
Y2 = X2
X3 = Grass
Y3 = X3
l = c("Cropland","Forest","Grass")
c1 = "darkgoldenrod2"
c2 = "forestgreen"
c3 = "olivedrab3"
ySMC = c(SMC_min,SMC_max)
yMP = c(MP_min,MP_max)
n1 = 10
n2 = 40
n3 = 80

dev.new()
par(mfrow=c(2,3))
plot(X1[,1],Y1[,3],pch=20,col=c1,main=paste("SMC",n1,"cm depth"),ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=ySMC)
points(X2[,1],Y2[,3],pch=20,col=c2)
points(X3[,1],Y3[,3],pch=20,col=c3)
plot(X1[,1],Y1[,6],pch=20,col=c1,main=paste("SMC",n2,"cm depth"),ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=ySMC)
legend("topleft",legend=l,pch=c(19, 19, 19),col=c(c1,c2,c3),bty="n")
points(X2[,1],Y2[,6],pch=20,col=c2)
points(X3[,1],Y3[,6],pch=20,col=c3)
plot(X1[,1],Y1[,9],pch=20,col=c1,main=paste("SMS",n3,"cm depth"),ylab="Soil moisture content [m³/m³]",xlab="Time since start of the simulation [hours]",ylim=ySMC)
points(X2[,1],Y2[,9],pch=20,col=c2)
points(X3[,1],Y3[,9],pch=20,col=c3)
plot(X1[,1],Y1[,2],pch=20,col=c1,main=paste("MP",n1,"cm depth"),ylab="Matric potential [hPa]",xlab="Time since start of the simulation [hours]",ylim=yMP)
points(X2[,1],Y2[,2],pch=20,col=c2)
points(X3[,1],Y3[,2],pch=20,col=c3)
plot(X1[,1],Y1[,5],pch=20,col=c1,main=paste("MP",n2,"cm depth"),ylab="Matric potential [hPa]",xlab="Time since start of the simulation [hours]",ylim=yMP)
points(X2[,1],Y2[,5],pch=20,col=c2)
points(X3[,1],Y3[,5],pch=20,col=c3)
plot(X1[,1],Y1[,8],pch=20,col=c1,main=paste("MP",n3,"cm depth"),ylab="Matric potential [hPa]",xlab="Time since start of the simulation [hours]",ylim=yMP)
points(X2[,1],Y2[,8],pch=20,col=c2)
points(X3[,1],Y3[,8],pch=20,col=c3)

#Make a figure where both the soil water storage of the three land uses can be compared
X1 = w_Cropland
Y1= X1
X2 = w_Forest
Y2 = X2
X3 = w_Grass
Y3 = X3
l = c("Cropland","Forest","Grass")
c1 = "darkgoldenrod2"
c2 = "forestgreen"
c3 = "olivedrab3"
yW = c(w_min,w_max)

dev.new()
plot(X1[,1],Y1[,2],pch=20,col=c1,main=paste("Soil water storage until 80 cm depth"),ylab="Soil water storage [mm]",xlab="Time since start of the simulation [hours]",ylim=yW)
legend("bottomleft",legend=l,pch=c(19, 19, 19),col=c(c1,c2,c3),bty="n")
points(X2[,1],Y2[,2],pch=20,col=c2)
points(X3[,1],Y3[,2],pch=20,col=c3)
