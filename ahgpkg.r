CLASS<- read.csv(text='Name,Sex,Age,Height,Weight
"Alfred ","M",14,69,112.5
"Alice","F",13,56.5,84
"Barbara","F",13,65.3,98
"Carol","F",14,62.8,102.5
"Henry","M",14,63.5,102.5
"James","M",12,57.3,83
"Jane","F",12,59.8,84.5
"Janet","F",15,62.5,112.5
"Jeffrey","M",13,62.5,84
"John","M",12,59,99.5
"Joyce","F",11,51.3,50.5
"Judy","F",14,64.3,90
"Louise","F",12,56.3,77
"Mary","F",15,66.5,112
"Philip","M",16,72,150
"Robert","M",12,64.8,128
"Ronald","M",15,67,133
"Thomas","M",11,57.5,85
"William","M",15,66.5,112
')

#Create new column
CLASS['Agegrp']<-CLASS[['Age']]<15


#load module and dataframe
 data(AirPassengers)
 AIR <- AirPassengers

CARS<- read.csv(text=
'Make,Model,Type,Origin,Drivetrain,Msrp,Invoice,Enginesize,Cylinders,Horsepower,Mpg_city,Mpg_highway,Weight,Wheelbase,Length
"Acura"," MDX","SUV","Asia","All",36945,33337,3.5,6,265,17,23,4451,106,189
"Acura"," RSX Type S 2dr","Sedan","Asia","Front",23820,21761,2,4,200,24,31,2778,101,172
"Acura"," TSX 4dr","Sedan","Asia","Front",26990,24647,2.4,4,200,22,29,3230,105,183
"Acura"," TL 4dr","Sedan","Asia","Front",33195,30299,3.2,6,270,20,28,3575,108,186
"Acura"," 3.5 RL 4dr","Sedan","Asia","Front",43755,39014,3.5,6,225,18,24,3880,115,197
"Acura"," 3.5 RL w/Navigation 4dr","Sedan","Asia","Front",46100,41100,3.5,6,225,18,24,3893,115,197
"Acura"," NSX coupe 2dr manual S","Sports","Asia","Rear",89765,79978,3.2,6,290,17,24,3153,100,174
"Audi"," A4 1.8T 4dr","Sedan","Europe","Front",25940,23508,1.8,4,170,22,31,3252,104,179
"Audi"," A41.8T convertible 2dr","Sedan","Europe","Front",35940,32506,1.8,4,170,23,30,3638,105,180
')

procFreqEx<-function(dfname,varname) {
df<-get(dfname)
opt<-lapply(list(varname),function(vari) as.data.frame(df[vari]))
dfCut <- data.frame(opt)
rc<-as.data.frame(table(dfCut))
rc}

procFreqExNew<-function(df,varname) {

opt<-lapply(list(varname),function(vari) as.data.frame(df[vari]))
dfCut <- data.frame(opt)
rc<-as.data.frame(table(dfCut))
rc}



procFreq<-function(v,varname) {

rc<-as.data.frame(table(v))
names(rc)[1]<-varname
rc}



means<-function(one){
	rc<-c(as.character(sd(one)), as.character(sum(one)),as.character(mean(one)),as.character(length(one)),as.character(max(one)),as.character(min(one)))
  names(rc)<-c('std','sum','avg','n','max','min')
  rc

}

procMeansCore<- function (vec,paras){
outNames<-unlist(strsplit(paras,' '))

r_value<-means(vec)
r_value[outNames]

}

procMeans<- function (DSN,v,paras,by){
f<-factor(get(DSN)[,c(by)])
lvl<-levels(f)

loop<-function(l){
#print(by)
print(l)
theOne<-data.frame(subset(get(DSN)[,c(v)],get(DSN)[,c(by)]==l))
procMeansCore(get('theOne')[,1],paras)
}
sapply(lvl,loop)

}

#procMeans('CLASS','Age','std sum','Age')

byDSN<-function (theDSN,byVar,theVar){
 byValues<-as.data.frame(table(theDSN[byVar]))
 class(byValues)


 subDSN<-function(value){
 	#as.numeric(levels(f))[f]. as.numeric(as.character(value))
   print(paste(byVar,'=',as.character(value)))
 	 oneDSN<-subset(theDSN,theDSN[byVar]==as.character(levels(value))[value])
	 # the , is very important to have
   procMeansCore(oneDSN[,theVar],'std sum')
   #out;
 }

 lapply(byValues$Var1,subDSN)

 }

 #byDSN(CLASS,'Age','Weight')

 singleBy<-function (theDSN,byVar){
	 DSNArr<-list();
	 byValues<-as.data.frame(table(theDSN[byVar]))

	 subDSN<-function(value){
	 oneDSN<-subset(theDSN,theDSN[byVar]==as.character(levels(value))[value])
	 DSNArr<-append(DSNArr,oneDSN)
	 }

	 lapply(byValues$Var1,subDSN)
 }
#dArr<-singleBy(CLASS,'Sex')


singleBy<-function (theDSN,byVar){
  allDSN<-list()
	#print(paste('class=',class(theDSN)))
  if (class(theDSN)=='list') allDSN<-append(allDSN,theDSN)
  else allDSN<-append(allDSN,list(theDSN))
  DSNArr<-list();

	for (oneDSN in allDSN){
	#	oneDSN<-as.data.frame(oneDSN)
    if (FALSE){
		print('In Dataset:')
		print(class(oneDSN))
		print(oneDSN)
		}

		byValues<-as.data.frame(table(oneDSN[byVar]))

		subDSN<-function(value){
			aDSN<-subset(oneDSN,oneDSN[byVar]==as.character(levels(value))[value])
			if (FALSE){
			print('Out Dataset:')
			print(aDSN)
			}
			### seems for append to outside var must to use <<-
			if (nrow(aDSN) != 0) {DSNArr<<-append(DSNArr,list(aDSN))}
		}

		lapply(byValues$Var1,subDSN)
	}
return(DSNArr)
}


#Arr1<-singleBy(CLASS,'Age')
#Arr2<-singleBy(Arr1,'Sex')



moreBy<-function (theDSN,byVars){

	 byList<-unlist(strsplit(byVars,' '))
	 sub<-function(value){
	 	paste(value,'---')
		singleBy(theDSN,value)
		# singleBy(CLASS,'Sex')
	 }
	 outDSN<-list(theDSN)
   print(length(byList))
	 for (one in byList){
	   print(one)
		 print('########3')
     outDSN<-singleBy(outDSN,one)
	 }
	 return(outDSN)

 }

all<-moreBy(CLASS,'Sex Age')
for (one in all){
 print(one)
 print(procMeansCore(one[,"Weight"],'std sum'))
}

writeToJSON<-function (dsn,dsnName){
	writeLines(paste('{"SASTableData+',toupper(dsnName),'":[',sep=""))

	for (rId in 1:nrow(dsn)){
		if (rId!=1) writeLines(',')
		writeLines('{')
		for (cId in 1:ncol(dsn)){
			if (cId!=1) writeLines(',')
			cat(paste('"',colnames(dsn)[cId],'":"',dsn[,cId][rId],'"',sep=""))
		}
		writeLines('}')
	}
	writeLines(']}')

}

sink("~/outfile.txt")
small<-subset(CLASS,CLASS['Age']==11)
writeToJSON(small,'newCLASS')
sink()
