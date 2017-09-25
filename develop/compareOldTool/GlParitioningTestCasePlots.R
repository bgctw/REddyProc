# stubs, removed from Juergens Evaluation script
#    that dealt with comparison to pvWave

#library(sirad)
#library(scales) # for plotting (function alpha())

check_quality <- TRUE   # plot halfhourly NEE time series as a quality check?

## results arrays
# evaluation metrics
metrics  <- c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
		"intercept","EF","SD","CRM","MPE","AC","ACu","ACs")
aggregation   <- c("halfhourly","daily","monthly")
vars          <- c("GPP","Reco")
NT_vs_DT_REddy <- DT_REddy_vs_pvwave <- array(NA,dim=c(length(sites),length(metrics),length(aggregation),length(vars)),
		dimnames=list(sites,metrics,aggregation,vars))


comparePvWave <- function(){
	#+++++++++++++++++++++
	# Evaluation HH values
	
	### 1) Comparison DT method Pvwave vs. REddyProc
	DT_REddy_vs_pvwave[s,,"halfhourly","GPP"]  <- c(unlist(modeval(df.Pvwave$GPP_HBLR, df.REddy$GPP_DT)))
	DT_REddy_vs_pvwave[s,,"halfhourly","Reco"] <- c(unlist(modeval(df.Pvwave$Reco_HBLR, df.REddy$Reco_DT)))
	
	
	### 2) nighttime vs. daytime in REddyProc
	## nighttime method in REddyProc still missing!!!
	# NT_vs_DT_REddy[s,,"GPP"]  <- c(unlist(modeval(df.REddy$GPP_f, df.REddy$GPP_DT))) 
	# NT_vs_DT_REddy[s,,"Reco"] <- c(unlist(modeval(df.REddy$Reco_f, df.REddy$Reco_DT))) 
	
	
	#+++++++++++++++++++++++++++++
	## add a few columns to the data frames for evaluation purposes 
	## the _agg columns are used for aggregation in aggregate() funciton below
	df.Pvwave$julday_agg <- c(1,df.Pvwave$julday[1:(nrow(df.Pvwave)-1)])
	df.Pvwave$Month_agg  <- c(1,df.Pvwave$Month[1:(nrow(df.Pvwave)-1)])
	df.Pvwave$Year_agg   <- c(df.Pvwave$Year[1],df.Pvwave$Year[1:(nrow(df.Pvwave)-1)])
	
	
	#### Aggregation daily
	if(nrow(df.REddy) != nrow(df.Pvwave)) stop("REddy vs Pvwave: row numbers do not match!")
	df.Pvwave.dd <- aggregate(df.Pvwave,by=list(df.Pvwave$julday_agg),mean,na.rm=T)
	df.REddy.dd  <- aggregate(df.REddy,by=list(df.Pvwave$julday_agg),mean,na.rm=T)
	## aggregate sd (for Pvwave, sd exists only for GPP in the output (SE = SD in Pvwave))
	df.Pvwave.dd$SE_GPP_HBLR <- aggregate(df.Pvwave$SE_GPP_HBLR,by=list(df.Pvwave$julday_agg),aggregate.sd)[,2] 
	df.REddy.dd$Reco_DT_SD   <- aggregate(df.REddy$Reco_DT_SD,by=list(df.Pvwave$julday_agg),aggregate.sd)[,2]
	df.REddy.dd$GPP_DT_SD    <- aggregate(df.REddy$GPP_DT_SD,by=list(df.Pvwave$julday_agg),aggregate.sd)[,2]
	
	DT_REddy_vs_pvwave[s,,"daily","GPP"]  <- c(unlist(modeval(df.Pvwave.dd$GPP_HBLR, df.REddy.dd$GPP_DT)))
	DT_REddy_vs_pvwave[s,,"daily","Reco"] <- c(unlist(modeval(df.Pvwave.dd$Reco_HBLR, df.REddy.dd$Reco_DT)))
}


.tmp.PlotAgainstPvWave <- function(){
	# deprecated: better read pvWave from own directory and plot then
	#+++++++++++++++++++++++++++++
	# Evaluation Monthly All
	graphics.off()
	par(mfrow=c(2,1),mar=c(4,4,1,1),mgp=c(1.8,0.4,0))
	transp <- 0.5
	
	r2_monthly <- round(summary(lm(Pvwave.mm.all$GPP_HBLR ~ REddy.mm.all$GPP_DT))$r.squared,3)
	plot(Pvwave.mm.all$GPP_HBLR ~ REddy.mm.all$GPP_DT,xlab="GPP_REddyProc",ylab="GPP_Pvwave",
			pch=20,col=alpha("black",transp),las=1,tcl=-0.2,xlim=c(0,20),ylim=c(0,20),cex=1.3)
	curve(1*x,from=-10,to=100,col="red",add=T)
	legend("bottomright","monthly",bty="n")
	legend("topleft",paste0("R^2 = ",r2_monthly),bty="n")
	
	
	#++++++++++++++++++++++++++++
	# Evaluation Annual
	r2_yearly <- round(summary(lm(Pvwave.yy.all$GPP_HBLR ~ REddy.yy.all$GPP_DT))$r.squared,3)
	plot(Pvwave.yy.all$GPP_HBLR ~ REddy.yy.all$GPP_DT,xlab="GPP_REddyProc",ylab="GPP_Pvwave",
			pch=20,col=alpha("black",transp),las=1,tcl=-0.2,xlim=c(0,11),ylim=c(0,11),cex=1.3)
	curve(1*x,from=-10,to=100,col="red",add=T)
	legend("bottomright","yearly",bty="n")
	legend("topleft",paste0("R^2 = ",r2_yearly),bty="n")
	
	dev.copy2pdf(file=paste0(path,"Plots/all_sites_monthly_yearly.pdf"),
			width=7.5,height=8,pointsize=11)
}

plotPartitioningResults <- function(){
	#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	### Plots
	#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	## 1) data quality check
	if (all_plots){
		if (check_quality){
			par(mfrow=c(2,2),oma=c(2,2,3,1),mar=c(4,4,1,1)) 
			cex <- 0.9
			plot(df.Pvwave$NEE_f,xlab="timestep",ylab="NEE_f (umol m-2 s-1)",las=1,pch=1,col="black",cex=cex)
			points(df.Pvwave$NEEorig,col="blue")
			legend("topleft",legend="NEEorig",col="blue",pch=1,bty="n",x.intersp=0.5,pt.lwd=2)
			plot(df.Pvwave$Tair_f,xlab="timestep",ylab="Tair_f (degC)",las=1,pch=1,col="black",cex=cex)
			plot(df.Pvwave$Rg_f,xlab="timestep",ylab="Rg_f (W m-2)",las=1,pch=1,col="black",cex=cex)
			plot(df.Pvwave$VPD_f,xlab="timestep",ylab="VPD_f (hPa)",las=1,pch=1,col="black",cex=cex)
			mtext(side=3,line=1,sites[s],cex=1.2,outer=T)
			
			dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_quality_check.pdf"),width=10,height=8,pointsize=11)
		}
		
		
		## 2) Scatterplots --> the good quality data are added on top of the others in a different color
		col.pvwave     <- "green3"  
		col.reddy      <- "grey60"
		col.reddy_good <- "black"
		
		graphics.off()
		par(mfrow=c(1,3),oma=c(0.5,0.5,3.5,0.5),mar=c(4,5,1,1))
		cex.pt   <- 1.2
		transp   <- 1  # transparency
		pch      <- 20
		
		# halfhourly
		plot(df.Pvwave$GPP_HBLR ~ df.REddy$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
				main="halfhourly",pch=pch,col=alpha(col.reddy,transp),cex=cex.pt)
		points(df.Pvwave$GPP_HBLR[df.REddy$FP_qc < 1] ~ df.REddy$GPP_DT[df.REddy$FP_qc < 1],pch=pch,col=alpha(col.reddy_good,transp),cex=cex.pt)
		legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","halfhourly","GPP"],2)),bty="n",cex=0.9)
		legend(y=0.1*max(df.Pvwave$GPP_HBLR,na.rm=T),x=0.35*max(df.REddy$GPP_DT,na.rm=T),legend=c("all","high quality"),
				col=c(col.reddy,col.reddy_good),bty="n",pch=pch,x.intersp=0.5)
		curve(1*x,from=-20,to=100,col="red",add=T)
		
		# daily
		plot(df.Pvwave.dd$GPP_HBLR ~ df.REddy.dd$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
				main="daily",pch=pch,col=alpha(col.reddy,transp),cex=cex.pt)
		points(df.Pvwave.dd$GPP_HBLR[df.REddy.dd$FP_qc < 1] ~ df.REddy.dd$GPP_DT[df.REddy.dd$FP_qc < 1],pch=pch,col=alpha(col.reddy_good,transp),cex=cex.pt)
		legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","daily","GPP"],2)),bty="n",cex=0.9)
		curve(1*x,from=-20,to=100,col="red",add=T)
		mtext(side=3,line=2.2,sites[s],cex=1.1)
		
		# monthly
		plot(df.Pvwave.mm$GPP_HBLR ~ df.REddy.mm$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
				main="monthly",pch=1,col="black")
		legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","monthly","GPP"],2)),bty="n",cex=0.9)
		curve(1*x,from=-20,to=100,col="red",add=T)
		
		# write to file
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_GPP_DT_PVwave_REddyProc.pdf"),
				width=9.5,height=5,pointsize=11)
		
		
		
		#### Scatterplots of NEE
		## a) comparison between Pvwave and REddyProc
		graphics.off()
		transp <- 1
		par(mfrow=c(1,2),mar=c(5,5,1,0.5),oma=c(0.5,0.5,3,0.5))
		
		
		daytime <- df.Pvwave$day > 50   # condition: daytime
		good    <- df.Pvwave$NEE_fqc < 1
		
		# all data (daytime and nighttime)
		plot(df.Pvwave$NEE_HBLR ~ df.REddy$NEE_DT,xlab="NEE_REddyProc",ylab="NEE_PVwave",las=1,
				pch=pch,col=alpha(col.reddy,transp),cex=cex.pt,main="NEE (all data)",mgp=c(4,1,0))
		points(df.Pvwave$NEE_HBLR[good] ~ df.REddy$NEE_DT[good],col=col.reddy_good,pch=pch)
		curve(1*x,from=-100,to=100,col="red",add=T)
		#mtext(side=3,line=2,text=sites[s])
		title(main=sites[s],outer=T,cex=1.5)
		
		# all data (only daytime)
		plot(df.Pvwave$NEE_HBLR[daytime] ~ df.REddy$NEE_DT[daytime],xlab="NEE_REddyProc",ylab="NEE_PVwave",las=1,
				pch=pch,col=alpha(col.reddy,transp),cex=cex.pt,main="NEE (daytime only)",mgp=c(4,1,0))
		points(df.Pvwave$NEE_HBLR[daytime & good] ~ df.REddy$NEE_DT[daytime & good],col=col.reddy_good,pch=pch)
		curve(1*x,from=-100,to=100,col="red",add=T)
		
		
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_NEE_PVwave_REddyProc.pdf"),
				width=9.5,height=5,pointsize=11)
		
		
		
		
		## b) comparison between the products and measurements
		graphics.off()
		transp <- 1
		pch <- 1
		par(mfrow=c(1,2),mar=c(5,5,1,0.5),oma=c(0.5,0.5,3,0.5))
		xlim <- c(min(df.Pvwave$NEE_HBLR,df.REddy$NEE_DT,na.rm=T),max(df.Pvwave$NEE_HBLR,df.REddy$NEE_DT,na.rm=T))
		ylim <- c(min(df.Pvwave$NEE_f,na.rm=T),max(df.Pvwave$NEE_f,na.rm=T))
		
		# Pvwave
		plot(df.Pvwave$NEE_f ~ df.Pvwave$NEE_HBLR,xlab="NEE_PVwave",ylab="NEE_f",las=1,xlim=xlim,ylim=ylim,
				pch=pch,col=alpha("black",transp),cex=cex.pt)
		curve(1*x,from=-100,to=100,col="red",add=T)
		title(main=sites[s],outer=T,cex=1.5)
		modPvwave <- lm(df.Pvwave$NEE_f ~ df.Pvwave$NEE_HBLR)
		abline(modPvwave,col="blue")
		mtext(side=3,line=-1.2,paste0("R^2 = ",round(summary(modPvwave)$r.squared,2)))
		
		# REddyProc
		plot(df.Pvwave$NEE_f ~ df.REddy$NEE_DT,xlab="NEE_REddyProc",ylab="NEE_f",las=1,xlim=xlim,ylim=ylim,
				pch=pch,col=alpha("black",transp),cex=cex.pt)
		curve(1*x,from=-100,to=100,col="red",add=T)
		modREddy <- lm(df.Pvwave$NEE_f ~ df.REddy$NEE_DT)
		abline(modREddy,col="blue")
		mtext(side=3,line=-1.2,paste0("R^2 = ",round(summary(modREddy)$r.squared,2)))
		#points(df.Pvwave$NEE_f[good] ~ df.REddy$NEE_DT[good],col="black",pch=20)
		
		# Nighttime method
		
		
		
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_NEE_obs_mod.pdf"),
				width=9.5,height=5,pointsize=11)
		
		
		
		
		## 3) Timeseries of GPP and Reco
		graphics.off()
		par(mfrow=c(1,2),mar=c(5,5,1,0.5),oma=c(0.5,0.5,3,0.5))
		
		GPP_good  <- df.REddy[,"GPP_DT"]
		Reco_good <- df.REddy[,"Reco_DT"]
		
		GPP_good[df.REddy[,"FP_qc"] > 0.5] <- NA
		Reco_good[df.REddy[,"FP_qc"] > 0.5] <- NA
		
		
		# GPP
		plot(df.Pvwave$GPP_HBLR,col=col.pvwave,xlab="Timestep",ylab=expression("GPP ("*mu*"mol m"^{-2}~"s"^{-1}*")"),las=1,
				ylim=c(min(c(df.Pvwave$GPP_HBLR,df.REddy$GPP_DT),na.rm=T),max(df.Pvwave$GPP_HBLR,na.rm=T) + 0.15*max(df.Pvwave$GPP_HBLR,na.rm=T)))
		points(df.REddy$GPP_DT,col=col.reddy)
		points(GPP_good,col=col.reddy_good)
		
		legend("topleft",legend=c("REddyProc (all)","REddyProc (high quality)","Pvwave"),col=c(col.reddy,col.reddy_good,col.pvwave),
				bty="n",pch=1,x.intersp=0.5,y.intersp=0.8,pt.lwd=3)
		mtext(side=3,line=1,sites[s],cex=1.2,outer=T)
		
		# Reco
		plot(df.Pvwave$Reco_HBLR,col=col.pvwave,xlab="Timestep",ylab=expression("Reco ("*mu*"mol m"^{-2}~"s"^{-1}*")"),las=1,
				ylim=c(min(c(df.Pvwave$Reco_HBLR,df.REddy$Reco_DT),na.rm=T),max(df.Pvwave$Reco_HBLR,na.rm=T) + 0.15*max(df.Pvwave$Reco_HBLR,na.rm=T)))
		points(df.REddy$Reco_DT,col=col.reddy)
		points(Reco_good,col=col.reddy_good)
		
		
		# write to file
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_GPP_Reco_DT_PVwave_REddyProc_timeseries.pdf"),
				width=11,height=7,pointsize=11)
		
		
		
		
		
		## 4) mean diurnal courses of GPP and Reco
		## data selection --> only summer months .. ok in this case, as all sites are on the northern hemisphere
		df.Pvwave.summer <- df.Pvwave[df.Pvwave[,"Month"] %in% c(5,6,7,8,9),]
		df.REddy.summer  <- df.REddy[df.Pvwave[,"Month"] %in% c(5,6,7,8,9),]
		
		mean_diurnal_Pvwave    <- aggregate(df.Pvwave.summer,by=list(df.Pvwave.summer[,9]),mean,na.rm=T)  # 9 should be the second "Hour" column
		mean_diurnal_REddyProc <- aggregate(df.REddy.summer,by=list(df.Pvwave.summer[,9]),mean,na.rm=T)
		
		graphics.off()
		par(mfrow=c(1,2),mar=c(5,5,1,0.5),oma=c(0.5,0.5,3,0.5))
		
		plot(mean_diurnal_Pvwave$GPP_HBLR ~ mean_diurnal_Pvwave[,10],col=col.pvwave,xlab="Hour",ylab=expression("GPP ("*mu*"mol m"^{-2}~"s"^{-1}*")"),las=1,type="l",lwd=2,
				ylim=c(min(c(mean_diurnal_Pvwave$GPP_HBLR,mean_diurnal_REddyProc$GPP_DT),na.rm=T),max(mean_diurnal_Pvwave$GPP_HBLR,na.rm=T) + 0.15*max(mean_diurnal_Pvwave$GPP_HBLR,na.rm=T)))
		points(mean_diurnal_REddyProc$GPP_DT ~ mean_diurnal_Pvwave[,10],col=col.reddy_good,type="l",lwd=2)
		
		
		legend("topleft",legend=c("REddyProc","Pvwave"),col=c(col.reddy_good,col.pvwave),
				bty="n",lty=1,x.intersp=0.5,y.intersp=0.8,lwd=3)
		mtext(side=3,line=1,sites[s],cex=1.2,outer=T)
		
		
		plot(mean_diurnal_Pvwave$Reco_HBLR ~ mean_diurnal_Pvwave[,10],col=col.pvwave,xlab="Hour",ylab=expression("Reco ("*mu*"mol m"^{-2}~"s"^{-1}*")"),las=1,type="l",lwd=2,
				ylim=c(min(c(mean_diurnal_Pvwave$Reco_HBLR,mean_diurnal_REddyProc$Reco_DT),na.rm=T),max(mean_diurnal_Pvwave$Reco_HBLR,na.rm=T) + 0.15*max(mean_diurnal_Pvwave$Reco_HBLR,na.rm=T)))
		points(mean_diurnal_REddyProc$Reco_DT ~ mean_diurnal_Pvwave[,10],col=col.reddy_good,type="l",lwd=2)
		
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_GPP_Reco_DT_PVwave_REddyProc_mean_diurnal_courses_May_September.pdf"),
				width=11,height=7,pointsize=11)
		
		
		
		
		
		## 5) Timeseries of the parameters (Pvwave and REddy)
		graphics.off()
		par(mfrow=c(2,3),mar=c(5,5,1,1),oma=c(0.5,0.5,3,0.5))
		
		pars_pvwave <- df.Pvwave[,c("Rrefopt_OrdE0_2_from","rb","beta","k","E0","alpha")]
		pars_pvwave[pars_pvwave < -9000] <- NA
		# str(df.REddy)
		
		# R_ref
		plot(df.REddy$FP_R_ref,col="black",xlab="timestep",ylab="R_ref",las=1,ylim=c(min(pars_pvwave$rb,df.REddy$FP_R_ref,na.rm=T),
						max(pars_pvwave$rb,df.REddy$FP_R_ref,na.rm=T) + 0.2*max(pars_pvwave$rb,df.REddy$FP_R_ref,na.rm=T))) # leave some extra space for legend 
		points(pars_pvwave$rb,col=col.pvwave)
		legend("topleft",legend=c("REddyProc","Pvwave"),col=c("black",col.pvwave),bty="n",pch=1,x.intersp=0.5)
		
		# R_ref_night (for REddyProc only at the moment)
		plot(df.REddy$FP_R_refNight,col="black",xlab="timestep",ylab="R_refNight",las=1,ylim=c(min(df.REddy$FP_R_ref,na.rm=T),
						max(df.REddy$FP_R_ref,na.rm=T) + 0.2*max(df.REddy$FP_R_ref,na.rm=T))) 
		
		#points(pars_pvwave$Rrefopt_OrdE0_2_from,col=col.pvwave)  # is it the right parameter?
		
		# E0
		plot(df.REddy$FP_E0,col="black",xlab="timestep",ylab="E0",las=1,ylim=c(min(pars_pvwave$E0,df.REddy$FP_E0,na.rm=T),
						max(pars_pvwave$E0,df.REddy$FP_E0,na.rm=T)))
		points(pars_pvwave$E0,col=col.pvwave)
		
		
		# alpha
		plot(df.REddy$FP_alpha,col="black",xlab="timestep",ylab="alpha",las=1,ylim=c(min(pars_pvwave$alpha,df.REddy$FP_alpha,na.rm=T),
						max(pars_pvwave$alpha,df.REddy$FP_alpha,na.rm=T))) 
		points(pars_pvwave$alpha,col=col.pvwave)
		
		
		# beta
		plot(df.REddy$FP_beta,col="black",xlab="timestep",ylab="beta",las=1,ylim=c(min(pars_pvwave$beta,df.REddy$FP_beta,na.rm=T),
						max(pars_pvwave$beta,df.REddy$FP_beta,na.rm=T))) 
		points(pars_pvwave$beta,col=col.pvwave)
		
		
		# k
		plot(df.REddy$FP_k,col="black",xlab="timestep",ylab="k",las=1,ylim=c(min(pars_pvwave$k,df.REddy$FP_k,na.rm=T),
						max(pars_pvwave$k,df.REddy$FP_k,na.rm=T)))
		points(pars_pvwave$k,col=col.pvwave)
		
		mtext(side=3,line=1,sites[s],cex=1.2,outer=T)
		
		
		# write to file
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_Parameters_Timeseries.pdf"),
				width=9,height=6.5,pointsize=11)
		
		
		
		
		## 6) Scattterplot of the parameters (Pvwave and REddy)
		graphics.off()
		par(mfrow=c(2,3),mar=c(5,5,1,1),oma=c(0.5,0.5,3,0.5),mgp=c(3,0.5,0))
		
		pars_pvwave <- df.Pvwave[,c("rb","beta","k","E0","alpha")]
		pars_pvwave[pars_pvwave < -9000] <- NA
		
		# the parameter values of REddyProc and Pvwave are shifted one value towards each other
		# temporary fix is to shift the Pvwave values one timestep forward (by deleting the first value):
		# pars_pvwave <- rbind(pars_pvwave[-1,],NA)
		# pars_pvwave <- rbind(pars_pvwave[-1,],NA)
		
		# R_ref
		plot(df.REddy$FP_R_ref ~ pars_pvwave$rb,xlab="Rb_Pvwave",ylab="Rb_REddy",las=1)
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		
		# E0
		plot(df.REddy$FP_E0 ~ pars_pvwave$E0,xlab="E0_Pvwave",ylab="E0_REddy",las=1)
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		
		# alpha
		plot(df.REddy$FP_alpha ~ pars_pvwave$alpha,xlab="alpha_Pvwave",ylab="alpha_REddy",las=1)
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		
		# beta
		plot(df.REddy$FP_beta ~ pars_pvwave$beta,xlab="beta_Pvwave",ylab="beta_REddy",las=1)
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		
		# k
		plot(df.REddy$FP_k ~ pars_pvwave$k,xlab="k_Pvwave",ylab="k_REddy",las=1)
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		
		mtext(side=3,line=1,sites[s],cex=1.2,outer=T)
		
		
		# write to file
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_Parameters_Scatterplot.pdf"),
				width=9,height=6.5,pointsize=11)
		
		
		
		## 7) Comparison to nighttime approach
		graphics.off()
		par(mfrow=c(2,2),oma=c(1,1,2.5,0.5))
		col.nt <- "orange"
		lim.reco  <- c(min(df.Pvwave$Reco,df.Pvwave$Reco_HBLR,df.REddy$Reco_DT,na.rm=T),
				max(df.Pvwave$Reco,df.Pvwave$Reco_HBLR,df.REddy$Reco_DT,na.rm=T))
		
		lim.gpp  <- c(min(df.Pvwave$GPP_f,df.Pvwave$GPP_HBLR,df.REddy$GPP_DT,na.rm=T),
				max(df.Pvwave$GPP_f,df.Pvwave$GPP_HBLR,df.REddy$GPP_DT,na.rm=T))
		
		#   plot(df.Pvwave$Reco,col=col.nt,ylim=ylim)
		#   points(df.Pvwave$Reco_HBLR,col=col.pvwave)
		#   points(df.REddy$Reco_DT,col=col.reddy_good)
		
		### Reco
		plot(df.Pvwave$Reco ~ df.Pvwave$Reco_HBLR,ylim=lim.reco,xlim=lim.reco,xlab="Daytime_Pvwave",ylab="Nighttime_MR",main="Reco")
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		mod.pvwave <- lm(df.Pvwave$Reco ~ df.Pvwave$Reco_HBLR)
		mtext(side=3,line=-1.8,paste0("R^2 = ",round(summary(mod.pvwave)$r.squared,2)))
		title(main=sites[s],outer=T,cex=2)
		
		plot(df.Pvwave$Reco ~ df.REddy$Reco_DT,ylim=lim.reco,xlim=lim.reco,xlab="Daytime_REddyProc",ylab="Nighttime_MR",main="Reco")
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		mod.reddy <- lm(df.Pvwave$Reco ~ df.REddy$Reco_DT)
		mtext(side=3,line=-1.8,paste0("R^2 = ",round(summary(mod.reddy)$r.squared,2)))
		
		
		### GPP  
		plot(df.Pvwave$GPP_f ~ df.Pvwave$GPP_HBLR,ylim=lim.gpp,xlim=lim.gpp,xlab="Daytime_Pvwave",ylab="Nighttime_MR",main="GPP")
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		mod.pvwave <- lm(df.Pvwave$GPP_f ~ df.Pvwave$GPP_HBLR)
		mtext(side=3,line=-1.8,paste0("R^2 = ",round(summary(mod.pvwave)$r.squared,2)))
		title(main=sites[s],outer=T,cex=1.5)  
		
		plot(df.Pvwave$GPP_f ~ df.REddy$GPP_DT,ylim=lim.gpp,xlim=lim.gpp,xlab="Daytime_REddyProc",ylab="Nighttime_MR",main="GPP")
		curve(1*x,from=-1000,to=1000,col="red",add=T)
		mod.reddy <- lm(df.Pvwave$GPP_f ~ df.REddy$GPP_DT)
		mtext(side=3,line=-1.8,paste0("R^2 = ",round(summary(mod.reddy)$r.squared,2)))
		
		
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_Reco_GPP_DT_NT_scatterplots.pdf"),
				width=9.5,height=9.5,pointsize=11)
		
		
		
		
		# Reco and GPP timeseries 
		graphics.off()
		par(mfrow=c(1,2),oma=c(1,1,2.5,0.5))
		plot(df.Pvwave$Reco,col=col.nt,ylim=lim.reco,type="l",xlab="timestep",ylab="Reco",las=1)
		points(df.Pvwave$Reco_HBLR,col=col.pvwave,type="l")
		points(df.REddy$Reco_DT,col=col.reddy_good,type="l")
		title(main=sites[s],outer=T,cex=1.5) 
		legend("topleft",legend=c("Nighttime","DT_Pvwave","DT_REddyProc"),col=c(col.nt,col.pvwave,col.reddy_good),
				lty=1,bty="n",x.intersp=0.5,y.intersp=0.5,cex=0.7,seg.len=0.7)
		
		plot(df.Pvwave$GPP_f,col=col.nt,ylim=lim.gpp,type="l",xlab="timestep",ylab="GPP",las=1)
		points(df.Pvwave$GPP_HBLR,col=col.pvwave,type="l")
		points(df.REddy$GPP_DT,col=col.reddy_good,type="l")
		
		dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_Reco_GPP_DT_NT_timeseries.pdf"),
				width=9.5,height=5,pointsize=11)
	}
} 

