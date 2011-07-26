## cin: regular CIN matrix, each row is a chromosome, each column is a sample(patient)
## clinical.inf: n*2 matrix, the 1st column is 'sample name', the second is 'label'

'heatmap.draw'<-function(cin, clinical.inf, heatmap.title='Chromosome instability index', cin.max.set=5) {
## re-arrange the cin and labels
	labels=matrix(clinical.inf[,2])
	samplenames=matrix(clinical.inf[,1])
	labels.sort=sort(labels)
	idx.sort=order(labels)
	samplenames=samplenames[idx.sort]
	cin=t(cin)
	cin=cin[samplenames,]
	
	n.samp=nrow(cin)	
	row.names=rownames(cin)
	
#	dev.new()
	dev=png(file=paste(heatmap.title, '.png',sep=''), title=heatmap.title, pointsize=9)
#	dev=pdf(file=paste(heatmap.title, '.pdf',sep=''), width=12, height=15, title=heatmap.title, pointsize=9)
#main.title=heatmap.title
	chr.len=22
#plot(x=c(-1,chr.len),y=c(-1,n.samp+1),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	if (n.samp > 11 && n.samp <= 22) {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+ 1.25),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else if (n.samp > 22 && n.samp <= 32) {
			plot(x=c(-1,chr.len),y=c(-1,n.samp+ 2),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else if (n.samp > 32 && n.samp <= 50) {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+3),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else if (n.samp > 50 && n.samp <= 75) {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+5),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else if (n.samp > 75 && n.samp <= 100) {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+6),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else if (n.samp > 100) {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+12),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	} else {
		plot(x=c(-1,chr.len),y=c(-1,n.samp+0.75),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	}
	title(main='heatmap CIN', cex.main = 1.25)
	
#	browser()
	
# palette set
	ramp <- colorRamp(c("black","red"))
	palette=rgb( ramp(seq(0, 1, length = 100)), max = 255)
#	browser()
# cin=log2(cin+1)
#	require(som)
#	cin=normalize(cin,byrow=FALSE)

#############################
	if(cin.max.set==-1) {
		cin.max=max(cin)
		cin.min=min(cin)
	}
	else{
		cin.max=cin.max.set
		cin.min=0
	}
	cin.mid=(cin.max+cin.min)/2
##############################	
	
#	cin.mid=sort(cin)[floor(nrow(cin)*ncol(cin)/2)]
#	gain.seq=seq(cin.mid, cin.max, length = 50)
#	loss.seq=seq(cin.min, cin.mid, length = 50)
#	whole.seq=c(loss.seq,gain.seq)
#	browser()
	whole.seq=seq(cin.min, cin.max, length = 100)
	
	if (n.samp > 11 && n.samp <= 22) {
		text(x=0,y=n.samp+ 0.85,labels='chr #',srt=90,cex=1.5,xpd=NA)
	} else if (n.samp > 22 && n.samp <= 32) {
		text(x=0,y=n.samp+1.25,labels='chr #',srt=90,cex=1.5,xpd=NA)
	} else if (n.samp > 32 && n.samp <= 50) {
		text(x=0,y=n.samp+1.75,labels='chr #',srt=90,cex=1.5,xpd=NA)
	} else if (n.samp > 50 && n.samp <= 100) {
		text(x=0,y=n.samp + 3.5,labels='chr #',srt=90,cex=1.5,xpd=NA)
	} else if (n.samp > 100) {
		text(x=0,y=n.samp + 7,labels='chr #',srt=90,cex=1.5,xpd=NA)
	} else {
		text(x=0,y=n.samp+0.5,labels='chr #',srt=90,cex=1.5,xpd=NA)
	}
	for (i in 1:n.samp){		
		for(j in 1:ncol(cin)) {
#text(x=j,y=n.samp+1,labels=j,srt=90,cex=1.0,xpd=NA)
			if (n.samp > 11 && n.samp <= 22 ) {
				text(x=j,y=n.samp+0.85,labels=j,srt=90,cex=1.0,xpd=NA)	
			} else if (n.samp > 22 && n.samp <= 32 ) {
				text(x=j,y=n.samp+1.25,labels=j,srt=90,cex=1.0,xpd=NA)
			} else if (n.samp > 32 && n.samp <= 50 ) {
					text(x=j,y=n.samp+2,labels=j,srt=90,cex=1.0,xpd=NA)	
			} else if (n.samp > 50 && n.samp <= 100) {
				text(x=j,y=n.samp+3.75,labels=j,srt=90,cex=1.0,xpd=NA)
			} else if (n.samp > 100) {
				text(x=j,y=n.samp+ 7.25,labels=j,srt=90,cex=1.0,xpd=NA)
			} else {
				text(x=j,y=n.samp+0.75,labels=j,srt=90,cex=1.0,xpd=NA)
			}
			cin.value=cin[i,j]
#####	idx.color=which(whole.seq>=cin.value)[1]
##############
			idx.color=tail(which(cin.value>=whole.seq),1)
##############
			rect(xleft=j-0.5, ybottom=i-1,xright=j+0.5,ytop=i, col=palette[idx.color], border=NA, ljoin=1)
		}
		text(x=-chr.len*0.02,y=(i-1)+0.5,labels=row.names[i],adj=1,srt=0,cex=1.0,xpd=NA)
	}

	type.classID=unique(labels.sort)
	n.subclass=rep(0,length(type.classID))
	for (i in 1:length(type.classID)){
		n.subclass[i]=sum(labels.sort==type.classID[i])
		if (i==1)
			mark.pos=n.subclass[i]/2
		else
			mark.pos=(sum(n.subclass[1:i])+sum(n.subclass[1:(i-1)]))/2
		text(x=chr.len+1,y=mark.pos,labels=type.classID[i],srt=90,cex=1.0,xpd=NA)
		if(i!=(length(type.classID)))
		abline(h=n.subclass[i],col='green', lwd=2, xpd=FALSE)
	}
	

#draw color bar

 	max.pos=chr.len
  	nlevels=length(palette)
  	x <- seq(1, max.pos, len=nlevels+1)
#pal.top = -0.25
#pal.bot = -0.45
	if (n.samp > 11 && n.samp <= 32) {
		pal.top = -0.35
		pal.bot = -0.55
	} else if (n.samp > 32 && n.samp <= 50) {
		pal.top = -0.45
		pal.bot = -0.75
	} else if (n.samp > 50 && n.samp <= 100) {
		pal.top = -0.65
		pal.bot = -1.05
	} else if (n.samp >100) {
		pal.top = -1.25
		pal.bot = -1.85
	} else {
		pal.top = -0.25
		pal.bot = -0.45
	}
  	rect(x[1:nlevels], pal.bot, x[-1], pal.top, col=palette, border=NA, ljoin=1,xpd=NA)
#  rect(1, pal.bot, max.pos, pal.top, col=NA, border='gray20')
  	segments(x0=c(1, max.pos/2, max.pos), y0=pal.bot,x1=c(1, max.pos/2, max.pos),
           y1=pal.top, col='black',lend=1,lwd=2,xpd=NA) # draw ticks
  	text(c(1, max.pos/2, max.pos), pal.bot-0.05,
      	   c(sprintf('%.2f', cin.min), sprintf('%.2f', cin.mid), sprintf('%.2f', cin.max)), adj=c(0.5, 1),xpd=NA)
	
#text(max.pos/2,pal.bot-2,heatmap.title,cex=1.5,xpd=NA)
	dev.off()
}



