## cin: cytobands CIN matrix, each row is a cytoband, each column is a sample(patient)
## clinical.inf: n*2 matrix, the 1st column is 'sample name', the second is 'label'
## chr: chromosome number
## annot: "chrom", "start", "end", "name",and "stain". Each row corresponds to a cytoband; which is hg18_annot.Rdata
## title_text: the saved file name and title


"cytobands_cin.draw" <-function(cin, clinical.inf, chr, annot, title_text='text', cin.max.set=5) {
	
	n.cur.chr=0
	sn.cur.chr=0
	for(i in 1:chr) {
		cur.chr=paste('chr',i,sep='')
		n.cur.chr=sum(annot[, 'chrom']==cur.chr)
		sn.cur.chr=sn.cur.chr+n.cur.chr
	}
	
	end.row.idx=sn.cur.chr
	start.row.idx=(sn.cur.chr-n.cur.chr+1)
	
	cin=t(cin[start.row.idx:end.row.idx, ])
	cur.chr=paste('chr',chr,sep='')
	annot=annot[annot[, 'chrom']==cur.chr,]
	
	## re-arrange the cin and labels
	labels=matrix(clinical.inf[,2])
	samplenames=matrix(clinical.inf[,1])
	labels.sort=sort(labels)
	idx.sort=order(labels)
	samplenames=samplenames[idx.sort]
	cin=cin[samplenames,]
	
	n.samp=nrow(cin)	
	row.names=rownames(cin)
	
#	dev.new() width=12, height=15
	dev=png(file=paste(title_text, '.png',sep=''), title=title_text, width=720, height=720, pointsize=9)
#	dev=pdf(file=paste(title_text, '.pdf',sep=''), width=12, height=15, title=title_text, pointsize=9)
	main.title=title_text
	chr.len=annot[nrow(annot), 'end']
	plot(x=c(-1,chr.len),y=c(-1,n.samp+1),xaxt='n',yaxt='n',ann=FALSE,xpd=NA,bty='n',type='n')
	title(main=paste('chromosome',chr,'cytobands CIN overview'),cex=4)
	
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
#################################


#	cin.mid=sort(cin)[floor(nrow(cin)*ncol(cin)/2)]
#	gain.seq=seq(cin.mid, cin.max, length = 50)
#	loss.seq=seq(cin.min, cin.mid, length = 50)
#	whole.seq=c(loss.seq,gain.seq)
#	browser()
	whole.seq=seq(cin.min, cin.max, length = 100)
	
	for (i in 1:n.samp){
		for(n.cyto in 1:ncol(cin)) {
			start=annot[n.cyto,'start']
			end=annot[n.cyto,'end']
			cin.value=cin[i,n.cyto]
#####	idx.color=which(whole.seq>=cin.value)[1]
#################
			idx.color=tail(which(cin.value>=whole.seq),1)
#################
			rect(xleft=start, ybottom=i-1,xright=end,ytop=i, col=palette[idx.color], border=NA, ljoin=1)
		}
		text(x=-chr.len*0.02,y=(i-1)+0.5,labels=row.names[i],adj=1,srt=0,cex=1.0,xpd=NA)
	}
	
#plot.cytobands(annot, bot=n.samp+0.05, top=n.samp+0.2+0.05)
	if (n.samp <= 11) {
		plot.cytobands(annot, bot=n.samp+0.1, top=n.samp+0.3+0.1)
	} else if (n.samp > 11 && n.samp <= 22) {
		plot.cytobands(annot, bot=n.samp+0.15, top=n.samp+0.5+0.15)
	} else if (n.samp > 22 && n.samp <= 32) {
		plot.cytobands(annot, bot=n.samp+0.2, top=n.samp+0.7+0.2)
	} else if (n.samp > 32 && n.samp <= 50) {
		plot.cytobands(annot, bot=n.samp+0.3, top=n.samp+1.0+0.3)
	} else if (n.samp > 50 && n.samp <= 75) {
		plot.cytobands(annot, bot=n.samp+0.4, top=n.samp+1.2+0.4)
	} else if (n.samp > 75 && n.samp <= 100) {
		plot.cytobands(annot, bot=n.samp+0.5, top=n.samp+1.8+0.5)
	} else {
		plot.cytobands(annot, bot=n.samp+1.6, top=n.samp+4+1.6)
	}

	type.classID=unique(labels.sort)
	n.subclass=rep(0,length(type.classID))
	for (i in 1:length(type.classID)){
		n.subclass[i]=sum(labels.sort==type.classID[i])
		if (i==1)
			mark.pos=n.subclass[i]/2
		else
			mark.pos=(sum(n.subclass[1:i])+sum(n.subclass[1:(i-1)]))/2
		text(x=chr.len*1.02,y=mark.pos,labels=type.classID[i],srt=90,cex=1.5,xpd=NA)
		if(i!=(length(type.classID)))
		abline(h=n.subclass[i],col='green',xpd=FALSE)
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
	
#text(max.pos/2,pal.bot-2,paste('chromosome',chr,'cytobands CIN overview'),cex=1.5,xpd=NA)
	dev.off()
	
}



##
## Plot cytobands
##
## Parameters:
##   cb      a data frame consists of columns "chrom", "start", "end", "name",
##           and "stain". Each row corresponds to a cytoband;
##   bot     the bottom position of a horizontally drawn chromosome;
##   top     the top position of the chromosome.
##
## Note that the unit of x-axis must be nucliotide base.
##
plot.cytobands <- function(cb, bot, top)
{
  
  require('bitops')
  
  get.stain.col <- function(stain) {
    switch(stain, 'gneg'='gray100', 'gpos25'='gray90', 'gpos50'='gray70',
           'gpos75'='gray40', 'gpos100'='gray0', 'gvar'='gray100',
           'stalk'='brown3', 'acen'='brown4', 'white')
  }

  cb <- cbind(cb, 'shape'=rep(0L, nrow(cb)))

  # determine shapes of bands
  I <- grep('^p', cb$name)
  cb[I[1], 'shape'] <- 1L
  cb[tail(I,1), 'shape'] <- 2L

  I <- grep('^q', cb$name)
  cb[I[1], 'shape'] <- 1L
  cb[tail(I,1), 'shape'] <- 2L

  for (i in which(cb$stain=='stalk')) {
    cb[i, 'shape'] <- as.integer(NA)
    if (i > 1)
      cb[i-1, 'shape'] <- bitOr(cb[i-1,'shape'], 2L)
    if (i < nrow(cb))
      cb[i+1, 'shape'] <- bitOr(cb[i+1,'shape'], 1L)
  }

  # convert stain to colors
  cb$stain <- sapply(cb$stain, get.stain.col)

  # draw cytobands
  for (i in 1:nrow(cb)) {
    start <- cb$start[i]
    end <- cb$end[i]
    color <- cb$stain[i]
    if (is.na(cb$shape[i])) {
      d <- (end-start) / 3
      segments(start+d, top, start+d, bot, col=color,xpd=NA)
      segments(end-d, top, end-d, bot, col=color,xpd=NA)
    }
    else {
      d <- (end-start) / 4 # horizontal and vertical sizes of corners
      d.y <- (top-bot) / 4
      if (cb$shape[i]==3L)
        polygon(c(start, start+d, end-d, end, end, end-d, start+d, start),
                c(top-d.y, top, top, top-d.y, bot+d.y, bot, bot, bot+d.y),
                col=color,xpd=NA)
      else if (cb$shape[i]==1L)
        polygon(c(start, start+d, end, end, start+d, start),
                c(top-d.y, top, top, bot, bot, bot+d.y), col=color,xpd=NA)
      else if (cb$shape[i]==2L)
        polygon(c(start, end-d, end, end, end-d, start),
                c(top, top, top-d.y, bot+d.y, bot, bot), col=color,xpd=NA)
      else if (cb$shape[i]==0L)
        polygon(c(start, end, end, start), c(top, top, bot, bot), col=color,xpd=NA)
      else
        stop('Invalid cytoband shape.')
    }
  }

  # draw cytoband names
  text((cb$start+cb$end)/2, top, cb$name, srt=90, cex=1.0, adj=c(-0.15,0.5),xpd=NA)
}

