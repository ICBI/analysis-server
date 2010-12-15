## cin: regular CIN matrix, each row is a chromosome, each column is a sample(patient)
## clinical.inf: n*2 matrix, the 1st column is 'sample name', the second is 'label'

'heatmap.draw'<-function(cin, clinical.inf, heatmap.title='Chromosome instability index'){
	require(gplots)
	labels=matrix(clinical.inf[,2])
	samplenames=matrix(clinical.inf[,1])
	labels.sort=sort(labels)
	idx.sort=order(labels)
	samplenames=samplenames[idx.sort]
	cin=cin[,samplenames]
	
	for(i in 1:nrow(cin)) {
		if(sum(cin[i,])==0)
			cin[i,]=cin[i,]+rnorm(ncol(cin))/10
	}
	
	u.labels=unique(labels.sort)
	n.labels=length(u.labels)
	color.pool=c('red','blue','green','yellow','pink','black')
	col.cin=NULL
	for (i in 1:n.labels) {
		n.curcol=sum(labels==u.labels[i])
		col.cin=c(col.cin, rep(color.pool[i], n.curcol))
	}
	dev=png(file=paste(heatmap.title, '.png',sep=''))
	#main.title=paste(heatmap.title, ' heatmap',sep='')
	main.title='CIN heatmap'
	for (i in 1:n.labels) {
		main.title=paste(main.title,'\n', color.pool[i], ' : ', u.labels[i])
	}
	heatmap.3(t(cin),RowSideColors=col.cin,col='greenred',dendrogram='none',Colv=FALSE,Rowv=FALSE, scale='column', trace='none', main=main.title)
#	browser()
	dev.off()
}
