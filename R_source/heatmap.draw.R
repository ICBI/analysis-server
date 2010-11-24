'heatmap.draw'<-function(cin, clinical.inf, heatmap.title='Chromosome instability index'){
	require(gplots)
	labels=matrix(clinical.inf[,2])
	samplenames=matrix(clinical.inf[,1])
	labels.sort=sort(labels)
	idx.sort=order(labels)
	samplenames=samplenames[idx.sort]
	cin=cin[,samplenames]
	
	u.labels=unique(labels.sort)
	n.labels=length(u.labels)
	color.pool=c('red','blue','green','yellow','pink','black')
	col.cin=NULL
	for (i in 1:n.labels) {
		n.curcol=sum(labels==u.labels[i])
		col.cin=c(col.cin, rep(color.pool[i], n.curcol))
	}
	dev=png(file=paste(heatmap.title, '.png',sep=''))
	main.title=paste(heatmap.title, ' heatmap',sep='')
	for (i in 1:n.labels) {
		main.title=paste(main.title,'\n', color.pool[i], ' : ', u.labels[i])
	}
	heatmap.2(cin,ColSideColors=col.cin,col='greenred',dendrogram='none',Colv=FALSE,Rowv=FALSE, scale='row', trace='none', main=main.title)
#	browser()
	dev.off()
}
