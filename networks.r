file = "../../../Downloads/scopus (1).bib"
file_2 = "../../../Downloads/scopus (2).bib"
M = convert2df(file,dbsource = "scopus", format = "bibtex")
C = convert2df(file_2,dbsource = "scopus", format = "bibtex")
M$PA = NA
MC = rbind(M,C)

file = "../../../Downloads/scopus (3).bib"
fosss = convert2df(file,dbsource = "scopus",format = "bibtex")


results = biblioAnalysis(MC, sep = ";")
plot(x = results, k = 10, pause = FALSE)

CR <- citations(MC, field = "article", sep = ";")
cbind(CR$Cited[1:10])
CS <- conceptualStructure(MC,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)


A = cocMatrix(MC, Field = "SO", sep = ";")

M <- metaTagExtraction(MC, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

# citation
NetMatrix <- biblioNetwork(MC, analysis = "co-citation", network = "references", sep = ";")# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
