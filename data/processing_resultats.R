library(plyr)
library(reshape)
library(stringr)
library(rjson)

# read all 1st tour data from resultats
files <- dir('resultats',pattern='csv')
files <- files[str_detect(files,'t1_')]

dta <- lapply(files,function(x) {
		print(x)
		res <- read.csv(file.path('resultats',x))
		if(str_detect(x,'ag')) {
			res <- res[,c('Code.dÃ.partement','ExprimÃ.s','Front.national')]
			names(res) <- c('code_insee','votes','FN')
			res <- ddply(res,.(code_insee),summarize,
				FN=sum(FN)/sum(votes)
			)
		} else {
			if(str_detect(x,'t2011t')) {
				nuance <- res[,c('Code.du.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Nuance')])]
				nuance <- melt(nuance,id.var=c('Code.du.dÃ.partement','ExprimÃ.s'))
				levels(nuance$variable) <- sapply(levels(nuance$variable),function(x) str_split(x,'Nuance')[[1]][2])
				names(nuance) <- c('code_insee','votes','variable','nuance')
				voix <- res[,c('Code.du.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Voix')])]
				voix <- melt(voix,id.var=c('Code.du.dÃ.partement','ExprimÃ.s'))
				levels(voix$variable) <- sapply(levels(voix$variable),function(x) str_split(x,'Voix')[[1]][2])
				names(voix) <- c('code_insee','votes','variable','voix')
			} else {
				nuance <- res[,c('Code.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Nuance')])]
				nuance <- melt(nuance,id.var=c('Code.dÃ.partement','ExprimÃ.s'))
				levels(nuance$variable) <- sapply(levels(nuance$variable),function(x) str_split(x,'\\.')[[1]][2])
				names(nuance) <- c('code_insee','votes','variable','nuance')
				voix <- res[,c('Code.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Voix')])]
				voix <- melt(voix,id.var=c('Code.dÃ.partement','ExprimÃ.s'))
				levels(voix$variable) <- sapply(levels(voix$variable),function(x) str_split(x,'\\.')[[1]][2])
				names(voix) <- c('code_insee','votes','variable','voix')
			}
			res <- merge(nuance,voix)
			res <- subset(res,nuance=='FN')
			res <- ddply(res,.(code_insee),summarize,
				FN=sum(voix)/sum(votes)
			)
		}
		levels(res$code_insee) <- sapply(levels(res$code_insee),function(ci) {
				if(nchar(ci)==1) {
					return(sprintf('%02i',as.numeric(ci)))
				} else {
					return(ci)
				}
			}
		)
		res$annee <- str_extract(x,'[0-9]{4,}')
		return(res)
	}
)
dta <- do.call('rbind',dta)
dta <- subset(dta,code_insee!='99')

# final <- cast(dta,code_insee~annee,value='FN',fun=max,fill=NULL)

final <- split(dta,dta$annee)
final <- lapply(final,function(df) with(df,setNames(FN,code_insee)) )

cat(toJSON(final),file=file.path('..','web','resultats.json'))
