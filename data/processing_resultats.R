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
			res <- res[,c('Code.dÃ.partement','DÃ.partement','ExprimÃ.s','Front.national')]
			names(res) <- c('code_insee','departement','votes','FN')
			res <- ddply(res,.(code_insee,departement),summarize,
				FN=sum(FN,na.rm=T)/sum(votes)
			)
		} else {
			if(str_detect(x,'t2015t')) {
				nuance <- res[,c('Code.du.département','Libellé.du.département','Exprimés',
						colnames(res)[str_detect(colnames(res),'Nuance')])]
				nuance <- melt(nuance,id.var=c('Code.du.département','Libellé.du.département','Exprimés'))
				levels(nuance$variable) <- sapply(levels(nuance$variable),
					function(x) {
						ncn <- as.numeric(str_split(x,'Nuance')[[1]][2])
						ncn[is.na(ncn)] <- 0
						return(1+ncn)
					}
				)
				names(nuance) <- c('code_insee','departement','votes','variable','nuance')
				voix <- res[,c('Code.du.département','Exprimés',
						colnames(res)[str_detect(colnames(res),'^Voix')])]
				voix <- melt(voix,id.var=c('Code.du.département','Exprimés'))
				levels(voix$variable) <- sapply(levels(voix$variable),
					function(x) {
						ncn <- as.numeric(str_split(x,'Voix')[[1]][2])
						ncn[is.na(ncn)] <- 0
						return(1+ncn)
					}
				)
				names(voix) <- c('code_insee','votes','variable','voix')
			} else if(str_detect(x,'t2011t')) {
				nuance <- res[,c('Code.du.dÃ.partement','LibellÃ..du.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Nuance')])]
				nuance <- melt(nuance,id.var=c('Code.du.dÃ.partement','LibellÃ..du.dÃ.partement','ExprimÃ.s'))
				levels(nuance$variable) <- sapply(levels(nuance$variable),function(x) str_split(x,'Nuance')[[1]][2])
				names(nuance) <- c('code_insee','departement','votes','variable','nuance')
				voix <- res[,c('Code.du.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Voix')])]
				voix <- melt(voix,id.var=c('Code.du.dÃ.partement','ExprimÃ.s'))
				levels(voix$variable) <- sapply(levels(voix$variable),function(x) str_split(x,'Voix')[[1]][2])
				names(voix) <- c('code_insee','votes','variable','voix')
			} else {
				nuance <- res[,c('Code.dÃ.partement','DÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Nuance')])]
				nuance <- melt(nuance,id.var=c('Code.dÃ.partement','DÃ.partement','ExprimÃ.s'))
				levels(nuance$variable) <- sapply(levels(nuance$variable),function(x) str_split(x,'\\.')[[1]][2])
				names(nuance) <- c('code_insee','departement','votes','variable','nuance')
				voix <- res[,c('Code.dÃ.partement','ExprimÃ.s',
						colnames(res)[str_detect(colnames(res),'Voix')])]
				voix <- melt(voix,id.var=c('Code.dÃ.partement','ExprimÃ.s'))
				levels(voix$variable) <- sapply(levels(voix$variable),function(x) str_split(x,'\\.')[[1]][2])
				names(voix) <- c('code_insee','votes','variable','voix')
			}
			res <- merge(nuance,voix)
			res <- subset(res,!is.na(voix))
			res <- subset(res,nuance=='FN')
			res <- ddply(res,.(code_insee,departement),summarize,
				FN=sum(voix,na.rm=T)/sum(votes)
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
dta <- subset(dta,!is.na(FN))
dta <- droplevels(dta)

final <- split(dta,dta$annee)
final <- lapply(final,function(df) with(df,setNames(FN,code_insee)) )

cat(toJSON(final),file=file.path('..','web','resultats.json'))

boxplot(FN~annee,data=dta)
with(dta,range(FN))