list.files('/Users/denaclink/Downloads/Data_test_Argus/TrainingSelections/',full.names = T)

ValidationFiles <- list.files('/Users/denaclink/Downloads/Data_test_Argus/ValidationSelections',full.names = T)

ValidationList <-lapply(ValidationFiles, read.delim)

ValidationDF <- do.call(rbind.data.frame,ValidationList)


TestFiles <- list.files('/Users/denaclink/Downloads/Data_test_Argus/TestSelections',full.names = T)

TestList <-lapply(TestFiles, read.delim)

TestDF <- do.call(rbind.data.frame,TestList)
