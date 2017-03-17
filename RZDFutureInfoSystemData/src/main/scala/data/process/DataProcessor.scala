package data.process

object DataProcessor {

    val PathName = "C:/Users/DByikov/Desktop/rgd/"

    val FileNameToSave = "allData.xls"
    val SavedExcelMainSheetName = "Data"

    val ClstPath = "C:/Users/DByikov/Desktop/clst/allCategories/"
    val ClstNamesHardNegative: Array[String] = Array[String]("clst29.xls", "clst35.xls")
    val ClstNamesSoftNegative: Array[String] = Array[String](
        "clst27.xls", "clst28.xls", "clst34.xls", "clst35.xls",
        "clst36.xls", "clst37.xls", "clst38.xls", "clst39.xls",
        "clst43.xls", "clst46.xls", "clst47.xls")

    val StopCategoriesForNegativeCall: Array[String] = Array[String]("1.71")
    val StopWordsForNegativeCall: Array[String] = Array[String]("Благодарность")

    val AcceptableCategoriesForPositiveCall: Array[String] = Array[String]("1.71", "1.72", "1.73", "1.74")
    val AcceptableWordsForPositiveCall: Array[String] = Array[String]("Благодарность")

    val KeyWordsAffiliatesToReplace: Array[String] = Array[String]("?", "+")

    val PercentileUnresolvedAbsolute = 0.07F
    val PercentileInProgressRelativeToUnresolved = 0.15F

    var allData: Seq[FullInfo] = new Array[FullInfo](0)

    def process(): Unit = {

        val dataExtractor = new DataExtractor
        allData = dataExtractor.extractFullInfoFromPath(PathName)
        ClstNamesHardNegative.foreach { fileName =>
            dataExtractor.extractRatingCallsFromExcel(
                excelFullFileName = ClstPath + fileName,
                stopCategories = StopCategoriesForNegativeCall,
                stopWords = StopWordsForNegativeCall,
                status = RatingStatus.HardNegative,
                resultToFill = allData)
        }
        ClstNamesSoftNegative.foreach { fileName =>
            dataExtractor.extractRatingCallsFromExcel(
                excelFullFileName = ClstPath + fileName,
                stopCategories = StopCategoriesForNegativeCall,
                stopWords = StopWordsForNegativeCall,
                status = RatingStatus.SoftNegative,
                resultToFill = allData)
        }
        dataExtractor.extractRatingCallsFromResult(
            allData,
            AcceptableCategoriesForPositiveCall,
            AcceptableWordsForPositiveCall,
            RatingStatus.Positive
        )

        allData.map { fullInfo =>
            if (RatingStatus.Unknown == fullInfo.ratingStatus) fullInfo.ratingStatus = RatingStatus.Neutral
            fullInfo
        }

        val responsiblesSeed = allData.filter(_.responsible != "").map(_.responsible).distinct
        val dataSim = new DataSim(allData)
        dataSim.simulateResponsibles(responsiblesSeed)

        dataSim.simulateResolvingStatuses(
            percentileUnresolved = PercentileUnresolvedAbsolute,
            percentileInProgress = PercentileUnresolvedAbsolute * PercentileInProgressRelativeToUnresolved
        )

        val affiliatesSeed =
            dataExtractor.extractAffiliatesSeedFromResult(KeyWordsAffiliatesToReplace, allData)

        dataSim.simulateAffiliates(KeyWordsAffiliatesToReplace, affiliatesSeed)

        val trainCodesSeeds = dataSim.simulateTrainsCodes(responsiblesSeed.length * 10, affiliatesSeed)

        dataSim.simulateTrainStaffsCodes(3 -> 6, trainCodesSeeds)
    }

    def saveAllDataExcel(excelFullFileName: String): Unit = {

        val excelProcessor = new ExcelProcessor(excelFullFileName)

        val data2Write = new Array[Seq[String]](allData.length)
        var i = 0
        allData.foreach { fullInfo =>
            val dataStr = new Array[String](14)
            dataStr(0) = fullInfo.date
            dataStr(1) = fullInfo.source
            dataStr(2) = fullInfo.cipher
            dataStr(3) = fullInfo.call
            dataStr(4) = fullInfo.subdivision
            dataStr(5) = fullInfo.addressed
            dataStr(6) = fullInfo.affiliate
            dataStr(7) = fullInfo.responsible
            dataStr(8) = fullInfo.comment
            dataStr(9) = fullInfo.category
            dataStr(10) = fullInfo.trainCode
            dataStr(11) = fullInfo.staffCode
            dataStr(12) = fullInfo.ratingStatus.toString
            dataStr(13) = fullInfo.resolvingStatus.toString

            data2Write(i) = dataStr

            i += 1
        }

        val sheetName = SavedExcelMainSheetName
        val oxTableLabels = Array[String](
            "date", "source", "cipher", "call", "subdivision", "addressed", "affiliate",
            "responsible", "comment", "category", "trainCode", "staffCode", "ratingStatus", "resolvingStatus")

        excelProcessor.writeTable(
            sheetName = sheetName,
            oxTableLabels = oxTableLabels,
            oyTableLabels = new Array[String](0),
            data = data2Write
        )
    }

    def main(args: Array[String]): Unit = {

        process()
        saveAllDataExcel(PathName + FileNameToSave)
    }
}
