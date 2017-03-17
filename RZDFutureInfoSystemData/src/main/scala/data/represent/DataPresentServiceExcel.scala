package data.represent

import data.process._

case class RatingStatusesByMonths(
                                     month: Int,
                                     totalHardNegative: Int,
                                     totalSoftNegative: Int,
                                     totalNeutral: Int,
                                     totalPositive: Int
                                 )

case class ResolvingStatusesByMonths(
                                        month: Int,
                                        totalResolved: Int,
                                        totalUnresolved: Int,
                                        totalInProgress: Int
                                    )

case class CategoryByNumberOfCallsAndResolvingStatuses(
                                                          category: String,
                                                          numberOfCalls: Int,
                                                          totalResolved: Int,
                                                          totalUnresolved: Int,
                                                          totalInProgress: Int
                                                      )

case class SourceByNumberOfCallsAndResolvingStatuses(
                                                          source: String,
                                                          numberOfCalls: Int,
                                                          totalResolved: Int,
                                                          totalUnresolved: Int,
                                                          totalInProgress: Int
                                                      )

object DataPresentServiceExcel {

    val ColNameDateInput = "date"
    val ColNameCategoryInput = "category"
    val ColNameSourceInput = "source"
    val ColNameRatingStatus = "ratingStatus"
    val ColNameResolvingStatus = "resolvingStatus"

    def main(args: Array[String]): Unit = {

        val dataPresentServiceExcel = new DataPresentServiceExcel(DataProcessor.PathName + DataProcessor.FileNameToSave)

        val ratingStatusesByMonths = dataPresentServiceExcel.accumulateRatingStatusesByMonths()
        val resolvingStatusesByMonths = dataPresentServiceExcel.accumulateResolvingStatusesByMonths()
        val categoriesByNumberOfCallsAndResolvingStatuses = dataPresentServiceExcel.representCategoriesByNumberOfCallsAndResolvingStatuses()
        val sourcesByNumberOfCallsAndResolvingStatuses = dataPresentServiceExcel.representSourcesByNumberOfCallsAndResolvingStatuses()
        println()
    }
}

class DataPresentServiceExcel(excelFullFileName: String) {

    val processor = new ExcelProcessor(excelFullFileName)

    def accumulateRatingStatusesByMonths(
                                            affiliate: Option[String] = None
                                        ): Seq[RatingStatusesByMonths] = {

        val statuses = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameRatingStatus,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val dates = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameDateInput,
            numberValuesAsFlt = true,
            rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = processor.getCountOfNonEmptyRows.getOrElse(statuses.size)
        )

        assert(dates.length == statuses.length,
            "Error to accumulate rating statuses by months: different date and statuses lengths!"
        )

        val datesStatusesByMonth = (dates zip statuses).
            filter(x => x._1.length >= 7).
            map { x =>
                x._1.substring(3, 5).toInt -> x._2
            }.
            groupBy(x => x._1).
            map(x => x._1 -> x._2.unzip._2).
            map(x => x._1 -> x._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))

        val res = new Array[RatingStatusesByMonths](datesStatusesByMonth.size)
        var i = 0
        datesStatusesByMonth.foreach { monthStatusesWithCount =>
            val totalHardNegative = if (monthStatusesWithCount._2.contains(RatingStatus.HardNegative.toString))
                monthStatusesWithCount._2(RatingStatus.HardNegative.toString)
            else 0
            val totalSoftNegative = if (monthStatusesWithCount._2.contains(RatingStatus.SoftNegative.toString))
                monthStatusesWithCount._2(RatingStatus.SoftNegative.toString)
            else 0
            val totalNeutral = if (monthStatusesWithCount._2.contains(RatingStatus.Neutral.toString))
                monthStatusesWithCount._2(RatingStatus.Neutral.toString)
            else 0
            val totalPositive = if (monthStatusesWithCount._2.contains(RatingStatus.Positive.toString))
                monthStatusesWithCount._2(RatingStatus.Positive.toString)
            else 0
            res(i) = RatingStatusesByMonths(
                month = monthStatusesWithCount._1,
                totalHardNegative = totalHardNegative,
                totalSoftNegative = totalSoftNegative,
                totalNeutral = totalNeutral,
                totalPositive = totalPositive
            )
            i += 1
        }

        res
    }

    def accumulateResolvingStatusesByMonths(
                                               affiliate: Option[String] = None
                                           ): Seq[ResolvingStatusesByMonths] = {

        val statuses = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameResolvingStatus,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val dates = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameDateInput,
            numberValuesAsFlt = true,
            rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = processor.getCountOfNonEmptyRows.getOrElse(statuses.size)
        )

        assert(dates.length == statuses.length,
            "Error to accumulate resolving statuses by months: different date and statuses lengths!"
        )

        val datesStatusesByMonth = (dates zip statuses).
            filter(x => x._1.length >= 7).
            map { x =>
                x._1.substring(3, 5).toInt -> x._2
            }.
            groupBy(x => x._1).
            map(x => x._1 -> x._2.unzip._2).
            map(x => x._1 -> x._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))

        val res = new Array[ResolvingStatusesByMonths](datesStatusesByMonth.size)
        var i = 0
        datesStatusesByMonth.foreach { monthStatusesWithCount =>
            val totalResolved = if (monthStatusesWithCount._2.contains(ResolvingStatus.Resolved.toString))
                monthStatusesWithCount._2(ResolvingStatus.Resolved.toString)
            else 0
            val totalUnresolved = if (monthStatusesWithCount._2.contains(ResolvingStatus.Unresolved.toString))
                monthStatusesWithCount._2(ResolvingStatus.Unresolved.toString)
            else 0
            val totalInProgress = if (monthStatusesWithCount._2.contains(ResolvingStatus.InProgress.toString))
                monthStatusesWithCount._2(ResolvingStatus.InProgress.toString)
            else 0
            res(i) = ResolvingStatusesByMonths(
                month = monthStatusesWithCount._1,
                totalResolved = totalResolved,
                totalUnresolved = totalUnresolved,
                totalInProgress = totalInProgress
            )
            i += 1
        }

        res
    }

    def representCategoriesByNumberOfCallsAndResolvingStatuses(
                                                                  affiliate: Option[String] = None
                                                              ): Seq[CategoryByNumberOfCallsAndResolvingStatuses] = {

        val statuses = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameResolvingStatus,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val categories = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameCategoryInput,
            numberValuesAsFlt = true,
            rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = processor.getCountOfNonEmptyRows.getOrElse(statuses.size)
        )

        assert(statuses.length == categories.length,
            "Error to represent categories by number of calls and resolving statuses: different categories and statuses lengths!"
        )

        val categoriesWithStatuses = (categories zip statuses).
            filter(x => x._1 != "").
            groupBy(x => x._1).
            map(x => x._1 -> x._2.unzip._2).
            map(x => x._1 -> x._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))

        val res = new Array[CategoryByNumberOfCallsAndResolvingStatuses](categoriesWithStatuses.size)
        var i = 0
        categoriesWithStatuses.foreach { categoryWithStatuses =>
            val totalResolved = if (categoryWithStatuses._2.contains(ResolvingStatus.Resolved.toString))
                categoryWithStatuses._2(ResolvingStatus.Resolved.toString)
            else 0
            val totalUnresolved = if (categoryWithStatuses._2.contains(ResolvingStatus.Unresolved.toString))
                categoryWithStatuses._2(ResolvingStatus.Unresolved.toString)
            else 0
            val totalInProgress = if (categoryWithStatuses._2.contains(ResolvingStatus.InProgress.toString))
                categoryWithStatuses._2(ResolvingStatus.InProgress.toString)
            else 0
            res(i) = CategoryByNumberOfCallsAndResolvingStatuses(
                category = categoryWithStatuses._1,
                numberOfCalls = totalResolved + totalUnresolved + totalInProgress,
                totalResolved = totalResolved,
                totalUnresolved = totalUnresolved,
                totalInProgress = totalInProgress
            )
            i += 1
        }

        res
    }

    def representSourcesByNumberOfCallsAndResolvingStatuses(
                                                                  affiliate: Option[String] = None
                                                              ): Seq[CategoryByNumberOfCallsAndResolvingStatuses] = {

        val statuses = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameResolvingStatus,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val sources = processor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = DataPresentServiceExcel.ColNameSourceInput,
            numberValuesAsFlt = true,
            rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = processor.getCountOfNonEmptyRows.getOrElse(statuses.size)
        )

        assert(statuses.length == sources.length,
            "Error to represent sources by number of calls and resolving statuses: different sources and statuses lengths!"
        )

        val sourcesWithStatuses = (sources zip statuses).
            filter(x => x._1 != "").
            groupBy(x => x._1).
            map(x => x._1 -> x._2.unzip._2).
            map(x => x._1 -> x._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))

        val res = new Array[CategoryByNumberOfCallsAndResolvingStatuses](sourcesWithStatuses.size)
        var i = 0
        sourcesWithStatuses.foreach { sourceWithStatuses =>
            val totalResolved = if (sourceWithStatuses._2.contains(ResolvingStatus.Resolved.toString))
                sourceWithStatuses._2(ResolvingStatus.Resolved.toString)
            else 0
            val totalUnresolved = if (sourceWithStatuses._2.contains(ResolvingStatus.Unresolved.toString))
                sourceWithStatuses._2(ResolvingStatus.Unresolved.toString)
            else 0
            val totalInProgress = if (sourceWithStatuses._2.contains(ResolvingStatus.InProgress.toString))
                sourceWithStatuses._2(ResolvingStatus.InProgress.toString)
            else 0
            res(i) = CategoryByNumberOfCallsAndResolvingStatuses(
                category = sourceWithStatuses._1,
                numberOfCalls = totalResolved + totalUnresolved + totalInProgress,
                totalResolved = totalResolved,
                totalUnresolved = totalUnresolved,
                totalInProgress = totalInProgress
            )
            i += 1
        }

        res
    }

    def findTopEarlyCalls(
                    numberOfCalls: Int
                    ) : Seq[FullInfo] {

    }

}
