
import com.typesafe.config.{Config, ConfigFactory}

import java.io.{File, FileNotFoundException}
import scala.collection.mutable.ListBuffer
import scala.io.Source




object movieDataProcessor extends App {

  try {


    val applicationConf = ConfigFactory.load("application.conf") //loading from configuration file
    val filepath = applicationConf.getString("app.filepath") //getting file path from configuration file

    def open(path: String): File = new File(filepath) //opening the CSV file


    //  reader.foreach(println)
    //to get list of array of string after reading
    def listOfRecords(filepath: String): ListBuffer[Array[String]] = {
      var list = new ListBuffer[Array[String]]()

      val source = Source.fromFile(filepath)
      source.getLines().take(10000).foreach(next => {
        var array: Array[String] = next.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<! )(?<! )") //splits the rows of the records and forms array of string
        list += array // adding the array to list
      })
      source.close()
      list.drop(1) //dropping the first row which contains the headings
    }


    //Checkpoint1
    def generateTitles(director: String, fromYear: Int, toYear: Int): Unit = {
    printLines(1)
      var list = listOfRecords(filepath)

      list = list.filter(_(3).toInt >= fromYear) // year>= given fromyear
        .filter(_(3).toInt <= toYear) // year<=toyear
        .filter(_(9).equals(director)) //director==given director
      //    list.foreach(res=>println(res(3)+" "+res(9)))
      println(s" Titles directed by given $director in the from $fromYear to $fromYear range")
      list.foreach(res => println(res(1)))



    }
      generateTitles(director = "D.W. Griffith",fromYear = 1910, toYear = 1980)



    //checkpoint2
    def generateEnglishTitle(userReviews: Int): Unit = {
    printLines(2)
      var list = listOfRecords(filepath)


      list = list.filter(_(8).equals("English")) //filter english titles
        .filter(_.lift(20)
          .flatMap(_.toIntOption).getOrElse(0) > userReviews)
      list = list.sortWith(_(20).toInt > _(20).toInt)

      //    list.foreach(c=>println(c(8)+" "+c(20)))
      println(s"English titles which have user reviews more than given user reviews $userReviews")
      list.foreach(res => println(res(1)))

    }

    generateEnglishTitle(10)

    //checkpoint3: 3. Generate highest budget titles for the given year and country filters
      def generateHighestBudgetTitles(year:Int, country: String): Unit = {
        printLines(3)
        var list=listOfRecords(filepath)

        list=list.filter(_(3).toInt==year) //filter according to given year
          .filter(_(7).equals(country)) //filter if country is equal to given country
          .filter(_(16).nonEmpty) // filter non empty budgets
          .sortBy(-_(16) //sort by decreaing order of budgets
            .replaceAll("[^\\d]","").toInt)
          .take(1)// pick the highest budget
//        list.foreach(res=>println(res(16)))
        println(s"Highest budget title for $year and $country")
        list.foreach(res=>println(res(1)))
      }
     generateHighestBudgetTitles(1914,"USA")

    //checkpoint4:Generate report of longest duration title for the given country filter, no of minimum votes
    // filter and sort by duration in descending order
    def generateLongestDurationTitle(country: String,minVotes: Int):Unit={
      printLines(4)
     var list=listOfRecords(filepath)

    list = list.filter(_(7).equals(country)) //filter list based on given country
      .filter(_(15).toInt >= minVotes) // filter list where votes> given min votes
    list = list.sortWith(_(6).toInt > _(6).toInt) //sort in descending order of duration
    list = list.take(1) //take the highest values
      println(s"Longest duration title for $country and has min votes $minVotes")
    list.foreach(res => println(res(1)))

  }
    generateLongestDurationTitle("Germany",530)

    //checkpoint5: Generate language wise report to count the titles for the given budget range and country filter and sort with count descending
    //Output: language : count
    def generateLanguageWiseReport(fromBudget: String, toBudget: String, country: String):Unit={
      printLines(5)
      var fromBudgetInt=fromBudget.replaceAll("\\D","").toInt //to convert the budget to int
      var toBudgetInt=toBudget.replaceAll("\\D","").toInt

      var list=listOfRecords(filepath)

      list=list.filter(_.length>=17)
        .filter(_(16).nonEmpty) // to check if budget is nonempty
        .filter(_(16).replaceAll("\\D","").toInt >= fromBudgetInt) //filter list to get values>=give frombudget
        .filter(_(16).replaceAll("\\D","").toInt <= toBudgetInt)   //filter list to get values<=give tobudget
        .filter(_(7).equals(country))

      var result=list.filter(_(8).nonEmpty).flatMap(_(8).split(",")) // split languages by comma and create a new row for each language
        .map(_.trim) // trim leading/trailing spaces from each language
        .map(_.replaceAll("^['\"]+|['\"]+$", ""))//trim leading/trailing inverted commas
        .groupBy(identity) // group by language
        .mapValues(_.size).toList.sortBy(-_._2)
      println(s"Count the titles for the given budget range $fromBudget to $toBudget and country $country")
       result.foreach(res=>println(res._1+": "+ res._2))

    }
    generateLanguageWiseReport("$5000","$100000","USA")

   def printLines(n: Int): Unit = {
     println()
     println(s"-------------------------------CHECKPOINT$n---------------------------------")
     println()
   }

  }
  catch {
    case x: FileNotFoundException => {
      println(x)
    }
    case y: NumberFormatException => {
      println(y)
    }
  }
}
