import Control._

class Entry(val line:String) {
  val cols = line.split(",")

  val pattern = "(..)/(..)/(....)".r
  val pattern(day, month, year) = cols(1)
  val origDate = cols(1)
  val date = s"${year}${month}${day}".toInt
  val amount = cols(3).toDouble
  val category = cols(4)
  val comment = cols(5)

  val Vodafone = ".*(VODAFONE).*".r
  val Subsistence = ".*(COMPASS GROUP).*".r
  val BusinessLunch = ".*(WAGAMAMA).*".r
  val ComputerEquipment = ".*(APPLE ONLINE STORE).*".r
  val ComputerSoftware = ".*(ITUNES).*".r
  val Dividend = ".*(DIVIDEND *FT).*".r
  val HmrcPaye = ".*(HMRC CUMBERNAULD.*846PW).*".r
  val HmrcVAT = ".*(HMRC VAT).*".r
  val Charges = ".*(CHARGES[*]520.*).*".r
  val HostEurope = ".*(Terrafino Cslt.*520.*Host Europ).*".r
  val PrivHealth = ".*(VITALITYHEALTH).*".r
  val Invoice = ".*(M.*THREE).*".r
  val Loyalty = ".*(Loyalty Reward).*".r
  val Commission = ".*(COMMISSION).*".r
  val Clearsky = ".*(CLEARSKY CONTRACTO.*MF/0000.*5).*".r
  val Salary = ".*(203.*401.*MOBILE-CHANNEL).*".r
  
  def getInfo() = comment match {
    case Vodafone(v) => ("Telephone", "Vodafone")
    case Subsistence(v) => ("Subsistence", "Client restaurant")
    case BusinessLunch(v) => ("Entertainment", "Business lunch")
    case ComputerEquipment(v) => ("Computer Equipment", "Computer Equipment")
    case Dividend(v) => ("Dividend", "Dividend M. Hanitzsch")
    case ComputerSoftware(v) => ("Computer Software", "Software")
    case HmrcPaye(v) => ("HMRC", "NIC/PAYE")
    case Charges(v) => ("Bank charges", "Barclays")
    case HostEurope(v) => ("Internet", "Hosting")
    case PrivHealth(v) => ("Insurance", "Private health M. Hanitzsch")
    case Invoice(v) => ("Sales Receipt", "Invoice")
    case Loyalty(v) => ("Bank", "Loyalty reward")
    case Commission(v) => ("Bank charges", "Barclays")
    case Clearsky(v) => ("Accountancy", "New Quay")
    case Salary(v) => ("Net Salary", "Salary M. Hanitzsch September")
    case HmrcVAT(v) => ("HMRC", "VAT")
    case _ => (comment, comment)
  }
  def getDescr() = comment

  var (ptype, descr) = getInfo()

  override def toString(): String = s"$origDate,$amount,$ptype,$descr"

  def getDate() = date
  def printLine(balance: Double) = {
    val newBalance = balance+amount
    val s = "%s,%.2f,%.2f,%s,%s".format(origDate,amount,newBalance,ptype,descr)
    println(s)
    newBalance
  }
    
}

object CSVeezer extends App {

  def readTextFile(f:String): Vector[String] = {
    try {
      val lines = using(io.Source.fromFile(f)) {
        source => (for (line <- source.getLines
                        if !line.startsWith("Number,Date")) yield line).toList
      }
      lines.toVector
    } catch {
      case e: Exception => { println("Could not read file!"); Vector() }
    }
  }

  val rows = readTextFile(args(0))
  val entries = rows.map(new Entry(_)).sortBy(_.getDate())
  var b: Double = args(1).toDouble
  var l: String = ""
  entries.foreach {
    e => { b = e.printLine(b) }
  }
}
