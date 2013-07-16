package code.model

import net.liftweb._
import util._
import common._
import http._

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import scala.xml._

object City extends City with LongKeyedMetaMapper[City] with Loggable {

  override def dbTableName = "t_city"
  override def dbAddTable = Full(populate _)

  private def populate {

    //if (City.count == 0) {
      logger.info("Populating City Table ...%s".format(City.count))

      City.create.name("WDC").sort(1).country(1).save()
      City.create.name("NY").sort(2).country(1).save()

      City.create.name("Beijing").sort(1).country(2).save()
      City.create.name("Shanghai").sort(2).country(2).save()
    //}
  }

}

class City extends LongKeyedMapper[City] with IdPK {

  def getSingleton = City

  object country extends MappedLongForeignKey(this, Country) {

    override def dbColumnName = "country_id"
    override def displayName = "Country"

    override def validSelectValues = Full(
      Country.findMap(OrderBy(Country.id, Ascending)) {
        case p: Country => Full(p.id.is -> p.name.is)
      })
  }

  object name extends MappedString(this, 100) {
    override def displayName = S.?("city.name")
  }

  object sort extends MappedInt(this) {
    override def dbColumnName = "sort"
  }

}