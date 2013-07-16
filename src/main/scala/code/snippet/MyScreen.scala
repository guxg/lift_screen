package code.snippet

import code.model._
import net.liftweb._
import mapper._
import util._
import common._
import Helpers._
import http._
import js._
import SHtml._
import scala.xml._
import FieldBinding._

object LabelStyle {
  
  def htmlize[T](item: ChoiceItem[T]): NodeSeq =
    <label class="radio">{ item.xhtml } { item.key.toString }</label>

  def toForm[T](holder: ChoiceHolder[T]): NodeSeq =
    holder.items.flatMap(htmlize)
}

trait MyCssBoundLiftScreen extends CssBoundLiftScreen {

  override def defaultToAjax_? : Boolean = true

  override def allTemplate = savedDefaultXml

  protected def defaultAllTemplate = super.allTemplate

  override def defaultFieldNodeSeq: NodeSeq =
    <div>
      <label class="label field"></label>
      <span class="value fieldValue"></span>
      <span class="help"></span>
      <div class="errors">
        <div class="error"></div>
      </div>
    </div>
}

class MyScreen extends MyCssBoundLiftScreen with Loggable {

  //override def defaultToAjax_? : Boolean = false  // fileupload needs this
  override def defaultToAjax_? : Boolean = true // bindLocalAction needs this

  override protected def hasUploadField = true

  val countries = Country.findAll(OrderBy(Country.sort, Ascending))

  import net.liftweb.http.SHtml._
  implicit val countryPromot: PairStringPromoter[Country] =
    new PairStringPromoter[Country] { def apply(in: Country): String = in.name.is }

  val country = select[Country](S.?("register.country"),
    countries.head,
    countries,
    FieldBinding("country", Self),
    FieldTransform(_ => bindLocalAction("select [onchange]", updateCityInfo _)))

  def updateCityInfo: JsCmd = {
    logger.info("Selecting Country:%s".format(country.get.name))
    //how to bind to register_city_field?
    JsCmds.Noop
  }

  val gender = new Field {
    
    type ValueType = Box[Gender.Value]
    def manifest = scala.Predef.manifest[Box[Gender.Value]]

    def name = "gender"

    def default = Empty

    import Gender._
    val options: Seq[Gender] = Gender.values.toSeq
    val radio: ChoiceHolder[Gender] = SHtml.radioElem(options, default) {
      selected =>
        {
          logger.info("Choice: " + selected)
          set(selected)
        }
    }

    override def toForm = Full(LabelStyle.toForm(radio))

    override def binding: Box[FieldBinding] = Full(FieldBinding(name, Self))
  }

  val photo = new Field {

    override def uploadField_? = true
    type ValueType = Box[OnDiskFileParamHolder]

    def manifest = scala.Predef.manifest[Box[OnDiskFileParamHolder]]

    def name = "photo"

    def default = Empty

    override def toForm = Full(SHtml.fileUpload(ul => {
      ul match {
        case fph: OnDiskFileParamHolder => set(Full(fph))
        case _ => set(Empty)
      }
    }))

    override def binding: Box[FieldBinding] = Full(FieldBinding(name, Self))
  }

  def formName = "register"

  def finish() {

    logger.info("register is finished.")

    //logger.info("uploaed file is %".format())
    photo.get.map(f => { logger.info("uploaed file is %s[%s] saved at %s".format(f.fileName, f.length, f.localFile.getAbsolutePath())) })

    S.notice("Finished " + photo.get.map(f => f.length).openOr(0) + " bytes.")

  }

}