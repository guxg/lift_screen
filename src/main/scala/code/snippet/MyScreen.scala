package code.snippet

import code.model._
import net.liftweb._
import mapper._
import util._
import common._
import Helpers._
import http._
import js._
import JsCmd._
import JsCmds._
import SHtml._
import scala.xml._
import FieldBinding._
import net.liftweb.http.js.JsCmds.SetHtml
import java.util.regex.Pattern

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
  implicit val cityPromot: PairStringPromoter[City] =
    new PairStringPromoter[City] { def apply(in: City): String = in.name.is }

  val country = select[Country](S.?("register.country"),
    countries.head,
    countries,
    FieldBinding("country", Self),
    FieldTransform(_ => bindLocalAction("select [onchange]", replaceCityInfo _)))

  def updateCityInfo: JsCmd = {
    logger.info("Selecting Country:%s".format(country.get.name))
    city.setOtherValue(country.get.cities.all)
    replayForm
  }

  def replaceCityInfo: JsCmd = {
    logger.info("Selecting Country:%s".format(country.get.name))
    city.setOtherValue(country.get.cities.all)

    val snapshot = createSnapshot

    val restoreAction = S.formGroup(-1)(SHtml.hidden(() =>
      snapshot.restore()) % new UnprefixedAttribute("data-lift-screen-control", Text("restoreAction"), Null)) \ "@name"
      
    val jsR = Run("""
              $('#%s input[data-lift-screen-control="restoreAction"]').attr('name',"%s");
              """.format(NextId.get,restoreAction))

    city.toForm.map(n => {
      val fn = n \ "@name"
      SetHtml("register_city_field",
        <div id="register_city_field">
          <span class="label" for="{fn}">{ city.name }</span>
          { n }
        </div>)
    }).openOr(Noop) & jsR

  }

  val city = select[City](S.?("register.city"),
    country.get.cities.head,
    country.get.cities,
    FieldBinding("city", Self))

  val gender = makeField[Box[Gender.Value], Seq[Gender.Value]](
    S.?("enroll.gender"),
    Empty,
    f => Box !! SHtml.radioElem[Gender.Value](f.otherValue, f.is)(f.set(_)).toForm,
    OtherValueInitializerImpl[Seq[Gender.Value]](() => Gender.values.toSeq),
    FieldBinding("gender", Self))
    
  val number = Pattern.compile("\\d+{11,14}")
  val tel = field(S.?("register.tel"), "", trim, valRegex(number, S.?("register.tel.number.invalid")), FieldBinding("tel", Self), Help(Text(S.?("register.tel.help"))))
    

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

  val agree = field("Agree", false, FieldBinding("agree", Self), FormParam("class" -> "red"))

  def formName = "register"

  override def validate = {
    val errors = super.validate
    for (error <- errors) {
      error.field.uniqueFieldId match {
        case Full(fieldName) => S.appendJs(JsCmds.Run("$('[for=\"" + fieldName + "\"]').parent().addClass('error')"))
        case _ =>
      }
    }
    errors
  }

  def finish() {

    logger.info("register is finished.")
    logger.info("Country:%s".format(country.get))
    logger.info("City:%s".format(city.get))

    //logger.info("uploaed file is %".format())
    photo.get.map(f => { logger.info("uploaed file is %s[%s] saved at %s".format(f.fileName, f.length, f.localFile.getAbsolutePath())) })

    S.notice("Finished " + photo.get.map(f => f.length).openOr(0) + " bytes.")

  }

}