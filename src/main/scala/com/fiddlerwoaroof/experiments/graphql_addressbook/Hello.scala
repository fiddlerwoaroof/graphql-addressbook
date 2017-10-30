package com.fiddlerwoaroof.experiments.graphql_addressbook

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import sangria.ast.Document
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}
import sangria.macros.derive._
import sangria.marshalling.sprayJson._
import sangria.parser.QueryParser
import sangria.schema._
import spray.json.{JsObject, JsString, JsValue}

import cats.Monoid
import cats.instances.all._


import scala.util.{Failure, Success}

trait Identifiable {
  def id: String
}

class AddressBook {

  import AddressBook.PNHelper

  private val Contacts = List(
    Contact("1",
      EnglishName("John", Some("Q"), "Public"),
      Address("1103 Foo St.", "Somewhere", "CA", "93432", "USA"),
      pn"333-444-3333"),
    Contact("2",
      EnglishName("John", None, "Doe"),
      Address("4321 Bellview Blvd", "Youngstown", "OH", "43923", "USA"),
      pn"435-2039"),
    Contact("3",
      EnglishName("Jane", None, "Gray"),
      Address("1102 Willow Way", "Seattle", "WA", "10123", "USA"),
      pn"435-2039"),
    Contact("4",
      EnglishName("Bob", None, "Marley"),
      Address("1103 Maricopa Ave.", "Thousand Oaks", "CA", "93342", "USA"),
      pn"435-2039"),
    Contact("5",
      EnglishName("Mary", Some("E"), "Williamson"),
      Address("1102 Birch Ln", "Portland", "OR", "00123", "USA"),
      pn"435-2039"),
  )

  def contact(id: String): Option[Contact] =
    Contacts find (_.id == id)

  def addressesByPartialName(name: String, addressType: String = "home"): Seq[Address] =
    Contacts
      .filter(_.name.name.toLowerCase contains name)
      .map(_.address)

  def contacts: List[Contact] = Contacts
}

object AddressBook {

  implicit class PNHelper(private val sc: StringContext) extends AnyVal {
    def pn(args: Any*): PhoneNumber = {
      val str = sc.parts.head
      val parts = str.split('-')
      parts match {
        case Array(cc, ac, pref, suf) => PhoneNumber(cc.toInt, ac.toInt, pref.toInt, suf.toInt)
        case Array(ac, pref, suf) => PhoneNumber(1, ac.toInt, pref.toInt, suf.toInt)
        case Array(pref, suf) => PhoneNumber(1, 805, pref.toInt, suf.toInt)
      }
    }
  }

}

case class Picture(width: Int, height: Int, url: Option[String])

case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, suffix: Int) {
  def formatted = f"(${areaCode}%03d) ${prefix}%03d-${suffix}%04d"

  def localFormatted = f"${prefix}%03d-${suffix}%04d"

  def intlFormatted = f"+$countryCode (${areaCode}%03d) ${prefix}%03d-${suffix}%04d"
}

case class Address(address: String, city: String, state: String, zip: String, country: String)

trait Name {
  def name: String

  def sortName: String
}

case class EnglishName(first: String, middle: Option[String], last: String) extends Name {
  def name: String = s"$first ${middle.map(x => s"$x ").getOrElse("")}$last"

  def sortName: String = s"$last, $first"
}

case class Contact(id: String, name: Name, address: Address, phoneNumber: PhoneNumber) extends Identifiable {
  def picture(size: Int): Picture =
    Picture(
      width = size,
      height = size,
      url = Some(s"//cdn.com/$size/$id.jpg"))
}

object Hello extends App {
  implicit val PictureType: ObjectType[Unit, Picture] =
    deriveObjectType[Unit, Picture](
      ObjectTypeDescription("The product picture"),
      DocumentField("url", "Picture CDN URL"))

  val IdentifiableType: InterfaceType[Unit, Identifiable] =
    InterfaceType(
      "Identifiable",
      "Entity that can be identified",
      fields[Unit, Identifiable](
        Field("id", StringType, resolve = _.value.id)))

  implicit val AddressType: ObjectType[Unit, Address] =
    deriveObjectType[Unit, Address](
      ObjectTypeDescription("A Contact's address"))

  implicit val PhoneNumberType: ObjectType[Unit, PhoneNumber] =
    deriveObjectType[Unit, PhoneNumber](
      IncludeMethods("formatted", "localFormatted", "intlFormatted"))

  val NameType: InterfaceType[Unit, Name] =
    InterfaceType(
      "Name",
      "An interface for things that represent a name",
      fields[Unit, Name](
        Field("name", StringType, resolve = _.value.name),
        Field("sortName", StringType, resolve = _.value.sortName)))

  implicit val EnglishNameType: ObjectType[Unit, EnglishName] =
    deriveObjectType[Unit, EnglishName](
      Interfaces(NameType))

  val ContactType =
    deriveObjectType[Unit, Contact](
      Interfaces(IdentifiableType),
      IncludeMethods("picture"))

  val Id = Argument("id", StringType)

  val QueryType =
    ObjectType("Query", fields[AddressBook, Unit](
      Field("contact", OptionType(ContactType),
        description = Some("Return product with specific `id`."),
        arguments = Id :: Nil,
        resolve = c => c.ctx.contact(c arg Id)),

      Field("contacts", ListType(ContactType),
        description = Some("Returns all products"),
        resolve = _.ctx.contacts)
    ))

  val schema = Schema(QueryType)

  implicit val system = ActorSystem("sangria-server")
  implicit val materializer = ActorMaterializer()

  import system.dispatcher

  val route: Route =
    (post & path("graphql")) {
      entity(as[JsValue]) {
        requestJson => graphQLEndpoint(requestJson)
      }
    } ~
      get {
        getFromResource("graphiql.html")
      }

  def graphQLEndpoint(requestJson: JsValue) = {
    val JsObject(fields) = requestJson
    val JsString(query) = fields("query")

    val operation = fields.get("operationName") collect {
      case JsString(op) => op
    }

    val vars = fields.get("variables") match {
      case Some(obj: JsObject) => obj
      case _ => JsObject.empty
    }

    QueryParser.parse(query) match {
      case Success(queryAst) =>
        complete(executeGraphQLQuery(queryAst, operation, vars))
      case Failure(error) =>
        complete(BadRequest, JsObject("error" -> JsString(error.getMessage)))
    }
  }

  def executeGraphQLQuery(query: Document, op: Option[String], vars: JsObject) =
    Executor.execute(schema, query, new AddressBook,
      variables = vars,
      operationName = op)
      .map(OK -> _)
      .recover {
        case error: QueryAnalysisError => BadRequest -> error.resolveError
        case error: ErrorWithResolver => InternalServerError -> error.resolveError
      }

  Http().bindAndHandle(route, "0.0.0.0", 4930)
}

