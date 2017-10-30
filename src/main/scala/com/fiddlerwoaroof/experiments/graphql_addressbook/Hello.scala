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
      EnglishName("John", Some("Apple"), "Seed"),
      Map("home" -> Address("1103 Foo St.", "Ventura", "CA", "93003", "USA")),
      Map("home" -> pn"333-444-3333")),
    Contact("1",
      EnglishName("Bob", None, "Marley"),
      Map("home" -> Address("1103 Maricopa Ave.", "Ventura", "CA", "93003", "USA")),
      Map("home" -> pn"435-2039")),
  )

  def contact(id: String): Option[Contact] =
    Contacts find (_.id == id)

  def addressesByPartialName(name: String, addressType: String = "home"): Seq[Address] =
    Contacts
      .filter(_.name.name.toLowerCase contains name)
      .flatMap(_.addresses get addressType)

  def addresses: List[Contact] = Contacts
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

case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, suffix: Int)

case class Address(address: String, city: String, state: String, zip: String, country: String)

trait Name {
  def name: String

  def sortName: String
}

case class EnglishName(first: String, middle: Option[String], last: String) extends Name {
  def name: String = s"$first ${middle.map(x => s"$x ").getOrElse("")}$last"

  def sortName: String = s"$last, $first"
}

case class Contact(id: String, name: Name, addresses: Map[String, Address], phoneNumbers: Map[String, PhoneNumber]) extends Identifiable {
  def picture(size: Int): Picture =
    Picture(
      width = size,
      height = size,
      url = Some(s"//cdn.com/$size/$id.jpg"))
}

object Hello extends App {
  implicit val PictureType =
    deriveObjectType[Unit, Picture](
      ObjectTypeDescription("The product picture"),
      DocumentField("url", "Picture CDN URL"))

  val IdentifiableType =
    InterfaceType(
      "Identifiable",
      "Entity that can be identified",
      fields[Unit, Identifiable](
        Field("id", StringType, resolve = _.value.id)))

  val AddressType =
    deriveObjectType[Unit, Address](
      Interfaces(IdentifiableType),
      IncludeMethods("picture", "sortName", "name"))

  val Id = Argument("id", StringType)

  val QueryType =
    ObjectType("Query", fields[AddressBook, Unit](
      Field("contact", OptionType(ContactType),
        description = Some("Return product with specific `id`."),
        arguments = Id :: Nil,
        resolve = c => c.ctx.contact(c arg Id)),

      Field("addressByPartialName", ListType(AddressType),
        description = Some("Return product with specific `id`."),
        arguments = Id :: Nil,
        resolve = c => c.ctx.addressByPartialName(c arg Id)),

      Field("addresses", ListType(AddressType),
        description = Some("Returns all products"),
        resolve = _.ctx.addresses)
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

