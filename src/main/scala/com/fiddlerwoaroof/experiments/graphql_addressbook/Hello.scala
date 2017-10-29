package com.fiddlerwoaroof.experiments.graphql_addressbook

import sangria.macros.derive._
import sangria.macros._
import sangria.schema._

import sangria.execution._
import sangria.marshalling.circe._

import io.circe.Json

trait Identifiable {
  def id: String
}

case class Picture(width: Int, height: Int, url: Option[String])

case class Product(id: String, name: String, description: String) extends Identifiable {
  def picture(size: Int): Picture =
    Picture(
      width = size,
      height = size,
      url = Some(s"//cdn.com/$size/$id.jpg"))
}

object Hello extends Greeting with App {
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

  val ProductType =
    deriveObjectType[Unit, Product](
      Interfaces(IdentifiableType),
      IncludeMethods("picture"))

  class ProductRepo {
    private val Products = List(
      Product("1", "Cheescake", "Tasty"),
      Product("2", "HEalth Potion", "+50 HP"),
    )

    def product(id: String): Option[Product] =
      Products find (_.id == id)

    def products: List[Product] = Products
  }

  val Id = Argument("id", StringType)

  val QueryType =
    ObjectType("Query", fields[ProductRepo, Unit](
      Field("product", OptionType(ProductType),
        description = Some("Return product with specific `id`."),
        arguments = Id :: Nil,
        resolve = c => c.ctx.product(c arg Id)),

      Field("products", ListType(ProductType),
        description = Some("Returns all products"),
        resolve = _.ctx.products)
    ))

  val query = graphql"""
      query MyProduct {
        product(id: "2") {
          name
          description

          picture(size: 500) {
            width, height, url
          }
        }

        products {
          name
        }
      }"""
}

trait Greeting {
  lazy val greeting: String = "hello"
}

