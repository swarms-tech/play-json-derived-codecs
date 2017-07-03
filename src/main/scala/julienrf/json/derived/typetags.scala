package julienrf.json.derived

import play.api.libs.json.{Reads, Json, OWrites, __}

trait TypeTagOWrites {
  def owrites[A](typeName: String, owrites: OWrites[A]): OWrites[A]
}

object TypeTagOWrites {

  val nested: TypeTagOWrites =
    new TypeTagOWrites {
      def owrites[A](typeName: String, owrites: OWrites[A]): OWrites[A] =
        OWrites[A](a => Json.obj(typeName -> owrites.writes(a)))
    }

  def flat(tagOwrites: OWrites[String]): TypeTagOWrites =
    new TypeTagOWrites {
      def owrites[A](typeName: String, owrites: OWrites[A]): OWrites[A] =
        OWrites[A](a => tagOwrites.writes(typeName) ++ owrites.writes(a))
    }

  /**
    * Encodes an tagged type, discards the type name
    *
    * For instance, consider the following type definition:
    *
    * {{{
    *   sealed trait Foo
    *   case class Bar(s: String, i: Int) extends Foo
    *   case object Baz extends Foo
    * }}}
    *
    * The JSON representation of `Bar("quux", 42)` is the following JSON object:
    *
    * {{{
    *   {
    *     "s": "quux",
    *     "i": 42
    *   }
    * }}}
    */
  def flat: TypeTagOWrites =
    new TypeTagOWrites {
      def owrites[A](typeName: String, owrites: OWrites[A]): OWrites[A] =
        OWrites[A](a => owrites.writes(a))
    }
}

trait TypeTagReads {
  def reads[A](typeName: String, reads: Reads[A]): Reads[A]
}

object TypeTagReads {

  val nested: TypeTagReads =
    new TypeTagReads {
      def reads[A](typeName: String, reads: Reads[A]): Reads[A] =
        (__ \ typeName).read(reads)
    }

  def flat(tagReads: Reads[String]): TypeTagReads =
    new TypeTagReads {
      def reads[A](typeName: String, reads: Reads[A]): Reads[A] =
        tagReads.filter(_ == typeName).flatMap(_ => reads)
    }

}
