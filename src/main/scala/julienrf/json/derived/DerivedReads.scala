package julienrf.json.derived

import play.api.libs.json.{Reads, __, JsError}
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, Coproduct, :+:, Inr, Inl, CNil}

trait DerivedReads[A] {
  def reads: Reads[A]
}

object DerivedReads extends DerivedReadsInstances

trait DerivedReadsInstances extends DerivedReadsInstances1 {

  implicit val readsCNil: DerivedReads[CNil] =
    new DerivedReads[CNil] {
      val reads = Reads[CNil] { _ => JsError("Unable to read this type") }
    }

  implicit def readsCoProduct[K <: Symbol, L, R <: Coproduct](implicit
    typeName: Witness.Aux[K],
    readL: Lazy[DerivedReads[L]],
    readR: Lazy[DerivedReads[R]]
  ): DerivedReads[FieldType[K, L] :+: R] =
    new DerivedReads[FieldType[K, L] :+: R] {
      def reads =
        (__ \ typeName.value.name).read(readL.value.reads)
          .map[FieldType[K, L] :+: R](l => Inl(field[K](l)))
          .orElse(readR.value.reads.map { r => Inr(r) })
  }

  implicit val readsHNil: DerivedReads[HNil] =
    new DerivedReads[HNil] {
      val reads = Reads.pure[HNil](HNil)
    }

  implicit def readsLabelledHList[A, K <: Symbol, H, T <: HList](implicit
    fieldName: Witness.Aux[K],
    readH: Lazy[Reads[H]],
    readT: Lazy[DerivedReads[T]]
  ): DerivedReads[FieldType[K, H] :: T] =
    new DerivedReads[FieldType[K, H] :: T] {
      def reads =
        for {
          h <- (__ \ fieldName.value.name).read(readH.value)
          t <- readT.value.reads
        } yield field[K](h) :: t
    }
}

trait DerivedReadsInstances1 {

  implicit def readsGeneric[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    derivedReads: Lazy[DerivedReads[R]]
  ): DerivedReads[A] =
    new DerivedReads[A] {
      def reads = derivedReads.value.reads.map(gen.from)
    }

}
