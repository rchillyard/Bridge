/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import scala.reflect.ClassTag

/**
  * Trait to define methods for rendering instances of case classes (with their various parameters),
  * containers (Seq and Option), etc..
  */
trait Loggables {

  /**
    * Method to return a Loggable[ Seq[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[ Seq[T] ]
    */
  def sequenceLoggable[T: Loggable]: Loggable[Seq[T]] = (ts: Seq[T]) => {
    val tl = implicitly[Loggable[T]]
    s"[${tl.toLog(ts.head)}, ... (${math.max(ts.size - 2, 0)}), ... ${tl.toLog(ts.last)}]"
  }

  /**
    * Method to return a Loggable[ Map[K, T] ].
    *
    * @tparam K the type of the keys.
    * @tparam T the underlying type of the values.
    * @return a Loggable[ Map[K, T] ]
    */
  def mapLoggable[K, T: Loggable](bookends: String = "{}"): Loggable[Map[K, T]] = (tKm: Map[K, T]) => {
    def z(k: K, t: T): String = k + ":" + implicitly[Loggable[T]].toLog(t)

    tKm.map((z _).tupled).mkString(bookends.substring(0, 1), ",", bookends.substring(1, 2))
  }

  /**
    * Method to return a Loggable[ Option[T] ].
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[ Option[T] ].
    */
  def optionLoggable[T: Loggable]: Loggable[Option[T]] = (to: Option[T]) => to match {
    case Some(t) => s"Some(${implicitly[Loggable[T]].toLog(t)})"
    case None => "None"
  }

  /**
    * Method to return a Loggable[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
    * It probably shouldn't ever be used in practice. It can cause strange initialization errors!
    * This note may be irrelevant now that we have overridden convertString to fix issue #1.
    *
    * @param construct a function P => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P0 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[T].
    */
  def toLog1[P0: Loggable, T <: Product : ClassTag](construct: P0 => T): Loggable[T] = (t: T) => {
    val Array(p0) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
    t.productPrefix + mapLoggable[String, String]("()").toLog(Map(p0 -> valueToLog[P0, T](t, 0)
    )
    )
  }

  /**
    * Method to return a Loggable[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
    *
    * @param construct a function (P1,P2) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[T].
    */
  def toLog2[P0: Loggable, P1: Loggable, T <: Product : ClassTag](construct: (P0, P1) => T): Loggable[T] = (t: T) => {
    val Array(p0, p1) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
    t.productPrefix + mapLoggable[String, String]("()").toLog(Map(
      p0 -> valueToLog[P0, T](t, 0),
      p1 -> valueToLog[P1, T](t, 1)
    )
    )
  }

  /**
    * Method to return a Loggable[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
    *
    * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[T].
    */
  def toLog3[P0: Loggable, P1: Loggable, P2: Loggable, T <: Product : ClassTag](construct: (P0, P1, P2) => T): Loggable[T] = (t: T) => {
    val Array(p0, p1, p2) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
    t.productPrefix + mapLoggable[String, String]("()").toLog(Map(
      p0 -> valueToLog[P0, T](t, 0),
      p1 -> valueToLog[P1, T](t, 1),
      p2 -> valueToLog[P2, T](t, 2)
    )
    )
  }

  /**
    * Method to return a Loggable[T] where T is a 4-ary Product and which is based on a function to convert a (P0,P1,P2,P3) into a T.
    *
    * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Loggable[T].
    */
  def toLog4[P0: Loggable, P1: Loggable, P2: Loggable, P3: Loggable, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T): Loggable[T] = (t: T) => {
    val Array(p0, p1, p2, p3) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
    t.productPrefix + mapLoggable[String, String]("()").toLog(Map(
      p0 -> valueToLog[P0, T](t, 0),
      p1 -> valueToLog[P1, T](t, 1),
      p2 -> valueToLog[P2, T](t, 2),
      p3 -> valueToLog[P3, T](t, 3)
    )
    )
  }

  def valueToLog[P: Loggable, T <: Product](t: T, i: Int): String = implicitly[Loggable[P]].toLog(t.productElement(i).asInstanceOf[P])

}

object Reflection {

  /**
    * This method is borrowed directly from Spray JsonReader.
    *
    * @param classTag rhw class tag.
    * @return an Array of String.
    */
  def extractFieldNames(classTag: ClassTag[_]): Array[String] = {
    import java.lang.reflect.Modifier

    import scala.util.control.NonFatal

    val clazz = classTag.runtimeClass
    try {
      // NOTE: copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // ... that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
      val fields = clazz.getDeclaredFields.filterNot { f =>
        import Modifier._
        (f.getModifiers & (TRANSIENT | STATIC | 0x1000 /* SYNTHETIC*/)) > 0
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName)
      fields.map(f => f.getName)
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
        "for '" + clazz.getName + "', please use the 'jsonFormat' overload with explicit field name specification", ex)
    }
  }

}
