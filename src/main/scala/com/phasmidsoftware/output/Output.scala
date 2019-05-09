package com.phasmidsoftware.output

import java.io.{Closeable, Flushable, Writer}

import scala.collection.mutable
import scala.language.postfixOps

/**
	* Trait to define the behavior of an AutoCloseable that is a pseudo-monadic IO type for output.
	*/
sealed trait Output extends AutoCloseable {
	/**
		* This method inserts a break into the output.
		* If the output is based on character sequences, then this method will cause a newline character to be inserted.
		*
		* @return this
		*/
	def insertBreak: Output

	/**
		* Append x to this Output.
		*
		* @param x an object of type Any
		* @return this Output.
		*/
	def :+(x: Any): Output

	/**
		* Append this to the Output formed by x.
		* NOTE: that this assumes that Output.apply is defined for String.
		*
		* @param x an Any
		* @return an Output, not necessarily this.
		*/
	def +:(x: Any): Output = Output(x.toString) ++ this

	/**
		* Concatenate this Output (first) and x (second).
		*
		* @param other the other Output object.
		* @return this Output.
		*/
	def ++(other: Output): Output

	/**
		* Concatenate each element of the iterator xs to this in turn.
		*
		* @param xs the iterator of Outputs.
		*           //		* @param separator an implicit separator
		* @return this Output.
		*/
	def ++(xs: Iterator[Output]): Output

	//	def ++(xs: Iterator[Output])(implicit separator: Output): Output

	/**
		* Concatenate each element of the iterable xs to this in turn.
		*
		* @param xs an iterable of Outputs.
		*           //		* @param separator an implicit separator
		* @return this Output.
		*/
	def ++(xs: Iterable[Output]): Output = this ++ xs.iterator

	//	def ++(xs: Iterable[Output])(implicit separator: Output): Output = this ++ xs.iterator
}

/**
	* Trait to define the behavior of an Output with a particular OutputType.
	*/
sealed trait TypedOutput extends Output {
	/**
		* The output type for this TypedOutput
		*/
	type OutputType

	/**
		* A "zero" element for this TypedOutput.
		* If OutputType is CharSequence, then we will return an empty String.
		*
		* @return a suitable empty OutputType.
		*/
	def zero: OutputType

	/**
		* Method to return an Output built from the OutputType x.
		*
		* @param x the input of type OutputType.
		* @return a new instance of Output.
		*/
	def unit(x: OutputType): Output

	/**
		* A converter from type Any to OutputType.
		*
		* @param x the input.
		* @return the input, expressed an an OutputType.
		*/
	def asOutputType(x: Any): OutputType

	/**
		* Method to append an x of type OutputType to this Output.
		*
		* @param x the input.
		* @return this Output.
		*/
	def append(x: OutputType): Output

	def :+(x: Any): Output =
		this append asOutputType(x)

	//	def ++(xs: Iterator[Output]): Output = xs.foldLeft(unit(zero))(_ ++ _)

	def ++(xs: Iterator[Output]): Output = if (xs.hasNext) {
		val first: Output = xs.next()

		def combine(o1: Output, o2: Output): Output =
			o1 :+ " " +: o2
		// TODO this almost works but what we really want is an immutable Output type, because, otherwise, the string in the separator can get consumed (if we use the "++" operator)
		//		xs.foldLeft[Output](first)(_ :+ separator +:  _)

		// TODO this is a kluge because we ignore the separator passed in
		//		xs.foldLeft(first)(_ ++ Output(" ") ++ _)
		xs.foldLeft(unit(zero) ++ first)(combine)
	}
	else
		unit(zero)
}

/**
	* Trait to define the behavior of a TypedOutput which has a buffer.
	*/
sealed trait BufferedOutput extends TypedOutput {
	/**
		* Method to determine if any data has been flushed to the backing sink.
		*
		* @return
		*/
	def isFlushed: Boolean

	/**
		* Method to determine if the buffer is empty.
		*
		* @return true if there is nothing in the buffer.
		*/
	def isEmpty: Boolean

	/**
		* Method to return the current content of the buffer.
		*
		* @return an OutputType
		*/
	def content: OutputType

	def contentBrief: String

	/**
		* Method to clear the contents of the buffer and return its value.
		*
		* @return the OutputType that was the contents of the buffer.
		*/
	def clear: OutputType

	/**
		* Method to return whether or not this Output is backed by real output type.
		*
		* @return true if this Output is backed.
		*/
	def isBacked: Boolean

	/**
		* Method to flush the buffer and return this.
		*
		* @return this Output.
		*/
	def flush: Output

	/**
		* Method to append input to the buffer.
		*
		* @param x the input to be appended.
		*/
	def appendToBuffer(x: OutputType): Unit

	/**
		* Method to determine if the buffer needs to be flushed, for example it's getting too big.
		*
		* @return true if it would be a good idea to flush the buffer.
		*/
	def needsToFlush: Boolean

	/**
		* Method to do a conditional flush of the buffer.
		*
		* @return this Output.
		*/
	def maybeFlush: Output = if (isBacked && needsToFlush) flush else this

	/**
		* Method to determine if the buffer is non-empty.
		*
		* @return true if the buffer is non-empty.
		*/
	def nonEmpty: Boolean = !isEmpty

	def ++(other: Output): Output =
		if (other eq this) this // NOTE: apparently redundant operation -- but should we be doubling the content of this?
		else concatenate(other)

	def append(x: OutputType): Output = {
		appendToBuffer(x)
		this
	}

	def unit(x: OutputType): Output = {
		appendToBuffer(x)
		this
	}

	def concatenate(output: Output): Output
}

sealed trait CharacterOutput extends TypedOutput {
	type OutputType = CharSequence

	def insertBreak: Output = append('\n'.toString)

	def zero: OutputType = ""

	def asOutputType(x: Any): CharSequence = x.toString

	def asLine(x: Any): Output = :+(x) insertBreak
}

sealed trait BackedOutput[A <: Appendable with AutoCloseable] extends TypedOutput {
	val appendable: A

	/**
		* This method copies the given content x to the backing object.
		*
		* @param x the content to be persisted.
		*/
	def persist(x: OutputType): Unit
}

sealed abstract class BufferedCharSequenceOutput[A <: Appendable with AutoCloseable with Flushable](val appendable: A, val sb: mutable.StringBuilder = new StringBuilder) extends CharacterOutput with BufferedOutput with BackedOutput[A] {

	/**
		* This method is essentially non-functional in the JVM.
		* It seemed like a good idea at the time!
		*/
	//	override def finalize(): Unit = close()

	def isEmpty: Boolean = sb.isEmpty

	def content: CharSequence = sb.mkString

	def clear: CharSequence = {
		val result = content
		sb.clear()
		result
	}

	def persist(x: CharSequence): Unit = if (isBacked) appendable.append(x)
	else throw OutputException(s"Cannot persist to unbacked Output")

	var flushed = false

	def isFlushed: Boolean = flushed

	/**
		* Flush the buffer (sb) to the output.
		*
		* @return an Output with an empty buffer.
		*/
	def flush: Output = {
		if (nonEmpty) {
			persist(content)
			clear
			flushed = true
		}
		this
	}

	def needsToFlush: Boolean = sb.length > NeedToFlush

	def appendToBuffer(x: CharSequence): Unit = sb.append(x)

	def close(): Unit = {
		flush
		//		for (c <- concatenations) {
		//			//				c.appendToBuffer(c.content)
		//			appendToBuffer(c.clear.asInstanceOf[CharSequence]) // TODO fix this up
		//			persist(content)
		//		}
		if (isBacked)
			appendable.flush()
		// TODO understand why we fail with "Terminated" condition if we try to close the appendable (in the case of a PrintWriter).
	}

	def concatenate(output: Output): Output = output match {
		case b: BufferedOutput =>
			if (isBacked) backedConcatenate(b)
			else concatenateOther(b)
		case _ =>
			throw OutputException(s"unsupported Output type: ${output.getClass}")
	}

	def contentBrief: String = content.toString.substring(0, Math.min(20, sb.length))

	private def backedConcatenate(output: BufferedOutput): Output = if (output.isBacked) {
		close()
		output
	}
	else
		concatenateOther(output)

	private def concatenateOther(output: BufferedOutput): Output = if (output.isFlushed)
		throw OutputException(s"cannot ++ non-backed Output with flushed Output")
	else
		transferContentFromBufferedOutput(output)

	//	{
	//		concatenations = concatenations :+ output
	//		this
	//	}


	private def transferContentFromBufferedOutput(o: BufferedOutput): Output = {
		appendToBuffer(o.clear.asInstanceOf[CharSequence]) // TODO fix this up
		maybeFlush
		o.close()
		this
	}

	/**
		* Arbitrary value just to avoid using up a lot of memory unnecessarily.
		*/
	private val NeedToFlush = 4096

	//	private var concatenations: Seq[BufferedOutput] = Nil
}

object Output {
	implicit def separator: Output = apply(" ")

	/**
		* Method to construct an empty WriterOutput based on writer.
		*
		* @param writer the writer which will back the new Output.
		* @return a new instance of Output.
		*/
	def apply(writer: Writer): Output = WriterOutput(writer)

	/**
		* Method to construct a new WriterOutput based on writer and the input s.
		*
		* @param writer the writer which will back the new Output.
		* @param s      the character sequence
		* @return a new instance of Output.
		*/
	def apply(writer: Writer, s: CharSequence): Output = apply(writer) :+ s

	/**
		* Method to construct an empty UnbackedOutput
		*
		* @return a new instance of UnbackedOutput.
		*/
	def empty: Output = UnbackedOutput()

	/**
		* Method to construct an UnbackedOutput based on the input s.
		*
		* @param s the character sequence
		* @return a new instance of UnbackedOutput.
		*/
	def apply(s: CharSequence): Output = empty :+ s

	def foldLeft[X](xs: Iterable[X])(o: Output)(f: (Output, X) => Output): Output = xs.foldLeft(o)(f)

	def reduce[X](xs: Iterable[X])(f: (Output, X) => Output): Output = foldLeft(xs)(empty)(f)

	def apply[X](xs: Iterable[X])(f: X => Output): Output = reduce(xs)(_ :+ f(_))
}

case class NonOutput() extends Appendable with Closeable with Flushable {
	def append(csq: CharSequence): Appendable = append(csq, 0, 0)

	def append(csq: CharSequence, start: Int, end: Int): Appendable = throw OutputException("cannot append to NonOutput")

	def append(c: Char): Appendable = append(c.toString)

	def close(): Unit = {}

	override def flush(): Unit = throw OutputException("cannot flush NonOutput")
}

case class UnbackedOutput() extends BufferedCharSequenceOutput[NonOutput](NonOutput()) {

	def isBacked: Boolean = false

	override def toString: String = sb.mkString
}

case class WriterOutput(writer: Writer) extends BufferedCharSequenceOutput[Writer](writer) {
	def isBacked: Boolean = true

	override def toString: String =
		s"WriterOutput: with writer: $writer and buffer: ${sb.mkString}"
}

case class OutputException(w: String) extends Exception(w)

trait Outputable {

	def output(output: Output): Output
}