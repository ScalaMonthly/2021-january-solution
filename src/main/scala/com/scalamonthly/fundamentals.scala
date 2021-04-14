package com.scalamonthly

import cats.parse.{Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Rfc5234.{char => _, _}
import cats.syntax.all._
import cats.data.NonEmptyList

/**
  * Here is a set of problems that are designed to help you learn the basics of working with cats-parse.
  * If you already know how to use cats-parse or just don't want to do these problems, feel free to skip ahead
  * to the main challenge.
  * 
  * If you need help while working on these fundamentals, be sure to check out the walk-through for answers and
  * explanations of how each of these problems is solved.
  */
object fundamentals {

    sealed abstract class Binary extends Product with Serializable
    object Binary {
        case object Zero extends Binary
        case object One extends Binary
    }
    /**
      * Create a parser that accepts a single character as input where that character can be either a '1' or a '0'.
      * The parser should return parsed values using the sealed hierarchy `Binary` above.
      * 
      * Example input: 1
      * Example output: Binary.One
      */
    val one: Parser[Binary] = {
        char('0').as(Binary.Zero) orElse char('1').as(Binary.One)
    }

    final case class LetterAndNumber(value: String) extends AnyVal
    /**
      * Create a parser that accepts two characters as input where the first character is any letter (case insensitive)
      * and the second character is any number (from 0-9 inclusive).
      * 
      * Example input: H8
      * Example output: LetterAndNumber("H8")
      * 
      * Hint: Look inside of `cats.parse.Rfc5234` for help with parsing numbers and letters.
      */
    val two: Parser[LetterAndNumber] = {
        (alpha ~ digit).string.map(LetterAndNumber)
    }

    final case class BinaryList(value: NonEmptyList[Binary]) extends AnyVal
    /**
      * Create a parser that is similar to the parser we built in fundamental `one`, but accepts a string of
      * multiple '1's and '0's and parses them into a `NonEmptyList`. This means that there must be at least 1
      * character for the input to be valid.
      * 
      * Example input: 1001
      * Example output: BinaryList(NonEmptyList.of(Binary.One, Binary.Zero, Binary.Zero, Binary.One))
      */
    val three: Parser[BinaryList] = {
        val binary = char('0').as(Binary.Zero) orElse char('1').as(Binary.One)
        binary.rep.map(BinaryList)
    }

    final case class Name(value: String) extends AnyVal
    /**
      * Build a parser for names where a name must contain only alpha characters (meaning letters only, case insensitive),
      * spaces, and single quotes. The first letter of the name must be an alpha character. After that any of the inputs are acceptable.
      * 
      * Invalid example input: 'Bob (leading single quote is not allowed)
      * Valid example input: Brian O'Brien III
      * Example output: Name("Brian O'Brien III")
      */
    val four: Parser[Name] = {
        val allowedChars = List(char('\''), alpha, sp)
        (alpha ~ oneOf(allowedChars).rep).string.map(Name)
    }

    final case class Score(left: Int, right: Int)
    /**
      * Build a parser that can understand sports scores for two opposing teams. The score should be represented as
      * the two teams' scores (numerical, any length) separated by a hyphen that has a single space on either side.
      * 
      * Example input: 123 - 456
      * Example output: Score(123, 456)
      */
    val five: Parser[Score] = {
        val multiDigit = digit.rep.string.map(_.toInt)
        ((multiDigit <* char('-').surroundedBy(sp)) ~ multiDigit).map(Score.tupled)
    }

    sealed abstract class MyTuple extends Product with Serializable
    object MyTuple {
        final case class Two(one: String, two: String) extends MyTuple
        final case class Three(one: String, two: String, three: String) extends MyTuple
    }
    /**
      * Create a parser that can identify tuples that have either two or three elements inside. Tuples should be
      * formatted as comma-separated values with no whitespace. Parsed values should be placed inside of the `MyTuple`
      * sealed hierarchy (located above).
      * 
      * Example input 1: hello,world
      * Example output 1: MyTuple.Two("hello", "world")
      * 
      * Example input 2: one,2,three
      * Example output 2: MyTuple.Three("one", "2", "three")
      */
    val six: Parser[MyTuple] = {
        val comma = char(',')
        val item = alpha.rep.string
        val two = ((item <* comma) ~ item).map(MyTuple.Two.tupled)
        val three = (item ~ item.surroundedBy(comma) ~ item).map { case ((a, b), c) => MyTuple.Three(a, b, c) }
        three.backtrack orElse two
    }

    final case class UserName(value: String) extends AnyVal
    /**
      * Create a parser for UserNames where a UserName is composed of segments of alpha characters (letters only, case insensitive)
      * separated by dots. Each alpha segment must contain at least 1 character and the input cannot end with a dot.
      * 
      * Example input: jess.day.one
      * Example output: UserName("jess.day.one")
      */
    val seven: Parser[UserName] = {
        val name = alpha.rep.string
        ((name.soft ~ char('.')).rep0.with1 ~ name).string.map(UserName)
    }

    /**
      * Construct a parser that will return successfully for any input that is composed entirely of `allowedChars`.
      * The string returned in a successful result should just be the same as the string that was being parsed to begin with.
      * 
      * Example input:
      *     Allowed chars: List('a', 'b', 'c')
      *     Input string: "abc"
      * Example output: "abc"
      *
      * @param allowedChars chars that are to be allowed inside of this string
      * @return a parser returning a string when the input is valid
      */
    def eight(allowedChars: List[Char]): Parser[String] = {
        charIn(allowedChars).rep.string
    }

    final case class CarType(make: String, model: Option[String])

    /**
      * Create a parser for CarTypes where a CarType is a make and a model of a car.
      * The make and model each may only be composed of alphabetical characters. The input
      * will be given as a make and model separated by a space. The model may or may not be provided.
      * 
      * Example input: Nissan Versa
      * Example output: CarType("Nissan", Some("Versa"))
      */
    val nine: Parser[CarType] = {
      val make = alpha.rep.string
      val model = alpha.rep.string.?
      ((make <* sp.?) ~ model).map(CarType.tupled)
    }

}