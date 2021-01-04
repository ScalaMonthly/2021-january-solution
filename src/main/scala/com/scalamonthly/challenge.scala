package com.scalamonthly

import cats.parse.{Parser, Parser1}
import cats.parse.Parser._
import cats.parse.Rfc5234.{char => _, _}
import cats.syntax.all._

object challenge {

    import model._

    def parse(input: String): Either[Parser.Error, Game] = parser.parse(input).map(_._2)

    private val parser: Parser[Game] = {

        def parserFromMapping[A](mapping: Map[Char, A]): Parser[A] = {
            charIn(mapping.keys.toList).map(mapping.get(_).get)
        }

        val file: Parser[File] = {
            import File._
            val files = Map('a' -> A, 'b' -> B, 'c' -> C, 'd' -> D, 'e' -> E, 'f' -> F, 'g' -> G, 'h' -> H)
            parserFromMapping(files)
        }

        val rank: Parser[Rank] = {
            import Rank._
            val ranks = Map('1' -> One, '2' -> Two, '3' -> Three, '4' -> Four, '5' -> Five, '6' -> Six, '7' -> Seven, '8' -> Eight)
            parserFromMapping(ranks)
        }

        val square: Parser[Square] = (file ~ rank).map(Square.tupled)

        val pieceType: Parser[PieceType] = {
            import PieceType._
            val pieceTypes = Map('K' -> King, 'Q' -> Queen, 'R' -> Rook, 'B' -> Bishop, 'N' -> Knight)
            parserFromMapping(pieceTypes)
        }

        val check: Parser[CheckStatus] = {
            val c = char('+').as(CheckStatus.Check)
            val cm = char('#').as(CheckStatus.Checkmate)
            c orElse cm
        }

        val disambiguator: Parser[Disambiguator] = {
            import Disambiguator._
            val fileSource = file.map(FileSource)
            val rankSource = rank.map(RankSource)
            val fileAndRankSource = square.map(FileAndRankSource)
            oneOf(List(fileAndRankSource.backtrack, fileSource, rankSource))
        }

        val move: Parser[Move] = {
            import Move._
            val castle: Parser[Castle] = {
                val kingSide = string("O-O").as(Castle.KingSide)
                val queenSide = string("O-O-O").as(Castle.QueenSide)
                queenSide orElse kingSide
            }

            val nonPawn: Parser[Standard] = (pieceType ~ char('x').? ~ square ~ check.?)
                .map { case (((pt, x), s), c) => Standard(pt, s, None, x.isDefined, c) }
            val nonPawnD: Parser[Standard] = (pieceType ~ disambiguator ~ char('x').? ~ square ~ check.?)
                .map { case ((((pt, d), x), s), c) => Standard(pt, s, Some(d), x.isDefined, c) }
            val promotion: Parser[PieceType] = char('=') *> pieceType
            val pawn: Parser[Move] = ((file.soft ~ char('x')).? ~ square ~ promotion.? ~ check.?)
                .map { case (((fx, sq), promo), cs) =>
                    val pMove = PawnMove(fx.map(_._1), sq, fx.isDefined, cs)
                    promo.map(Promotion(pMove, _)).getOrElse(pMove)
                }

            oneOf(List(castle, nonPawnD.backtrack, nonPawn, pawn))
        }

        val turn: Parser1[Turn] = {
            import Turn._
            val anyWhiteSpace = oneOf1(List(wsp, cr, crlf, lf)).rep
            val turnNumber = digit.rep1 *> char('.') *> anyWhiteSpace
            val mv1 = move
            val mv2 = move.surroundedBy(anyWhiteSpace)
            val fullTurn = (mv1 ~ mv2).map(FullTurn.tupled)
            val partialTurn = (mv1 <* anyWhiteSpace).map(PartialTurn)
            val t = fullTurn.backtrack orElse partialTurn
            turnNumber *> t
        }

        val outcome: Parser[Outcome] = {
            import Outcome._
            val draw = string("1/2-1/2").as(Draw)
            val whiteWins = string("1-0").as(WhiteWins)
            val blackWins = string("0-1").as(BlackWins)
            val unknown = char('*').as(Unknown)
            oneOf(List(draw.backtrack, whiteWins, blackWins, unknown))
        }

        (turn.backtrack.rep1 ~ outcome).map(Game.tupled)
    }

}