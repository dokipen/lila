package lila.openingPractice

import chess.format.Uci

case class OpeningLine(
    id: OpeningLineId,
    name: String,
    eco: String, // ECO code like "C45"
    moves: NonEmptyList[Uci],
    description: Option[String]
)
