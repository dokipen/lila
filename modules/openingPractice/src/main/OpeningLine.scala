package lila.openingPractice

import chess.format.Uci

case class OpeningLine(
    id: OpeningLineId,
    name: String,
    eco: Option[String], // ECO code like "C45", None for community content
    moves: NonEmptyList[Uci],
    description: Option[String]
)
