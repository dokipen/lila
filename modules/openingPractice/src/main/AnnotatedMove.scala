package lila.openingPractice

import chess.format.Uci

case class AnnotatedMove(
    uci: Uci,
    comment: Option[String] // Teaching comment, e.g., "Controls the center"
)
