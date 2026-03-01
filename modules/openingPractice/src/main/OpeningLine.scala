package lila.openingPractice


case class OpeningLine(
    id: OpeningLineId,
    name: String,
    eco: Option[String], // ECO code like "C45", None for community content
    moves: NonEmptyList[AnnotatedMove],
    description: Option[String]
)
