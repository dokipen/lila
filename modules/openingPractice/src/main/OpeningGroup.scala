package lila.openingPractice

import chess.Color

case class OpeningGroup(
    id: OpeningGroupId,
    name: String,
    familyId: OpeningFamilyId,
    tier: Int, // difficulty level
    lines: NonEmptyList[OpeningLineId],
    color: Color,
    prerequisites: List[OpeningGroupId]
)
