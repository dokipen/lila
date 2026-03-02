package lila.openingPractice

import lila.db.dsl.{ *, given }

final class OpeningGroupRepo(
    private val coll: Coll,
    cacheApi: lila.memo.CacheApi
)(using Executor):

  import BsonHandlers.given

  private val allGroupsCache = cacheApi.unit[List[OpeningGroup]]:
    _.refreshAfterWrite(10.minutes).buildAsyncFuture: _ =>
      coll.list[OpeningGroup]($empty)

  def findById(id: OpeningGroupId): Fu[Option[OpeningGroup]] =
    coll.byId[OpeningGroup](id.value)

  def findByFamily(familyId: OpeningFamilyId): Fu[List[OpeningGroup]] =
    coll.list[OpeningGroup]($doc("familyId" -> familyId))

  def listAll: Fu[List[OpeningGroup]] = allGroupsCache.get({})
