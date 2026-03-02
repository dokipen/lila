package lila.openingPractice

import lila.db.dsl.{ *, given }

final class OpeningGroupRepo(
    private val coll: Coll,
    cacheApi: lila.memo.CacheApi
)(using Executor):

  import BsonHandlers.given

  private val allGroupsCache = cacheApi.unit[List[OpeningGroup]]:
    _.refreshAfterWrite(10.minutes).buildAsyncFuture: _ =>
      coll.list[OpeningGroup]($empty, 500)

  def findById(id: OpeningGroupId): Fu[Option[OpeningGroup]] =
    allGroupsCache.get({}).map(_.find(_.id == id))

  def findByFamily(familyId: OpeningFamilyId): Fu[List[OpeningGroup]] =
    allGroupsCache.get({}).map(_.filter(_.familyId == familyId))

  def listAll: Fu[List[OpeningGroup]] = allGroupsCache.get({})
