package dataset.util

import java.util.Date

/**
  * This object contains all case classes used in the dataset of commits.
  * Do not change this file, otherwise autograding may fail.
  */
object Commit {
// only missing _id (_id needs to be removed from the picture...
    case class Commit(node_id: String,
                      sha: String,
                      url: String,
                      commit: CommitData,
                      author: Option[User],
                      committer: Option[User],
                      parents: List[Parent],
                      stats: Option[Stats],
                      files: List[File])

    case class CommitData(author: CommitUser,
                          committer: CommitUser,
                          message: String,
                          tree: Tree,
                          comment_count: Int,
                          verification: Verification)

    case class CommitUser(name: String, email: String, date: Date)

    case class User(id: Long,
                    login: String,
                    avatar_url: String,
                    `type`: String,
                    site_admin: Boolean)

    case class Verification(verified: Boolean,
                            reason: String,
                            signature: Option[String],
                            payload: Option[String])

    case class Stats(total: Int, additions: Int, deletions: Int)

    case class File(sha: Option[String],
                    filename: Option[String],
                    status: Option[String],
                    additions: Int,
                    deletions: Int,
                    changes: Int,
                    blob_url: Option[String],
                    raw_url: Option[String],
                    contents_url: Option[String],
                    patch: Option[String])

    case class Parent(sha: String)

    case class Tree(sha: String)
}
