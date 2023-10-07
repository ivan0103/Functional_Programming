package dataset.util

object XMLDatafile {
  case class Badge(id : Int,
                    uid : Int,
                    name : String,
                    badgeDate : String,
                    badgeclass : Int,
                    tagbased : Boolean)
}
