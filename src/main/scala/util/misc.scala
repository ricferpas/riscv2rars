package util

object misc {
  implicit class RichIterable[A](l: Iterable[A]) {
    def filterDuplicates: Seq[A] = new Iterator[A] {
      val seen = new collection.mutable.HashSet[A]
      var i = l.iterator
      def next() = {
        val ret = i.next()
        seen += ret
        i = i.dropWhile(seen(_))
        ret
      }
      def hasNext = i.hasNext
    }.toSeq
  }
}
