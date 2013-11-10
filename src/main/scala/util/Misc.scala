package util

import language.implicitConversions
import java.io.File
import scala.reflect.ClassTag

object misc {
  class FileLinesIterator(file: File) extends Iterator[String] {
    def this(fname: String) = this(new File(fname))
    val f = new java.io.BufferedReader(new java.io.FileReader(file))
    var n = f.readLine
    def next = {
      val r = n;
      n = f.readLine;
      if (n == null) {
        f.close
      }
      r
    }
    def hasNext = n != null
  }

  class RecursiveFileIterator(files: File*) extends Iterator[File] {
    def this(fname: String) = this(new File(fname))
    var stack = (files filter (f ⇒ f.isDirectory && !f.listFiles.isEmpty) map (_.listFiles.iterator)).toList
    def hasNext = !stack.isEmpty
    def next = {
      val n = stack.head.next
      if (!stack.head.hasNext) {
        stack = stack.tail
      }
      if (n.isDirectory && !n.listFiles.isEmpty) {
        stack = n.listFiles.iterator :: stack
      }
      n
    }
  }

  implicit val SymbolOrdering = Ordering.fromLessThan[Symbol](_.name < _.name)

  def now = compat.Platform.currentTime

  def time[A](s: String)(f: ⇒ A): A = {
    val tstart = now
    val ret = f
    val t = (now - tstart).toDouble / 1000
    printf("Spent %.3fs %s.\n", t, s)
    ret
  }

  def printToFile[T](fname: String)(thunk: ⇒ T): T = printToFile(new File(fname))(thunk)
  def printToFile[T](f: File)(thunk: ⇒ T): T = {
    val s = new java.io.FileOutputStream(f)
    try Console.withOut(s)(thunk)
    finally s.close()
  }

  object csv {
    def printCSV(l: Iterable[Iterable[Any]]) {
      for (i ← l) {
        val it = i.iterator
        for (j ← it) {
          print(j.toString)
          if (it.hasNext) print(",")
        }
        print("\n")
      }
    }
    def printCSV[A](l: Iterable[A])(f: A ⇒ Iterable[Any]): Unit = printCSV(l map f)
  }

  class RichIterable[A](l: Iterable[A]) extends Proxy {
    def self = l

    def allEquals = {
      if (l.isEmpty) true
      else {
        val it = l.iterator
        val first = it.next
        it forall (first == _)
      }
    }

    def countItems: Seq[(A, Int)] = {
      val s = collection.mutable.Map[A, Int]()
      for (i ← l) s(i) = s.getOrElse(i, 0) + 1
      var ret = new collection.mutable.ArrayBuffer[(A, Int)]
      for (i ← l) if (s(i) > 0) {
        ret += (i -> s(i))
        s(i) = 0
      }
      ret.result
    }

    def countItemsSorted: Seq[(A, Int)] = {
      val s = collection.mutable.Map.empty[A, Int]
      for (i ← l) s(i) = s.getOrElse(i, 0) + 1
      s.sortWith((kva, kvb) ⇒ kva._1.toString < kvb._1.toString)
    }

    def groupBy[B](f: A ⇒ B): Map[B, List[A]] = {
      val s = collection.mutable.Map[B, collection.mutable.ListBuffer[A]]()
      for (i ← l; g = f(i)) {
        val b = s.getOrElseUpdate(g, new collection.mutable.ListBuffer[A])
        b += i
      }
      Map() ++ (s map (kv ⇒ kv._1 -> kv._2.toList))
    }

    def groupByAndMap[B, C](fg: A ⇒ B, f: A ⇒ C): Map[B, List[C]] = {
      val s = collection.mutable.Map[B, collection.mutable.ListBuffer[C]]()
      for (i ← l; g = fg(i)) {
        val b = s.getOrElseUpdate(g, new collection.mutable.ListBuffer[C])
        b += f(i)
      }
      Map() ++ (s map (kv ⇒ kv._1 -> kv._2.toList))
    }

    def groupByAndApply[B, C](fg: A ⇒ B, f: Seq[A] ⇒ C): Map[B, C] = {
      val s = collection.mutable.Map[B, collection.mutable.ListBuffer[A]]()
      for (i ← l; g = fg(i)) {
        val b = s.getOrElseUpdate(g, new collection.mutable.ListBuffer[A])
        b += i
      }
      Map() ++ (s map (kv ⇒ kv._1 -> f(kv._2.toSeq)))
    }

    def sortWith(lt: (A, A) ⇒ Boolean): Seq[A] = {
      l.toSeq sortWith lt
    }

    def sortAsStrings = sortWith(_.toString < _.toString)

    def filterDuplicates: Iterator[A] = new Iterator[A] {
      val seen = new collection.mutable.HashSet[A]
      var i = l.iterator
      def next = {
        val ret = i.next
        seen += ret
        i = i.dropWhile(seen(_))
        ret
      }
      def hasNext = i.hasNext
    }

    def toArray[B >: A: ClassTag]: Array[B] = {
      l.toList.toArray
    }
  }
  implicit def traversable2richIterable[A](l: TraversableOnce[A]): RichIterable[A] = new RichIterable(l.toIterable)

  implicit class RichInputStream(val is: java.io.InputStream) {
    def copyToFile(f: File) {
      val os = new java.io.FileOutputStream(f)
      val buffer = new Array[Byte](1024 * 3)
      var r = is.read(buffer)
      while (r >= 0) {
        os.write(buffer, 0, r)
        r = is.read(buffer)
      }
      os.close
      is.close
    }

    def readAndDiscard {
      val buffer = new Array[Byte](1024 * 3)
      var r = is.read(buffer)
      while (r >= 0) {
        r = is.read(buffer)
      }
      is.close
    }
  }
}


