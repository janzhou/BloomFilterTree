package org.janzhou.hashing

import scala.reflect.ClassTag

class BloomFilterTree[T:ClassTag](
  val false_positive_rate:Double = 0.001
) {

  class Node[T:ClassTag](val bf: BloomFilter[T], val childs: Iterable[Node[T]], val values: Iterable[T])

  private var leaf = Iterable[Node[T]]()
  private var root = new Node(null, null, null)

  def add(that:Iterable[Iterable[T]]):this.type = {
    val input = (leaf.map(_.values) ++ that)
    val size = input.map( _.size ).fold(0)(_ + _)
    leaf = input.map(array => {
      val bf:BloomFilter[T] = BloomFilter.optimallySized[T](
        size, false_positive_rate
      )

      bf += array
      new Node(bf, Iterable[Node[T]](), array)
    })

    def level(nodes:Iterable[Node[T]], groupSize:Int):Iterable[Node[T]] = {
      nodes.grouped(groupSize).toList.map(group => {
        val bf = BloomFilter.optimallySized[T]( size, false_positive_rate )
        group.foreach( bf |= _.bf )
        new Node(bf, group, null)
      })
    }

    val bf = BloomFilter.optimallySized[T]( size, false_positive_rate )

    level(level(leaf, 32), 16).foreach( bf |= _.bf )

    root = new Node(bf, leaf, null)

    this
  }

  def search(element:T):Iterable[Iterable[T]] = {
    if( root.bf != null ) {
      val hashes = root.bf.hashValues(element)
      def searchnode(node:Node[T]):Iterable[Iterable[T]] = {
        val firstChild = if (
          node.bf.containsHashes(hashes) && node.childs != null && node.childs.size >= 1
        ) {
          node.childs.head
        } else null

        if ( firstChild != null && firstChild.values != null ) { // this child is leaf
          node.childs.filter( x =>
            x.bf.containsHashes(hashes)// && x.values.exists(_ == element)
          ).map(_.values)
        } else { // not leaf
          node.childs.flatMap( searchnode _ )
        }
      }
      searchnode(root)
    } else  Iterable[Iterable[T]]()
  }
}
