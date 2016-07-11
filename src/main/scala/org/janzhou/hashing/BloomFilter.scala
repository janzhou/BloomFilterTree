package org.janzhou.hashing

import scala.util.hashing.MurmurHash3
import scala.collection.mutable.BitSet
import scala.reflect.ClassTag

class BloomFilter[T:ClassTag](
  val numBuckets: Int,
  val numHashFunctions: Int,
  val bits: BitSet
) {
  def this(numBuckets: Int, numHashFunctions: Int) = this(numBuckets, numHashFunctions, new BitSet(numBuckets))
  def this(numBuckets: Int) = this(numBuckets, 3)

  def hashValues(key: T) = {
    val hash1 = key.##
    val hash2 = math.abs(MurmurHash3.mixLast(0, hash1))

    for {
      i <- 0 to numHashFunctions
    } yield {
      val h = hash1 + i * hash2
      val nextHash = if (h < 0) ~h else h
      nextHash % numBuckets
    }
  }

  def containsHashes(hashes:Iterable[Int]):Boolean = {
    hashes.forall(i => bits.contains(i))
  }

  def contains(o: T): Boolean = {
    containsHashes(hashValues(o))
  }

  /**
  *
  * Calculates the load of the bloom filter. If this is near 1, there will be lots of false positives.
  *
  * @return the fraction of bits that are set
  */
  def load: Double = bits.size.toDouble / numBuckets

  override def equals(other: Any) = other match {
    case that: BloomFilter[_] =>
    this.numBuckets == that.numBuckets && this.numHashFunctions == that.numHashFunctions && this.bits == that.bits
    case _ => false
  }

  def +=(o: T): this.type = {
    hashValues(o).foreach(i => bits += i)
    this
  }

  def +=(o: Iterable[T]): this.type = {
    o.foreach( this += _ )
    this
  }

  def &(that: BloomFilter[T]) = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits & that.bits)
  }


  private def checkCompatibility(that: BloomFilter[T]) {
    require(that.numBuckets == this.numBuckets, "Must have the same number of buckets to intersect")
    require(that.numHashFunctions == this.numHashFunctions, "Must have the same number of hash functions to intersect")
  }

  def |(that: BloomFilter[T]) = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits | that.bits)
  }

  def |=(that: BloomFilter[T]):this.type = {
    checkCompatibility(that)
    this.bits |= that.bits
    this
  }

  def &=(that: BloomFilter[T]):this.type = {
    checkCompatibility(that)
    this.bits &= that.bits
    this
  }

  def &~=(that: BloomFilter[T]):this.type = {
    checkCompatibility(that)
    this.bits &~= that.bits
    this
  }

  def &~(that: BloomFilter[T]) = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits &~ that.bits)
  }

}

object BloomFilter {
  /**
  * Returns the optimal number of buckets  (m) and hash functions (k)
  *
  * The formula is:
  * {{{
  * val m = ceil(-(n * log(p)) / log(pow(2.0, log(2.0))))
  * val k = round(log(2.0) * m / n)
  * }}}
  *
  * @param expectedNumItems
  * @param falsePositiveRate
  * @return
  */
  def optimalSize(expectedNumItems: Double, falsePositiveRate: Double): (Int, Int) = {
    val n = expectedNumItems
    val p = falsePositiveRate
    import scala.math._
    val m = ceil(-(n * log(p)) / log(pow(2.0, log(2.0))))
    val k = round(log(2.0) * m / n)
    (m.toInt, k.toInt)
  }

  /**
  * Returns a BloomFilter that is optimally sized for the expected number of inputs and false positive rate
  * @param expectedNumItems
  * @param falsePositiveRate
  * @tparam T
  * @return
  */
  def optimallySized[T:ClassTag](expectedNumItems: Double, falsePositiveRate: Double): BloomFilter[T] = {
    val (buckets, funs) = optimalSize(expectedNumItems, falsePositiveRate)
    new BloomFilter[T](buckets, funs)
  }
}
