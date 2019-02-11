package org.bitbucket.state

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
case class SimpleRNG(seed: Long) extends RNG{
  override def next: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

