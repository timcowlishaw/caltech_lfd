package caltechUtils;
import scala.util.Random;

object Util {
  val r = new Random
  def randomN : Double = r.nextDouble + r.nextDouble - 1;
}

import Util._

object Point {
  def nRandom(n : Int) : Seq[Point] = {
    return (for (i <- 1 to n) yield Point(randomN, randomN));
  }
}

case class Point(val x : Double, val y : Double);

object Classification {

  def fromFunctionResult(n : Double) : Classification = {
    if(n >= 0) High else Low;
  }
}

sealed trait Classification {
  val asInt : Int;
}

case object High extends Classification {
  val asInt = 1;
}

case object Low extends Classification {
  val asInt = -1;
}

case class Labelled(val point : Point, val classification : Classification) {
  def correctUnder(func : LearningFunction) : Boolean = {
    return func(point) == classification;
  }
}

object LearningFunction {
  val zero = LearningFunction(0,0,0);
  def random : LearningFunction = {
    val r = new Random;
    return LearningFunction(randomN, randomN, randomN);
  }
}

case class LearningFunction(val bias : Double, val coeffx : Double, val coeffy : Double) {
  def apply(point : Point) : Classification = {
    return Classification.fromFunctionResult(bias + coeffx * point.x + coeffy * point.y);
  }

  def label(point: Point) : Labelled = {
    return Labelled(point, this(point));
  }

  def transform(x : Double) : Double = {
    return 0-(bias/coeffy + x*coeffx/coeffy);
  }

  def updateFrom(example : Labelled) : LearningFunction = {
    val cls = example.classification.asInt
    val deltaX = example.point.x * cls;
    val deltaY = example.point.y * cls;
    var deltaB = cls;
    return LearningFunction(bias + deltaB, coeffx + deltaX, coeffy + deltaY);
  }
}
