package perceptron;
import caltechUtils._
import scala.annotation.tailrec;
import scala.swing.{Point => _, _};
import scala.swing.Swing._;
import java.awt.{Color,Graphics2D,geom};

class PerceptronLearner(trainingPoints : Traversable[Labelled]) {
  @tailrec
  final def learn(f : LearningFunction = LearningFunction.zero, iters : Int = 0) : (LearningFunction, Int) = {
    val misclassified = trainingPoints.find(!_.correctUnder(f));
    return misclassified match {
      case Some(point) => learn(f.updateFrom(point), iters+1);
      case None => (f, iters);
    }
  }
}

object PerceptronDisplay extends SimpleSwingApplication {

  def coordConvert(point : Point) : (Int, Int) = coordConvert(point.x, point.y);

  def coordConvert(x : Double, y : Double) : (Int, Int) = (200 + (200 * x).toInt, 200 - (200 * y).toInt);

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (400,400)
    override def paintComponent(g : Graphics2D) =  {
      super.paintComponent(g);
      val trainingPoints = Point.nRandom(10);
      val target = LearningFunction.random;
      val trainingData = trainingPoints map { target.label(_) }
      val learner = new PerceptronLearner(trainingData);
      val (learnt, iters) = learner.learn(LearningFunction.zero);
      println("Converged in "+iters+" iterations.");
      val testingPoints = Point.nRandom(1000);
      val testingData = testingPoints map { point => (target.label(point), learnt.label(point)) }
      val nWrong = testingData.filter { case (target, learnt) => target != learnt }.length
      println(nWrong);
      println("P(f(x) =/= g(x)) =~ "+(nWrong/1000.toDouble))



      trainingData foreach { case Labelled(point, classification) => {
        classification match {
          case High => g.setColor(Color.red);
          case Low => g.setColor(Color.blue);
        }
        val (pointX, pointY) = coordConvert(point);
        g.draw(new geom.Ellipse2D.Double(pointX, pointY, 5, 5));
      }}
      val targetPath = new geom.GeneralPath;
      val (targetFromX, targetFromY) = coordConvert(-1, target.transform(-1));
      val (targetToX, targetToY) = coordConvert(1, target.transform(1));
      targetPath.moveTo(targetFromX, targetFromY);
      targetPath.lineTo(targetToX, targetToY);
      g.setColor(Color.black);
      g.draw(targetPath);
      val learntPath = new geom.GeneralPath;
      val (learntFromX, learntFromY) = coordConvert(-1, learnt.transform(-1));
      val (learntToX, learntToY) = coordConvert(1, learnt.transform(1));
      learntPath.moveTo(learntFromX, learntFromY);
      learntPath.lineTo(learntToX, learntToY);
      g.setColor(Color.green);
      g.draw(learntPath);
    }
  }

  def top = new MainFrame {
    title = "Perceptron"
    contents = ui
  }
}

object PerceptronStats extends App {
  val n = 100
  val stats = (1 to 1000) map { i =>
    val trainingPoints = Point.nRandom(n);
    val target = LearningFunction.random;
    val trainingData = trainingPoints map { target.label(_) }
    val learner = new PerceptronLearner(trainingData);
    val (learnt, iters) = learner.learn(LearningFunction.zero);
    val testingPoints = Point.nRandom(1000);
    val testingData = testingPoints map { point => (target.label(point), learnt.label(point)) }
    val nWrong = testingData.filter { case (target, learnt) => target != learnt }.length
    val p = nWrong/1000.toDouble
    (iters, p)
  }
  val meanIters = stats.map { (t: (Int, Double)) => t._1 }.reduce {_ + _} / 1000.toDouble
  val meanP = stats.map { (t: (Int, Double)) => t._2 }.reduce {_ + _} / 1000.toDouble
  println("For n = "+n+":")
  println("Convergence in "+meanIters+" on average")
  println("E[P(f(x) =/= g(x))] = "+meanP)

}
