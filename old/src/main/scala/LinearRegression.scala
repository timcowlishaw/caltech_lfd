package linearRegression;
import caltechUtils._
import scala.swing.{Point => _, _};
import scala.swing.Swing._;
import breeze.linalg._
import java.awt.{Color,Graphics2D,geom};
class LinearRegressionLearner(trainingPoints : Traversable[Labelled]) {
  breeze.linalg.useNativeLibraries = false;
  def learn : LearningFunction = {
    val y = new DenseVector(trainingPoints.map(_.classification.asInt.toDouble).toArray)
    val X = new DenseMatrix(3, trainingPoints.size, trainingPoints.flatMap {l => Traversable(1, l.point.x, l.point.y)}.toArray);
    val Xdagger = LinearAlgebra.inv(X.t * X) * X.t
    val w = Xdagger * y
    return new LearningFunction(w(0), w(1), w(2))
  }
}

object LinearRegressionDisplay extends SimpleSwingApplication {

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
      val learner = new LinearRegressionLearner(trainingData);
      val learnt = learner.learn;
      val testingPoints = Point.nRandom(1000);
      val testingData = testingPoints map { point => (target.label(point), learnt.label(point)) }
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

