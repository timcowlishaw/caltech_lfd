package cointossing;

object CoinTossing extends App {
  val experiment = new CoinTossingExperiment(1000, 10, 100000);
  val result = experiment.run;
  println("v1: " + result.v1)
  println("vRand: " + result.vRand)
  println("vMin: " + result.vMin)
}

class CoinTossingResult(val v1 : Double, val vRand : Double, val vMin : Double);

class CoinTossingExperiment(coins : Int, flips : Int, nTrials : Int) {
  val random = new scala.util.Random
  def run : CoinTossingResult = {
    val trials = for (i <- 1 to nTrials) yield {
      for (j <- 1 to coins) yield { random.nextInt(flips + 1) / flips.toDouble };
    }
    val c1s = for (trial <- trials) yield {
      trial(0)
    }
    val cRands = for (trial <- trials) yield {
      trial(random.nextInt(coins))
    }
    val cMins = for (trial <- trials) yield {
      trial.min
    }
    val v1 = c1s.sum / nTrials.toDouble
    val vRand = cRands.sum / nTrials.toDouble
    val vMin = cMins.sum / nTrials.toDouble
    return new CoinTossingResult(v1, vRand, vMin);
  }
}

