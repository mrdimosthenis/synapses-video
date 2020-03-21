import java.awt.{BorderLayout, Color}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.swing.JFrame
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.svg.{SVGDocumentLoaderAdapter, SVGDocumentLoaderEvent}
import synapses.Library.{ActivationFunction, NeuralNetwork, Statistics}
import scala.util.Random

object Main {

  val random: Random = new Random()

  val svgCanvas = new JSVGCanvas

  var network: NeuralNetwork = createNetwork()

  var dataset: LazyList[(List[Double],List[Double])]  = LazyList.empty

  var i = 0

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("JSVGCanvas Demo")
    frame.setSize(1280, 720)
    MainCanvas(frame)
  }

  def createNetwork(): NeuralNetwork = {
    val layers = List(3, 9, 8, 7, 6)

    def activationF(layerIndex: Int): ActivationFunction =
      layerIndex match {
        case 0 => ActivationFunction.sigmoid
        case 1 => ActivationFunction.tanh
        case 2 => ActivationFunction.sigmoid
        case 3 => ActivationFunction.identity
      }

    def weightInitF(layerIndex: Int): Double =
      Math.pow(4 - layerIndex, 0) * (1.0 - 2.0 * random.nextDouble())

    NeuralNetwork.customizedInit(
      layers,
      activationF,
      weightInitF
    )
  }

  def updateImage(): Unit = {
    val svgFilePath: String = "draw_net.svg"
    val svg = NeuralNetwork.toSvg(network)
    Files.write(
      Paths.get(svgFilePath),
      svg.getBytes(StandardCharsets.UTF_8)
    )
    svgCanvas.loadSVGDocument("file:" + svgFilePath)
    svgCanvas.addSVGDocumentLoaderListener(new SVGMYDocumentLoaderListener())
  }

  class SVGMYDocumentLoaderListener extends SVGDocumentLoaderAdapter {
    override def documentLoadingCompleted(e: SVGDocumentLoaderEvent): Unit = {
      e.getSVGDocument.getRootElement.setAttribute("viewBox", "0 0 350 350")
    }
  }

  def updateNetwork(): Unit = {
    val learningRate = 0.25
    val x = random.nextDouble()
    val y = random.nextDouble()
    val z = random.nextDouble()
    val inputValues = List(x, y, z)
    val expectedOutput = List(
      0.0,
      1.0,
      x,
      y,
      x * y,
      x * y * z * z,
    )
    val prediction = NeuralNetwork.prediction(network, inputValues)
    dataset = (expectedOutput, prediction) +: dataset
    network = NeuralNetwork.fit(
      network,
      learningRate,
      inputValues,
      expectedOutput
    )
  }

  def MainCanvas(frame: JFrame) {
    frame.getContentPane.setLayout(new BorderLayout)
    frame.getContentPane.add("Center", svgCanvas)
    frame.setVisible(true)
    svgCanvas.setSize(1280, 720)
    svgCanvas.setBackground(Color.BLACK)

    while (true) {
      updateNetwork()
      if (i % 1000 == 0) {
        updateImage()
        val rmsr = Statistics.rootMeanSquareError(dataset)
        println(rmsr)
        dataset = LazyList.empty
      }
      i = i + 1
    }

  }

}
