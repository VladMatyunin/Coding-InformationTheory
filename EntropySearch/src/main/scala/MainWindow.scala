
import java.awt.BorderLayout
import java.io.File
import javax.swing.{JButton, JLabel, JPanel}

import scala.swing._
import scala.swing.event.ButtonClicked

/**
  * Created by Vlad on 02.11.2017.
  */
class MainWindow extends MainFrame with DocumentSelectedListener{
  val executor = new EntropyExecutor
  title = "Entropy Search"
  preferredSize = new Dimension(300, 200)
  resizable = false
  var selectedDocument: File=_

  val header = new Header(this)
  contents = new GridPanel(4, 4) {
    contents += header
    contents += Display
    contents += new GridPanel(4,4)
    contents += new Button {
      text = "Execute"
      reactions += {
        case ButtonClicked(_) =>
          if (selectedDocument!=null) {
            val result = executor.executeEntropy(selectedDocument)
            Display.setValues(result._1, result._2, result._3, result._4)
          }

      }
    }
  }

  override def documentSelected(file: File): Unit = {
    selectedDocument = file
  }
}

object Display extends GridPanel(4, 4) {
  private var labelEntropy = new Label("0")
  private var labelFullEntropy = new Label("0")
  private var labelSymbols = new Label("0")
  private var labelAlphabet = new Label("0")

  contents += new Label("Entropy")
  contents += labelEntropy
  contents += new Label("Conditional entropy")
  contents += labelFullEntropy
  contents += new Label("Number of Symbols")
  contents += labelSymbols
  contents += new Label("Alphabet")
  contents += labelAlphabet


  def setValues(entropy: Double, fullEntropy: Double, symbols: Int, alphabet: Int): Unit = {
    labelEntropy.text = entropy.toString
    labelFullEntropy.text = fullEntropy.toString
    labelSymbols.text = symbols.toString
    labelAlphabet.text = alphabet.toString
  }
}

class Header(listener: DocumentSelectedListener) extends GridPanel(1, 2) {
  private val docListener = listener
  private var pathTextField = new TextField()
  private var selectedFile:File = _
  private var chooseFileButton = new Button("Choose")
  private val fileChooser = new FileChooser

  chooseFileButton.reactions += {
    case ButtonClicked(_) => {
      fileChooser.showDialog(this, "Choose File") match {
        case FileChooser.Result.Approve =>
          pathTextField.text = fileChooser.selectedFile.getAbsolutePath
          selectedFile = fileChooser.selectedFile
          docListener.documentSelected(selectedFile)
        case FileChooser.Result.Cancel => ;
        case FileChooser.Result.Error => ;
      }
    }
  }
  def getFile:File = selectedFile

  contents += pathTextField
  contents += chooseFileButton
}

trait DocumentSelectedListener {
  def documentSelected(file: File)
}