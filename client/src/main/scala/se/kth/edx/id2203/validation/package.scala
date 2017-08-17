package se.kth.edx.id2203

import java.io.{ File, FileOutputStream, PrintStream }

import se.kth.edx.id2203.validation.ValidationUtils._
import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation._
import se.kth.edx.id2203.validation.testers._
import se.sics.kompics.sl._
import se.sics.kompics.{ ComponentDefinition => _, Port => _ }
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.reflect.runtime.universe._

package object validation {

    val outputLinkMsg = "\nYour submission has been locally simulated and validated.\n" +
        "\n<a href=\"\\static\\log.txt\" target=\"_blank\">Click Here</a> to view the output of the simulation."

    val successMsg = (id: String) =>
        s"Congratulations! Your implementation of the '$id' satisfies all properties! " +
            s"\nA unique token has been generated for your submission right below. Please do not edit. \n";

    val errorMsg = (id: String) =>
        s"Unfortunately your implementation of the '$id' does not satisfy all properties. " + s"" +
            s"\nPlease make sure your implementation covers all guarantees and try again. "

    val finalMsg = "Final Comments\n" +
        "**************";

    val hackyHTMLTableStart =
        "<style type=\"text/css\">.tg  {border-collapse:collapse;border-spacing:0;border-color:#aabcfe;}.tg " +
            "td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;" +
            "border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:#aabcfe;" +
            "color:#669;background-color:#e8edff;}.tg " +
            "th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;" +
            "border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:#aabcfe;" +
            "color:#039;background-color:#b9c9fe;}.tg .tg-mb3i{background-color:#D2E4FC;text-align:right;vertical-align:top}.tg " +
            ".tg-lqy6{text-align:right;vertical-align:top}.tg " +
            ".tg-vlnx{background-color:#3071a9;color:#ffffff;text-align:center;vertical-align:top}.tg " +
            ".tg-6k2t{background-color:#D2E4FC;vertical-align:top}.tg tg-6k2l{background-color:#ffb3b3;vertical-align:top}.tg .tg-yw4l{vertical-align:top}</style><table class=\"tg\">  " +
            "<tr>    <th class=\"tg-vlnx\" colspan=\"3\">Correction Results</th>  </tr>";

    val hackyHTMLTableLine = (css: String, passmsg: String, propId: String, validations: Array[ValidationResult]) =>
        "<tr>    <td class=\"" + css + "\">" + passmsg + "</td> <td class=\"" + css + "\">" + propId + "</td> " +
            "<td class=\"" + css + "\">" + validations.foldLeft("")((str, next) => str + next.toString() + "<br>") + "</td>  </tr>  ";
    val hackHTMLTableEnd = "</table>";

    val styleByResult = (res: Boolean) => if (res) "tg-6k2t" else "tg-6k2l";
    val msgByResult = (res: Boolean) => if (res) "PASSED" else "FAILED";

    var defaultOutput: PrintStream = System.out;
    var defaultError: PrintStream = System.err;
    var outputRedirected = false;

    case class GradingToken(gradingToken: Array[Byte]);

    implicit val gradingFormat = jsonFormat1(GradingToken)

    def outToFile(): Unit = {
        val staticOutDir: File = new File("/tmp/output")
        if (!staticOutDir.exists()) {
            staticOutDir.mkdir
        }
        defaultOutput = System.out; // save current endpoint
        defaultError = System.err; // save current endpoint

        System.out.flush();
        System.err.flush();
        Console.out.flush();
        Console.err.flush();
        val outFile: File = new File("/tmp/output/log.txt")
        if (outFile.exists()) {
            outFile.delete();
        }
        val pso = new PrintStream(outFile);
        System.setOut(pso);
        System.setErr(pso);
        Console.withOut(System.out) _;
        Console.withErr(System.err) _;
        outputRedirected = true;
    }

    def outReset(): Unit = {
        if (outputRedirected) {
            System.out.flush();
            System.err.flush();
            Console.flush();
            System.out.close();
            //System.err.close(); // should be the same here
            System.setOut(defaultOutput);
            System.setErr(defaultError);
            Console.withOut(System.out) _;
            Console.withErr(System.err) _;
            outputRedirected = false;
        }
    }

    def checkBLE[BLETYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Ballot Leader Election", BLETester[BLETYPE]())
    }

    def checkEPFD[EPFDTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Eventually Perfect Failure Detector", EPFDTester[EPFDTYPE]())
    }

    def checkBEB[BEBTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Best Effort Broadcast", BEBTester[BEBTYPE]())
    }

    def checkRB[BEBTYPE <: ComponentDefinition: TypeTag, RBTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Reliable Broadcast", RBTester[BEBTYPE, RBTYPE]())
    }

    def checkCRB[BEBTYPE <: ComponentDefinition: TypeTag, RBTYPE <: ComponentDefinition: TypeTag, CRBTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Causal Order Reliable Broadcast", CRBTester[BEBTYPE, RBTYPE, CRBTYPE]())
    }

    def checkNNAR[ARTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile()
        validateAndPrint("NN Atomic Register", ABDTester[ARTYPE])
    };

    def checkSingleValueConsensus[ACTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        validateAndPrint("Single Value Consensus", PaxosTester[ACTYPE])
    }

    def checkExample[ACTYPE <: ComponentDefinition: TypeTag](): Unit = {
        outToFile();
        val props = Array(
            ValidationGroup("Example Run", Array(SimValidator(null, 0, "You have run the example successfully!", "", true, true))));
        validateAndPrint("Game of Life", props);
    }

    def validateAndPrint(componentId: String, props: Iterable[ValidationGroup], maxPoints: Int = 1): Unit = {
        val passed = !props.exists(!_.isValid());
        outReset();
        println(outputLinkMsg)
        println(hackyHTMLTableStart);
        props.foreach(p => println(hackyHTMLTableLine(styleByResult(p.isValid()), msgByResult(p.isValid()), p.id, p.validators.map(_.getResult()).toArray)))
        println(hackHTMLTableEnd);
        println(finalMsg)
        if (passed) {
            println(successMsg(componentId));
            println(GradingToken(Encryptor.encrypt(gradeComponent(componentId, props, maxPoints).toJson.toString)).toJson.toString);
        } else {
            println(errorMsg(componentId));
        }
    }

    def gradeComponent(id: String, results: Iterable[ValidationGroup], points: Int): ValidationReport = {
        ValidationReport(id, !results.exists(!_.isValid()),
            results.map(x => PropertyResult(x.id, x.isValid())), points);
    }
}







