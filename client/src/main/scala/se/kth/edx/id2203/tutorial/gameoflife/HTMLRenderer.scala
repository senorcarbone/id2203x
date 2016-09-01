package se.kth.edx.id2203.tutorial.gameoflife

import scalatags.Text.all._
import java.io.File
import java.io.FileWriter
import java.awt.Desktop

object HTMLRenderer {
    val gameS = Thread.currentThread().getContextClassLoader.getResourceAsStream("game.js");
    val game = scala.io.Source.fromInputStream(gameS).mkString;

    val gridStyle = scalatags.Text.tags2.style("""
    #grid_area {
        background: #fff;
        clear: left;
        float: left;
        width: 360px;
        padding: 0.5em;
        margin: 0;
        display: inline;
       }

    #grid {
        border: 1px solid #eed;;
        margin: 4px;
      }
""");
    val gridArea = div(id := "gridArea", p(id := "gentext", "Loading..."), canvas(id := "grid_canvas", width := 351, height := 351));

    def page(data: String) = div(gridStyle, gridArea, script(raw(game)), script(s"""
        var gameData = $data;
        game.drawGame(gameData, 0)
""")).render;

    val defaultHeader = head(
        title := "Game of Life Test",
        charset := "utf-8"
    );

    def render(jsStr: String) {
        val pg = page(jsStr);
        println(pg);
        // for testing only
        //val fullPage = html(lang := "en")(header,
        //    body(raw(pg))).render;
        //val htmlFile = File.createTempFile("goltest", ".html");
        //val writer = new FileWriter(htmlFile);
        //writer.write(fullPage);
        //writer.close();
        //Desktop.getDesktop().browse(htmlFile.toURI());
    }
}