package se.kthx.grader

import scalatags.Text.all._

object HTMLLayout {
    val stylesheet1 = "main.css";
    val stylesheet2 = "normalize.css";
    val titleText = "KTHx id2203x Grader";
    val defaultHeader = head(
            title := titleText, 
            charset := "utf-8", 
            link(rel := "stylesheet", tpe := "text/css", href := stylesheet1),
            link(rel := "stylesheet", tpe := "text/css", href := stylesheet2)
            );

    def base(header: Tag, content: Tag): Tag = html(lang := "en")(header,
        body(div(cls := "mainbox")(
            h1(titleText),
            div(cls := "contentbox")(content))
            )
    )
    
    def withText(content: String): Tag = {
        base(defaultHeader, p(content))
    }
    
    def withText(content: String, style: String): Tag = {
        base(defaultHeader, p(cls := style)(content))
    }
}