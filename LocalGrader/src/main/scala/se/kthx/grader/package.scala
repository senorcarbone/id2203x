package se.kthx

package object grader {
  def nl2br(s: String) = {
    s.replaceAll("\n", "<br />")
  }
}
