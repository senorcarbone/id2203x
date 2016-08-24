package se.kth.edx.id2203.validation

import se.kth.edx.id2203.validation.ValidationUtils.{PropertyResult, ValidationGroup, ValidationReport}


//FIXME put grades

object Grading {
  
  def gradeEPFD(results: Iterable[ValidationGroup]): ValidationReport = {
    ValidationReport("Eventually Perfect Failure Detector", !results.exists(!_.isValid()), 
      results.map(x => PropertyResult(x.id, x.isValid())),10);
  }
}
