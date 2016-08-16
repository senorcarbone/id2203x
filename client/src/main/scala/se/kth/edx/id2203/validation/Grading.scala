package se.kth.edx.id2203.validation

import se.kth.edx.id2203.validation.ValidationUtils.{PropertyResult, ValidationGroup}

object Grading {
  
  def gradeEPFD(results: Iterable[ValidationGroup]): Iterable[PropertyResult] = {
    def convert(validationGroup: ValidationGroup): PropertyResult = {
      validationGroup match {
        case ValidationGroup("Eventual Correctness", _) => registerGrade(validationGroup, 10);
        case _ => registerGrade(validationGroup, 1)
      }
    }
    results.map(convert)
  }

  def registerGrade(vGroup: ValidationGroup, grade: Int): PropertyResult = PropertyResult(vGroup.id, vGroup.isValid(), grade);

}
