package com.github.jedesah.codesheet.api

import org.scalatest.FunSpec

import ScalaCodeSheet._

class ScalaCodeSheetSuite extends FunSpec {

  describe("Expressions") {
  	it("literal") {
  		computeResults("1") === ExpressionResult(1, line = 1)
  	}
  }
}