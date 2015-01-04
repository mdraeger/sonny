package org.draegisoft.sonny.test

import org.draegisoft.sonny._
import Dom._
import Style._

import org.scalatest._

import TestValues._

class StyleSpec extends FlatSpec with Matchers{

  "StyleSpec" should "create the styletree out of DOM and CSS" in {
     styletree equals (styleTree(dom, css2))
  }
}
