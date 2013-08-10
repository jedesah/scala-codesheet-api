package com.github.jedesah.AugmentedCollections

import org.specs2.mutable._

class AugmentedCollectionsSpec extends Specification {
	"AugmentedStringList" should {
		"safeUpdate" in {
			"overflow" in {
				List("d", "3", "y").safeUpdate(5, "hey") ==== List("d", "3", "y", "", "", "hey")
			}
			"normal" in {
				List("d", "3", "y").safeUpdate(1, "hey") ==== List("d", "hey", "y")
			}
		}
	}
}