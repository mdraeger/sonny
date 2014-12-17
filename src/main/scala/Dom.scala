package org.draegisoft.sonny

import java.nio.charset.StandardCharsets

class Dom (bytes: Array[Byte]) {

  def this(string: String) = this (string getBytes (StandardCharsets.UTF_8))
}
