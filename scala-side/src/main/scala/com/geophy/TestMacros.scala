package com.geophy

object TestMacros extends App:
  val wrapped = Entrypoint.autoCreate(((x: Int) => x + 2)(20))
  wrapped.print