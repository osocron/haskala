package com.geophy

import cats.effect._

object CMain extends IOApp.Simple:
  val run = IO.println("Hello World!")