package com.geophy

import zio._

object ZMain extends ZIOAppDefault:

  opaque type Greeting = String
  object Greeting:
    def apply(s: String): Greeting = s

  opaque type Name = String
  object Name:
    def apply(s: String): Name = s

  trait GreetingService:
    def greeting: UIO[Greeting]

  object GreetingService:

    def greeting =
      ZIO.serviceWith[GreetingService](_.greeting)

    val live: ULayer[GreetingService] =
      ZLayer.succeed(
        new:
          def greeting: UIO[Greeting] = ZIO.succeed(Greeting("Hello"))
      )

  trait NameService:
    def name: UIO[Name]

  object NameService:

    def name = ZIO.serviceWith[NameService](_.name)

    val live: ULayer[NameService] = ZLayer.succeed(
      new:
        def name: UIO[Name] = ZIO.succeed(Name("World"))
    )

  val eff: URIO[GreetingService & NameService, String] =
    for {
      greeting <- GreetingService.greeting
      name <- NameService.name
    } yield s"$greeting $name"

  val run =
    eff
      .provide(GreetingService.live ++ NameService.live)
      .debug("RESULT")
      .exitCode
