package com.geophy

import cats.Monad

type Path
type LoadResult
type OutputPath
type Result[A]

sealed trait DataSourceReader[A]:
  def read(p: Path): A

enum DataSource[A]:
  self =>
    def map[B](f: A => B)(using DataSourceReader[A]): DataSource[B] = self match
      case Read(p) => Result(f(run))
      case Result(d) => Result(f(d))

    def run(using r: DataSourceReader[A]): A = self match
      case Read(p) => r.read(p)
      case Result(d) => d
  
  private case Read(path: Path)
  private case Result(data: A)
end DataSource

enum ETL[A]:
  self =>
    def map[B](f: A => B)(using DataSourceReader[A]): ETL[B] = self match
      case Extract(s) => Extract(s.map(f))

    def flatMap[B](f: A => ETL[B])(using DataSourceReader[A]): ETL[B] = self match
      case Extract(s) => f(s.run)

  case Extract(source: DataSource[A])
end ETL

object ETL:
  def read[A](source: DataSource[A]): ETL[A] = Extract(source)
end ETL

object ETLApp extends App:

  val dataSource: DataSource[String] = ???
  val data = ""
  val etl1 = ETL.Extract(dataSource)
  val etl2 = ETL.Extract(dataSource)

  given DataSourceReader[String] with
    def read(p: Path): String = "Hello implicits"

  for
    data <- etl1
    data2 <- etl2
  yield ()