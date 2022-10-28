package com.geophy

import java.net.URI

object ETLdsl:

  // enum Path:
  //   case S3Path(uri: URI)

  // case class ComponentDependencies()

  // enum DataSource:
  //   case ReadDataSource(inputPath: Path)
  //   case WriteDataSource(outputPath: Path)
  //   case DataSourceWithDependencies(dependencies: ComponentDependencies)
  //   case ConfiguredDataSource(
  //       dependencies: ComponentDependencies,
  //       configuration: AppConfig
  //   )

  //   def read(inputPath: Path): WriteDataSource = ???

  //   def provide(
  //       dependencies: ComponentDependencies
  //   ): DataSourceWithDependencies = ???

  // end DataSource

  // object DataSource:
  //   extension (ds: DataSourceWithDependencies)
  //     def provide(config: AppConfig): ConfiguredDataSource = ???

  //   extension (ds: ConfiguredDataSource)
  //     def provide(logic: Any => Any): Program = ???

  //   def read(a: Any): Any = ???
  //   def write(a: Any): Any = ???
  // end DataSource

  // case class AppConfig(inputPath: Path, outputPath: Path)

  // case class Program()

  // import DataSource._

  // def procedure(
  //     dataSource: DataSource,
  //     configuration: AppConfig,
  //     dependencies: ComponentDependencies
  // ): Program =
  //   dataSource
  //     .provide(dependencies)
  //     .provide(configuration)
  //     .provide(
  //       logic = DataSource.read andThen DataSource.write
  //     )

  // Version 2

  // enum Path:
  //   case S3Path(uri: URI)

  // enum TechnicalName:
  //   case Fa, Yardi, WD, Rca, Pdf

  // object PropertyUniverse:
  //   import TechnicalName._
  //   val dataSources: List[DataSource] = ???

  // case class DataSource(technicalName: TechnicalName, location: Path)
  // case class DataSet[A]()

  // def readFromS3(ds: DataSource): DataSet[TechnicalName] = ???

  // def writeToS3(ds: DataSet[TechnicalName]): Unit = ???

  // val program =
  //   PropertyUniverse.dataSources.foreach(readFromS3 andThen writeToS3)

  
  //  Version 3

  /**
   * What is the domain of the Property Pipeline?
   * 
   * Word storm:
   * 
   * PropertyUniverse: Refers to the pool of data sources that make up Property Data
   * Data Source: A collection of datasets that contain Property related data
   * Property: A collection of real estate assets leaglly owned by a set of entities
   * 
   * Life cycle of a Data Source:
   *    - 
   * 
  */


end ETLdsl
