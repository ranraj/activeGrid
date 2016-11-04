package com.imaginea.activegrid.core.models

/**
 * Created by ranjithrajd on 4/11/16.
 */
sealed trait SoftwareProcess
case object NginxProcess extends SoftwareProcess
case object GraphiteProcess extends SoftwareProcess
case object Apache2Process extends SoftwareProcess
case object ApacheProcess extends SoftwareProcess
case object CollectDProcess extends SoftwareProcess
case object RubyProcess extends SoftwareProcess
case object MysqlDProcess extends SoftwareProcess
case object MysqlProcess extends SoftwareProcess
case object JavaProcess extends SoftwareProcess
case object PythonProcess extends SoftwareProcess

object SoftwareProcess{
  def getAllProcess: List[SoftwareProcess] =
    JavaProcess :: MysqlDProcess :: MysqlProcess ::
      RubyProcess :: CollectDProcess ::
       ApacheProcess :: Apache2Process :: PythonProcess :: NginxProcess :: Nil
}
trait KnownSoftware{
  val name: String
  val provider: String
  val process: List[SoftwareProcess]
  val discoverApplications: Boolean
  val applicationDiscoveryHelper: Option[ApplicationDiscovery]
}
case object Tomcat extends KnownSoftware {
  override val name: String = "Tomcat"
  override val applicationDiscoveryHelper: ApplicationDiscovery = TomcatApplicationDiscovery
  override val provider: String = "Apache Software Foundation"
  override val process: List[SoftwareProcess] = List(JavaProcess)
  override val discoverApplications: Boolean = true
}
case object Apache extends KnownSoftware {
  override val name: String = "Tomcat"
  override val applicationDiscoveryHelper: Option[ApplicationDiscovery] = None
  override val provider: String = "Apache Software Foundation"
  override val process: List[SoftwareProcess] = List(ApacheProcess,Apache2Process)
  override val discoverApplications: Boolean = false
}
case object Mysql extends KnownSoftware {
  override val name: String = "Mysql"
  override val applicationDiscoveryHelper: Option[ApplicationDiscovery] = None
  override val provider: String = "Oracle Corporation"
  override val process: List[SoftwareProcess] = List(MysqlDProcess,MysqlProcess)
  override val discoverApplications: Boolean = false
}
case object PServer extends KnownSoftware {
  override val name: String = "PServer"
  override val applicationDiscoveryHelper: Option[ApplicationDiscovery] = None
  override val provider: String = "Pramati Technologies"
  override val process: List[SoftwareProcess] = List(JavaProcess)
  override val discoverApplications: Boolean = true
}
case object Nginx extends KnownSoftware {
  override val name: String = "Nginx"
  override val applicationDiscoveryHelper: Option[ApplicationDiscovery] = None
  override val provider: String = "Nginx, Inc"
  override val process: List[SoftwareProcess] = List(NginxProcess)
  override val discoverApplications: Boolean = false
}
case object Graphite extends KnownSoftware {
  override val name: String = "Graphite"
  override val applicationDiscoveryHelper: Option[ApplicationDiscovery] = None
  override val provider: String = "Graphite"
  override val process: List[SoftwareProcess] = List(GraphiteProcess)
  override val discoverApplications: Boolean = false
}

trait ApplicationDiscovery{
  def discoverApplications(pid: String, sshSession: SSHSession): Set[String]
}

case object TomcatApplicationDiscovery extends ApplicationDiscovery{
  override def discoverApplications(pid: String, sshSession: SSHSession): Set[String] = ???
}