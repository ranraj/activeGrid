package com.imaginea.activegrid.core.models

/**
 * Created by ranjithrajd on 3/11/16.
 */
case class SSHBasedStrategy (topology: Topology,
                             softwares: List[Software]
                              ){
  val PROCESS_TAG: String = "process"

  def collectInstanceDetails(site : Site ,cloudInstance: CloudInstance,userName: String,keyLocation:String,passPharse: Option[String]): Unit ={
    val portOption = cloudInstance.sshAccessInfo.map( ssh => ssh.port)
    val hostProp = PrivateIpAndPublicDns
    val connectionOptions = hostProp match {
      case PrivateIpAndPublicDns => cloudInstance.privateDnsName :: cloudInstance.publicIpAddress :: Nil
      case PublicDns => cloudInstance.publicIpAddress :: Nil
      case PrivateIp => cloudInstance.privateDnsName :: Nil
    }
    connectionOptions.foreach( connectionOption => {
      val sshSession = new SSHSession(serverIp = connectionOption,
        userName = userName,
        keyLocation = keyLocation,
        passPhrase = passPharse,
        port = portOption)
      val osInfo = sshSession.executeCommand("uname -ar")
      //resolve OS

      if(osInfo.isDefined && osInfo.get.toLowerCase().contains("ubuntu")){
        val cloudInstanceNew1 = cloudInstance.copy(tags = List("osinfo" -> "ubuntu"))
        val connectionInfo = sshSession
          .executeCommand("sudo netstat -tnap | grep ESTABLISHED | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 0 {print substr($5, 0, index($5, \":\"))}'");

        if(connectionInfo.isDefined){
          //resolve Connections
          val liveConnections = connectionResolver(connectionInfo,cloudInstance,connectionOption)
          val cloudInstanceNew2 = cloudInstanceNew1.copy(liveConnections = liveConnections.toList)

          //resolveProcesses
          val processList = sshSession
            .executeCommand("sudo netstat -tnap | grep LISTEN | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 0 {print $7}'");
          val cloudInstanceNew3 = cloudInstanceNew2.copy(processes = Set.empty)
          if(processList.isDefined){
            resolveProcesses(site,cloudInstanceNew3.asInstanceOf[CloudInstance],connectionOption,processList.get)
          }
        }
      }
    })

  }
  private def resolveProcesses(site: Site,cloudInstance: CloudInstance,serverIp: String,processList: String)={

    val cloudInstance1 = cloudInstance.copy(tags = cloudInstance.tags.filter(tag => tag._1.equals(PROCESS_TAG)))
    val knowProcessList = SoftwareProcess.getAllProcess

    processList.split(";").map(process => {
      val index = process.indexOf("/")
      val pid = process.substring(0, index)
      val pname = process.substring(index + 1)
      knowProcessList.contains(pname)
      //TODO : create process map
    })

  }
  private def connectionResolver(connectionInfo: Option[String] , instance:CloudInstance , serverIp:String): Set[InstanceConnection] ={
      connectionInfo.map( connectionInfoVal => {
        connectionInfoVal.split(";").toSet.flatMap(otherNodeIp => {
          topology.getInstanceByIp(connectionInfoVal).filter( otherNode =>
            instance.instanceId.isDefined && otherNode.instanceId.isDefined).map( otherNode => {
            new InstanceConnection(None,instance.instanceId.get,otherNode.instanceId.get,List.empty)
          })
          })
        }).getOrElse(Set.empty)
      }

  def init(): Unit ={
    topology.nodes.foreach{ instance1 =>
      val keypairOption = instance1.sshAccessInfo.map(_.keyPair)
      val userName =
        keypairOption.map(keyPair =>
          keyPair.filePath.map( filePathValue =>
            instance1.asInstanceOf[CloudInstance].sshAccessInfo.map( ssh => ssh.userName)
          )
        )
    }
  }
}

sealed trait HostProperty
case object PrivateIp extends HostProperty
case object PublicDns extends HostProperty
case object PrivateIpAndPublicDns extends HostProperty
