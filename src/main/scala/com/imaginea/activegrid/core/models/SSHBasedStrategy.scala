package com.imaginea.activegrid.core.models

import com.imaginea.activegrid.core.models.SoftwareProcess.{JavaProcess, PythonProcess}

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
            resolveProcesses(site,cloudInstanceNew3.asInstanceOf[CloudInstance],connectionOption,processList.get,sshSession)
          }
        }
      }
    })

  }
  private def resolveProcesses(site: Site,cloudInstance: CloudInstance,serverIp: String,processList: String, sshSession: SSHSession)={

    val cloudInstance1 = cloudInstance.copy(tags = cloudInstance.tags.filter(tag => tag._1.equals(PROCESS_TAG)))
    val knowProcessList = SoftwareProcess.getAllProcess

    val processMap = processList.split(";").map(process => {
      val index = process.indexOf("/")
      val pid = process.substring(0, index)
      val pname = process.substring(index + 1)
      (pid -> pname)
    }).toMap.filter( process => knowProcessList.contains(process._2))

    processMap.foreach { case (pid, pname) =>
      val processName: String = SoftwareProcess.toSoftwareProcess(pname) match {
        case JavaProcess => {
          val procInfo = sshSession.executeCommand("ps -p " + pid
            + " -o args=ARGS | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 1'"); // -o
          // vsz=MEMORY
          procInfo.filter( procarg => {
              KnownSoftware.getStartUpClazzMap.contains(procarg)
            }).head
        }
        case PythonProcess => {
         val procInfo = sshSession.executeCommand("ps -p " + pid
            + " | grep -v CMD | awk '{print $4}'")
          val result = if (procInfo.contains("carbon-cache")) {
            val chkLb = "sudo netstat -apnt |grep LISTEN|grep :80|awk 'BEGIN {OFS=\",\";ORS=\";\"} NR > 0 {print $4}'"
            val netStatInfo = sshSession.executeCommand(chkLb)
            netStatInfo.map( netStat => {
              netStat.split(";").filter(host => host.endsWith(":80")).map( tmp => KnownSoftware.Graphite.name).head
            }).getOrElse(pname)
          }else{
            pname
          }
          result
        }
        case _ => pname
      }
      val softwareAssociatedWithProcess = getSoftwareAssociatedWithProcess(processName)
      val processInfo = new ProcessInfo(pid = pid.toInt
        , parentPid = pid.toInt
        ,name = processName
        ,software = softwareAssociatedWithProcess
        ,id = None
        ,command = None
        ,owner = None
        ,residentBytes = None
        ,softwareVersion = None
      )
      val cloudInstance2 = cloudInstance.copy(processes = cloudInstance.processes + processInfo
        ,tags = (PROCESS_TAG -> processName) :: cloudInstance.tags )
    }
  }

  private def getSoftwareAssociatedWithProcess(processName: String): Option[Software] = {
    val softwareLabel: String = "SoftwaresTest2"
    val nodesList = GraphDBExecutor.getNodesByLabel(softwareLabel)
    val softwares = nodesList.flatMap(node => Software.fromNeo4jGraph(node.getId))

    softwares.find(software => software.processNames.contains(processName))
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
