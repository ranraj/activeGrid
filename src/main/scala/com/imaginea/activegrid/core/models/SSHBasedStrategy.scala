package com.imaginea.activegrid.core.models

import com.imaginea.activegrid.core.models.SoftwareProcess.{PythonProcess, JavaProcess}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
 * Created by ranjithrajd on 3/11/16.
 */
case class SSHBasedStrategy(topology: Topology,
                            softwares: List[Software],
                            execute: Boolean
                             ) {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val PROCESS_TAG: String = "process"
  val ROLES_TAG_KEY: String = "roles"
  val APPLICATION_TAG_KEY = "stack"

  def init :Unit ={
    topology.nodes.foreach { instance =>
      val keypairOption = instance.sshAccessInfo.map(_.keyPair)

      keypairOption.map(keyPair =>
        keyPair.filePath.map(keyFilePath => {
          //Add AKKA for concurrent
          val cloudInstance = instance.asInstanceOf[Instance]
          val userName = cloudInstance.sshAccessInfo.map(ssh =>
            ssh.userName).orElse(keyPair.defaultUser)

          val newInstance = collectInstanceDetails(topology.site, cloudInstance, userName.get, keyFilePath, keyPair.passPhrase)
          logger.info(s"Establishing service to collect instance details ${newInstance}")
        }
        )
      )
    }
  }

  def collectInstanceDetails(site: Site1, cloudInstance: Instance, userName: String, keyLocation: String, passPharse: Option[String]): Instance = {
    val portOption = cloudInstance.sshAccessInfo.map(ssh => ssh.port)
    val hostProp: HostProperty = PrivateIpAndPublicDns

    val connections: List[Option[String]] = hostProp match {
      case PrivateIpAndPublicDns => List(cloudInstance.privateDnsName, cloudInstance.publicIpAddress)
      case PublicDns => List(cloudInstance.publicIpAddress)
      case PrivateIp => List(cloudInstance.privateDnsName)
    }
    for{connection <- connections
        connectionOption <- connection
    }{
      val sshSession = new SSHSession(serverIp = connectionOption,
        userName = userName,
        keyLocation = keyLocation,
        passPhrase = passPharse,
        port = portOption)
      logger.info("Establishing Ssh connection")
      val osInfo = sshSession.executeCommand("uname -ar")
      //resolve OS

      if (osInfo.isDefined && osInfo.get.toLowerCase().contains("ubuntu")) {
        val cloudInstanceNew1 = cloudInstance.copy(tags = List(KeyValueInfo(None,"osinfo" , "ubuntu")))
        val connectionInfo = sshSession
          .executeCommand("sudo netstat -tnap | grep ESTABLISHED | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 0 {print substr($5, 0, index($5, \":\"))}'");

        if (connectionInfo.isDefined) {
          //resolve Connections
          val liveConnections = connectionResolver(connectionInfo, cloudInstance, connectionOption)
          val cloudInstanceNew2 = cloudInstanceNew1.copy(liveConnections = liveConnections.toList)

          //resolveProcesses
          val processList = sshSession
            .executeCommand("sudo netstat -tnap | grep LISTEN | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 0 {print $7}'");
          val cloudInstanceNew3 = cloudInstanceNew2.copy(processes = Set.empty)
          if (processList.isDefined) {
            val (cloudInstanceResult ,siteResult) = resolveProcesses(site, cloudInstanceNew3, connectionOption, processList.get, sshSession)
            //TODO: Return cloudInstanceResult and siteResult
            logger.info(s"Site ${siteResult}")
            logger.info(s"Instance ${cloudInstanceResult}")
          }else{
            logger.info(s"Process list is empty $processList")
          }
          sshSession.close();
          logger.info("Closing Ssh connection")
        }
      }
    }
   /* connections.foreach(connection => {

    })*/
    cloudInstance
  }

  private def resolveProcesses(site1: Site1, cloudInstance: Instance, serverIp: String, processList: String, sshSession: SSHSession) : (Instance,Site1) = {
    /*
     * Collect map of process id and name and
     * filter with know software process
     */
    def findProcessMap(processList: String): Map[String,String]={
      processList.split(";").map(process => {
        val index = process.indexOf("/")
        val pid = process.substring(0, index)
        val pName = process.substring(index + 1)
        (pid -> pName)
      }).toMap.filter(process => SoftwareProcess.getAllProcess.contains(process._2))
    }
    /*
     * Find process name for JavaProcess and Python Process
     */
    def findProcessName(pname: String, pid: String): String = {
      SoftwareProcess.toSoftwareProcess(pname) match {
        case JavaProcess => {
          val procInfo = sshSession.executeCommand("ps -p " + pid
            + " -o args=ARGS | awk 'BEGIN {OFS=\",\"; ORS= \";\"} NR > 1'"); // -o
          // vsz=MEMORY
          procInfo.filter(procarg => {
            KnownSoftware.getStartUpClazzMap.contains(procarg)
          }).head
        }
        case PythonProcess => {
          val procInfo = sshSession.executeCommand("ps -p " + pid
            + " | grep -v CMD | awk '{print $4}'")
          val result = if (procInfo.contains("carbon-cache")) {
            val chkLb = "sudo netstat -apnt |grep LISTEN|grep :80|awk 'BEGIN {OFS=\",\";ORS=\";\"} NR > 0 {print $4}'"
            val netStatInfo = sshSession.executeCommand(chkLb)
            netStatInfo.map(netStat => {
              netStat.split(";").filter(host => host.endsWith(":80")).map(tmp => KnownSoftware.Graphite.name).head
            }).getOrElse(pname)
          } else {
            pname
          }
          result
        }
        case _ => pname
      }
    }
    // Remove process tag in Instance
    val cloudInstance1 = cloudInstance.copy(tags = cloudInstance.tags.filter(tag => !tag.key.equals(PROCESS_TAG))).asInstanceOf[Instance]
    // Find the process map
    // Iterate and add process details in Instance
   val (cloudInstanceResult , siteResult) = findProcessMap(processList).foldLeft((cloudInstance1 -> site1))((acc, process)  => {
      val (pId, pName) = process
      val (cloudInstance,site) = acc
      //Find process name with clazz name
      val processName: String = findProcessName(pId,pName)

      // Collect all softwares which associated with process name
      val softwareAssociatedWithProcessOpt = findSoftwareAssociatedWithProcess(processName)

      //asInstance need to handle using type
      // Update processInfo and tags in Instance
      val instance2: Instance = cloudInstance.copy(processes = cloudInstance.processes +
        new ProcessInfo(pid = Option(pId.toInt)
          , parentPid = Option(pId.toInt)
          , name = Option(processName)
          , software = softwareAssociatedWithProcessOpt
          , id = None
          , command = None
          , owner = None
          , residentBytes = None
          , softwareVersion = None
        )
        , tags = KeyValueInfo(None,PROCESS_TAG,processName) :: cloudInstance1.tags)

      softwareAssociatedWithProcessOpt.map(softwareAssociatedWithProcess => {
        val cloudInstance3 = setRole(instance2,softwareAssociatedWithProcess)
        if(softwareAssociatedWithProcess.discoverApplications){
          val apps = resolveApplication(sshSession,pId,pName,softwareAssociatedWithProcess)

          val cloudInstanceFinal = apps.foldLeft(cloudInstance3)((accInstance, app) => {
            accInstance.copy(tags = (accInstance.tags :+ KeyValueInfo(None,APPLICATION_TAG_KEY,app)))
          })
          val applicationsAndInstance = apps.map(app  => {
            new Application(None,app,app,"1.0",List(cloudInstance3),softwareAssociatedWithProcess,List.empty,/*None,*/0)
          }).toList

          (cloudInstanceFinal -> site.copy(applications = applicationsAndInstance))
        }else{
          (cloudInstance3 -> site)
        }
      }).getOrElse((cloudInstance -> site))
    })
    (cloudInstanceResult -> siteResult)
  }
  /* Resolve application */
  private def resolveApplication(sshSession: SSHSession,pid: String,processName: String,software: Software): Set[String] ={
    def processFileName(fileName: String): Option[String]={
      val f = fileName.trim
      if(f.contains("/WEB-INF")){
        f.split("/WEB-INF")(0)
        Some(f.substring(f.lastIndexOf("/") + 1))
      }else{
        None
      }
    }
    val fileInfoOption = sshSession.executeCommand("sudo lsof -p " + pid
      + "| awk 'BEGIN {OFS=\"|\";ORS=\";\"} NR > 0 {print $3,$9,$7}'")

    SoftwareProcess.toSoftwareProcess(processName) match {
      case SoftwareProcess.JavaProcess => {
        val filesOption = fileInfoOption.map(_.split(";"))
        val applications: Array[String] =  filesOption.map(files => {
          files.flatMap(file => file.split("\\|").flatMap(fileData => processFileName(fileData)))
        }).getOrElse(Array.empty)
        val knowSoftware = KnownSoftware.toKnownSoftware(software.name)
        val otherApplications = knowSoftware.applicationDiscoveryHelper.map(_.discoverApplications(pid ,sshSession)).getOrElse(Set.empty)
        (applications ++ otherApplications).toSet
      }
      case _ => Set.empty
    }
  }
  private def setRole(instance: Instance, softwareAssociatedWithProcess: Software): Instance = {
    val roleOption = InstanceRole.hasSoftware(softwareAssociatedWithProcess)
    val tagOption = instance.tags.find(tag => tag.value.equals(ROLES_TAG_KEY))
    val role = roleOption.map(r => {
      tagOption.map(tag => {
        tag.copy(value = r.name)
      }).getOrElse(KeyValueInfo(None,ROLES_TAG_KEY,r.name))
    })
    role.map(role => instance.copy(tags = instance.tags))
      .getOrElse(instance)
  }

  private def findSoftwares: List[Software] ={
    val softwareLabel: String = "SoftwaresTest2"
    val nodesList = Neo4jRepository.getNodesByLabel(softwareLabel)
    nodesList.flatMap(node => Software.fromNeo4jGraph(node.getId))
  }
  private def findSoftwareAssociatedWithProcess(processName: String): Option[Software] = {
    findSoftwares.find(software => software.processNames.contains(processName))
  }

  private def connectionResolver(connectionInfo: Option[String], instance: Instance, serverIp: String): Set[InstanceConnection] = {
    connectionInfo.map(connectionInfoVal => {
      connectionInfoVal.split(";").toSet[String]
        .flatMap(otherNodeIp =>
          topology.getInstanceByIp(otherNodeIp)
            .filter(otherNode =>
              instance.instanceId.isDefined && otherNode.instanceId.isDefined)
            .map(otherNode => {
              new InstanceConnection(None, instance.instanceId.get, otherNode.instanceId.get, List.empty)
            }))
    }).getOrElse(Set.empty)
  }

}

sealed trait HostProperty

case object PrivateIp extends HostProperty

case object PublicDns extends HostProperty

case object PrivateIpAndPublicDns extends HostProperty
