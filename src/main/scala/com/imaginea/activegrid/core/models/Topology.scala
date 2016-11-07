package com.imaginea.activegrid.core.models

/**
 * Created by ranjithrajd on 2/11/16.
 */
case class Topology(site: Site1,
                    keyNames : Set[String] = Set.empty
                    ) {

  val nodes: List[Instance] = site.instances.map(instance => instance.asInstanceOf[CloudInstance])
  val idVsInstance: Map[String,Instance] = nodes.map(node => (node.instanceId.get -> node)).toMap

  //TODO : getInstanceByIp
  def getInstanceByIp(serverIp: String ): Option[CloudInstance] = {
    val instanceResult = nodes.find(instance => {
      val cloudInstance = instance.asInstanceOf[CloudInstance]
      val privateIpAddress = cloudInstance.privateIpAddress
      val publicIpAddress = cloudInstance.publicIpAddress
      privateIpAddress.equals(serverIp) || publicIpAddress.equals(serverIp) || cloudInstance.name.equals(serverIp)
    }).map(_.asInstanceOf[CloudInstance])
    instanceResult
  }
}

object Topology{

}