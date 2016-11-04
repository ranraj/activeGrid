package com.imaginea.activegrid.core.models

/**
 * Created by ranjithrajd on 2/11/16.
 */
case class CloudInstance(override val id: Option[Long],
                         accountInfo: AccountInfo,
                         override val  name : String,
                         availabilityZone: String,
                         privateDnsName: String,
                         privateIpAddress: String,
                         publicIpAddress: String,
                         override val instanceType: Option[String],
                         elasticIP: String,
                         monitoring: String,
                         rootDeviceType: String,
                         override val image: Option[ImageInfo],
                         blockDeviceMappings: List[InstanceBlockDeviceMappingInfo],
                         securityGroups: List[SecurityGroupInfo],
                         reservedInstance: Boolean,
                         region: String,
                         override val sshAccessInfo: Option[SSHAccessInfo],
                         override val tags: List[(String,String)]
                         ) extends Instance(None, None, name, None, None, None, None, None, None, None, None, List.empty[(String,String)], None, List.empty[InstanceConnection], List.empty[InstanceConnection], Set.empty[ProcessInfo], None, List.empty[InstanceUser]){
}
