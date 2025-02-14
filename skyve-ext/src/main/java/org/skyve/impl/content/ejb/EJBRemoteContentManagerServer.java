package org.skyve.impl.content.ejb;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.SearchResults;

/**
 * To Setup remoting comms in wildly 33...
 * 
 * We need to edit the standalone.xml to enable EJB communication within our dev servers.
 * start command prompt in admin mode
 * cd $WILDFLY_HOME/bin/
 * add-user.[sh|bat] -a -u ejb -p ejb -g
 * (adds an application user called ejb with password ejb with no groups)
 * 
 * Open your Wildfly installations' standalone/configuration/standalone.xml
 * Add
 *				<application-security-domain name="ejb" security-domain="ApplicationDomain"/>
 * to
 * 		<subsystem xmlns="urn:jboss:domain:ejb3:10.0">
 * 			<application-security-domains>
 * after
 * 				<application-security-domain name="other" security-domain="ApplicationDomain"/>
 * 
 * Add
 * 				<authentication-client>
 *					<authentication-configuration name="ejb-outbound-configuration" authentication-name="ejb" authorization-name="ejb" realm="ApplicationRealm">
 *						<credential-reference clear-text="ejb"/>
 *					</authentication-configuration>
 * 					<authentication-context name="ejb-outbound-context">
 *						<match-rule authentication-configuration="ejb-outbound-configuration"/>
 *					</authentication-context>
 *				</authentication-client>
 * to
 *         <subsystem xmlns="urn:wildfly:elytron:community:18.0" final-providers="combined-providers" disallowed-providers="OracleUcrypto">
 * before
 * 				<providers>
 * 
 * Add
 *                        <mechanism mechanism-name="PLAIN"/>
 * to
 *				<sasl>
 *					<sasl-authentication-factory name="application-sasl-authentication" sasl-server-factory="configured" security-domain="ApplicationDomain">
 *						<mechanism-configuration>
 * before
 * 							<mechanism mechanism-name="JBOSS-LOCAL-USER" realm-mapper="local"/>
 * 
 * Add
 * 				<outbound-connections>
 *					<remote-outbound-connection name="ejb-outbound-connection" outbound-socket-binding-ref="ejb-outbound" authentication-context="ejb-outbound-context"/>
 *				</outbound-connections>
 * to
 * 			<subsystem xmlns="urn:jboss:domain:remoting:7.0">
 * after
 * 				<http-connector name="http-remoting-connector" connector-ref="default" sasl-authentication-factory="application-sasl-authentication"/>
 * 
 * Add
 *			<outbound-socket-binding name="ejb-outbound">
 *				<remote-destination host="<remote-host-name-or-IP" port="8080"/>
 *			</outbound-socket-binding>
 * to
 * 		<socket-binding-group name="standard-sockets" default-interface="public" port-offset="${jboss.socket.binding.port-offset:0}">
 * after
 * 			<socket-binding name="txn-status-manager" port="4713"/>
 * 
 * Add src/main/webapp/WEB-INF/jbos-ejb-client.xml with the contents
 *	<jboss-ejb-client xmlns="urn:jboss:ejb-client:1.0">
 *		<client-context>
 *			<ejb-receivers>
 *				<remoting-ejb-receiver outbound-connection-ref="ejb-outbound-connection"/>
 * 			</ejb-receivers>
 *		</client-context>
 *	</jboss-ejb-client>
 *
 * The remote ejb will be at the following JNDI name (through the ejb outbound connection referenced by the web archive name)
 * ejb:/<war name of remote deployment without ".war" - eg skyve-war>//EJBRemoteContentManagerServerBean!org.skyve.impl.content.ejb.EJBRemoteContentManagerServer
 *
 * @author mike
 */
public interface EJBRemoteContentManagerServer {
	public void put(BeanContent content) throws Exception;

	/**
	 * 
	 * @param content The AttachmentContent to put.
	 * @param index	whether to index of not
	 * @return	The contentId.
	 * @throws Exception
	 */
	public String put(AttachmentContent content, boolean index) throws Exception;

	public void update(AttachmentContent content) throws Exception;

	public AttachmentContent getAttachment(String contentId) throws Exception;
	
	public void removeBean(String bizId) throws Exception;
	
	public void removeAttachment(String contentId) throws Exception;
	
	public SearchResults google(String search, int maxResults) throws Exception;
}
