package org.skyve.impl.content.ejb;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.SearchResults;

/**
 * To Setup remoting comms in wildly 10...
 * 
 * We need to edit the standalone.xml to enable EJB communication within our dev servers.
 * start command prompt in admin mode
 * cd $WILDFLY_HOME/bin/
 * add-user.[sh|bat] with
 * user type = b (Application User)
 * username = ejb
 * groups = <none>
 * remoting server to server = yes
 * copy the <secret value="c3lzdGVtMDE=" /> output.
 * 
 * Open your Wildfly installations' standalone/configuration/standalone.xml
 * under <management><security-realms> 
 * add the following (replace the secret value with the one output by add-user)
 *		<security-realm name="ejb-security-realm">
 *  		<server-identities>
 *      		<secret value="c3lzdGVtMDE="/>
 *  		</server-identities>
 *		</security-realm>
 * under <subsystem xmlns="urn:jboss:domain:remoting:3.0">
 * add the following 
 *		<outbound-connections>
 *			<remote-outbound-connection name="remote-ejb-connection" outbound-socket-binding-ref="remote-ejb" username="ejb" security-realm="ejb-security-realm" protocol="http-remoting">
 *				<properties>
 *					<property name="SASL_POLICY_NOANONYMOUS" value="false"/>
 *					<property name="SSL_ENABLED" value="false"/>
 *				</properties>
 *			</remote-outbound-connection>
 *		</outbound-connections>
 * under <socket-binding-group name="standard-sockets">
 * add the following 
 *		<outbound-socket-binding name="remote-ejb">
 *			<remote-destination host="localhost" port="8080"/>
 *		</outbound-socket-binding>
 * Add /WEB-INF/jboss-ejb-client.xml to your project with the following contents...
 * <jboss-ejb-client xmlns="urn:jboss:ejb-client:1.0">
 *     <client-context>
 *         <ejb-receivers>
 *             <remoting-ejb-receiver outbound-connection-ref="remote-ejb-connection"/>
 *         </ejb-receivers>
 *     </client-context>
 * </jboss-ejb-client>
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
