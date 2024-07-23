package org.skyve.impl.content.rest;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.util.Util;

import jakarta.enterprise.context.RequestScoped;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;

/**
 * This class is used to expose the content server via REST to another Skyve server.
 * To use this, the contentManager property of the factories property 
 * in the JSON file should be set to this class name and the web.xml for "/rest/content" exposed.
 * 
 * The RestFilter in web.xml is usually set to the ForbiddenFIlter.
 * Putting RegexFilter in front of "/rest/content" enables IP limiting and headers/parameters checking
 * for security.
 * This endpoint should not be exposed to the internet. It's only for content remoting.
 * 
 * @author mike
 *
 * <p/>
 * Like this...<p/>
 * <pre>
 *  JSON
 *  		...
 *			// Factory settings
 *			factories: {
 *			...
 *				// Skyve content manager class
 *				contentManagerClass: "org.skyve.impl.content.jdbc.RestRemoteContentManagerServer"},
 *			...
 *  WEB.XML
 *  		...
 *		    <filter>
 *		        <display-name>APIFilter</display-name>
 *		        <filter-name>APIFilter</filter-name>
 *		        <filter-class>org.skyve.impl.web.filter.rest.ForbiddenFilter</filter-class>
 *		    </filter>
 *			<filter-mapping>
 *		        <filter-name>APIFilter</filter-name>
 *				<url-pattern>/rest/content/*</url-pattern>
 *		    </filter-mapping>
 *		    <filter>
 *		        <display-name>ContentFilter</display-name>
 *		        <filter-name>ContentFilter</filter-name>
 *		        <filter-class>org.skyve.impl.web.filter.rest.RegexFilter</filter-class>
 *		    </filter>
 *			<init-param>
 *				<param-name>RemoteAddr</param-name>
 *				<param-value>127\.0\.0\.1</param-value>
 *			</init-param>
 *			<filter-mapping>
 *		        <filter-name>RestFilter</filter-name>
 *				<url-pattern>/rest/content/*</url-pattern>
 *		    </filter-mapping>
 * </pre>
 */
@Path("content/")
@RequestScoped
public class RestRemoteContentManagerServer {
	protected static final String ATTACHMENT_PATH = "/attachment";
	protected static final String BEAN_PATH = "/bean";

	@PUT
	@Path(BEAN_PATH)
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response put(String content) {
		try (ContentManager cm = EXT.newContentManager()) {
			BeanContent result = StateUtil.decode64(content);
			Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.put() received for " + result.getBizId());
		
			cm.put(result);
			return Response.ok().build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}
	
	@PUT
	@Path(ATTACHMENT_PATH)
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response put(String content, @QueryParam("index") boolean index) {
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent result = StateUtil.decode64(content);
			Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.put() received for " + result.getBizId() + " attribute " + result.getAttributeName());

			cm.put(result, index);
			return Response.ok(result.getContentId()).build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}

	@POST
	@Path(ATTACHMENT_PATH)
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response update(String content) {
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent result = StateUtil.decode64(content);
			Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.update() received for " + result.getContentId());

			cm.update(result);
			return Response.ok().build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}
	
	@GET
	@Path(ATTACHMENT_PATH + "/{contentId}")
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response getAttachment(@PathParam("contentId") String contentId) {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.getAttachment() received for " + contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent content = cm.getAttachment(contentId);
			if (content != null) {
				String result = StateUtil.encode64(content);
				return Response.ok(result).build();
			}
			return Response.status(Status.NOT_FOUND).build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}

	@DELETE
	@Path(BEAN_PATH + "/{bizId}")
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response removeBean(@PathParam(Bean.DOCUMENT_ID) String bizId) {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.removeBean() received for " + bizId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeBean(bizId);
			return Response.ok().build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}

	@DELETE
	@Path(ATTACHMENT_PATH + "/{contentId}")
	@Produces(MediaType.TEXT_PLAIN)
	@SuppressWarnings("static-method")
	public Response removeAttachment(@PathParam("contentId") String contentId) {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.removeAttachment() received for " + contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeAttachment(contentId);
			return Response.ok().build();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return Response.serverError().build();
	}
}
