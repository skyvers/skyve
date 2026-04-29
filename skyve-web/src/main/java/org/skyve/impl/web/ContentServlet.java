package org.skyve.impl.web;

import java.io.IOException;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.OWASP;
import org.skyve.util.Thumbnail;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Servlet that retrieves and streams {@link AttachmentContent} blobs managed by the
 * Skyve {@link ContentManager}.
 * <p>
 * Given a content UUID supplied as the {@code _r} request parameter, the servlet
 * opens a {@link ContentManager} session, loads the attachment, enforces
 * document-level and content-level access control, and streams the bytes back to
 * the client.  Thumbnail scaling is supported via the standard {@code _w}/{@code _h}
 * parameters inherited from the base class.
 * </p>
 * <p>
 * Access control requirements:
 * <ul>
 *   <li>The request must be authenticated (a Skyve {@link User} must be present in
 *       the session).</li>
 *   <li>The {@code moduleName}, {@code documentName}, and {@code binding} parameters
 *       must all be present so that the framework can locate the owning
 *       {@link Attribute} and evaluate {@link UserAccess} rules.</li>
 *   <li>Users with the text-search permission bypass the {@code UserAccess} check but
 *       still go through row-level content access checks.</li>
 * </ul>
 * </p>
 */
public class ContentServlet extends AbstractResourceServlet {
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(ContentServlet.class);

	/**
	 * {@link AbstractResource} implementation backed by a {@link ContentManager} session
	 * and an {@link AttachmentContent} handle.
	 * <p>
	 * The {@link ContentManager} is opened when the resource is created and is closed in
	 * {@link #dispose()} to ensure the underlying store connection is always released.
	 * </p>
	 */
	private static class ContentResource extends AbstractResource {
		@SuppressWarnings("resource")
		private ContentManager cm;
		private AttachmentContent content;

		/**
		 * Closes the {@link ContentManager} session and releases the
		 * {@link AttachmentContent} reference before delegating to the superclass.
		 */
		@Override
		public void dispose() throws Exception {
			super.dispose();
			if (cm != null) {
				cm.close();
			}
			content = null;
			cm = null;
		}

		/**
		 * Returns the last-modified timestamp of the attachment from its stored metadata,
		 * or the superclass default when no content has been resolved.
		 *
		 * @return milliseconds since the epoch
		 */
		@Override
		public long getLastModified() {
			return (content != null) ? content.getLastModified().getTime() : super.getLastModified();
		}

		/**
		 * Loads the attachment bytes into a {@link Thumbnail}.
		 * <p>
		 * When both {@code imageWidth} and {@code imageHeight} are positive the content
		 * is scaled; otherwise the raw attachment bytes are wrapped directly.
		 * </p>
		 *
		 * @return a {@link Thumbnail} wrapping the attachment, or {@code null} if no
		 *         content has been resolved
		 * @throws IOException if the attachment bytes cannot be read
		 */
		@Override
		protected Thumbnail load() throws IOException {
			Thumbnail result = null;
			if (content != null) {
				if ((imageWidth > 0) && (imageHeight > 0)) { // a thumbnail image
					result = new Thumbnail(content, imageWidth, imageHeight);
				}
				else { // full content
					result = new Thumbnail(content);
				}
			}
			return result;
		}

		/**
		 * Returns the MIME content type stored with the attachment metadata.
		 *
		 * @return the content type string, or {@code null} if no content has been resolved
		 */
		@Override
		protected String resolveContentType() {
			return (content != null) ? content.getContentType() : null;
		}

		/**
		 * Returns the original file name stored with the attachment metadata.
		 *
		 * @return the file name, or {@code null} if no content has been resolved
		 */
		@Override
		protected String resolveFileName() {
			return (content != null) ? content.getFileName() : null;
		}
	}

	/**
	 * Adds HTTP response headers that enable inline display and short-term caching of
	 * the attachment.
	 * <p>
	 * Sets {@code Content-Disposition} to {@code inline} with the OWASP-sanitised file
	 * name, enables one-minute browser caching, and sets {@code Accept-Ranges: bytes}
	 * to support pause-and-resume downloads.
	 * </p>
	 *
	 * @param resource the resolved resource
	 * @param response the HTTP response
	 * @throws IOException if the file name cannot be resolved from the resource
	 */
	@Override
	protected void addResponseHeaders(Resource resource, HttpServletResponse response)
	throws IOException {
		StringBuilder disposition = new StringBuilder(32);
		disposition.append("inline; filename=\"");
		disposition.append(OWASP.sanitiseFileName(resource.getFileName()));
		disposition.append('"');
		response.setHeader("Content-Disposition", disposition.toString());
		// NOTE - the image is not cached unless there is a content length and the following headers
		// NOTE - THIS MUST BE SET FIRST BEFORE WRITING TO THE STREAM
		response.setHeader("Cache-Control", "cache");
		response.setHeader("Pragma", "cache");
		response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
		// The following allows partial requests which are useful for large media or downloading files with pause and resume functions.
		response.setHeader("Accept-Ranges", "bytes");
	}
	
	/**
	 * Enforces access control for the attachment.
	 * <p>
	 * The method is a no-op when no content was found (the servlet will return a 404).
	 * Otherwise it verifies:
	 * <ol>
	 *   <li>The request is authenticated.</li>
	 *   <li>Module, document, and binding parameters are all present.</li>
	 *   <li>The binding resolves to a valid {@link Attribute}.</li>
	 *   <li>The user has the required {@link UserAccess} for the content attribute
	 *       (bypassed for users with the text-search permission).</li>
	 *   <li>The user passes the row-level content access check via
	 *       {@link User#canAccessContent}.</li>
	 * </ol>
	 * </p>
	 *
	 * @param resource         the resolved {@link ContentResource}
	 * @param moduleName       the module name from the request
	 * @param documentName     the document name from the request
	 * @param binding          the attribute binding from the request
	 * @param resourceFileName the content UUID
	 * @param user             the authenticated user, or {@code null} if not logged in
	 * @param uxui             the UX/UI name for access checking
	 * @throws SecurityException if the user is not authorised to access the content
	 */	@Override
	protected void secureResource(Resource resource,
									String moduleName,
									String documentName,
									String binding,
									String resourceFileName,
									User user,
									String uxui)
	throws SecurityException {
		ContentResource contentResource = (ContentResource) resource;
		if (contentResource.content == null) {
			return; // content was not found, nothing to secure
		}
		if (user == null) {
			throw new SecurityException(moduleName + '.' + documentName + '.' + binding, "anonymous");
		}
		else if ((moduleName == null) || (documentName == null) || (binding == null)) {
			throw new DomainException("Module name, document name & binding are required to secure a content resource");
		}

		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																module,
																document,
																binding);
		Attribute attribute = target.getAttribute();
		// If binding points nowhere, then deny
		if (attribute == null) { // should never happen
			throw new SecurityException(moduleName + '.' + documentName + '.' + binding, user.getName());
		}

		// Check that the user has access - use the full binding here
		// NB If you can text search you should already be able to see anything you have access to
		if (! user.canTextSearch()) {
			EXT.checkAccess(user, UserAccess.content(moduleName, documentName, binding), uxui);
		}

		// Check that user has content access - Use the content module and document and the target attribute name
		AttachmentContent content = contentResource.content;
		if (! user.canAccessContent(content.getBizId(),
									content.getBizModule(),
									content.getBizDocument(),
									content.getBizCustomer(),
									content.getBizDataGroupId(),
									content.getBizUserId(),
									attribute.getName())) {
			throw new SecurityException(moduleName + '.' + documentName + '.' + binding, user.getName());
		}
	}

	/**
	 * Creates a {@link ContentResource} for the attachment identified by the request
	 * parameters.
	 * <p>
	 * The content UUID is expected in the {@code _r} parameter and must be a 36-character
	 * string (standard UUID length).  An authenticated user and resolved customer are
	 * also required; any missing precondition is logged and an empty resource is returned
	 * (the servlet will respond with a 404).
	 * </p>
	 * <p>
	 * If the {@code _nm} (no-markup) query parameter is present, any stored markup on
	 * the attachment is cleared so that raw SVG content can be edited inline.
	 * </p>
	 *
	 * @param request the current HTTP request
	 * @param params  parsed request parameters
	 * @return a {@link ContentResource} (possibly with no backing content if not found)
	 * @throws Exception if the content manager cannot be opened or the content cannot
	 *                   be retrieved
	 */
	@Override
	protected Resource createResource(HttpServletRequest request, RequestParams params) throws Exception {
		ContentResource resource = new ContentResource();
		resource.imageWidth = params.imageWidth();
		resource.imageHeight = params.imageHeight();

		if (params.resourceFileName() == null) {
			LOGGER.error("No resource file name or data file name in the URL");
		}
		else {
			if ((params.moduleName() == null) || (params.documentName() == null)) {
				LOGGER.error("No _doc parameter in the URL");
			}
			if ((params.user() != null) &&
					(params.customer() != null) &&
					(params.resourceFileName().length() == 36)) { // its a valid UUID in length at least
				resource.cm = EXT.newContentManager();
				resource.content = resource.cm.getAttachment(params.resourceFileName());
				// if &_nm is in the URL then don't include markup - we are most probably editing SVG
				// PS The content is never put so the mutation below is OK
				if (request.getParameterMap().containsKey(AbstractWebContext.NO_MARKUP)) {
					resource.content.setMarkup(null);
				}
			}
			else {
				LOGGER.error("No skyve user or customer or the contentId is not valid");
			}
		}
		return resource;
	}
}
