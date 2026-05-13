package org.skyve.impl.web;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.util.Thumbnail;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Base servlet for serving binary or text resources (files, content-managed blobs,
 * dynamic images, etc.) from the Skyve repository.
 * <p>
 * The servlet extracts a common set of request parameters once per HTTP request,
 * delegates to the subclass to locate and secure the resource, then streams the
 * bytes back to the client with an appropriate content type and any extra response
 * headers the subclass wishes to add.
 * </p>
 * <p>
 * A thread-local {@link Resource} handle ({@link #RESOURCES}) is used to share the
 * resolved resource across the Servlet API's separate {@code getLastModified} and
 * {@code doGet} calls within the same request, and is always disposed in the
 * {@link #service} finalisation block.
 * </p>
 * <p>
 * Concrete subclasses must implement {@link #createResource} and may optionally
 * override {@link #secureResource} and {@link #addResponseHeaders}.
 * </p>
 */
public abstract class AbstractResourceServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractResourceServlet.class);

	/**
	 * Parsed common request parameters shared across resource servlet implementations.
	 */
	protected record RequestParams(
			@Nullable String moduleName,
			@Nullable String documentName,
			@Nullable String binding,
			@Nullable String resourceFileName,
			int imageWidth,
			int imageHeight,
			@Nullable User user,
			@Nullable Customer customer,
			@Nullable String customerName) {}

	/**
	 * Represents a resolved, ready-to-serve resource.
	 * <p>
	 * Instances are held in a thread-local and are always disposed after the response
	 * has been written; implementations should release any file handles, streams, or
	 * cached byte arrays in {@link #dispose()}.
	 * </p>
	 */
	protected interface Resource {
		/**
		 * Releases all resources held by this object.
		 * Called unconditionally after the response has been written.
		 */
		@SuppressWarnings("java:S112") // Dispose often involves Closeables
		void dispose() throws Exception;

		/**
		 * Returns the last-modified timestamp of the resource for HTTP conditional
		 * request support, or {@code -1} if unknown.
		 *
		 * @return milliseconds since the epoch, or {@code -1}
		 */
		default long getLastModified() {
			return -1;
		}

		/**
		 * Returns the raw bytes of the resource.
		 *
		 * @return the resource bytes, or {@code null} if the resource could not be found
		 * @throws IOException if the bytes cannot be read
		 */
		@Nullable byte[] getBytes() throws IOException;

		/**
		 * Returns the MIME content type of the resource, e.g. {@code "image/png"}.
		 *
		 * @return the content type string, or {@code null} if it cannot be determined
		 * @throws IOException if the content type cannot be determined
		 */
		@Nullable String getContentType() throws IOException;

		/**
		 * Returns the file name to use for the resource, e.g. in a
		 * {@code Content-Disposition} header.
		 *
		 * @return the file name, or {@code null} if not applicable
		 * @throws IOException if the file name cannot be determined
		 */
		@Nullable String getFileName() throws IOException;
	}

	/**
	 * Base resource implementation that provides thumbnailing behaviour.
	 * Subclasses supply the backing data (file or content) via {@link #load()},
	 * {@link #resolveContentType()} and {@link #resolveFileName()}.
	 */
	protected abstract static class AbstractResource implements Resource {
		protected int imageWidth = 0;
		protected int imageHeight = 0;
		private @Nullable Thumbnail image;

		/**
		 * Returns the bytes of this resource, loading them on first access.
		 *
		 * @return the resource bytes, or {@code null} if the backing data is not available
		 * @throws IOException if the data cannot be read
		 */
		@Override
		public byte[] getBytes() throws IOException {
			if (image == null) {
				image = load();
			}
			return (image == null) ? null : image.getBytes();
		}

		/**
		 * Dispose of any resources held by this object.
		 * Subclasses should override and call super.dispose() after disposing of their own resources
		 */
		@Override
		public void dispose() throws Exception{
			image = null;
		}
		
		/**
		 * Loads the backing data into {@link #image}.
		 * Implementations must guard against repeated loading (check {@code image == null}).
		 */
		protected abstract @Nullable Thumbnail load() throws IOException;

		/**
		 * Returns the content type when not in thumbnail mode.
		 */
		protected abstract @Nullable String resolveContentType();

		/**
		 * Returns the file name when not in thumbnail mode.
		 */
		protected abstract @Nullable String resolveFileName();

		/**
		 * Returns the MIME content type for this resource.
		 * <p>
		 * When thumbnail dimensions are set the type is derived from the scaled
		 * {@link Thumbnail}; otherwise {@link #resolveContentType()} is used.
		 * </p>
		 *
		 * @return the content type, or {@code null} if it cannot be determined
		 * @throws IOException if the thumbnail must be loaded to determine the type
		 */
		@Override
		public String getContentType() throws IOException {
			if ((imageWidth > 0) && (imageHeight > 0)) {
				if (image == null) {
					image = load();
				}
				if (image != null) {
					return image.getMimeType().toString();
				}
				return null;
			}
			return resolveContentType();
		}

		/**
		 * Returns a file name for this resource.
		 * <p>
		 * When thumbnail dimensions are set a synthetic name such as
		 * {@code "thumbnail.png"} is returned; otherwise {@link #resolveFileName()}
		 * is used.
		 * </p>
		 *
		 * @return the file name, or {@code null} if it cannot be determined
		 * @throws IOException if the thumbnail must be loaded to determine the type
		 */
		@Override
		public String getFileName() throws IOException {
			if ((imageWidth > 0) && (imageHeight > 0)) {
				if (image == null) {
					image = load();
				}
				if (image != null) {
					return "thumbnail." + image.getMimeType().getStandardFileSuffix();
				}
				return null;
			}
			return resolveFileName();
		}
	}

	/**
	 * Used to hold the resource per thread.
	 */
	private static final ThreadLocal<Resource> RESOURCES = new ThreadLocal<>();

	/**
	 * Dispatches the request via the standard servlet pipeline and guarantees that the
	 * thread-local {@link Resource} is disposed of when the response has been written,
	 * even if an exception is thrown.
	 *
	 * @param request  the current HTTP request
	 * @param response the current HTTP response
	 * @throws ServletException if the request dispatching fails
	 * @throws IOException      if an I/O error occurs
	 */
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		try {
			super.service(request, response);
		}
		finally {
			Resource resource = RESOURCES.get();
			if (resource != null) {
				try {
					resource.dispose();
				}
				catch (Exception e) {
					LOGGER.error("Could not dispose of the thread-local content resource properly. It has been removed from the thread local storage.", e);
				}
				finally {
					RESOURCES.remove();
				}
			}
		}
	}

	/**
	 * Returns the last-modified timestamp of the resource identified by the request.
	 * <p>
	 * If the resource has not yet been created it is resolved and cached in the
	 * thread-local store so that the subsequent {@link #doGet} call can reuse it
	 * without repeating the potentially expensive lookup.
	 * </p>
	 *
	 * @param request the current HTTP request
	 * @return milliseconds since the epoch, or {@code -1} if unknown or on error
	 */
	@Override
	protected long getLastModified(HttpServletRequest request) {
		try {
			Resource resource = RESOURCES.get();
			if (resource == null) {
				RequestParams params = parseRequestParams(request);
				resource = createResource(request, params);
				secureResource(resource,
								params.moduleName(),
								params.documentName(),
								params.binding(),
								params.resourceFileName(),
								params.user(),
								UserAgent.getUxUi(request).getName());
				RESOURCES.set(resource);
			}

			return resource.getLastModified();
		}
		catch (Exception e) {
			LOGGER.error("Problem getting the last modified of the resource - {}", e.toString(), e);
			return -1;
		}
	}

	/**
	 * Handles GET requests by resolving the resource, setting the appropriate content
	 * type and headers, and streaming the bytes to the response.
	 * <p>
	 * The resource is retrieved from the thread-local cache when
	 * {@link #getLastModified} has already populated it (conditional request path);
	 * otherwise it is created fresh.  A {@code 404} is returned if the resource bytes
	 * are {@code null}; a {@code 403} for a {@link SecurityException}; and a
	 * {@code 500} for any other error.
	 * </p>
	 *
	 * @param request  the current HTTP request
	 * @param response the current HTTP response
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		try {
			Resource resource = RESOURCES.get();
			if (resource == null) {
				RequestParams params = parseRequestParams(request);
				resource = createResource(request, params);
				secureResource(resource,
								params.moduleName(),
								params.documentName(),
								params.binding(),
								params.resourceFileName(),
								params.user(),
								UserAgent.getUxUi(request).getName());
				RESOURCES.set(resource);
			}

			String contentType = resource.getContentType();
			if (contentType != null) {
				response.setContentType(contentType);
				// Only set char encoding for text types as it can do weird things in chrome - eg for images
				if (contentType.startsWith("text/")) {
					response.setCharacterEncoding(StandardCharsets.UTF_8.name());
				}
			}
			else { // if we don't know the content type, set the char encoding to be safe (no charset sniffing)
				response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			}

			addResponseHeaders(resource, response);

			byte[] bytes = resource.getBytes();
			if (bytes == null) {
				response.sendError(HttpServletResponse.SC_NOT_FOUND);
				LOGGER.error("Problem getting the resource - {} was not found.", resource.getFileName());
				return;
			}

			response.setContentLength(bytes.length);
			try (OutputStream out = response.getOutputStream()) {
				Util.chunkBytesToOutputStream(bytes, out);
				out.flush();
			}
		}
		catch (SecurityException e) {
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			LOGGER.error("Problem getting the resource - {}", e.toString(), e);
		}
		catch (Exception e) {
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			LOGGER.error("Problem getting the resource - {}", e.toString(), e);
		}
	}


	/**
	 * Creates and populates a Resource for the given request.
	 * Implementations must set either resource.file or resource.cm/resource.content,
	 * then call secure() before returning.
	 *
	 * @param request the HTTP request
	 * @return the populated Resource - never null
	 * @throws Exception if there is a problem creating the resource, including SecurityException if access should be denied
	 */
	@SuppressWarnings("java:S112") // Allow throwing generic Exception to cover all possible issues in resource creation, including IO and security exceptions.
	protected abstract @Nonnull Resource createResource(HttpServletRequest request, RequestParams params) throws Exception;

	/**
	 * Called after the response content type is set, before bytes are written.
	 * Subclasses may override to add Content-Disposition, caching, or other headers.
	 *
	 * @param resource the resolved resource
	 * @param response the HTTP response
	 * @throws IOException if there is a problem accessing the resource properties
	 */
	protected void addResponseHeaders(Resource resource, HttpServletResponse response)
	throws IOException {
		// no-op by default
	}

	/**
	 * Throws SecurityException if the resource should not be served.
	 * The default implementation is a no-op; override to add access control.
	 *
	 * @param resource			The resolved resource - never null.
	 * @param moduleName		The module name in the request - may be null.
	 * @param documentName		The document name in the request - may be null.
	 * @param binding			The binding in the request - can be null.
	 * @param resourceFileName	The file/content identifier - never null.
	 * @param user				The logged in user or null if not logged in.
	 * @param uxui				UxUi name for access checking.
	 * @throws SecurityException
	 */
	protected void secureResource(@Nonnull Resource resource,
									@Nullable String moduleName,
									@Nullable String documentName,
									@Nullable String binding,
									@Nonnull String resourceFileName,
									@Nullable User user,
									@Nullable String uxui)
	throws SecurityException {
		// no-op by default; override to add access control
	}
	
	/**
	 * Parses common request parameters shared by all resource servlet implementations.
	 * Also resolves the current user and customer from the session and cookies.
	 *
	 * @param request the HTTP request
	 * @return the parsed request parameters - never null
	 */
	@SuppressWarnings("java:S3776") // complexity is justified by having all the parameters processed in one place
	private static RequestParams parseRequestParams(HttpServletRequest request) {
		String documentName = Util.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME));
		String moduleName = null;
		if (documentName != null) {
			int dotIndex = documentName.indexOf('.');
			if (dotIndex < 0) {
				LOGGER.error("Module/Document is malformed in the URL");
			}
			else {
				moduleName = documentName.substring(0, dotIndex);
				documentName = documentName.substring(dotIndex + 1);
			}
		}
		String binding = Util.processStringValue(request.getParameter(AbstractWebContext.BINDING_NAME));
		String resourceFileName = Util.processStringValue(request.getParameter(AbstractWebContext.RESOURCE_FILE_NAME));

		int imageWidth = 0;
		int imageHeight = 0;
		try {
			String imageWidthParam = UtilImpl.processStringValue(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_NAME));
			if (imageWidthParam != null) {
				imageWidth = Integer.parseInt(imageWidthParam);
			}
			String imageHeightParam = UtilImpl.processStringValue(request.getParameter(DynamicImageServlet.IMAGE_HEIGHT_NAME));
			if (imageHeightParam != null) {
				imageHeight = Integer.parseInt(imageHeightParam);
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			imageWidth = 0;
			imageHeight = 0;
			LOGGER.error("Width/Height is malformed in the URL");
		}

		HttpSession session = request.getSession(false);
		User user = (session == null) ? null : (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		Customer customer = null;
		String customerName = null;
		if (user != null) { // we are logged in
			customer = user.getCustomer();
			customerName = customer.getName();
			AbstractPersistence.get().setUser(user);
		}
		else { // not logged in
			// Get the customer name from the JSON properties, if defined
			if (UtilImpl.CUSTOMER != null) {
				customerName = UtilImpl.CUSTOMER;
			}
			else {
				// Get the customer name if it is in a cookie from the HomeServlet
				Cookie[] cookies = request.getCookies();
				if (cookies != null) {
					for (int i = 0, l = cookies.length; i < l; i++) {
						Cookie cookie = cookies[i];
						if (AbstractWebContext.CUSTOMER_COOKIE_NAME.equals(cookie.getName())) {
							customerName = cookie.getValue();
							break;
						}
					}
				}
			}
		}

		return new RequestParams(moduleName,
									documentName,
									binding,
									resourceFileName,
									imageWidth,
									imageHeight,
									user,
									customer,
									customerName);
	}
}
