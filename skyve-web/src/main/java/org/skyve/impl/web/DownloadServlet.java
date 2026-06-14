package org.skyve.impl.web;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import org.slf4j.Logger;

/**
 * Implements file downloads for Skyve.
 *
 * <p>Servlet API override parameters are intentionally left unannotated because
 * {@link HttpServlet} does not declare nullness constraints for them.
 */
public class DownloadServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DownloadServlet.class);

	/**
	 * Resolves and streams the result of a document download action for the current conversation bean.
	 *
	 * <p>This method restores the cached web conversation, binds its persistence context to the current
	 * thread, re-establishes the authenticated user, enforces download-action security, executes document
	 * interception hooks, and writes either in-memory bytes, a backing file, or a streamed payload to the
	 * servlet response.
	 *
	 * <p>Side effects: mutates response headers, may cache the conversation again after streaming completes,
	 * and writes a minimal HTML error payload when download generation fails unexpectedly.
	 *
	 * @param request the HTTP request containing conversation, document, and resource identifiers
	 * @param response the HTTP response used to stream generated download content
	 * @throws ServletException when servlet processing fails before response streaming can complete
	 * @throws IOException when request or response I/O fails
	 */
	@Override
	@SuppressWarnings({"java:S1989", "java:S3776", "java:S6541"}) // there exists JavaEE error pages; complexity OK
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processDownload(request, response, false);
	}

	/**
	 * Handles HEAD requests through the same action/security path as GET without
	 * writing the download body.
	 *
	 * @param request the HTTP request containing conversation, document, and resource identifiers
	 * @param response the HTTP response used to return generated download headers
	 * @throws ServletException when servlet processing fails before response headers can be completed
	 * @throws IOException when request or response I/O fails
	 */
	@Override
	@SuppressWarnings({"java:S1989", "java:S3776", "java:S6541"}) // there exists JavaEE error pages; complexity OK
	protected void doHead(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processDownload(request, response, true);
	}

	@SuppressWarnings({"java:S3776", "java:S6541"}) // Existing download lifecycle is intentionally kept together.
	private void processDownload(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response, boolean headOnly)
	throws IOException {
		try {
			String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
			AbstractWebContext webContext = getCachedConversation(contextKey, request);
			if (webContext == null) {
				throw new ConversationEndedException(request.getLocale());
			}

			AbstractPersistence persistence = webContext.getConversation();
			persistence.setForThread();
				try {
					try {
						persistence.begin();
						HttpSession session = request.getSession(false);
						User user = (session == null) ? null : (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
						if (user == null) {
							throw new SessionEndedException(request.getLocale());
						}
						persistence.setUser(user);
	
						CustomerImpl customer = (CustomerImpl) user.getCustomer();
			
						String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);
						int dotIndex = documentName.indexOf('.');
						String moduleName = documentName.substring(0, dotIndex);
						documentName = documentName.substring(dotIndex + 1);
						Module module = customer.getModule(moduleName);
						Document document = module.getDocument(customer, documentName);
						String resourceName = request.getParameter(AbstractWebContext.RESOURCE_FILE_NAME);
						if (! user.canExecuteAction(document, resourceName)) {
							throw new SecurityException(resourceName, user.getName());
						}
						DownloadAction<Bean> downloadAction = document.getDownloadAction(customer, resourceName, true);
						Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);

					boolean vetoed = customer.interceptBeforeDownloadAction(document, resourceName, bean, webContext);
					Download result = null;
					if (! vetoed) {
						result = downloadAction.download(bean, webContext);
						@SuppressWarnings("resource") // The lifecycle requires processed() before close(); the finally block handles both.
						WebFileInputStream stream = result.getInputStream();
						try { // ensure stream is always closed
							customer.interceptAfterDownloadAction(document, resourceName, bean, result, webContext);
							writeDownloadResponse(result, request.getHeader("Range"), request.getHeader("If-Range"), response, headOnly);
						}
						finally {
							if (stream != null) {
								stream.processed();
								stream.close();
							}
						}
					}
					else {
						response.setContentLength(0);
					}

					// lastly put the conversation in the cache, after the response is sent
					// and all lazy loading of domain objects has been realised
					cacheConversation(webContext);
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
				persistence.rollback();
				throw t;
			}
			finally {
				persistence.commit(true);
			}
		}
		catch (Throwable t) {
			LOGGER.error("Problem generating the download", t);
			if (! response.isCommitted()) {
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(StandardCharsets.UTF_8.name());
				if (! headOnly) {
					try (OutputStream out = response.getOutputStream()) {
						out.write("<html><head/><body><h3>".getBytes(StandardCharsets.UTF_8));
						out.write("An error occured whilst processing your report.".getBytes(StandardCharsets.UTF_8));
						out.write("</body></html>".getBytes(StandardCharsets.UTF_8));
					}
				}
			}
		}
	}

	@SuppressWarnings("static-method") // Test seam; subclasses override this to avoid the static StateUtil dependency.
	@Nullable AbstractWebContext getCachedConversation(@Nullable String contextKey, @Nonnull HttpServletRequest request)
	throws Exception {
		return StateUtil.getCachedConversation(contextKey, request);
	}

	@SuppressWarnings("static-method") // Test seam; subclasses override this to observe lifecycle delegation.
	void writeDownloadResponse(@Nonnull Download result,
								@Nullable String rangeHeader,
								@Nullable String ifRangeHeader,
								@Nonnull HttpServletResponse response,
								boolean headOnly)
	throws IOException {
		DownloadResponseWriter.write(result, rangeHeader, ifRangeHeader, response, headOnly);
	}

	@SuppressWarnings("static-method") // Test seam; subclasses override this to avoid the static StateUtil dependency.
	void cacheConversation(@Nonnull AbstractWebContext webContext)
	throws Exception {
		StateUtil.cacheConversation(webContext);
	}
}
