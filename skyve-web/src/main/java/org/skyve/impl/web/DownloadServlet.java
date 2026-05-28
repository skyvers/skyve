package org.skyve.impl.web;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.IOUtils;
import org.skyve.content.Disposition;
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
import org.skyve.util.Util;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import org.slf4j.Logger;

/**
 * Implements file downloads for Skyve.
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
	@SuppressWarnings("java:S1989") // there exists JavaEE error pages
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		try (OutputStream out = response.getOutputStream()) {
			try {
				String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
				AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request);
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
			        	byte[] bytes = null;
			        	File file = null;
						if (! vetoed) {
							result = downloadAction.download(bean, webContext);
							WebFileInputStream stream = result.getInputStream();
							try { // ensure stream is always closed
								customer.interceptAfterDownloadAction(document, resourceName, bean, result, webContext);
	
								bytes = result.getBytes();
								if (bytes == null) {
									file = result.getFile();
								}
								if ((bytes == null) && (file == null)) {
									if (stream != null) {
										try (BufferedInputStream bis = new BufferedInputStream(stream)) {
											try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
												bis.transferTo(baos);
												bytes = baos.toByteArray();
											}
										}
									}
								}
							}
							finally {
								if (stream != null) {
									stream.processed();
									stream.close();
								}
							}
						}
			            
			            if (result != null) {
							response.setContentType(result.getMimeType().toString());
							response.setCharacterEncoding(StandardCharsets.UTF_8.name());
							StringBuilder header = new StringBuilder(64);
							Disposition disposition = result.getDisposition();
							header.append((disposition == null) ? 
											Disposition.attachment.toString() : 
											disposition.toString());
							header.append("; filename=\"").append(result.getFileName()).append('"');
							response.setHeader("Content-Disposition", header.toString());
			            }
			
			            if (bytes != null) {
			            	response.setContentLength(bytes.length);
			            }
			            else if (file != null) {
			            	response.setContentLengthLong(file.length());
			            }
			            else {
			            	response.setContentLength(0);
			            }
			            
			    		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
			    		response.setHeader("Cache-Control", "cache");
			            response.setHeader("Pragma", "cache");
			            response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
						// The following allows partial requests which are useful for large media or downloading files with pause and resume functions.
						// NOTE - This doesn't support partial requests - Spring does.
			            // response.setHeader("Accept-Ranges", "bytes");
			
						if (bytes != null) {
							Util.chunkBytesToOutputStream(bytes, out);
						}
						else if (file != null) {
							try (FileInputStream fis = new FileInputStream(file)) {
								IOUtils.copy(fis, out);
							}
						}
			            out.flush();
			            
						// lastly put the conversation in the cache, after the response is sent
						// and all lazy loading of domain objects has been realised
						StateUtil.cacheConversation(webContext);
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
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(StandardCharsets.UTF_8.name());
				out.write("<html><head/><body><h3>".getBytes(StandardCharsets.UTF_8));
				out.write("An error occured whilst processing your report.".getBytes(StandardCharsets.UTF_8));
				out.write("</body></html>".getBytes(StandardCharsets.UTF_8));
			}
		}
	}
}
