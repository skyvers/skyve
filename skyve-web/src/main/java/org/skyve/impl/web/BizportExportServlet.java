package org.skyve.impl.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.Objects;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ActionUnavailableException;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.view.ActionAvailability;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.OWASP;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Handles HTTP requests for this Skyve web endpoint.
 *
 * <p>Servlet API override parameters are intentionally left unannotated because
 * {@link HttpServlet} does not declare nullness constraints for them.
 */
public class BizportExportServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(BizportExportServlet.class);

	/**
	 * Executes a BizPort export action for the current conversation and streams the resulting workbook.
	 *
	 * <p>This method restores the cached conversation, binds its persistence context to the current thread,
	 * resolves the authenticated user, checks export-action permissions, executes pre/post interception hooks,
	 * serializes the generated workbook, and writes the appropriate attachment response headers.
	 *
	 * <p>The export is refused unless the view of the bean's own document, for the request's UX/UI, offers a reachable
	 * trigger for it in the bean's current state (see {@link ActionAvailability}). The bean is the conversation bean, or
	 * the bean that the request's form binding ({@code _b}) zooms in on from it. The form binding is used only for the
	 * check: the export action runs against the conversation as before. SmartClient saves the form before it requests
	 * the export, so a saved edit that changes the action's conditions decides the check.
	 *
	 * <p>Side effects: mutates servlet response headers, may recache the conversation after successful export,
	 * and writes a minimal HTML error response if export generation fails unexpectedly. A refusal's
	 * {@link ActionUnavailableException} is caught, not thrown: it is logged as a warning, rolled back, and written as
	 * the same HTML error response showing the refusal's localised message.
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
				    	Principal userPrincipal = request.getUserPrincipal();
				    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName());
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
						// Check against the view of the bean the client is on (zoomed in through _b), using the bean's own document,
						// as _doc is not checked against the bean.
						Bean bean = Objects.requireNonNull(WebUtil.getConversationBeanFromRequest(webContext, request), "bean");
						Document beanDocument = customer.getModule(bean.getBizModule()).getDocument(customer, bean.getBizDocument());
						String uxui = UserAgent.getSelection(request).getUxUi().getName();
						ActionAvailability.check(user,
													beanDocument,
													beanDocument.getView(uxui,
																			customer,
																			bean.isCreated() ?
																				ViewType.edit.toString() :
																				ViewType.create.toString()),
													uxui,
													bean,
													resourceName);
	
						BizExportAction bizPortAction = document.getBizExportAction(customer, resourceName, true);
						ByteArrayOutputStream baos = new ByteArrayOutputStream();
	
						boolean vetoed = customer.interceptBeforeBizExportAction(document, resourceName, webContext);
						BizPortWorkbook result = null;
						if (! vetoed) {
							result = bizPortAction.bizExport(webContext);
							customer.interceptAfterBizExportAction(document, resourceName, result, webContext);
							if (result != null) {
								result.write(baos);
							}
						}
			            byte[] bytes = baos.toByteArray();
			            
			            if (result != null) {
							switch (result.getFormat()) {
								case xls:
									response.setContentType(MimeType.excel.toString());
									response.setCharacterEncoding(StandardCharsets.UTF_8.name());
									response.setHeader("Content-Disposition", "attachment; filename=\"" + OWASP.sanitiseFileName(resourceName + "-export.xls") + '"');
									break;
								case xlsx:
									response.setContentType(MimeType.xlsx.toString());
									response.setCharacterEncoding(StandardCharsets.UTF_8.name());
									response.setHeader("Content-Disposition", "attachment; filename=\"" + OWASP.sanitiseFileName(resourceName + "-export.xlsx") + '"');
									break;
								default:
							}
			            }
			
			            response.setContentLength(bytes.length);
			            
			    		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
			    		response.setHeader("Cache-Control", "cache");
			            response.setHeader("Pragma", "cache");
			            response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
			
			            out.write(bytes);
			            out.flush();

						// lastly put the conversation in the cache, after the response is sent
						// and all lazy loading of domain objects has been realised
						StateUtil.commitAndCacheConversation(webContext);
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
			catch (ActionUnavailableException e) {
				// Logged as a warning when refused, so show the user the refusal's message rather than the generic one
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(StandardCharsets.UTF_8.name());
				out.write(("<html><head/><body><h3>" + OWASP.escapeHtml(e.getMessages().get(0).getText()) + "</h3></body></html>").getBytes(StandardCharsets.UTF_8));
			}
			catch (Throwable t) {
				LOGGER.error("Problem generating the export", t);
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(StandardCharsets.UTF_8.name());
				out.write("<html><head/><body><h3>".getBytes(StandardCharsets.UTF_8));
				out.write("An error occured whilst processing your report.".getBytes(StandardCharsets.UTF_8));
				out.write("</body></html>".getBytes(StandardCharsets.UTF_8));
			}
		}
	}
}
