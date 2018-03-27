package org.skyve.impl.web;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.Disposition;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

public class DownloadServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		try (OutputStream out = response.getOutputStream()) {
			try {
				String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
	        	AbstractWebContext webContext = WebUtil.getCachedConversation(contextKey, request, response);
	        	if (webContext == null) {
	        		throw new ConversationEndedException();
	        	}
	        	
	    		AbstractPersistence persistence = webContext.getConversation();
	    		persistence.setForThread();
				try {
					try {
						persistence.begin();
						User user = WebUtil.processUserPrincipalForRequest(request,
								request.getUserPrincipal() != null ? request.getUserPrincipal().getName() : null, true);
						if (user == null) {
							throw new SessionEndedException();
						}
						persistence.setUser(user);
	
						AbstractRepository repository = AbstractRepository.get();
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
						DownloadAction<Bean> downloadAction = repository.getDownloadAction(customer, 
																							document, 
																							resourceName,
																							true);
						Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
			        	
						boolean vetoed = customer.interceptBeforeDownloadAction(document, resourceName, bean, webContext);
						Download result = null;
			        	byte[] bytes = null;
						if (! vetoed) {
							result = downloadAction.download(bean, webContext);
							customer.interceptAfterDownloadAction(document, resourceName, bean, result, webContext);
	
							try (BufferedInputStream bis = new BufferedInputStream(result.getInputStream())) {
								try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
									bytes = new byte[1024]; // 1K
									int bytesRead = 0;
									while ((bytesRead = bis.read(bytes)) > 0) {
										baos.write(bytes, 0, bytesRead);
									}
									bytes = baos.toByteArray();
								}
							}
						}
			            
			            if (result != null) {
							response.setContentType(result.getMimeType().toString());
							response.setCharacterEncoding(Util.UTF8);
							StringBuilder header = new StringBuilder(64);
							Disposition disposition = result.getDisposition();
							header.append((disposition == null) ? 
											Disposition.attachment.toString() : 
											disposition.toString());
							header.append("; filename=\"").append(result.getFileName()).append('"');
							response.setHeader("Content-Disposition",  header.toString());
			            }
			
			            response.setContentLength((bytes != null) ? bytes.length : 0);
			            
			    		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
			    		response.setHeader("Cache-Control", "cache");
			            response.setHeader("Pragma", "cache");
			            response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
			
			            out.write(bytes);
			            out.flush();
			            
						// lastly put the conversation in the cache, after the response is sent
						// and all lazy loading of domain objects has been realised
						WebUtil.putConversationInCache(webContext);
					}
					catch (InvocationTargetException e) {
						throw e.getTargetException();
					}
				}
				finally {
					persistence.commit(true);
				}
			}
			catch (Throwable t) {
				System.err.println("Problem generating the download - " + t.toString());
				t.printStackTrace();
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(Util.UTF8);
				out.write("<html><head/><body><h3>".getBytes(Util.UTF8));
				out.write("An error occured whilst processing your report.".getBytes(Util.UTF8));
				out.write("</body></html>".getBytes(Util.UTF8));
			}
		}
	}
}
