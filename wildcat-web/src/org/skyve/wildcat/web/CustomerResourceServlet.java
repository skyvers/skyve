package org.skyve.wildcat.web;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebContext;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.view.DownloadAreaType;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
		
public class CustomerResourceServlet extends HttpServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	private static final class Resource {
		private ContentManager cm;
		private AttachmentContent content;
		private File file;

		void dispose() throws Exception {
			if (cm != null) {
				cm.close();
				cm = null;
			}
		}

		long getLastModified() {
			long result = -1;

			if (file != null) {
				result = file.lastModified();
			}
			if (content != null) {
				result = content.getLastModified().getTime();
			}

			return result;
		}

		@SuppressWarnings("resource") // This is closed by the servlet once it is read
		InputStream getStream() 
		throws FileNotFoundException {
			InputStream result = null;

			if (file != null) {
				result = new FileInputStream(file);
			}
			if (content != null) {
				result = content.getContentStream();
			}

			return result;
		}

		boolean isContent() {
			return (content != null);
		}
		
		MimeType getMimeType() {
			MimeType result = MimeType.plain;

			if (file != null) {
				MimeType.fromFileName(file.getName());
			}
			if (content != null) {
				result = content.getMimeType();
			}
			
			return result;
		}

		Resource(HttpServletRequest request)
		throws Exception {
			String resourceArea = request.getServletPath().substring(1); // get rid of slash at front
			String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);
			String moduleName = null;
			if ((documentName == null) || (documentName.length() == 0)) {
				System.err.println("No modoc in the URL");
			}
			else {
				int dotIndex = documentName.indexOf('.');
				moduleName = documentName.substring(0, dotIndex);
				documentName = documentName.substring(dotIndex + 1);
			}
			String binding = request.getParameter(AbstractWebContext.BINDING_NAME);
			String resourceFileName = request.getParameter(AbstractWebContext.RESOURCE_FILE_NAME);

			if ((resourceFileName == null) || (resourceFileName.length() == 0)) {
				System.err.println("No resource file name or data file name in the URL");
			}
			else {
				User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
				Customer customer = null;
				String customerName = null;
				if (user != null) { // we are logged in
					customer = user.getCustomer();
					customerName = customer.getName();
					AbstractPersistence.get().setUser(user);
				}
				else { // not logged in
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

				if (DownloadAreaType.content.toString().equals(resourceArea)) {
					if ((user != null) && (customer != null)) {
						Module module = customer.getModule(moduleName);
						Document document = module.getDocument(customer, documentName);
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer,
																				module,
																				document,
																				binding);
						cm = EXT.newContentManager();
						content = cm.get(resourceFileName);
						if (content != null) {
							// Check that the user has access
							if (! user.canAccessContent(content.getBizId(),
															content.getBizModule(),
															content.getBizDocument(),
															content.getBizCustomer(),
															content.getBizDataGroupId(),
															content.getBizUserId(),
															target.getAttribute().getName())) {
								throw new SecurityException(moduleName + '.' + documentName + '.' + binding,
																user.getName());
							}
						}
					}
				} 
				else if (DownloadAreaType.resources.toString().equals(resourceArea)) {
					AbstractRepository repository = AbstractRepository.get();
					File tempFile = repository.findResourceFile(resourceFileName, customerName, moduleName);
					if (tempFile.exists()) {
						file = tempFile;
					}
					else {
						int underscoreIndex = resourceFileName.lastIndexOf('_');
						if (underscoreIndex > 0) {
							int dotIndex = resourceFileName.lastIndexOf('.');
							if (dotIndex > underscoreIndex) {
								String baseFileName = resourceFileName.substring(0, underscoreIndex) + 
														resourceFileName.substring(dotIndex);
								tempFile = repository.findResourceFile(baseFileName, customerName, moduleName);
								if (tempFile.exists()) {
									file = tempFile;
								}
							}
						}
					}
				} 
				else {
					throw new IllegalStateException("Unsupported resource area " + resourceArea);
				}
			}
		}
	}

	/**
	 * Used to hold the agent per thread
	 */
	private static final ThreadLocal<Resource> RESOURCES = new ThreadLocal<>();

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
					UtilImpl.LOGGER.severe("Could not dispose of the thread-local content resource properly.  It has been removed from the thread local storage.");
					e.printStackTrace();
				}
				finally {
					RESOURCES.remove();
				}
			}
		}
	}

	@Override
	protected long getLastModified(HttpServletRequest request) {
		try {
			Resource resource = RESOURCES.get();
			if (resource == null) {
				resource = new Resource(request);
				RESOURCES.set(resource);
			}

			return resource.getLastModified();
		} 
		catch (Exception e) {
			System.err.println("Problem getting the customer resource - " + e.toString());
			e.printStackTrace();
			return -1;
		}
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		try {
			Resource resource = RESOURCES.get();
			if (resource == null) {
				resource = new Resource(request);
				RESOURCES.set(resource);
			}

			response.setContentType(resource.getMimeType().toString());
			response.setCharacterEncoding(ServletConstants.UTF8);
			if (resource.isContent()) {
				StringBuilder disposition = new StringBuilder(32);
				disposition.append("inline; filename=\"content.");
				disposition.append(resource.getMimeType().getStandardFileSuffix());
				disposition.append('"');
				response.setHeader("Content-Disposition", disposition.toString());
			}

			try (InputStream resourceStream = resource.getStream()) {
				if (resourceStream == null) {
					response.sendError(404);
					return;
				}
	
				try (OutputStream out = response.getOutputStream()) {
					try (BufferedInputStream bis = new BufferedInputStream(resourceStream)) {
						try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
							byte[] bytes = new byte[1024]; // 1K
							int bytesRead = 0;
							while ((bytesRead = bis.read(bytes)) > 0) {
								baos.write(bytes, 0, bytesRead);
							}
							bytes = baos.toByteArray();
			
							response.setContentLength(bytes.length);
			
							if (resource.isContent()) {
								// NOTE - the image is not cached unless there is a content length, and the header following headers
								// NOTE - THIS MUST BE SET FIRST BEFORE WRITING TO THE STREAM
								response.setHeader("Cache-Control", "cache");
						        response.setHeader("Pragma", "cache");
						        response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
							}
			
							out.write(bytes, 0, bytes.length);
							out.flush();
						}
					}
				}
			}
		} 
		catch (Exception e) {
			System.err.println("Problem getting the customer resource - " + e.toString());
			e.printStackTrace();
		}
	}
}
