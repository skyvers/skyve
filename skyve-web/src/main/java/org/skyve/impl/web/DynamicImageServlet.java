package org.skyve.impl.web;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.Duration;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.slf4j.Logger;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import net.coobird.thumbnailator.Thumbnails;
import net.coobird.thumbnailator.Thumbnails.Builder;

public class DynamicImageServlet extends HttpServlet {
	private static final long serialVersionUID = 5180477867432555312L;
	
    private static final Logger HTTP_LOGGER = Category.HTTP.logger();
	
	public static final String IMAGE_NAME = "_n";
	public static final String IMAGE_WIDTH_NAME = "_w";
	public static final String IMAGE_HEIGHT_NAME = "_h";
	public static final String IMAGE_WIDTH_ZOOM_NAME = "_wz";
	public static final String IMAGE_HEIGHT_ZOOM_NAME = "_hz";

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		// State required for rendering the image
		Exception exception = null;
		ImageFormat format = null;
		BufferedImage image = null;
		Float quality = null;
		Duration cacheTime = null;
		
		// Scoped here for monitoring
		Document document = null;
		String imageName = null;
		
		// Collect and validate the request parameters, get the dynamic image class and generate the image
		try {
			String moduleDotDocumentName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));
			if (moduleDotDocumentName == null) {
				throw new ServletException("No module.document name in the URL");
			}

			imageName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_NAME)));
			if (imageName == null) {
				throw new ServletException("No image name in the URL");
			}
			
			String widthParam = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_WIDTH_NAME)));
			if (widthParam == null) {
				throw new ServletException("No image width in the URL");
			}
			int width = Integer.parseInt(widthParam);

			String heightParam = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_HEIGHT_NAME)));
			if (heightParam == null) {
				throw new ServletException("No image height in the URL");
			}
			int height = Integer.parseInt(heightParam);

			int widthZoom = 100;
			String widthZoomParam = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_WIDTH_ZOOM_NAME)));
			if (widthZoomParam != null) {
				widthZoom = Integer.parseInt(widthZoomParam);
			}

			int heightZoom = 100;
			String heightZoomParam = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_HEIGHT_ZOOM_NAME)));
			if (heightZoomParam != null) {
				heightZoom = Integer.parseInt(heightZoomParam);
			}
	        
			String contextKey = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME)));
			AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request);
        	if (webContext == null) {
        		throw new ConversationEndedException(request.getLocale());
        	}

        	Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
        	if (bean == null) {
        		throw new ConversationEndedException(request.getLocale());
        	}

    		AbstractPersistence persistence = webContext.getConversation();
    		persistence.setForThread();
        	
	    	Principal userPrincipal = request.getUserPrincipal();
	    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName());
			if (user == null) {
				throw new SessionEndedException(request.getLocale());
			}
			persistence.setUser(user);

			int dotIndex = moduleDotDocumentName.lastIndexOf('.');
			String moduleName = moduleDotDocumentName.substring(0, dotIndex);
			String documentName = moduleDotDocumentName.substring(dotIndex + 1);
			Customer customer = user.getCustomer();
			document = customer.getModule(moduleName).getDocument(customer, documentName);
			
			UxUi uxui = UserAgent.getUxUi(request);
			EXT.checkAccess(user, UserAccess.dynamicImage(moduleName, documentName, imageName), uxui.getName());

			DynamicImage<Bean> dynamicImage = document.getDynamicImage(customer, imageName);
			format = dynamicImage.getFormat();
			quality = dynamicImage.getCompressionQuality();
			cacheTime = dynamicImage.getCacheTime();
			image = dynamicImage.getImage(bean,
											(int) (width * (widthZoom / 100.0)), 
											(int) (height * (heightZoom / 100.0)), 
											user);
		}
		catch (Exception e) {
			exception = e;
		}
		finally {
			if (format == null) {
				format = ImageFormat.png;
			}
		}
		
		// Set the appropriate response headers for the dynamic image
		// (before getting the output stream)
		try {
			// Set invariant headers
			response.setCharacterEncoding(Util.UTF8);
			// The following allows partial requests which are useful for large media or downloading files with pause and resume functions.
			response.setHeader("Accept-Ranges", "bytes");
			response.setContentType(format.getMimeType().toString());

			if (exception == null) { // no problem encountered yet
				// Set cache header based on image cache time
				if (cacheTime == null) {
					response.addHeader("Cache-Control", "private,no-cache,no-store");
				}
				else {
					// Note this header is first in case there is an arithmetic error
					response.addDateHeader("Expires", System.currentTimeMillis() + cacheTime.toMillis());
					response.setHeader("Cache-Control", "cache");
					response.setHeader("Pragma", "cache");
				}
			}
		}
		catch (Exception e) {
			exception = e;
		}
		
		// We are ready now to render the image, or render the error image
		try (OutputStream out = response.getOutputStream()) {
			try {
				if (exception == null) { // no problem encountered yet
					// Create a thumb nail and punch out the servlet output stream
					Builder<BufferedImage> b = Thumbnails.of(image).scale(1.0).outputFormat(format.toString());
					if (quality != null) {
						b.outputQuality(quality.floatValue());
					}
					b.toOutputStream(out);
				}
			}
			catch (Exception e) {
				exception = e;
			}
			finally {
				if (image != null) {
					image.flush();
				}
			}
			
			// We've had a problem - don't throw here - just log it as its not a show-stopper if the image doesn't render.
			if (exception != null) {
				response.addHeader("Cache-Control", "private,no-cache,no-store");
				HTTP_LOGGER.warn("Problem generating the dynamic image", exception);
	
				BufferedImage blankImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
				Graphics g = blankImage.getGraphics();
				g.setColor(Color.WHITE);
				g.fillRect(0, 0, 1, 1);
				Thumbnails.of(blankImage).scale(1.0).outputFormat(format.toString()).toOutputStream(out);
				exception.printStackTrace();
			}
		}
		
		// If we have a document and image, measure the request.
		if ((document != null) && (imageName != null)) {
			Monitoring.measure(RequestKey.dynamicImage(document, imageName));
		}
	}
}
