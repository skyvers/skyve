package org.skyve.impl.web;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;

import org.skyve.content.MimeType;
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

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import net.coobird.thumbnailator.Thumbnails;
import net.coobird.thumbnailator.Thumbnails.Builder;

public class DynamicImageServlet extends HttpServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	public static final String IMAGE_NAME = "_n";
	public static final String IMAGE_WIDTH_NAME = "_w";
	public static final String IMAGE_HEIGHT_NAME = "_h";
	public static final String IMAGE_WIDTH_ZOOM_NAME = "_wz";
	public static final String IMAGE_HEIGHT_ZOOM_NAME = "_hz";

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-Control", "private,no-cache,no-store");
		// The following allows partial requests which are useful for large media or downloading files with pause and resume functions.
		response.setHeader("Accept-Ranges", "bytes");

		ImageFormat format = null;
		try (OutputStream out = response.getOutputStream()) {
			try {
				String moduleDotDocumentName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));
				if (moduleDotDocumentName == null) {
					throw new ServletException("No module.document name in the URL");
				}
	
				String imageName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(IMAGE_NAME)));
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
	        	AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
	        	if (webContext == null) {
	        		throw new ConversationEndedException(request.getLocale());
	        	}
	        	
	    		AbstractPersistence persistence = webContext.getConversation();
	    		persistence.setForThread();
	        	
	        	Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
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
				Document document = customer.getModule(moduleName).getDocument(customer, documentName);
				
				UxUi uxui = UserAgent.getUxUi(request);
				user.checkAccess(UserAccess.dynamicImage(moduleName, documentName, imageName), uxui.getName());

				DynamicImage<Bean> dynamicImage = document.getDynamicImage(customer, imageName);
				BufferedImage image = dynamicImage.getImage(bean,
															(int) (width * (widthZoom / 100.0)), 
															(int) (height * (heightZoom / 100.0)), 
															user);
				try {
					format = dynamicImage.getFormat();
					if (format == null) {
						format = ImageFormat.png;
					}
					Float quality = dynamicImage.getCompressionQuality();
					Builder<BufferedImage> b = Thumbnails.of(image).scale(1.0).outputFormat(format.toString());
					if (quality != null) {
						b.outputQuality(quality.floatValue());
					}
		
					response.setContentType(format.getMimeType().toString());
					b.toOutputStream(out);
				}
				catch (@SuppressWarnings("unused") Exception e) {
					image.flush();
				}
			}
			// Don't throw here - just log it as its not a show-stopper if the image doesn't render.
			catch (Exception e) {
				System.err.println("Problem generating the dynamic image - " + e.toString());
				// pump out a blank image
				if (format == null) {
					format = ImageFormat.png;
					response.setContentType(MimeType.png.toString());
				}
	
				BufferedImage blankImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
				Graphics g = blankImage.getGraphics();
				g.setColor(Color.WHITE);
				g.fillRect(0, 0, 1, 1);
				Thumbnails.of(blankImage).scale(1.0).outputFormat(format.toString()).toOutputStream(out);
				e.printStackTrace();
			}
		}
	}
}
