package org.skyve.wildcat.web.bizport;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.ServletConstants;
import org.skyve.wildcat.web.WebUtil;
import org.skyve.wildcat.web.upload.AbstractUploadServlet;

public class BizportExportServlet extends HttpServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		try (OutputStream out = response.getOutputStream()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				AbstractRepository repository = AbstractRepository.get();
				User user = persistence.getUser();
				Customer customer = user.getCustomer();
	
				String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);
				int dotIndex = documentName.indexOf('.');
				String moduleName = documentName.substring(0, dotIndex);
				documentName = documentName.substring(dotIndex + 1);
				Module module = customer.getModule(moduleName);
				Document document = module.getDocument(customer, documentName);
				BizExportAction bizPortAction = repository.getBizExportAction(customer, 
																				document, 
																				request.getParameter(AbstractUploadServlet.BIZPORT_NAME));
				String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
	        	AbstractWebContext context = WebUtil.getCachedConversation(contextKey, request, response);
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
		        BizPortWorkbook result = bizPortAction.bizExport(context);
				result.write(baos);
	            byte[] bytes = baos.toByteArray();
	            
				switch (result.getFormat()) {
				case xls:
					response.setContentType(MimeType.excel.toString());
					response.setCharacterEncoding(ServletConstants.UTF8);
					response.setHeader("Content-Disposition", "inline; filename=\"bizport.xls\"");
					break;
				case xlsx:
					break;
				default:
				}
	
	            response.setContentLength(bytes.length);
	            
	    		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
	    		response.setHeader("Cache-Control", "cache");
	            response.setHeader("Pragma", "cache");
	            response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
	
	            out.write(bytes);
	            out.flush();
			}
			catch (Exception e) {
				System.err.println("Problem generating the export - " + e.toString());
				e.printStackTrace();
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(ServletConstants.UTF8);
				out.write("<html><head/><body><h3>".getBytes(ServletConstants.UTF8));
				out.write("An error occured whilst processing your report.".getBytes(ServletConstants.UTF8));
				out.write("</body></html>".getBytes(ServletConstants.UTF8));
			}
			finally {
				persistence.commit(true);
			}
		}
	}
}
