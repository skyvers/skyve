package org.skyve.wildcat.web;

import java.io.File;
import java.io.FileInputStream;
import java.util.Map;

import javax.jcr.Session;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.skyve.content.MimeType;
import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.content.StreamContent;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.upload.AbstractUploadServlet;

public final class ContentUploadServlet extends AbstractUploadServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	@Override
	@SuppressWarnings("resource") // cannot close the file input stream as the content repo needs to process it outside of this method
	public StringBuilder processFile(File file, 
								HttpServletRequest request, 
								Map<String, Object> requestParameters)
	throws Exception {
	    StringBuilder response = new StringBuilder(512);
	    
	    String bizCustomer = AbstractPersistence.get().getUser().getCustomer().getName();

		HttpSession httpSession = request.getSession();
		String moduleAndDocument = (String) httpSession.getAttribute(MODULE_DOT_DOCUMENT_NAME);
		String dataGroupId = (String) httpSession.getAttribute(DATA_GROUP_ID_NAME);
		if ((dataGroupId != null) && (dataGroupId.length() == 0)) {
			dataGroupId = null;
		}
		String userId = (String) httpSession.getAttribute(USER_ID_NAME);
		if ((userId != null) && (userId.length() == 0)) {
			userId = null;
		}
		String[] moduleAndDocumentTokens = moduleAndDocument.split("\\.");
		String bizId = (String) httpSession.getAttribute(DOCUMENT_ID_NAME);
		if ((bizId != null) && (bizId.length() == 0)) {
			bizId = null;
		}
		String binding = (String) httpSession.getAttribute(BINDING_NAME);
		if ((binding != null) && (binding.length() == 0)) {
			binding = null;
		}
		String contentId = (String) httpSession.getAttribute(CONTENT_ID_NAME);
		if ((contentId != null) && (contentId.length() == 0)) {
			contentId = null;
		}

		Session session = ContentUtil.getFullSession(bizCustomer);
		try {
			StreamContent content = new StreamContent(bizCustomer, 
														moduleAndDocumentTokens[0], 
														moduleAndDocumentTokens[1],
														dataGroupId, 
														userId, 
														bizId, 
														binding);
			content.setUuid(contentId);
			content.setVersionable(false);
			content.setMimeType(MimeType.fromFileName(file.getName()));
			content.setStream(new FileInputStream(file));
			content = ContentUtil.put(session, content, false);

			
			response.append("<html>");
			response.append("<head>");
			response.append("<meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\">");
			response.append("<meta name=\"robots\" content=\"noindex\">");
			response.append("<meta http-equiv=\"expires\" content=\"0\">");
			response.append("<meta http-equiv=\"pragma\" content=\"no-cache\">");
			response.append("<script language=\"javascript\" type=\"text/javascript\" src=\"");
			response.append(request.getContextPath());
			response.append("/desktop/upload-min.js?v=").append(UtilImpl.JAVASCRIPT_FILE_VERSION).append("\"></script></head>");
			response.append("<body onload=\"BizHub.upload.populateParentWidgetAndClose('");
			response.append(request.getSession().getAttribute(MODULE_DOT_DOCUMENT_NAME));
			response.append("', '");
			response.append(request.getSession().getAttribute(DATA_GROUP_ID_NAME));
			response.append("', '");
			response.append(request.getSession().getAttribute(USER_ID_NAME));
			response.append("', '");
			response.append(request.getSession().getAttribute(BINDING_NAME));
			response.append("', '");
			response.append(content.getUuid());
			response.append("');\">");
			response.append("</body></html>");

			return response;
		}
		finally {
			try {
				session.logout();
			}
			finally {
				file.delete();
			}
		}
	}
}
