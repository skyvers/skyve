package org.skyve.wildcat.web.upload;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;
import java.util.logging.Level;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.ServletConstants;

public abstract class AbstractUploadServlet extends HttpServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	public static final String MODULE_DOT_DOCUMENT_NAME = "_doc";
	public static final String DOCUMENT_ID_NAME = "_id";
	public static final String BINDING_NAME = "_b";
	public static final String DATA_GROUP_ID_NAME = "_dg";
	public static final String USER_ID_NAME = "_u";
	public static final String CONTENT_ID_NAME = "_cid";
	public static final String BIZPORT_NAME = "_n";
	public static final String ERROR_NAME = "_e";
	public static final String MESSAGE_NAME = "_m";
	
	public static String SUBMIT_NAME = "submit";
	
	//Button names
	public static final String UPLOAD_FILES = "Upload";

	public static final String SELECT_FILES_NAME = "selectedFiles";
	public static final String DOWNLOAD_FILE_NAME = "downloadFile";
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) 
	throws ServletException, IOException {
		processRequest(req, resp);
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) 
	throws ServletException, IOException {
		processRequest(req, resp);
	}

	/**
	 * Process the file uploaded.
	 * 
	 * @param file	The temporary file uploaded
	 * @param request	the request that got us here.
	 * @param requestParameters	The parameters present in the HttpMultiPartRequest.
	 * @return	The servlet response content.
	 * @throws Exception
	 */
	public abstract StringBuilder processFile(File file,
										HttpServletRequest request,
										Map<String, Object> requestParameters)
	throws Exception;
	
	private void processRequest(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
			persistence.setUser(user);
			persistence.begin();
			
			if ((request.getContentType() != null) && 
					request.getContentType().toLowerCase().startsWith("multipart")) {
				response.setContentType(MimeType.html.toString());
				response.setCharacterEncoding(ServletConstants.UTF8);
				try (PrintWriter writer = response.getWriter()) {
					HttpMultiPartParser parser = new HttpMultiPartParser();
					boolean error = false;
					StringBuilder responseSB = null;
					try {
						int bstart = request.getContentType().lastIndexOf("oundary=");
						String bound = request.getContentType().substring(bstart + 8);
						int clength = request.getContentLength();
						Map<String, Object> ht = parser.processData(request.getInputStream(), bound, UtilImpl.TEMP_DIRECTORY, clength);
	
						// Set the module, document and binding in case the upload JSP is shown again
						request.setAttribute(MODULE_DOT_DOCUMENT_NAME, ht.get(MODULE_DOT_DOCUMENT_NAME));
						request.setAttribute(DATA_GROUP_ID_NAME, ht.get(DATA_GROUP_ID_NAME));
						request.setAttribute(USER_ID_NAME, ht.get(USER_ID_NAME));
						request.setAttribute(BINDING_NAME, ht.get(BINDING_NAME));
						request.setAttribute(BIZPORT_NAME, ht.get(BIZPORT_NAME));
						
						if (ht.get("myFile") != null) {
							FileInfo fi = (FileInfo) ht.get("myFile");
							File f = fi.file;
							UploadInfo info = UploadMonitor.getInfo(fi.clientFileName);
							if ((info != null) && info.aborted) {
								f.delete();
								request.setAttribute(ERROR_NAME, "Upload aborted");
								error = true;
							}
							else {
							    responseSB = processFile(f, request, ht);
							}
						}
						else {
							request.setAttribute(ERROR_NAME, "No file selected for upload");
							error = true;
						}
						request.getSession().setAttribute(MODULE_DOT_DOCUMENT_NAME, ht.get(MODULE_DOT_DOCUMENT_NAME));
						request.getSession().setAttribute(DATA_GROUP_ID_NAME, ht.get(DATA_GROUP_ID_NAME));
						request.getSession().setAttribute(USER_ID_NAME, ht.get(USER_ID_NAME));
					}
					catch (Exception e) {
						UtilImpl.LOGGER.log(Level.SEVERE, "error", e);
						request.setAttribute(ERROR_NAME, "Error " + e + ". Upload aborted");
						error = true;
					}
					if (! error) {
						request.setAttribute(MESSAGE_NAME, "File upload correctly finished.");
						writer.print(responseSB);
	
						request.getSession().removeAttribute(MODULE_DOT_DOCUMENT_NAME);
						request.getSession().removeAttribute(DATA_GROUP_ID_NAME);
						request.getSession().removeAttribute(USER_ID_NAME);
						request.getSession().removeAttribute(BINDING_NAME);
						request.getSession().removeAttribute(BIZPORT_NAME);
					}
					else {
						throw new ServletException("A problem was encountered uploading");
					}
				}
			}
		}
		catch (MetaDataException e) {
			persistence.rollback();
			throw new ServletException(e);
		}
		finally {
			persistence.commit(true);
		}
	}
	
	/**
	 * This Method converts a byte size in a kbytes or Mbytes size, depending on the size
	 *     @param size The size in bytes
	 *     @return String with size and unit
	 */
	public static String convertFileSize(long size) {
		int divisor = 1;
		String unit = "bytes";
		if (size >= 1024 * 1024) {
			divisor = 1024 * 1024;
			unit = "MB";
		}
		else if (size >= 1024) {
			divisor = 1024;
			unit = "KB";
		}
		if (divisor == 1) return size / divisor + " " + unit;
		String aftercomma = "" + 100 * (size % divisor) / divisor;
		if (aftercomma.length() == 1) aftercomma = "0" + aftercomma;
		return size / divisor + "." + aftercomma + " " + unit;
	}
}
