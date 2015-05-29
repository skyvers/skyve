package org.skyve.wildcat.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.content.SearchResult;
import org.skyve.wildcat.content.SearchResults;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.JSONUtil;
import org.skyve.wildcat.util.UtilImpl;

public class TextSearchServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	@Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		Enumeration<String> parameterNames = request.getParameterNames();
		while (parameterNames.hasMoreElements()) {
			String name = parameterNames.nextElement();
			String value = request.getParameter(name);
			UtilImpl.LOGGER.info(name + " = " + value);
		}

		String criteria = request.getParameter("query");
		
		AbstractPersistence persistence = null;
		try {
			try (ContentManager cm = EXT.newContentManager()) {
				User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
				persistence = AbstractPersistence.get();
				persistence.setUser(user);
				Customer customer = user.getCustomer();

				SearchResults results = cm.google(criteria, 100);

	            response.setContentType(MimeType.json.toString());
	            response.setCharacterEncoding(ServletConstants.UTF8);
	            response.addHeader("Cache-control", "private,no-cache,no-store"); // never
	            response.addDateHeader("Expires", 0); // never

	            try (PrintWriter pw = response.getWriter()) {
		            StringBuilder message = new StringBuilder(512);
		            message.append("{response:{data:[");
		
		            Iterator<SearchResult> resultIterator = results.getResults().iterator();
		            StringBuilder url = new StringBuilder(128);
		            while (resultIterator.hasNext()) {
						SearchResult result = resultIterator.next();
						try {
							String moduleName = result.getModuleName();
							String documentName = result.getDocumentName();
							String bizId = result.getBizId();
							
							Module module = customer.getModule(moduleName);
							Document document = module.getDocument(customer, documentName);
							PersistentBean bean = persistence.retrieve(document, bizId, false);
		
							// Use JSONUtil here to ensure that everything is escaped properly
							
				            Map<String, Object> row = new TreeMap<>();
				            row.put("icon", document.getIcon16x16RelativeFileName());
				            row.put("doc", document.getSingularAlias());
				            row.put(Bean.BIZ_KEY, (bean != null) ? bean.getBizKey() : null);
				            row.put("excerpt", result.getExcerpt());
				            row.put("score", new Integer(result.getScore()));
		                    url.setLength(0);
		                    url.append("?m=");
		                    url.append(moduleName).append("&d=").append(documentName);
		                    url.append("&i=").append(bizId);
				            row.put("data", url.toString());
		
				            String attributeName = result.getAttributeName();
				            if (attributeName == null) { // bean content
				            	row.put("content", null);
				            }
				            else { // attachment content
					            url.setLength(0);
			                    url.append("content?_doc=");
			                    url.append(moduleName).append('.').append(documentName);
			                    url.append("&_n=").append(result.getContentId());
			                    url.append("&_b=").append(attributeName);
					            row.put("content", url.toString());
				            }
				            message.append(JSONUtil.marshall(customer, row, null)).append(',');
						}
						catch (Exception e) { // don't allow anything that goes wrong to stop us returning the searches
							e.printStackTrace();
							resultIterator.remove(); // remove the offending result
						}
					}
	
					// append summary row
					message.append("{time:").append(results.getSearchTimeInSecs()).append(",suggestion:");
					String suggestion = results.getSuggestion();
					if (suggestion != null) {
						message.append('\'').append(suggestion).append('\'');
					}
					else {
						message.append("null");
					}
				
					message.append("}],status:0,");
					message.append("startRow:0,endRow:");
					// rows could have been removed above if the bizkey couldn't be found
					message.append(results.getResults().size());
					message.append(",totalRows:");
					message.append(results.getResults().size());
					message.append("}}");
	
					pw.append(message);
					pw.flush();
	            }
            }
		}
		catch (Exception e) {
			throw new ServletException("Could not search the content repository", e);
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
    }
}