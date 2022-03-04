package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.Principal;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.util.Util;

public class SmartClientTextSearchServlet extends HttpServlet {
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
			persistence = AbstractPersistence.get();
			try (ContentManager cm = EXT.newContentManager()) {
				persistence.begin();
				Principal userPrincipal = request.getUserPrincipal();
				User user = WebUtil.processUserPrincipalForRequest(request, 
																	(userPrincipal == null) ? null : userPrincipal.getName(),
																	true);
				if (user == null) {
					throw new SessionEndedException(request.getLocale());
				}
				persistence.setUser(user);
				Customer customer = user.getCustomer();

				SearchResults results = cm.google(criteria, 100);

	            response.setContentType(MimeType.json.toString());
	            response.setCharacterEncoding(Util.UTF8);
	            response.addHeader("Cache-control", "private,no-cache,no-store"); // never
	            response.addDateHeader("Expires", 0); // never

	            try (PrintWriter pw = response.getWriter()) {
		            StringBuilder message = new StringBuilder(512);
			    	message.append(SmartClientListServlet.ISC_JSON_PREFIX);
		            message.append("{\"response\":{\"data\":[");
		
		            Iterator<SearchResult> resultIterator = results.getResults().iterator();
		            StringBuilder url = new StringBuilder(128);
		            StringBuilder iconMarkup = new StringBuilder(64);
		            while (resultIterator.hasNext()) {
						SearchResult result = resultIterator.next();
						try {
							String moduleName = result.getModuleName();
							String documentName = result.getDocumentName();
							String bizId = result.getBizId();
							
							Module module = customer.getModule(moduleName);
							Document document = module.getDocument(customer, documentName);
							PersistentBean bean = persistence.retrieve(document, bizId);
		
							// Use JSONUtil here to ensure that everything is escaped properly
							
				            Map<String, Object> row = new TreeMap<>();
				            String icon16 = document.getIcon16x16RelativeFileName();
				            String iconStyleClass = document.getIconStyleClass();
				            iconMarkup.setLength(0);
				            if (iconStyleClass != null) {
				            	iconMarkup.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
				            }
				            else if (icon16 != null) {
					            iconMarkup.append("<img style=\"width:16px;height:16px\" src=\"resources?_doc=");
					            iconMarkup.append(moduleName).append('.').append(documentName);
					            iconMarkup.append("&_n=").append(icon16).append("\"/>");
				            }
				            row.put("icon", iconMarkup.toString());
				            row.put("doc", document.getLocalisedSingularAlias());
				            row.put(Bean.BIZ_KEY, (bean != null) ? bean.getBizKey() : null);
				            row.put("excerpt", result.getExcerpt());
				            row.put("score", Integer.valueOf(result.getScore()));
		                    url.setLength(0);
		                    url.append("?m=");
		                    url.append(moduleName).append("&d=").append(documentName);
		                    url.append("&i=").append(bizId);
				            row.put("data", url.toString());
		
				            if (result.isAttachment()) {
					            url.setLength(0);
			                    url.append("content?_doc=");
			                    url.append(moduleName).append('.').append(documentName);
			                    url.append("&_n=").append(result.getContentId());
			                    url.append("&_b=").append(result.getAttributeName());
					            row.put("content", url.toString());
				            }
				            else {
				            	row.put("content", null);
				            }
				            message.append(JSON.marshall(customer, row)).append(',');
						}
						catch (Exception e) { // don't allow anything that goes wrong to stop us returning the searches
							e.printStackTrace();
							resultIterator.remove(); // remove the offending result
						}
					}
	
					// append summary row
					message.append("{\"time\":").append(results.getSearchTimeInSecs()).append(",\"suggestion\":");
					String suggestion = results.getSuggestion();
					if (suggestion != null) {
						message.append('\'').append(suggestion).append('\'');
					}
					else {
						message.append("null");
					}
				
					message.append("}],\"status\":0,");
					message.append("\"startRow\":0,\"endRow\":");
					// rows could have been removed above if the bizkey couldn't be found
					message.append(results.getResults().size());
					message.append(",\"totalRows\":");
					message.append(results.getResults().size());
					message.append("}}");
			    	message.append(SmartClientListServlet.ISC_JSON_SUFFIX);
	
					pw.append(message);
					pw.flush();
	            }
            }
		}
		catch (Exception e) {
			throw new ServletException("Could not search the content repository", e);
		}
		finally {
			// This can be null here if EXT.newContentManager fails
			if (persistence != null) {
				persistence.commit(true);
			}
		}
    }
}