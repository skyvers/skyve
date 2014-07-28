package org.skyve.wildcat.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.jcr.Session;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.content.SearchResult;
import org.skyve.wildcat.content.SearchResults;
import org.skyve.wildcat.metadata.view.DownloadAreaType;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.JSONUtil;
import org.skyve.wildcat.util.UtilImpl;

public class TextSearchServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	private static final String CRITERIA_NAME = "criteria";
	
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
		long firstResult = Long.parseLong(request.getParameter("_startRow"));
		int maxResults = Integer.parseInt(request.getParameter("_endRow"));
		
		AbstractPersistence persistence = null;
		Session session = null;
		try {
			User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
			persistence = AbstractPersistence.get();
			persistence.setUser(user);
			Customer customer = user.getCustomer();
			session = ContentUtil.getSearchSession(customer.getName());

			SearchResults results = ContentUtil.search(session, criteria, new Long(firstResult), new Integer(maxResults));

            response.setContentType(MimeType.json.toString());
            response.setCharacterEncoding(ServletConstants.UTF8);
            response.addHeader("Cache-control", "private,no-cache,no-store"); // never
            response.addDateHeader("Expires", 0); // never

            try (PrintWriter pw = response.getWriter()) {
	            long totalResults = results.getTotalResults();
	
	            StringBuilder message = new StringBuilder(512);
	            message.append("{response:{data:[");
	
	            int errors = 0;
	            Iterator<SearchResult> resultIterator = results.getResults().iterator();
	            StringBuilder url = new StringBuilder(128);
	            while (resultIterator.hasNext()) {
					SearchResult result = resultIterator.next();
					try {
						Module module = customer.getModule(result.getModuleName());
						Document document = module.getDocument(customer, result.getDocumentName());
						PersistentBean bean = persistence.retrieve(document, result.getBizId(), false);
						if (bean != null) {
							result.setBizKey(bean.getBizKey());
						}
	
						// Use JSONUtil here to ensure that everything is escaped properly
						
			            Map<String, Object> row = new TreeMap<>();
			            row.put(Bean.BIZ_KEY, result.getBizKey());
			            row.put("excerpt", result.getExcerpt());
			            row.put("score", new Integer(result.getScore()));
	                    url.setLength(0);
	                    url.append("?m=");
	                    url.append(result.getModuleName()).append("&d=").append(result.getDocumentName());
	                    url.append("&i=").append(result.getBizId());
			            row.put("data", url.toString());
	
			            String contentId = result.getContentId();
			            if (contentId == null) {
			            	row.put("content", null);
			            }
			            else {
				            url.setLength(0);
		                    url.append("content?_doc=");
		                    url.append(result.getModuleName()).append('.').append(result.getDocumentName());
		                    url.append("&_n=").append(result.getContentId());
		                    url.append("&_b=").append(result.getBinding());
				            row.put("content", url.toString());
			            }
			            message.append(JSONUtil.marshall(customer, row, null)).append(',');
					}
					catch (Exception e) { // don't allow anything that goes wrong to stop us returning the searches
						e.printStackTrace();
						errors++;
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
				message.append("startRow:").append(firstResult);
				message.append(",endRow:");
				// rows could have been removed above if the bizkey couldn't be found
				message.append(Math.min(totalResults, results.getResults().size()));
				message.append(",totalRows:");
				message.append(totalResults - errors);
				message.append("}}");
	
				pw.append(message);
				pw.flush();
            }
		}
		catch (Exception e) {
			throw new ServletException("Could not search the content repository", e);
		}
		finally {
			try {
    			if (session != null) {
    				session.logout();
    			}
			}
			finally {
    			if (persistence != null) {
    				persistence.commit(true);
    			}
			}
		}
    }
	
	@Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("DO POST");
		String criteria = request.getParameter(CRITERIA_NAME);
		response.setContentType(MimeType.html.toString());
		response.setCharacterEncoding(ServletConstants.UTF8);

		try (PrintWriter writer = response.getWriter()) {
			renderHeaderAndForm(writer, criteria);
	
			Session session = null;
			try {
				User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
				Customer customer = user.getCustomer();
				session = ContentUtil.getSearchSession(customer.getName());
	
				SearchResults results = ContentUtil.search(session, criteria, null, null);
				writer.print(JSONUtil.marshall(customer, results, null));
				writer.print("<p/>");
				writer.print(results.getTotalResults());
				writer.print(" results found in ");
				writer.print(results.getSearchTimeInSecs());
				writer.print(" seconds.<p/>");
				if (results.getSuggestion() != null) {
					writer.print("Did you mean ");
					writer.print(results.getSuggestion());
				}
				writer.print("<table>");
				
				for (SearchResult result : results.getResults()) {
					writer.print("<tr><td>Excerpt:-<td><td>");
					writer.print(result.getExcerpt());
					writer.print("</td></tr><tr><td>&nbsp;</td><td>");
					
					writer.print("<a ");
					writer.print("href=\"");
					writer.print(DownloadAreaType.content.toString());
					writer.print("?_n=");
					writer.print(result.getContentId());
					writer.print("\" target=\"_blank\">");
					writer.print(result.getBinding());
					writer.print("</a>");
	
					writer.print(" from ");
					
					writer.print("<a href=\"");
					writer.print(org.skyve.util.Util.getDocumentUrl(result.getModuleName(), result.getDocumentName(), result.getBizId()));
					writer.print("\" target=\"_parent\">");
					writer.print(result.getDocumentName());
					writer.print("</a></td></tr>");
				}
			}
			catch (Exception e) {
				throw new ServletException("Could not search the content repository", e);
			}
			finally {
				if (session != null) {
					session.logout();
				}
			}
			writer.print("</table>");
			writer.print("</body></html>");
			
			writer.flush();
		}
    }
	
	private static void renderHeaderAndForm(PrintWriter writer, String criteria) {
		writer.print("<html><head/><body><form method=\"post\"><input type=\"text\" name=\"");
		writer.print(CRITERIA_NAME);
		writer.print("\" value=\"");
		writer.print(criteria);
		writer.print("\"/><input type=\"submit\" name=\"Search\"/></form>");
	}
}