package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.util.Util;

/**
 * Service for previous values.
 */
public class SmartClientPrevServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientPrev - get....");
		processRequest(request, response);
	}  	
	
	@Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientPrev - post....");
		processRequest(request, response);
	}
	
	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
    private static void processRequest(HttpServletRequest request, HttpServletResponse response) 
	throws IOException {
    	response.setContentType(MimeType.json.toString());
        response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		try (PrintWriter pw = response.getWriter()) {
	        AbstractPersistence persistence = AbstractPersistence.get();
	        try {
				try {
			    	persistence.begin();
			    	Principal userPrincipal = request.getUserPrincipal();
			    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
					if (user == null) {
						throw new SessionEndedException();
					}
			    	persistence.setUser(user);
	
			    	Customer customer = user.getCustomer();
			    	Document document = null;
			    	try {
			            Module module = customer.getModule(request.getParameter(AbstractWebContext.MODULE_NAME));
			    	    document = module.getDocument(customer, request.getParameter(AbstractWebContext.DOCUMENT_NAME));
					}
			    	catch (Exception e) {
			    	    throw new ServletException("Mal-formed URL", e);
			    	}
			    	String field = request.getParameter(AbstractWebContext.BINDING_NAME);
			    	if ((field == null) || (field.isEmpty())) {
						throw new ServletException("Mal-formed URL");
			    	}
			    	String value = request.getParameter("value");
			    	if (value != null) {
			    		value = value.trim();
			    	}
			    	if (! user.canReadDocument(document)) {
						throw new SecurityException("read this data", user.getName());
					}
					String _startRow = request.getParameter("_startRow");
					int startRow = (_startRow == null) ? 0 : Integer.parseInt(_startRow);
					String _endRow = request.getParameter("_endRow");
					int endRow = (_endRow == null) ? Integer.MAX_VALUE : Integer.parseInt(_endRow);
//					String textMatchStyle = request.getParameter("_textMatchStyle");
//					FilterOperator operator = null;
//					if (textMatchStyle != null) {
//						operator = FilterOperator.valueOf(textMatchStyle);
//					}
	
					StringBuilder bizQL = new StringBuilder(128);
					bizQL.append("select count(distinct bean.").append(field);
					bizQL.append(") as totalRows from {").append(document.getOwningModuleName()).append('.');
					bizQL.append(document.getName()).append("} as bean");
					if (value != null) {
						bizQL.append(" where bean.").append(field).append(" like '%").append(value).append("%'");
					}
					Bean total = persistence.newBizQL(bizQL.toString()).projectedResult();
					long totalRows = ((Number) BindUtil.get(total, "totalRows")).longValue();
					
					bizQL.setLength(0);
	                bizQL.append("select distinct bean.").append(field);
	                bizQL.append(" as value from {").append(document.getOwningModuleName()).append('.');
	                bizQL.append(document.getName()).append("} as bean");
					if (value != null) {
						bizQL.append(" where bean.").append(field).append(" like '%").append(value).append("%'");
					}
					
					List<Bean> values = persistence.newBizQL(bizQL.toString()).setFirstResult(startRow).setMaxResults(endRow - startRow).projectedResults();
					
					StringBuilder message = new StringBuilder(1024);
					message.append("{\"response\":{");
			        message.append("\"status\":0,");
			        message.append("\"startRow\":").append(startRow);
			        message.append(",\"endRow\":");
			        message.append(Math.min(totalRows, endRow));
			        message.append(",\"totalRows\":");
			        message.append(totalRows);
			        message.append(",\"data\":");
			        Set<String> propertyNames = new TreeSet<>();
			        propertyNames.add("value");
			        message.append(JSON.marshall(customer, values, propertyNames));
			        message.append("}}");
			        pw.append(message);
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
		    	t.printStackTrace();
		    	persistence.rollback();
	
		    	SmartClientEditServlet.produceErrorResponse(t, Operation.fetch, false, pw);
			}
		    finally {
		    	if (persistence != null) {
		    		persistence.commit(true);
		    	}
		    }
		}
	}
}
