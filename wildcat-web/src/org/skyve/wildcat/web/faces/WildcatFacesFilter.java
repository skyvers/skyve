package org.skyve.wildcat.web.faces;

import java.io.IOException;

import javax.faces.application.ViewExpiredException;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.domain.messages.SessionEndedException;
import org.skyve.util.Util;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.WebUtil;

public class WildcatFacesFilter implements Filter {
	// This is used when the principal is not (or no longer) logged in.
	// This must be a protected resource so that the login page is displayed
	private String forwardURI;
	// This is used when an error is encountered redirecting or otherwise processing a faces page.
    private String errorURI;
    // This is used when a public unsecured page (see unsecuredURLPrefixes) loses it session.
    private String expiredURI;
    // A list of all unsecured public pages
    private String[] unsecuredURLPrefixes;
    
    @Override
    public void init(FilterConfig config) throws ServletException {
    	forwardURI = config.getInitParameter("forward");
    	errorURI = config.getInitParameter("error");
    	expiredURI = config.getInitParameter("expired");
    	String urls = config.getInitParameter("unsecured");
    	if (urls != null) {
    		unsecuredURLPrefixes = urls.split("\n");
			for (int i = 0, l = unsecuredURLPrefixes.length; i < l; i++) {
				unsecuredURLPrefixes[i] = Util.processStringValue(unsecuredURLPrefixes[i]);
			}
    	}
    }

    @Override
    public void destroy() {
    	forwardURI = null;
    	errorURI = null;
    	unsecuredURLPrefixes = null;
    }

    @Override
    public void doFilter(ServletRequest req,
                            ServletResponse resp,
                            FilterChain chain)
    throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) req;
		
        StringBuilder absoluteContextURL = new StringBuilder(64);
        absoluteContextURL.append(request.getScheme()).append("://").append(request.getServerName());
        absoluteContextURL.append(':').append(request.getServerPort()).append(request.getContextPath());
    
    	try {
	        // Test if this URL is unsecured, and bug out if so
	        // NB can't use queryString here as there could be AJAX posts etc in faces so not good practice
	        String pathToTest = request.getServletPath();
	        if (unsecuredURLPrefixes != null) {
		        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
		        	if (pathToTest.startsWith(unsecuredURLPrefix)) {
	        			chain.doFilter(req, resp);
		        		return;
		        	}
		        }
	        }
	        
            if (request.getUserPrincipal() == null) { // not logged in
                HttpServletResponse response = (HttpServletResponse) resp;
                if (isAjax(request)) {
                    response.getWriter().print(xmlPartialRedirectToPage(absoluteContextURL.toString() + forwardURI));
                    response.flushBuffer();
                }
                else {
                	response.sendRedirect(absoluteContextURL.toString() + forwardURI);
                }
            }
            else {
        		chain.doFilter(req, resp);
            }
        }
        catch (Exception e) {
        	e.printStackTrace();
        	Throwable c = e.getCause();
        	
        	Util.LOGGER.throwing("WildcatFacesFilter", "doFilter", e);
        	// redirect to appropriate page
            HttpServletResponse response = (HttpServletResponse) resp;

            String uri = errorURI;
            if ((e instanceof ViewExpiredException) || 
            		(c instanceof ViewExpiredException) ||
            		(e instanceof SessionEndedException) ||
            		(c instanceof SessionEndedException)) {
            	uri = expiredURI;
            }
            
            if (isAjax(request)) {
                response.getWriter().print(xmlPartialRedirectToPage(absoluteContextURL.toString() + uri));
                response.flushBuffer();
            }
            else {
				RequestDispatcher rd = request.getRequestDispatcher(uri);
				rd.forward(request, response);
            }
        }
		finally {
			if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("WildcatFacesFilter - DISCONNECT PERSISTENCE");
			AbstractPersistence persistence = AbstractPersistence.get();
			persistence.commit(true);
			if (UtilImpl.FACES_TRACE) WebUtil.logConversationsStats();
		}
    }
    
    private static String xmlPartialRedirectToPage(String page) {
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version='1.0' encoding='UTF-8'?>");
        sb.append("<partial-response><redirect url=\"").append(page).append("\"/></partial-response>");
        return sb.toString();
    }

    private static boolean isAjax(HttpServletRequest request) {
        return "XMLHttpRequest".equals(request.getHeader("X-Requested-With"));
    }
}
