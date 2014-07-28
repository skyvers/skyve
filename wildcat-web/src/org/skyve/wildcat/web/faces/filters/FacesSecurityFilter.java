package org.skyve.wildcat.web.faces.filters;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.util.Util;

public class FacesSecurityFilter implements Filter {
    private String forwardURI;
    private String errorURI;
    private String[] unsecuredURLPrefixes;
    
    @Override
    public void init(FilterConfig config) throws ServletException {
    	forwardURI = config.getInitParameter("forward");
    	errorURI = config.getInitParameter("error");
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

        // Test if this URL is unsecured, and bug out if so
        // NB cant use queryString here as there could be AJAX posts etc in faces so not good practice
        String pathToTest = request.getServletPath();
        if (unsecuredURLPrefixes != null) {
	        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
	        	if (pathToTest.startsWith(unsecuredURLPrefix)) {
	                chain.doFilter(req, resp);
	                return;
	        	}
	        }
        }
        
        StringBuilder absoluteContextURL = new StringBuilder(64);
        absoluteContextURL.append(request.getScheme()).append("://").append(request.getServerName());
        absoluteContextURL.append(':').append(request.getServerPort()).append(request.getContextPath());
        
        try {
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
        	Util.LOGGER.throwing("FaceSecurityFilter", "doFilter", e);
        	// redirect to error page
            HttpServletResponse response = (HttpServletResponse) resp;

            if (isAjax(request)) {
                response.getWriter().print(xmlPartialRedirectToPage(absoluteContextURL.toString() + errorURI));
                response.flushBuffer();
            }
            else {
				RequestDispatcher rd = request.getRequestDispatcher(errorURI);
				rd.forward(request, response);
            }
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
