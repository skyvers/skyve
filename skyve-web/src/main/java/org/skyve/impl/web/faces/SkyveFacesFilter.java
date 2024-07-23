package org.skyve.impl.web.faces;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

import jakarta.faces.application.ViewExpiredException;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class SkyveFacesFilter implements Filter {
	// This is used when the principal is not (or no longer) logged in.
	// This must be a protected resource so that the login page is displayed
	private String forwardURI;
	// This is used when an error is encountered redirecting or otherwise processing a faces page.
    private String errorURI;
    // This is used when a public unsecured page (see unsecuredURLPrefixes) loses it session.
    private String expiredURI;
    // A list of all unsecured public pages
    private String[] unsecuredURLPrefixes;
    // The value of jakarta.faces.DEFAULT_SUFFIX or ".jsf";
    private String facesSuffix = ".jsf";
    
    @Override
    public void init(FilterConfig config) throws ServletException {
    	forwardURI = Util.processStringValue(config.getInitParameter("forward"));
    	errorURI = Util.processStringValue(config.getInitParameter("error"));
    	expiredURI = Util.processStringValue(config.getInitParameter("expired"));
    	String urls = Util.processStringValue(config.getInitParameter("unsecured"));
    	if (urls != null) {
    		unsecuredURLPrefixes = urls.split("\n");
			for (int i = 0, l = unsecuredURLPrefixes.length; i < l; i++) {
				unsecuredURLPrefixes[i] = Util.processStringValue(unsecuredURLPrefixes[i]);
			}
    	}
    	
    	String facesSuffixParameter = config.getServletContext().getInitParameter("jakarta.faces.DEFAULT_SUFFIX");
    	if ((facesSuffixParameter != null) && (! facesSuffixParameter.isEmpty())) {
    		facesSuffix = facesSuffixParameter;
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
        HttpServletResponse response = (HttpServletResponse) resp;
		
    	try {
	        String pathToTest = request.getServletPath();

	        // If this ain't a faces page then bug out
	        if (! pathToTest.endsWith(facesSuffix)) {
    			chain.doFilter(req, resp);
        		return;
	        }
	        // Test if this URL is unsecured in the web.xml, and bug out if so
	        // NB can't use queryString here as there could be AJAX posts etc in faces so not good practice
	        if (unsecuredURLPrefixes != null) {
		        for (String unsecuredURLPrefix : unsecuredURLPrefixes) {
		        	if ((unsecuredURLPrefix != null) && pathToTest.startsWith(unsecuredURLPrefix)) {
	        			chain.doFilter(req, resp);
		        		return;
		        	}
		        }
	        }
	        
	        // NB Get the routes each time here in case dev mode is on
	        Router router = CORE.getRepository().getRouter();

	        // Test if this URL is unsecured in the router, and bug out if so
	        if (router.isUnsecured(pathToTest)) {
    			chain.doFilter(req, resp);
        		return;
	        }
	        
	        if (request.getUserPrincipal() == null) { // not logged in
                // NB Can't use the referer header as if we traverse a data grid, 
                // the URL does not represent all of the state required to perform a get and redisplay the page.
                // This is because part of the state is temporarily saved in the session.
                // String redirect = WebUtil.getRefererHeader(request);
                String redirect = Util.getSkyveContextUrl() + forwardURI;
                redirect = response.encodeRedirectURL(redirect);

                // Can't use FacesContext.getCurrentInstance().getExternalContext().redirect() here coz the faces context could be gone
				if (FacesUtil.isAjax(request)) {
					response.setContentType(MimeType.xml.toString());
			        response.setCharacterEncoding(Util.UTF8);
					try (PrintWriter pw = response.getWriter()) {
						pw.print(FacesUtil.xmlPartialRedirect(redirect));
					}
					response.flushBuffer();
				}
				else {
					response.sendRedirect(redirect);
				}
	        }
            else {
        		chain.doFilter(req, resp);
            }
        }
        catch (Exception e) {
        	e.printStackTrace();
        	Throwable c = e.getCause();
        	
        	Util.LOGGER.log(Level.SEVERE, "SkyveFacesFilter.doFilter", e);
        	e.printStackTrace();
        	
        	// redirect to appropriate page
            String uri = errorURI;
            if ((e instanceof ViewExpiredException) || 
            		(c instanceof ViewExpiredException) ||
            		(e instanceof SessionEndedException) ||
            		(c instanceof SessionEndedException)) {
            	uri = expiredURI;
            }
            
            // Can't use FacesContext.getCurrentInstance().getExternalContext().redirect() here coz the faces context could be gone
			if (FacesUtil.isAjax(request)) {
				response.setContentType(MimeType.xml.toString());
		        response.setCharacterEncoding(Util.UTF8);
				try (PrintWriter pw = response.getWriter()) {
					pw.print(FacesUtil.xmlPartialRedirect(Util.getSkyveContextUrl() + uri));
				}
				response.flushBuffer();
			}
			else {
				response.sendRedirect(Util.getSkyveContextUrl() + uri);
			}
        }
		finally {
			if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("SkyveFacesFilter - DISCONNECT PERSISTENCE");
			AbstractPersistence persistence = AbstractPersistence.get();
			persistence.commit(true);
			if (UtilImpl.FACES_TRACE) StateUtil.logStateStats();
		}
    }
}
