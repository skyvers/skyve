package org.skyve.impl.web.filter;

import org.skyve.util.Util;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Extend this filter to be able to exclude static URLs from your filter.
 * Calling staticURLPrefix() can determine whether the request is for a static resource or not.
 * These are defined in a context parameter in web.xml.
 */
public abstract class ExcludeStaticFilter implements Filter {
    // A list of all static URL prefixes
    private String[] staticURLPrefixes;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String urls = Util.processStringValue(config.getServletContext().getInitParameter("staticURLPrefixes"));
		if (urls != null) {
			staticURLPrefixes = urls.split("\n");
			for (int i = 0, l = staticURLPrefixes.length; i < l; i++) {
				staticURLPrefixes[i] = Util.processStringValue(staticURLPrefixes[i]);
			}
		}
	}

	@Override
	public void destroy() {
		staticURLPrefixes = null;
	}

	/**
	 * Determines if this request is for a static resource or not.
	 * @param request	The request to check
	 * @return	true if the request is to a static web resource (as defined in web.xml "staticURLPrefixes" context-param), otherwise false.
	 */
	protected boolean staticURLPrefix(HttpServletRequest request) {
		String servletPath = request.getServletPath();

        // Test if this URL is a static URL prefix in the web.xml, and bug out if so
        // NB can't use queryString here as there could be AJAX posts etc in faces so not good practice
        if (staticURLPrefixes != null) {
	        for (String staticURLPrefix : staticURLPrefixes) {
	        	if ((staticURLPrefix != null) && servletPath.startsWith(staticURLPrefix)) {
	        		return true;
	        	}
	        }
        }
        
        return false;
	}
}
