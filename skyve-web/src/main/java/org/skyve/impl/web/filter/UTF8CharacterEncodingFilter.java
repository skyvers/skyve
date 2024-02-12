package org.skyve.impl.web.filter;

import java.io.IOException;

import org.skyve.util.Util;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;

/**
 * Sets the char encoding to UTF-8 on the request if not set, and preemptively sets UTF-8 on the response.
 */
public class UTF8CharacterEncodingFilter implements Filter {
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		// nothing to see here
	}
	
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (request.getCharacterEncoding() == null) {
			request.setCharacterEncoding(Util.UTF8);
		}
		// preemptively set to UTF8 before calling the chain because ServletRequest.getWriter() will set the content type in stone
		response.setCharacterEncoding(Util.UTF8);
		chain.doFilter(request, response);
		
		// TODO Note that we can't set the contentType or character encoding here after calling the filter chain as the response is committed.
		// So to add selective char encodings we would have to look at the servlet path and exclude certain mappings.
		// Regex is too hard.
		// A CSV of path prefixes to exclude is doable if really needed.
		// See how this pans out with flutter.
	}

	@Override
	public void destroy() {
		// nothing to see here
	}
}
