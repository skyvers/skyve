package org.skyve.impl.web;

import jakarta.servlet.*;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.skyve.impl.util.UtilImpl;

import java.io.IOException;

@WebFilter("*.properties")
public class LocalePropertiesMimeTypeFilter implements Filter {
    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        Filter.super.init(filterConfig);
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {

        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        String requestURI = httpRequest.getRequestURI();
        String localesURL = "/" + UtilImpl.SMART_CLIENT_DIR + "/locales/";

        // Force javascript MIME type for the response of smartclient locale .properties files
        if (requestURI != null && requestURI.contains(localesURL) &&
                (requestURI.contains("frameworkMessages") || requestURI.contains("skyveMessages"))) {
            httpResponse.setContentType("text/javascript");
        }

        chain.doFilter(request, response);
    }

    @Override
    public void destroy() {
        Filter.super.destroy();
    }
}
