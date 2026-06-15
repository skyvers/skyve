package org.skyve.impl.web.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

class DocsServletTest {
    @Test
    @SuppressWarnings("static-method")
    void doGetForwardsToDefaultSwaggerUriWhenNoInitParamConfigured() throws Exception {
        DocsServlet servlet = new DocsServlet();
        ServletConfig config = mock(ServletConfig.class);
        ServletContext context = mock(ServletContext.class);
        RequestDispatcher dispatcher = mock(RequestDispatcher.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(config.getServletContext()).thenReturn(context);
        when(config.getInitParameter("docsPath")).thenReturn(null);
        when(context.getRequestDispatcher("/swagger.xhtml")).thenReturn(dispatcher);

        servlet.init(config);
        servlet.doGet(request, response);

        verify(context).getRequestDispatcher("/swagger.xhtml");
        verify(dispatcher).forward(request, response);
    }

    @Test
    @SuppressWarnings("static-method")
    void doGetForwardsToConfiguredDocsUri() throws Exception {
        DocsServlet servlet = new DocsServlet();
        ServletConfig config = mock(ServletConfig.class);
        ServletContext context = mock(ServletContext.class);
        RequestDispatcher dispatcher = mock(RequestDispatcher.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(config.getServletContext()).thenReturn(context);
        when(config.getInitParameter("docsPath")).thenReturn("/custom-docs.xhtml");
        when(context.getRequestDispatcher("/custom-docs.xhtml")).thenReturn(dispatcher);

        servlet.init(config);
        servlet.doGet(request, response);

        verify(context).getRequestDispatcher("/custom-docs.xhtml");
        verify(dispatcher).forward(request, response);
    }
}