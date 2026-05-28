package org.skyve.impl.web.service;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Forwards the public documentation route to the configured documentation page.
 */
public class DocsServlet extends HttpServlet {
	private static final long serialVersionUID = 1761838386499749629L;

	/**
	 * This is the path to the swagger xhtml page which loads the Swagger UI documentation,
	 * or to a custom application documentation path to be forwarded to from the <code>/docs</code> route.
	 */
	private String docsUri = "/swagger.xhtml";

	/**
	 * Initializes the target documentation URI from servlet init parameters.
	 *
	 * @param config servlet configuration that may include a {@code docsPath} override
	 * @throws ServletException if servlet initialization fails
	 */
	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		if (StringUtils.isNotBlank(config.getInitParameter("docsPath"))) {
			docsUri = config.getInitParameter("docsPath");
		}
	}

	/**
	 * Forwards incoming documentation requests to the configured documentation URI.
	 *
	 * @param req inbound servlet request
	 * @param resp outbound servlet response
	 * @throws ServletException if the target resource cannot be dispatched
	 * @throws IOException if forwarding fails due to I/O errors
	 */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		// redirect will show the docsUri in the browser url bar
		// forward will keep the url the same as the mapped servlet path in the web.xml
		RequestDispatcher dispatcher = getServletContext().getRequestDispatcher(docsUri);
		dispatcher.forward(req, resp);
	}
}
