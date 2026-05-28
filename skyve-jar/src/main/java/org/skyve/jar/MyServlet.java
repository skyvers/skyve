package org.skyve.jar;

import java.io.IOException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Serves a minimal HTTP endpoint used by the standalone JAR bootstrap example.
 */
public class MyServlet extends HttpServlet {
	private static final long serialVersionUID = 1913363923642038056L;

	/**
	 * Writes a static response body for HTTP GET requests.
	 *
	 * @param req the inbound servlet request; not {@code null}
	 * @param resp the outbound servlet response; not {@code null}
	 * @throws ServletException if servlet processing fails before writing the
	 *         response
	 * @throws IOException if the response writer cannot be obtained or written
	 */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		resp.getWriter().print("SHITER");
	}
}
