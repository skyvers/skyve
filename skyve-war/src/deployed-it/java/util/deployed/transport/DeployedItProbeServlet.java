package util.deployed.transport;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import util.deployed.transport.DeployedItEventRecorder.Event;

/** Provides the authenticated correlation and event-probe transport for deployed test suites. */
@WebServlet(urlPatterns = "/deployed-it/probe", asyncSupported = true)
public final class DeployedItProbeServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String CORRELATION_HEADER = "X-Skyve-Deployed-It-Correlation";
	private static final String TOKEN_HEADER = "X-Skyve-Deployed-It-Token";
	private static final String SESSION_TOKEN_PREFIX = DeployedItProbeServlet.class.getName() + ".token.";

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (! authenticated(request, response)) {
			return;
		}
		String correlationId = request.getHeader(CORRELATION_HEADER);
		if (! validCorrelation(correlationId)) {
			response.sendError(HttpServletResponse.SC_BAD_REQUEST);
			return;
		}

		String token = UUID.randomUUID().toString();
		request.getSession(true).setAttribute(SESSION_TOKEN_PREFIX + correlationId, token);
		DeployedItEventRecorder.start(correlationId);
		writeJson(response, "{\"correlationId\":\"" + correlationId + "\",\"token\":\"" + token + "\"}");
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String correlationId = authorisedCorrelation(request, response);
		if (correlationId == null) {
			return;
		}
		List<Event> events = DeployedItEventRecorder.snapshot(correlationId);
		StringBuilder json = new StringBuilder(64 + (events.size() * 64));
		json.append("{\"correlationId\":\"").append(correlationId).append("\",\"events\":[");
		for (int i = 0, size = events.size(); i < size; i++) {
			if (i > 0) {
				json.append(',');
			}
			Event event = events.get(i);
			json.append("{\"timestamp\":\"").append(jsonString(event.timestamp())).append("\",\"name\":\"")
					.append(jsonString(event.name())).append("\"}");
		}
		json.append("]}");
		writeJson(response, json.toString());
	}

	@Override
	protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String correlationId = authorisedCorrelation(request, response);
		if (correlationId == null) {
			return;
		}
		DeployedItEventRecorder.remove(correlationId);
		request.getSession(false).removeAttribute(SESSION_TOKEN_PREFIX + correlationId);
		response.setStatus(HttpServletResponse.SC_NO_CONTENT);
	}

	private static String authorisedCorrelation(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (! authenticated(request, response)) {
			return null;
		}
		String correlationId = request.getHeader(CORRELATION_HEADER);
		String token = request.getHeader(TOKEN_HEADER);
		HttpSession session = request.getSession(false);
		Object expected = (session == null) ? null : session.getAttribute(SESSION_TOKEN_PREFIX + correlationId);
		if ((! validCorrelation(correlationId)) || (token == null) || (! token.equals(expected))) {
			response.sendError(HttpServletResponse.SC_FORBIDDEN);
			return null;
		}
		return correlationId;
	}

	private static boolean authenticated(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (request.getUserPrincipal() == null) {
			response.sendError(HttpServletResponse.SC_UNAUTHORIZED);
			return false;
		}
		return true;
	}

	private static boolean validCorrelation(String value) {
		return (value != null) && value.matches("[A-Za-z0-9][A-Za-z0-9._-]{7,127}");
	}

	private static String jsonString(String value) {
		return value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "\\r").replace("\n", "\\n");
	}

	private static void writeJson(HttpServletResponse response, String json) throws IOException {
		response.setCharacterEncoding("UTF-8");
		response.setContentType("application/json");
		response.getWriter().write(json);
	}
}
