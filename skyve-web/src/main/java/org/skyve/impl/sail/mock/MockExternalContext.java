package org.skyve.impl.sail.mock;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.Principal;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.FacesException;
import jakarta.faces.context.ExternalContext;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Provides in-memory or servlet-backed external state for Faces tests.
 *
 * <p>The no-argument form exposes mutable, instance-local maps for application, request, session, header, cookie,
 * and parameter state. The servlet-backed form delegates request and response behaviour to the supplied
 * {@link HttpServletRequest} and {@link HttpServletResponse}; application and session state consequently use their
 * associated {@link ServletContext} and {@link HttpSession}. Spring Test servlet mocks can be supplied directly.
 *
 * <p>Ownership: this context does not own or release supplied servlet objects, streams, or writers.
 *
 * <p>Not thread-safe: instances are intended to remain confined to one test or SAIL execution.
 */
public class MockExternalContext extends ExternalContext {
	private final Map<String, Object> applicationMap = new HashMap<>();
	private final Map<String, String> initParameterMap = new HashMap<>();
	private final Map<String, Object> requestCookieMap = new HashMap<>();
	private final Map<String, String> requestHeaderMap = new HashMap<>();
	private final Map<String, String[]> requestHeaderValuesMap = new HashMap<>();
	private final Map<String, Object> requestMap = new HashMap<>();
	private final Map<String, String> requestParameterMap = new HashMap<>();
	private final Map<String, String[]> requestParameterValuesMap = new HashMap<>();
	private final Map<String, Object> sessionMap = new HashMap<>();

	private @Nullable Locale requestLocale = Locale.getDefault();
	private List<Locale> requestLocales = Collections.singletonList(requestLocale);
	private @Nullable HttpServletRequest request;
	private @Nullable HttpServletResponse response;

	/**
	 * Creates a container-independent context backed by mutable in-memory maps.
	 *
	 * <p>The request and response are initially {@code null}. Servlet-only operations return their documented fallback
	 * value or have no effect until servlet objects are assigned.
	 */
	public MockExternalContext() {
		// Default constructor for SAIL execution without a servlet container.
	}

	/**
	 * Creates a context backed by the supplied servlet request and response.
	 *
	 * <p>Request, response, application, and session operations delegate to these objects. They may be servlet mocks,
	 * including those provided by Spring Test.
	 *
	 * @param request the request to delegate to; must not be {@code null}
	 * @param response the response to delegate to; must not be {@code null}
	 */
	public MockExternalContext(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response) {
		this.request = request;
		this.response = response;
	}

	/**
	 * Delegates dispatch to the configured servlet request and response, or does nothing without them.
	 *
	 * @param path the dispatcher path understood by the configured request; must not be {@code null}
	 * @throws IOException if the servlet dispatcher cannot forward the request
	 * @throws FacesException if the servlet dispatcher reports a {@link ServletException}
	 */
	@Override
	public void dispatch(@Nonnull String path) throws IOException {
		if ((request != null) && (response != null)) {
			RequestDispatcher dispatcher = request.getRequestDispatcher(path);
			if (dispatcher != null) {
				try {
					dispatcher.forward(request, response);
				}
				catch (ServletException e) {
					throw new FacesException(e);
				}
			}
		}
	}

	/**
	 * Encodes the action URL through the configured servlet response, or returns it unchanged without one.
	 */
	@Override
	public @Nonnull String encodeActionURL(@Nonnull String url) {
		return response == null ? url : response.encodeURL(url);
	}

	/**
	 * Returns the supplied name unchanged because the mock has no portlet namespace.
	 */
	@Override
	public @Nonnull String encodeNamespace(@Nonnull String name) {
		return name;
	}

	/**
	 * Encodes the resource URL through the configured servlet response, or returns it unchanged without one.
	 */
	@Override
	public @Nonnull String encodeResourceURL(@Nonnull String url) {
		return response == null ? url : response.encodeURL(url);
	}

	/**
	 * Encodes the WebSocket URL through the configured servlet response, or returns it unchanged without one.
	 */
	@Override
	public @Nonnull String encodeWebsocketURL(@Nonnull String url) {
		return response == null ? url : response.encodeURL(url);
	}

	/**
	 * Encodes a partial-action URL through the configured servlet response, or returns it unchanged without one.
	 */
	@Override
	public @Nonnull String encodePartialActionURL(@Nonnull String url) {
		return response == null ? url : response.encodeURL(url);
	}

	/**
	 * Adds a cookie to the configured servlet response, or does nothing without one.
	 *
	 * <p>Recognised properties are {@code domain}, {@code maxAge}, {@code path}, {@code secure}, and {@code httpOnly};
	 * all other entries are copied as cookie attributes.
	 *
	 * @param name the cookie name; must not be {@code null}
	 * @param value the cookie value; must not be {@code null}
	 * @param properties optional cookie properties; may be {@code null}
	 */
	@Override
	public void addResponseCookie(@Nonnull String name,
									@Nonnull String value,
									@Nullable Map<String, Object> properties) {
		HttpServletResponse currentResponse = response;
		if (currentResponse == null) {
			return;
		}

		Cookie cookie = new Cookie(name, value);
		if (properties != null) {
			for (Map.Entry<String, Object> entry : properties.entrySet()) {
				String property = entry.getKey();
				Object propertyValue = entry.getValue();
				switch (property) {
					case "domain" -> cookie.setDomain((String) propertyValue);
					case "maxAge" -> cookie.setMaxAge(((Number) propertyValue).intValue());
					case "path" -> cookie.setPath((String) propertyValue);
					case "secure" -> cookie.setSecure(Boolean.TRUE.equals(propertyValue));
					case "httpOnly" -> cookie.setHttpOnly(Boolean.TRUE.equals(propertyValue));
					default -> cookie.setAttribute(property, String.valueOf(propertyValue));
				}
			}
		}
		currentResponse.addCookie(cookie);
	}

	/**
	 * Returns live servlet-context attributes, or mutable in-memory application state without a servlet request.
	 *
	 * @return a mutable live map; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, Object> getApplicationMap() {
		ServletContext servletContext = servletContext();
		if (servletContext != null) {
			return attributeMap(servletContext::getAttributeNames,
					servletContext::getAttribute,
					servletContext::setAttribute,
					servletContext::removeAttribute);
		}
		return applicationMap;
	}

	/**
	 * Returns the authentication mechanism delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getAuthType() {
		return request == null ? null : request.getAuthType();
	}

	/**
	 * Returns the servlet context from the configured servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable Object getContext() {
		return request == null ? null : request.getServletContext();
	}

	/**
	 * Returns the configured servlet context name, or {@code null} without a servlet request.
	 */
	@Override
	public @Nullable String getContextName() {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getServletContextName();
	}

	/**
	 * Returns the configured servlet application context path, or {@code null} without a servlet request.
	 */
	@Override
	public @Nullable String getApplicationContextPath() {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getContextPath();
	}

	/**
	 * Resolves a MIME type through the configured servlet context, or returns {@code null} without one.
	 */
	@Override
	public @Nullable String getMimeType(@Nonnull String file) {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getMimeType(file);
	}

	/**
	 * Resolves a real path through the configured servlet context, or returns {@code null} without one.
	 */
	@Override
	public @Nullable String getRealPath(@Nonnull String path) {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getRealPath(path);
	}

	/**
	 * Returns a configured initialisation parameter.
	 */
	@Override
	public @Nullable String getInitParameter(@Nonnull String name) {
		ServletContext servletContext = servletContext();
		return servletContext == null ? initParameterMap.get(name) : servletContext.getInitParameter(name);
	}

	/**
	 * Returns servlet initialisation parameters, or mutable in-memory parameters without a servlet request.
	 */
	@Override
	public @Nonnull Map<String, String> getInitParameterMap() {
		ServletContext servletContext = servletContext();
		if (servletContext != null) {
			return readOnlyMap(servletContext::getInitParameterNames, servletContext::getInitParameter);
		}
		return initParameterMap;
	}

	/**
	 * Returns the authenticated user delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRemoteUser() {
		return request == null ? null : request.getRemoteUser();
	}

	/**
	 * Returns the configured request object, or {@code null} when none has been assigned.
	 */
	@Override
	public @Nullable HttpServletRequest getRequest() {
		return request;
	}

	/**
	 * Stores the request object returned by {@link #getRequest()}.
	 *
	 * <p>The Faces API declares this boundary as {@link Object}; this implementation accepts servlet requests only.
	 *
	 * @param request the servlet request to delegate to, or {@code null} to restore container-independent behaviour
	 * @throws ClassCastException if {@code request} is non-null and is not an {@link HttpServletRequest}
	 */
	@Override
	public void setRequest(@Nullable Object request) {
		this.request = (HttpServletRequest) request;
	}

	/**
	 * Returns the request scheme delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestScheme() {
		return request == null ? null : request.getScheme();
	}

	/**
	 * Returns the server name delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestServerName() {
		return request == null ? null : request.getServerName();
	}

	/**
	 * Returns the server port delegated by a servlet request, or {@code -1} without one.
	 */
	@Override
	public int getRequestServerPort() {
		return request == null ? -1 : request.getServerPort();
	}

	/**
	 * Sets character encoding on the configured servlet request, or does nothing without one.
	 *
	 * @param encoding the character encoding name; must not be {@code null}
	 * @throws UnsupportedEncodingException if the request does not support {@code encoding}
	 */
	@Override
	public void setRequestCharacterEncoding(@Nonnull String encoding) throws UnsupportedEncodingException {
		if (request != null) {
			request.setCharacterEncoding(encoding);
		}
	}

	/**
	 * Returns the context path delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestContextPath() {
		return request == null ? null : request.getContextPath();
	}

	/**
	 * Returns the cookies supplied by the configured servlet request.
	 *
	 * @return an unmodifiable snapshot keyed by cookie name, or a mutable in-memory map without a request; never
	 * {@code null}
	 */
	@Override
	public @Nonnull Map<String, Object> getRequestCookieMap() {
		HttpServletRequest currentRequest = request;
		if (currentRequest != null) {
			Map<String, Object> result = new HashMap<>();
			Cookie[] cookies = currentRequest.getCookies();
			if (cookies != null) {
				for (Cookie cookie : cookies) {
					result.put(cookie.getName(), cookie);
				}
			}
			return Collections.unmodifiableMap(result);
		}
		return requestCookieMap;
	}

	/**
	 * Returns a read-only live view of single-valued servlet request headers.
	 *
	 * @return an unmodifiable live view when servlet-backed, or a mutable in-memory map otherwise; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, String> getRequestHeaderMap() {
		return request == null ? requestHeaderMap : readOnlyMap(request::getHeaderNames, request::getHeader);
	}

	/**
	 * Returns a read-only live view of multi-valued servlet request headers.
	 *
	 * @return an unmodifiable live view when servlet-backed, or a mutable in-memory map otherwise; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, String[]> getRequestHeaderValuesMap() {
		return request == null ? 
				requestHeaderValuesMap :
				readOnlyMap(request::getHeaderNames, name -> Collections.list(request.getHeaders(name)).toArray(String[]::new));
	}

	/**
	 * Returns the servlet request locale, or the configured in-memory locale without a servlet request.
	 */
	@Override
	public @Nullable Locale getRequestLocale() {
		return request == null ? requestLocale : request.getLocale();
	}

	/**
	 * Sets the request locale returned by {@link #getRequestLocale()} and {@link #getRequestLocales()}.
	 *
	 * @param requestLocale the request locale, or {@code null} to simulate an unavailable locale
	 */
	public void setRequestLocale(@Nullable Locale requestLocale) {
		this.requestLocale = requestLocale;
		requestLocales = Collections.singletonList(requestLocale);
	}

	/**
	 * Returns the servlet request locales, or the configured in-memory locales without a servlet request.
	 */
	@Override
	public @Nonnull Iterator<Locale> getRequestLocales() {
		return request == null ? requestLocales.iterator() : Collections.list(request.getLocales()).iterator();
	}

	/**
	 * Returns live servlet request attributes, or mutable in-memory request state without a servlet request.
	 *
	 * @return a mutable live map; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, Object> getRequestMap() {
		if (request != null) {
			return attributeMap(request::getAttributeNames,
					request::getAttribute,
					request::setAttribute,
					request::removeAttribute);
		}
		return requestMap;
	}

	/**
	 * Returns servlet request parameters, or mutable in-memory parameters without a servlet request.
	 *
	 * @return an unmodifiable live view when servlet-backed, or a mutable in-memory map otherwise; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, String> getRequestParameterMap() {
		HttpServletRequest currentRequest = request;
		if (currentRequest != null) {
			return readOnlyMap(() -> Collections.enumeration(currentRequest.getParameterMap().keySet()), currentRequest::getParameter);
		}
		return requestParameterMap;
	}

	/**
	 * Returns the names in the single-valued request parameter map.
	 */
	@Override
	public @Nonnull Iterator<String> getRequestParameterNames() {
		return request == null ? 
					requestParameterMap.keySet().iterator() :
					Collections.list(request.getParameterNames()).iterator();
	}

	/**
	 * Returns servlet request parameter values, or mutable in-memory values without a servlet request.
	 */
	@Override
	public @Nonnull Map<String, String[]> getRequestParameterValuesMap() {
		if (request != null) {
			return Collections.unmodifiableMap(request.getParameterMap());
		}
		return requestParameterValuesMap;
	}

	/**
	 * Returns path information delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestPathInfo() {
		return request == null ? null : request.getPathInfo();
	}

	/**
	 * Returns the servlet path delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestServletPath() {
		return request == null ? null : request.getServletPath();
	}

	/**
	 * Returns request character encoding delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestCharacterEncoding() {
		return request == null ? null : request.getCharacterEncoding();
	}

	/**
	 * Returns request content type delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable String getRequestContentType() {
		return request == null ? null : request.getContentType();
	}

	/**
	 * Returns request content length delegated by a servlet request, or {@code -1} without one.
	 */
	@Override
	public int getRequestContentLength() {
		return request == null ? -1 : request.getContentLength();
	}

	/**
	 * Returns a resource from the configured servlet context, or {@code null} without one.
	 *
	 * @param path the context-relative resource path; must begin with {@code /}
	 * @return the resource URL, or {@code null} when unavailable
	 * @throws MalformedURLException if the servlet context cannot form a URL for {@code path}
	 */
	@Override
	public @Nullable URL getResource(@Nonnull String path) throws MalformedURLException {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getResource(path);
	}

	/**
	 * Returns a resource stream from the configured servlet context, or {@code null} without one.
	 *
	 * <p>Ownership: the caller is responsible for closing a returned stream.
	 *
	 * @param path the context-relative resource path; must begin with {@code /}
	 * @return a newly opened stream, or {@code null} when unavailable
	 */
	@Override
	@SuppressWarnings("resource") // ExternalContext transfers ownership of the returned stream to its caller.
	public @Nullable InputStream getResourceAsStream(@Nonnull String path) {
		ServletContext servletContext = servletContext();
		return servletContext == null ? null : servletContext.getResourceAsStream(path);
	}

	/**
	 * Returns resource paths from the configured servlet context, or an empty set without one.
	 *
	 * @param path the context-relative directory path; must begin with {@code /}
	 * @return the resource paths supplied by the servlet context, or an empty set when unavailable; never {@code null}
	 */
	@Override
	public @Nonnull Set<String> getResourcePaths(@Nonnull String path) {
		ServletContext servletContext = servletContext();
		Set<String> result = servletContext == null ? null : servletContext.getResourcePaths(path);
		return result == null ? Collections.emptySet() : result;
	}

	/**
	 * Returns the configured response object, or {@code null} when none has been assigned.
	 */
	@Override
	public @Nullable HttpServletResponse getResponse() {
		return response;
	}

	/**
	 * Stores the response object returned by {@link #getResponse()}.
	 *
	 * <p>The Faces API declares this boundary as {@link Object}; this implementation accepts servlet responses only.
	 *
	 * @param response the servlet response to delegate to, or {@code null} to restore container-independent behaviour
	 * @throws ClassCastException if {@code response} is non-null and is not an {@link HttpServletResponse}
	 */
	@Override
	public void setResponse(@Nullable Object response) {
		this.response = (HttpServletResponse) response;
	}

	/**
	 * Returns the configured servlet response stream, or {@code null} without one.
	 *
	 * <p>Ownership: the caller controls the returned stream; {@link #release()} does not close it.
	 *
	 * @return the response output stream, or {@code null} without a configured response
	 * @throws IOException if the response cannot provide its output stream
	 */
	@Override
	@SuppressWarnings("resource") // ExternalContext transfers ownership of the response stream to its caller.
	public @Nullable OutputStream getResponseOutputStream() throws IOException {
		return response == null ? null : response.getOutputStream();
	}

	/**
	 * Returns the configured servlet response writer, or {@code null} without one.
	 *
	 * <p>Ownership: the caller controls the returned writer; {@link #release()} does not close it.
	 *
	 * @return the response writer, or {@code null} without a configured response
	 * @throws IOException if the response cannot provide its writer
	 */
	@Override
	@SuppressWarnings("resource") // ExternalContext transfers ownership of the response writer to its caller.
	public @Nullable Writer getResponseOutputWriter() throws IOException {
		return response == null ? null : response.getWriter();
	}

	/**
	 * Returns response character encoding delegated by a servlet response, or {@code null} without one.
	 */
	@Override
	public @Nullable String getResponseCharacterEncoding() {
		return response == null ? null : response.getCharacterEncoding();
	}

	/**
	 * Returns response content type delegated by a servlet response, or {@code null} without one.
	 */
	@Override
	public @Nullable String getResponseContentType() {
		return response == null ? null : response.getContentType();
	}

	/**
	 * Sets character encoding on the configured servlet response, or does nothing without one.
	 */
	@Override
	public void setResponseCharacterEncoding(@Nullable String encoding) {
		if (response != null) {
			response.setCharacterEncoding(encoding);
		}
	}

	/**
	 * Sets content type on the configured servlet response, or does nothing without one.
	 */
	@Override
	public void setResponseContentType(@Nullable String contentType) {
		if (response != null) {
			response.setContentType(contentType);
		}
	}

	/**
	 * Returns the session delegated by a servlet request, creating it when requested, or {@code null} without one.
	 *
	 * @param create whether to create a session when none exists
	 * @return the current servlet session, a newly created session when requested, or {@code null}
	 */
	@Override
	public @Nullable Object getSession(boolean create) {
		return request == null ? null : request.getSession(create);
	}

	/**
	 * Returns the delegated session id, optionally creating the session, or {@code null} without one.
	 *
	 * @param create whether to create a session when none exists
	 * @return the current session identifier, or {@code null} when no session is available
	 */
	@Override
	public @Nullable String getSessionId(boolean create) {
		HttpSession session = request == null ? null : request.getSession(create);
		return session == null ? null : session.getId();
	}

	/**
	 * Returns the delegated session timeout, creating the session if necessary, or {@code -1} without a request.
	 */
	@Override
	public int getSessionMaxInactiveInterval() {
		return request == null ? -1 : request.getSession(true).getMaxInactiveInterval();
	}

	/**
	 * Sets the delegated session timeout, creating the session if necessary, or does nothing without a request.
	 */
	@Override
	public void setSessionMaxInactiveInterval(int interval) {
		if (request != null) {
			request.getSession(true).setMaxInactiveInterval(interval);
		}
	}

	/**
	 * Invalidates an existing delegated session, or does nothing when no session is active.
	 */
	@Override
	public void invalidateSession() {
		if (request != null) {
			HttpSession session = request.getSession(false);
			if (session != null) {
				session.invalidate();
			}
		}
	}

	/**
	 * Returns live servlet session attributes, or mutable in-memory session state without a servlet request.
	 *
	 * <p>Side effects: creates a servlet session when a request is configured and no session exists.
	 *
	 * @return a mutable live map; never {@code null}
	 */
	@Override
	public @Nonnull Map<String, Object> getSessionMap() {
		if (request != null) {
			HttpSession session = request.getSession(true);
			return attributeMap(session::getAttributeNames,
									session::getAttribute,
									session::setAttribute,
									session::removeAttribute);
		}
		return sessionMap;
	}

	/**
	 * Returns the principal delegated by a servlet request, or {@code null} without one.
	 */
	@Override
	public @Nullable Principal getUserPrincipal() {
		return request == null ? null : request.getUserPrincipal();
	}

	/**
	 * Returns whether the configured servlet request grants the supplied role.
	 */
	@Override
	public boolean isUserInRole(@Nonnull String role) {
		return request != null && request.isUserInRole(role);
	}

	/**
	 * Indicates whether the configured servlet request is secure.
	 */
	@Override
	public boolean isSecure() {
		return request != null && request.isSecure();
	}

	/**
	 * Delegates a log message to the servlet context, or ignores it without one.
	 */
	@Override
	public void log(@Nonnull String message) {
		ServletContext servletContext = servletContext();
		if (servletContext != null) {
			servletContext.log(message);
		}
	}

	/**
	 * Delegates a log message and exception to the servlet context, or ignores them without one.
	 */
	@Override
	public void log(@Nonnull String message, @Nonnull Throwable exception) {
		ServletContext servletContext = servletContext();
		if (servletContext != null) {
			servletContext.log(message, exception);
		}
	}

	/**
	 * Delegates a redirect to the servlet response, or does nothing without one.
	 *
	 * @param url the redirect location; must not be {@code null}
	 * @throws IOException if the configured response cannot send the redirect
	 */
	@Override
	public void redirect(@Nonnull String url) throws IOException {
		if (response != null) {
			response.sendRedirect(url);
		}
	}

	/**
	 * Replaces the named response header, or does nothing without a configured response.
	 *
	 * @param name the header name; must not be {@code null}
	 * @param value the header value; must not be {@code null}
	 */
	@Override
	public void setResponseHeader(@Nonnull String name, @Nonnull String value) {
		if (response != null) {
			response.setHeader(name, value);
		}
	}

	/**
	 * Appends a value to the named response header, or does nothing without a configured response.
	 *
	 * @param name the header name; must not be {@code null}
	 * @param value the header value; must not be {@code null}
	 */
	@Override
	public void addResponseHeader(@Nonnull String name, @Nonnull String value) {
		if (response != null) {
			response.addHeader(name, value);
		}
	}

	/**
	 * Sets the response buffer size, or does nothing without a configured response.
	 *
	 * @param size the requested buffer size in bytes; must not be negative
	 */
	@Override
	public void setResponseBufferSize(int size) {
		if (response != null) {
			response.setBufferSize(size);
		}
	}

	/**
	 * Returns the configured response buffer size.
	 *
	 * @return the buffer size in bytes, or {@code 0} without a configured response
	 */
	@Override
	public int getResponseBufferSize() {
		return response == null ? 0 : response.getBufferSize();
	}

	/**
	 * Reports whether the configured response has committed its status and headers.
	 *
	 * @return {@code true} when the configured response is committed; otherwise {@code false}
	 */
	@Override
	public boolean isResponseCommitted() {
		return response != null && response.isCommitted();
	}

	/**
	 * Clears the configured response buffer, headers, and status, or does nothing without a response.
	 *
	 * @throws IllegalStateException if the configured response is already committed
	 */
	@Override
	public void responseReset() {
		if (response != null) {
			response.reset();
		}
	}

	/**
	 * Sends an error through the configured response, or does nothing without one.
	 *
	 * @param statusCode the HTTP error status code
	 * @param message the optional error message; may be {@code null}
	 * @throws IOException if the configured response cannot send the error
	 * @throws IllegalStateException if the configured response is already committed
	 */
	@Override
	public void responseSendError(int statusCode, @Nullable String message) throws IOException {
		if (response != null) {
			response.sendError(statusCode, message);
		}
	}

	/**
	 * Sets the configured response status, or does nothing without a response.
	 *
	 * @param statusCode the HTTP status code
	 */
	@Override
	public void setResponseStatus(int statusCode) {
		if (response != null) {
			response.setStatus(statusCode);
		}
	}

	/**
	 * Flushes the configured response buffer, or does nothing without a response.
	 *
	 * <p>Side effects: commits the configured response.
	 *
	 * @throws IOException if the configured response cannot flush its buffer
	 */
	@Override
	public void responseFlushBuffer() throws IOException {
		if (response != null) {
			response.flushBuffer();
		}
	}

	/**
	 * Sets the response content length, or does nothing without a configured response.
	 *
	 * @param length the content length in bytes; must not be negative
	 */
	@Override
	public void setResponseContentLength(int length) {
		if (response != null) {
			response.setContentLength(length);
		}
	}

	/**
	 * Performs no resource cleanup because servlet-object lifecycle remains owned by the caller.
	 */
	@Override
	public void release() {
		// No external resources are retained.
	}

	private @Nullable ServletContext servletContext() {
		return request == null ? null : request.getServletContext();
	}

	private static @Nonnull <V> Map<String, V> readOnlyMap(@Nonnull Supplier<Enumeration<String>> names,
													@Nonnull Function<String, V> getter) {
		return Collections.unmodifiableMap(new DelegatingMap<>(names, getter, null, null));
	}

	private static @Nonnull Map<String, Object> attributeMap(@Nonnull Supplier<Enumeration<String>> names,
														@Nonnull Function<String, Object> getter,
														@Nonnull BiConsumer<String, Object> setter,
														@Nonnull Consumer<String> remover) {
		return new DelegatingMap<>(names, getter, setter, remover);
	}

	private static final class DelegatingMap<V> extends AbstractMap<String, V> {
		private final Supplier<Enumeration<String>> names;
		private final Function<String, V> getter;
		private final BiConsumer<String, V> setter;
		private final Consumer<String> remover;

		private DelegatingMap(@Nonnull Supplier<Enumeration<String>> names,
								@Nonnull Function<String, V> getter,
								@Nullable BiConsumer<String, V> setter,
								@Nullable Consumer<String> remover) {
			this.names = names;
			this.getter = getter;
			this.setter = setter;
			this.remover = remover;
		}

		@Override
		public boolean equals(@Nullable Object object) {
			return super.equals(object);
		}

		@Override
		public int hashCode() {
			return super.hashCode();
		}

		@Override
		public @Nullable V get(@Nullable Object key) {
			return key instanceof String name ? getter.apply(name) : null;
		}

		@Override
		public @Nullable V put(@Nonnull String key, @Nullable V value) {
			if (setter == null) {
				throw new UnsupportedOperationException();
			}
			V result = getter.apply(key);
			setter.accept(key, value);
			return result;
		}

		@Override
		public @Nullable V remove(@Nullable Object key) {
			if (remover == null) {
				throw new UnsupportedOperationException();
			}
			if (! (key instanceof String name)) {
				return null;
			}
			V result = getter.apply(name);
			remover.accept(name);
			return result;
		}

		@Override
		public @Nonnull Set<Entry<String, V>> entrySet() {
			return new AbstractSet<>() {
				@Override
				public @Nonnull Iterator<Entry<String, V>> iterator() {
					Enumeration<String> enumeration = names.get();
					return new Iterator<>() {
						private String current;

						@Override
						public boolean hasNext() {
							return enumeration.hasMoreElements();
						}

						@Override
						public @Nonnull Entry<String, V> next() {
							if (! hasNext()) {
								throw new NoSuchElementException();
							}
							current = enumeration.nextElement();
							return new SimpleImmutableEntry<>(current, getter.apply(current));
						}

						@Override
						public void remove() {
							if ((current == null) || (remover == null)) {
								throw new UnsupportedOperationException();
							}
							remover.accept(current);
							current = null;
						}
					};
				}

				@Override
				public int size() {
					return Collections.list(names.get()).size();
				}
			};
		}
	}
}
