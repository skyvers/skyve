package org.skyve.impl.sail.mock;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.ELContext;
import jakarta.faces.application.Application;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.application.FacesMessage.Severity;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.ResponseStream;
import jakarta.faces.context.ResponseWriter;
import jakarta.faces.lifecycle.Lifecycle;
import jakarta.faces.render.RenderKit;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Provides a thread-confined Faces context for SAIL execution and view tests.
 *
 * <p>Instances may use mutable in-memory external state or delegate to supplied servlet request and response
 * objects. An installed context must be released by the same thread that installed it.
 */
public class MockFacesContext extends FacesContext implements AutoCloseable {
	private @Nonnull Application a = new MockApplication();
	private @Nonnull ELContext elc = new MockELContext();
	private final @Nonnull MockExternalContext externalContext;
	private final @Nonnull Map<String, List<FacesMessage>> messages = new LinkedHashMap<>();
	private @Nonnull UIViewRoot root = new UIViewRoot();
	@SuppressWarnings("resource")
	private @Nullable ResponseWriter responseWriter = null;
	private boolean postback = false;
	private boolean released = false;
	/** The context this one replaced on the thread, put back when this one is released; or null. */
	private @Nullable FacesContext replaced = null;

	/**
	 * Creates a container-independent context backed by mutable in-memory state.
	 */
	public MockFacesContext() {
		externalContext = new MockExternalContext();
	}

	/**
	 * Creates a context backed by the supplied servlet request and response.
	 *
	 * @param request the request delegated to by the external context; must not be {@code null}
	 * @param response the response delegated to by the external context; must not be {@code null}
	 */
	public MockFacesContext(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response) {
		externalContext = new MockExternalContext(request, response);
	}

	/**
	 * Creates and installs a mock context as the current context for this thread.
	 *
	 * <p>Side effects: sets the JSF thread-local current context. The returned context must be closed or released
	 * by the same thread, preferably with a try-with-resources statement.
	 *
	 * @return the newly installed mock context
	 * @throws IllegalStateException when a Faces context is already installed on the current thread
	 */
	@SuppressWarnings("resource")
	public static @Nonnull MockFacesContext get() {
		return install(new MockFacesContext());
	}

	/**
	 * Creates and installs a servlet-backed mock context as the current context for this thread.
	 *
	 * <p>Side effects: sets the JSF thread-local current context. The returned context must be closed by the same
	 * thread, preferably with a try-with-resources statement.
	 *
	 * @param request the request delegated to by the external context; must not be {@code null}
	 * @param response the response delegated to by the external context; must not be {@code null}
	 * @return the newly installed mock context
	 * @throws IllegalStateException when a Faces context is already installed on the current thread
	 */
	@SuppressWarnings("resource")
	public static @Nonnull MockFacesContext get(@Nonnull HttpServletRequest request,
													@Nonnull HttpServletResponse response) {
		return install(new MockFacesContext(request, response));
	}

	/**
	 * Creates a mock context and installs it in place of the thread's current context, such as a real request's,
	 * which {@link #release()} puts back. Use it to build SAIL component trees inside a request without touching the
	 * request's managed beans: the builders take generated ids from the request's page bean whenever the current
	 * context is a real one.
	 *
	 * <p>Side effects: sets the JSF thread-local current context until the returned context is released. It must be
	 * closed by the same thread, preferably with a try-with-resources statement.
	 *
	 * @return the newly installed mock context
	 */
	@SuppressWarnings("resource")
	public static @Nonnull MockFacesContext replacingCurrent() {
		MockFacesContext result = new MockFacesContext();
		result.replaced = FacesContext.getCurrentInstance();
		setCurrentInstance(result);
		return result;
	}

	private static @Nonnull MockFacesContext install(@Nonnull MockFacesContext result) {
		if (FacesContext.getCurrentInstance() != null) {
			throw new IllegalStateException("A FacesContext is already installed on the current thread");
		}

		setCurrentInstance(result);
		return result;
	}

	/**
	 * Returns the mock JSF application used for component and behaviour creation.
	 */
	@Override
	public @Nonnull Application getApplication() {
		return a;
	}

	/**
	 * Returns a mock EL context suitable for view rendering tests.
	 */
	@Override
	public @Nonnull ELContext getELContext() {
		return elc;
	}

	/**
	 * Returns the active mock view root.
	 */
	@Override
	public @Nonnull UIViewRoot getViewRoot() {
		return root;
	}

	/**
	 * Sets the active mock view root.
	 */
	@Override
	public void setViewRoot(@SuppressWarnings("null") @Nonnull UIViewRoot root) {
		this.root = root;
	}

	/**
	 * Returns the client IDs for which messages have been retained, in insertion order.
	 */
	@Override
	public @Nonnull Iterator<String> getClientIdsWithMessages() {
		return messages.keySet().iterator();
	}

	/**
	 * Returns the mock external context used for request, session, and application state.
	 */
	@Override
	public @Nonnull MockExternalContext getExternalContext() {
		return externalContext;
	}

	/**
	 * Returns no lifecycle because lifecycle integration is not required in these tests.
	 */
	@Override
	public @Nullable Lifecycle getLifecycle() {
		return null;
	}

	/**
	 * Returns the greatest severity among retained messages, or {@code null} when there are none.
	 */
	@Override
	public @Nullable Severity getMaximumSeverity() {
		Severity result = null;
		for (List<FacesMessage> clientMessages : messages.values()) {
			for (FacesMessage message : clientMessages) {
				Severity severity = message.getSeverity();
				if ((severity != null) && ((result == null) || (severity.getOrdinal() > result.getOrdinal()))) {
					result = severity;
				}
			}
		}
		return result;
	}

	/**
	 * Returns retained messages in client-ID and insertion order.
	 */
	@Override
	public @Nonnull Iterator<FacesMessage> getMessages() {
		List<FacesMessage> result = new ArrayList<>();
		for (List<FacesMessage> clientMessages : messages.values()) {
			result.addAll(clientMessages);
		}
		return result.iterator();
	}

	/**
	 * Returns retained messages for the supplied client ID in insertion order.
	 */
	@Override
	public @Nonnull Iterator<FacesMessage> getMessages(@Nullable String clientId) {
		List<FacesMessage> result = messages.get(clientId);
		return result == null ? Collections.emptyIterator() : result.iterator();
	}

	/**
	 * Returns no render kit because render kit resolution is not required in these tests.
	 */
	@Override
	public @Nullable RenderKit getRenderKit() {
		return null;
	}

	/**
	 * Indicates that rendering should proceed during mock execution.
	 */
	@Override
	public boolean getRenderResponse() {
		return true;
	}

	/**
	 * Indicates that response completion has not been requested.
	 */
	@Override
	public boolean getResponseComplete() {
		return false;
	}

	/**
	 * Returns no response stream because this mock uses {@link ResponseWriter} only when set explicitly.
	 */
	@Override
	public @Nullable ResponseStream getResponseStream() {
		return null;
	}

	/**
	 * Ignores direct response stream assignment in the mock context.
	 */
	@Override
	public void setResponseStream(@Nullable ResponseStream responseStream) {
		// nothing to see here
	}

	/**
	 * Returns the currently assigned response writer.
	 */
	@Override
	public @Nullable ResponseWriter getResponseWriter() {
		return responseWriter;
	}

	/**
	 * Stores the response writer for subsequent rendering assertions.
	 */
	@Override
	public void setResponseWriter(@Nullable ResponseWriter responseWriter) {
		this.responseWriter = responseWriter;
	}

	/**
	 * Retains a message for subsequent assertions.
	 */
	@Override
	public void addMessage(@Nullable String clientId, @SuppressWarnings("null") @Nonnull FacesMessage message) {
		messages.computeIfAbsent(clientId, ignored -> new ArrayList<>()).add(Objects.requireNonNull(message, "message"));
	}

	/**
	 * Marks the context as released and removes it from the current thread when this context is installed there.
	 *
	 * <p>Side effects: clears the JSF thread-local current context only when it refers to this instance, putting back
	 * the context it replaced, if any (see {@link #replacingCurrent()}).
	 */
	@Override
	public void release() {
		if (FacesContext.getCurrentInstance() == this) {
			setCurrentInstance(replaced);
		}
		released = true;
	}

	/**
	 * Releases this context so it can be used with try-with-resources.
	 */
	@Override
	public void close() {
		release();
	}

	/**
	 * Indicates whether {@link #release()} has been called.
	 */
	@Override
	public boolean isReleased() {
		return released;
	}

	/**
	 * Indicates whether this context represents a JSF postback.
	 */
	@Override
	public boolean isPostback() {
		return postback;
	}

	/**
	 * Sets whether this context represents a JSF postback.
	 *
	 * @param postback {@code true} for a postback request
	 */
	public void setPostback(boolean postback) {
		this.postback = postback;
	}

	/**
	 * No-op in mock execution.
	 */
	@Override
	public void renderResponse() {
		// nothing to see here
	}

	/**
	 * No-op in mock execution.
	 */
	@Override
	public void responseComplete() {
		// nothing to see here
	}
}
