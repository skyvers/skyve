package org.skyve.impl.web.faces;

import org.primefaces.PrimeFaces;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.util.OWASP;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.FacesException;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Provides utility methods shared by Skyve web rendering and request handling paths.
 */
public class FacesUtil {
	/**
	 * Owns a mock Faces context installed for one SAIL execution scope.
	 *
	 * <p>Closing the scope releases only the context installed by that scope. A scope created while another Faces
	 * context is active is a no-op. The scope is thread-confined and must be closed by the thread that created it.
	 */
	public static final class SailFacesContextScope implements AutoCloseable {
		@SuppressWarnings("resource")
		private @Nullable MockFacesContext context;

		private SailFacesContextScope(@Nullable MockFacesContext context) {
			this.context = context;
		}

		/**
		 * Releases the context owned by this scope, if any.
		 */
		@Override
		public void close() {
			if (context != null) {
				context.close();
				context = null;
			}
		}
	}

	/**
	 * Prevents utility-class instantiation.
	 */
	private FacesUtil() {
		// nothing to see here
	}

	/**
	 * Session key used to store a temporary managed bean during redirect navigation.
	 */
	// used to place a bean temporarily in the session when a redirect is done during navigate
	public static final String MANAGED_BEAN_NAME_KEY = "skyveFacesBean";

	/**
	 * View key used to cache responsive form style metadata.
	 */
	// used to get the responsive form grid out of the view root when required
	public static final String FORM_STYLES_KEY = "skyveFormStyles";

	/**
	 * View map key marking a view that has rendered the session's CSRF token, so that its postbacks must send it back
	 * (see {@link SkyveFacesPhaseListener}).
	 */
	static final String CSRF_TOKEN_RENDERED_KEY = "skyveCsrfTokenRendered";

	private static final String SET_STYLE_CLASS_METHOD_NAME = "setStyleClass";

	/**
	 * Returns the session's CSRF token ({@link StateUtil#getSessionToken(HttpSession)}), masked afresh for a hidden
	 * {@code csrfToken} input ({@link StateUtil#maskToken(String)}), and marks the current view so that every postback
	 * of it must send the token back. The mark is kept in the view map, which the view state restores on each
	 * postback whatever the scope of the view's bean. The token is the same for every view and request of the
	 * session; only its mask differs.
	 *
	 * <p>Side effects: puts {@link #CSRF_TOKEN_RENDERED_KEY} in the current view's map; issues the session's token
	 * the first time.
	 *
	 * @return a mask of the session's CSRF token
	 */
	public static @Nonnull String getCsrfToken() {
		FacesContext fc = FacesContext.getCurrentInstance();
		UIViewRoot vr = fc.getViewRoot();
		if (vr != null) {
			vr.getViewMap().put(CSRF_TOKEN_RENDERED_KEY, Boolean.TRUE);
		}
		return StateUtil.maskToken(StateUtil.getSessionToken((HttpSession) fc.getExternalContext().getSession(true)));
	}

	/**
	 * Resolves a named JSF-managed bean from the active EL context.
	 *
	 * @param name the EL bean name; must not be {@code null}
	 * @return the resolved bean instance; never {@code null}
	 * @throws FacesException if no bean with the supplied name is available
	 */
	public static @Nonnull Object getNamed(final @Nonnull String name) {
		FacesContext fc = FacesContext.getCurrentInstance();
		ELContext elContext = fc.getELContext();
		Object result = elContext.getELResolver().getValue(elContext, null, name);

		if (result == null) {
			throw new FacesException("Object with name '" + name + 
										"' was not found. Check your faces-config.xml or @Named annotation.");
		}

		return result;
	}

	/**
	 * Assigns a value into an EL target expression.
	 *
	 * @param value the value to assign; may be {@code null}
	 * @param valueExpression the target EL expression; must not be {@code null}
	 */
	public static void set(final @Nullable Object value, final @Nonnull String valueExpression) {
		FacesContext facesContext = FacesContext.getCurrentInstance();
		ELContext elContext = facesContext.getELContext();
		ExpressionFactory ef = facesContext.getApplication().getExpressionFactory();

		ValueExpression targetExpression = ef.createValueExpression(elContext, valueExpression, Object.class);
		targetExpression.setValue(elContext, value);
	}

	/**
	 * Creates a method expression bound to the active JSF EL context.
	 *
	 * @param expression the method expression text; must not be {@code null}
	 * @param expectedReturnType the expected return type; must not be {@code null}
	 * @param expectedParamTypes the expected parameter types; must not be {@code null}
	 * @return the compiled method expression; never {@code null}
	 */
	public static @Nonnull MethodExpression createMethodExpression(@Nonnull String expression,
																	@Nonnull Class<?> expectedReturnType,
																	@Nonnull Class<?>[] expectedParamTypes) {
		try {
			FacesContext fc = FacesContext.getCurrentInstance();
			ExpressionFactory factory = fc.getApplication().getExpressionFactory();
			return factory.createMethodExpression(fc.getELContext(), expression, expectedReturnType, expectedParamTypes);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			throw new FacesException("Method expression '" + expression + "' could not be created.");
		}
	}

	/**
	 * Builds an XML partial-response redirect payload for AJAX redirects.
	 *
	 * Use this only when there may be no faces context (ie view has expired)
	 * otherwise should use FacesContext.getCurrentInstance().getExternalContext().redirect();
	 * @param url the redirect destination; must not be {@code null}
	 * @return XML partial-response text containing a redirect instruction; never {@code null}
	 */
	public static @Nonnull String xmlPartialRedirect(@Nonnull String url) {
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version='1.0' encoding='UTF-8'?>");
		sb.append("<partial-response><redirect url=\"").append(url.replace("&", "&amp;")).append("\"/></partial-response>");
		return sb.toString();
	}

	/**
	 * Indicates whether the request originated from an XMLHttpRequest call.
	 *
	 * @param request the incoming servlet request; must not be {@code null}
	 * @return {@code true} when the XHR request header is present
	 */
	public static boolean isAjax(@Nonnull HttpServletRequest request) {
		return "XMLHttpRequest".equals(request.getHeader("X-Requested-With"));
	}

	private static final String PRIMEFACES_IGNORE_AUTO_UPDATE = "primefaces.ignoreautoupdate";

	/**
	 * Indicates whether PrimeFaces auto-update should be suppressed for this request.
	 *
	 * @return {@code true} when the ignore-auto-update request parameter is set
	 */
	public static boolean isIgnoreAutoUpdate() {
		return Boolean.TRUE.toString().equals(FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(PRIMEFACES_IGNORE_AUTO_UPDATE));
	}

	/**
	 * Executes a client-side JavaScript redirect via PrimeFaces.
	 *
	 * @param url the redirect destination URL; must not be {@code null}
	 */
	public static void jsRedirect(@Nonnull String url) {
		PrimeFaces.current().executeScript("window.location='" + OWASP.escapeJsStringWithHtmlFormatting(url) + "'");
	}

	/**
	 * Sets the style-class attribute on a JSF component using reflection.
	 *
	 * <p>This helper exists because JSF component types do not expose a shared style-class contract through a
	 * common interface.
	 *
	 * @param component the component to update, or {@code null} to do nothing
	 * @param styleClass the style class to assign; may be {@code null}
	 * @throws DomainException when the target component type does not expose a compatible setter
	 */
	public static void setStyleCLass(@Nullable UIComponent component, @Nullable String styleClass) {
		if (component != null) {
			try {
				component.getClass().getMethod(SET_STYLE_CLASS_METHOD_NAME, String.class).invoke(component, styleClass);
			}
			catch (Exception e) {
				throw new DomainException("Cant setStyleClass() on component" + component, e);
			}
		}
	}

	/**
	 * Opens an ownership-aware mock Faces-context scope for SAIL execution.
	 *
	 * <p>If no Faces context is active, this method installs a {@link MockFacesContext} which is released when the
	 * returned scope closes. If a context is already active, the returned scope leaves it untouched.
	 *
	 * @return a scope which must be closed by the current thread
	 */
	@SuppressWarnings("resource")
	public static @Nonnull SailFacesContextScope withSailFacesContextIfNeeded() {
		MockFacesContext context = FacesContext.getCurrentInstance() == null ? MockFacesContext.get() : null;
		return new SailFacesContextScope(context);
	}

	/**
	 * Indicates whether the current thread is running with a real JSF FacesContext rather than the mock SAIL context.
	 */
	public static boolean isRealFacesContext() {
		FacesContext fc = FacesContext.getCurrentInstance();
		return ((fc != null) && (! (fc instanceof MockFacesContext)));
	}
}
