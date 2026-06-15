package org.skyve.impl.web.faces;

import java.lang.reflect.Field;

import org.primefaces.PrimeFaces;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.sail.mock.MockFacesContext;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.FacesException;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Provides utility methods shared by Skyve web rendering and request handling paths.
 */
public class FacesUtil {
	/**
	 * Prevent instantiation
	 */
	private FacesUtil() {
		// nothing to see here
	}

	/**
	 * Session key used to store a temporary managed bean during redirect navigation.
	 */
	// used to place a bean temporarily in the session when a redirect is done during navigate
	public static String MANAGED_BEAN_NAME_KEY = "skyveFacesBean";

	/**
	 * View key used to cache responsive form style metadata.
	 */
	// used to get the responsive form grid out of the view root when required
	public static String FORM_STYLES_KEY = "skyveFormStyles";

	private static final String SET_STYLE_CLASS_METHOD_NAME = "setStyleClass";

	/**
	 * Resolves a named JSF-managed bean from the active EL context.
	 *
	 * @param name the EL bean name
	 * @return the resolved bean instance
	 * @throws FacesException if no bean with the supplied name is available
	 */
	public static Object getNamed(final String name) {
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
	 * @param value the value to assign
	 * @param valueExpression the target EL expression
	 */
	public static void set(final Object value, final String valueExpression) {
		FacesContext facesContext = FacesContext.getCurrentInstance();
		ELContext elContext = facesContext.getELContext();
		ExpressionFactory ef = facesContext.getApplication().getExpressionFactory();

		ValueExpression targetExpression = ef.createValueExpression(elContext, valueExpression, Object.class);
		targetExpression.setValue(elContext, value);
	}

	/**
	 * Creates a method expression bound to the active JSF EL context.
	 *
	 * @param expression the method expression text
	 * @param expectedReturnType the expected return type
	 * @param expectedParamTypes the expected parameter types
	 * @return the compiled method expression
	 */
	public static MethodExpression createMethodExpression(String expression, Class<?> expectedReturnType, Class<?>[] expectedParamTypes) {
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
	 * @param url the redirect destination
	 * @return XML partial-response text containing a redirect instruction
	 */
	public static String xmlPartialRedirect(String url) {
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version='1.0' encoding='UTF-8'?>");
		sb.append("<partial-response><redirect url=\"").append(url.replace("&", "&amp;")).append("\"/></partial-response>");
		return sb.toString();
	}

	/**
	 * Indicates whether the request originated from an XMLHttpRequest call.
	 *
	 * @param request the incoming servlet request
	 * @return {@code true} when the XHR request header is present
	 */
	public static boolean isAjax(HttpServletRequest request) {
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
	 * @param url the redirect destination URL
	 */
	public static void jsRedirect(String url) {
		PrimeFaces.current().executeScript(String.format("window.location='%s'", url));
	}

	/**
	 * Sets the style-class attribute on a JSF component using reflection.
	 *
	 * <p>This helper exists because JSF component types do not expose a shared style-class contract through a
	 * common interface.
	 *
	 * @throws DomainException when the target component type does not expose a compatible setter
	 */
	public static void setStyleCLass(UIComponent component, String styleClass) {
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
	 * Installs a mock FacesContext for the current thread when SAIL execution is running outside a real JSF request.
	 *
	 * <p>Side effects: mutates the thread-local {@link FacesContext} holder used by JSF so that SAIL view
	 * traversal can execute component-building code paths without an active servlet-driven Faces lifecycle.
	 */
	public static void setSailFacesContextIfNeeded() {
		try {
			if (FacesContext.getCurrentInstance() == null) {
				Field instanceField = FacesContext.class.getDeclaredField("instance");
				instanceField.setAccessible(true);
				@SuppressWarnings("unchecked")
				ThreadLocal<FacesContext> instance = (ThreadLocal<FacesContext>) instanceField.get(null);
				instance.set(new MockFacesContext());
			}
		}
		catch (Exception e) {
			throw new DomainException("Cannot set mock SAIL faces context", e);
		}
	}

	/**
	 * Removes the mock SAIL FacesContext from the current thread when one was installed earlier.
	 *
	 * <p>Side effects: clears the JSF thread-local {@link FacesContext} only when the active context is the
	 * mock SAIL implementation.
	 */
	public static void resetSailFacesContextIfNeeded() {
		try {
			FacesContext fc = FacesContext.getCurrentInstance();
			if (fc instanceof MockFacesContext) {
				Field instanceField = FacesContext.class.getDeclaredField("instance");
				instanceField.setAccessible(true);
				@SuppressWarnings("unchecked")
				ThreadLocal<FacesContext> instance = (ThreadLocal<FacesContext>) instanceField.get(null);
				instance.remove();
			}
		}
		catch (Exception e) {
			throw new DomainException("Cannot reset mock faces context", e);
		}
	}

	/**
	 * Indicates whether the current thread is running with a real JSF FacesContext rather than the mock SAIL context.
	 */
	public static boolean isRealFacesContext() {
		FacesContext fc = FacesContext.getCurrentInstance();
		return ((fc != null) && (! (fc instanceof MockFacesContext)));
	}
}
