package org.skyve.impl.web.faces;

import java.lang.reflect.Field;

import org.primefaces.PrimeFaces;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.views.FacesView;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.FacesException;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

public class FacesUtil {
	/**
	 * Prevent instantiation
	 */
	private FacesUtil() {
		// nothing to see here
	}

	// used to place a bean temporarily in the session when a redirect is done during navigate
	public static String MANAGED_BEAN_NAME_KEY = "skyveFacesBean";
	// used to get the responsive form grid out of the view root when required
	public static String FORM_STYLES_KEY = "skyveFormStyles";

	private static final String SET_STYLE_CLASS_METHOD_NAME = "setStyleClass";

	public static FacesView getManagedBean(final String beanName) {
		FacesContext fc = FacesContext.getCurrentInstance();
		FacesView result;

		ELContext elContext = fc.getELContext();
		result = (FacesView) elContext.getELResolver().getValue(elContext, null, beanName);

		if (result == null) {
			throw new FacesException("Managed bean with name '" + beanName + 
										"' was not found. Check your faces-config.xml or @Named annotation.");
		}

		return result;
	}

	public static void set(final Object value, final String valueExpression) {
		FacesContext facesContext = FacesContext.getCurrentInstance();
		ELContext elContext = facesContext.getELContext();
		ExpressionFactory ef = facesContext.getApplication().getExpressionFactory();

		ValueExpression targetExpression = ef.createValueExpression(elContext, valueExpression, Object.class);
		targetExpression.setValue(elContext, value);
	}

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

	// Use this only when there may be no faces context (ie view has expired)
	// otherwise should use FacesContext.getCurrentInstance().getExternalContext().redirect();
	public static String xmlPartialRedirect(String url) {
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version='1.0' encoding='UTF-8'?>");
		sb.append("<partial-response><redirect url=\"").append(url.replace("&", "&amp;")).append("\"/></partial-response>");
		return sb.toString();
	}

	public static boolean isAjax(HttpServletRequest request) {
		return "XMLHttpRequest".equals(request.getHeader("X-Requested-With"));
	}

	private static final String PRIMEFACES_IGNORE_AUTO_UPDATE = "primefaces.ignoreautoupdate";

	public static boolean isIgnoreAutoUpdate() {
		return Boolean.TRUE.toString().equals(FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(PRIMEFACES_IGNORE_AUTO_UPDATE));
	}

	public static void jsRedirect(String url) {
		PrimeFaces.current().executeScript(String.format("window.location='%s'", url));
	}

	/**
	 * Uses reflection to set the style class of a UIComponent. This method is
	 * required because faces doesn't factor CSS methods in an interface.
	 * 
	 * @param component
	 * @param styleClass
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
	 * Used to set a mock SAIL Faces Context for this thread if necessary and set Skyve in "SAIL mode".
	 * SAIL mode ensures that view models are loaded without injection (runtime = false)
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
	 * Used to reset the mock SAIL Faces Context for this thread if necessary and reset "SAIL mode"
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
	 * Indicates if this is an actual faces request.
	 */
/*
	public static boolean isRealFacesContext() {
		FacesContext fc = FacesContext.getCurrentInstance();
		return ((fc != null) && (! (fc instanceof MockFacesContext)));
	}
*/
}
