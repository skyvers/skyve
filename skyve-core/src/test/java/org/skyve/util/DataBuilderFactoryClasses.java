package org.skyve.util;

import org.skyve.impl.domain.AbstractBean;

/**
 * Stand-ins for the domain classes of admin.Part and admin.Widget, which extends Part, for the data
 * factory tests of {@link DataBuilderTest}. They are public so that the acme customer's
 * <code>customers.acme.modules.admin.domain.WidgetOverride</code>, which the test makes at run
 * time, can extend them.
 */
public final class DataBuilderFactoryClasses {
	private DataBuilderFactoryClasses() {
		// no instances
	}

	/** Stands in for the generated domain class of admin.Part, which admin.Widget extends. */
	public abstract static class Part extends AbstractBean {
		private static final long serialVersionUID = 1L;
		public static final String MODULE_NAME = "admin";
		public static final String DOCUMENT_NAME = "Part";
	}

	/** Stands in for the generated domain class of admin.Widget. */
	public abstract static class Widget extends Part {
		private static final long serialVersionUID = 1L;
		@SuppressWarnings("hiding")
		public static final String MODULE_NAME = "admin";
		@SuppressWarnings("hiding")
		public static final String DOCUMENT_NAME = "Widget";
	}

	/** Stands in for admin.Widget's hand-written extension. */
	public abstract static class WidgetExtension extends Widget {
		private static final long serialVersionUID = 1L;
	}

	/** Stands in for a bean class of admin.Widget that extends its extension outside a customer's domain package. */
	public abstract static class WidgetSubclass extends WidgetExtension {
		private static final long serialVersionUID = 1L;
	}
}
