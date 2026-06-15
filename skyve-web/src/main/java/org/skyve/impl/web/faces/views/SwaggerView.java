package org.skyve.impl.web.faces.views;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;

/**
 * Provides locale-dependent JSF state for {@code swagger/swagger.xhtml}.
 *
 * <p>Thread-confined: request-scoped instances are created and initialised for a
 * single JSF request.
 */
@RequestScoped
@Named("swagger")
public class SwaggerView extends LocalisableView {
	private static final long serialVersionUID = 7131633137630078153L;

	/**
	 * Initialises inherited localisation state after dependency injection.
	 *
	 * <p>Side effects: reads the current Faces request locale and Skyve user via
	 * {@link #initialise()}.
	 */
	@PostConstruct
	private void postConstruct() {
		initialise();
	}
}
