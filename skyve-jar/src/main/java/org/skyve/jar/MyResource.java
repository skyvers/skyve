package org.skyve.jar;

import jakarta.enterprise.context.RequestScoped;
import jakarta.enterprise.inject.spi.BeanManager;
import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;

/**
 * Exposes a request-scoped diagnostic resource for validating CDI integration
 * in the standalone runtime.
 *
 * <p>Threading: instances are request-scoped and therefore not shared across
 * concurrent requests.
 */
@RequestScoped
@Path("/")
public class MyResource {
	/**
	 * Resolves bean metadata for the current CDI context.
	 */
	@Inject
	private BeanManager manager;
	
    /**
     * Returns a textual representation of the active {@link BeanManager}.
     *
     * @return the bean manager description for the current request context;
     *         never {@code null} when CDI bootstrap succeeds
     */
    @Path("/")
    @GET
    public String getManager() {
        return manager.toString();
    }
}
