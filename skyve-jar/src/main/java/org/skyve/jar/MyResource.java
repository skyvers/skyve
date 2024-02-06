package org.skyve.jar;

import jakarta.enterprise.context.RequestScoped;
import jakarta.enterprise.inject.spi.BeanManager;
import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;

@RequestScoped
@Path("/")
public class MyResource {
	@Inject
	private BeanManager manager;
	
    @Path("/")
    @GET
    public String getManager() {
        return manager.toString();
    }
}
