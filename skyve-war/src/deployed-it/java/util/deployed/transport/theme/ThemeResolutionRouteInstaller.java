package util.deployed.transport.theme;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.router.fluent.FluentDirect;
import org.skyve.metadata.router.fluent.FluentRouter;

import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;
import jakarta.servlet.annotation.WebListener;

/** Installs the overlay-only direct route used by the deployed theme lifecycle fixture. */
@WebListener
public final class ThemeResolutionRouteInstaller implements ServletContextListener {
	@Override
	public void contextInitialized(ServletContextEvent event) {
		Router router = CORE.getRepository().getRouter();
		if (router == null) {
			throw new IllegalStateException(
					"Could not install the deployed theme lifecycle route: no router is configured");
		}
		new FluentRouter(router).addDirect(new FluentDirect()
				.path("/deployed-it/theme-lifecycle.xhtml")
				.uxui("phone"));
	}
}
