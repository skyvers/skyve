package modules.admin.Dashboard;

import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.spi.CDI;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpSession;
import modules.admin.Dashboard.chain.DashboardChainService;
import modules.admin.domain.Dashboard;

/**
 * Observer that automatically adds activated dashboards to the repository during application startup.
 * <p>
 * This ensures that any dashboards with their activated flag set to true are properly loaded
 * and available in the repository when the application starts.
 * </p>
 * 
 * @see Observer
 * @see Dashboard
 * @see DashboardExtension
 */
public class DashboardObserver implements Observer {

	private static final Logger LOGGER = LoggerFactory.getLogger(DashboardObserver.class);
	@Inject
	private transient DashboardChainService dashboardChainService;

	@SuppressWarnings("boxing")
	@Override
	public void startup(Customer customer) {
		AbstractPersistence persistence = null;
		try {
			persistence = AbstractPersistence.get();
			persistence.begin();

			// Find a candidate user context
			persistence.setUser(new SuperUser());

			// Retrieve dashboards that are activated
			DocumentQuery qDashboards = persistence.newDocumentQuery(Dashboard.MODULE_NAME, Dashboard.DOCUMENT_NAME);
			qDashboards.getFilter().addEquals(Dashboard.activatedPropertyName, Boolean.TRUE);
			List<DashboardExtension> dashboards = qDashboards.beanResults();

			if (dashboards != null && !dashboards.isEmpty()) {
				int numberOfDashboards = dashboards.size();
				int dashboardsAddedCount = 0;
				LOGGER.info("Adding {} activated dashboards to the repository", numberOfDashboards);

				dashboardChainService = (dashboardChainService != null)
						? dashboardChainService
						: CDI.current().select(DashboardChainService.class).get();

				// Fetch repository
				DefaultRepository r = (DefaultRepository) CORE.getRepository();
				for (DashboardExtension dashboard : dashboards) {
					// Activate the dashboards by adding them to the repository
					dashboardChainService.activateDashboard(dashboard, r);
					LOGGER.info("{} out of {} dashboards added to repository", dashboardsAddedCount++, numberOfDashboards);
				}
			}

		} catch (Exception e) {
			LOGGER.error("Not all active dashboards have been added to the repository", e);
		}

	}

	@Override
	public void shutdown(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void beforeBackup(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void afterBackup(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void beforeRestore(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void afterRestore(Customer customer) {
		// TODO Auto-generated method stub

	}

	@Override
	public void login(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

	@Override
	public void logout(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

}
