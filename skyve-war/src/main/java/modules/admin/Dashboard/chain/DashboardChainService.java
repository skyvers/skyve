package modules.admin.Dashboard.chain;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.chain.services.FavouritesService;

/**
 * Service facade that uses the Chain of Responsibility pattern to process dashboard operations.
 * This class provides a clean interface for dashboard loading and activation while
 * delegating the actual work to the processor chain.
 */
@Default
public class DashboardChainService {

	@Inject
	private FavouritesService favouritesService;
	private static final Logger LOGGER = LoggerFactory.getLogger(DashboardChainService.class);

	/**
	 * Loads a user dashboard by processing it through the appropriate chain.
	 * 
	 * @param dashboard The dashboard to load
	 * @param repository The repository to apply changes to
	 * @return true if loading was successful, false otherwise
	 */
	public boolean loadDashboard(DashboardExtension dashboard, DefaultRepository repository) {
		if (!validateInputs(dashboard, repository)) {
			return false;
		}

		try {
			// Create processing context for LOAD_DASHBOARD
			DashboardProcessingContext context = new DashboardProcessingContext(
					dashboard,
					DashboardProcessingContext.ProcessingType.LOAD_DASHBOARD,
					repository);

			// Set the module in the context if available
			populateModuleInContext(dashboard, context);
			
			// Set the favourites service in the context
			context.setFavouritesService(favouritesService);

			// Get the load dashboard processor chain
			DashboardProcessor processorChain = DashboardProcessorChainFactory.createLoadDashboardChain();

			if (processorChain == null) {
				return false;
			}

			// Process the context through the chain
			DashboardProcessingContext result = processorChain.process(context);

			// Handle results
			return handleProcessingResults(result);

		} catch (Exception e) {
			LOGGER.error("Failed to load dashboard", e);
			return false;
		}
	}

	/**
	 * Activates a dashboard by processing it through the appropriate chain.
	 * 
	 * @param dashboard The dashboard to activate
	 * @param repository The repository to apply changes to
	 * @return true if activation was successful, false otherwise
	 */
	public boolean activateDashboard(DashboardExtension dashboard, DefaultRepository repository) {
		if (!validateInputs(dashboard, repository)) {
			return false;
		}

		try {
			// Create processing context for ACTIVATE_DASHBOARD
			DashboardProcessingContext context = new DashboardProcessingContext(
					dashboard,
					DashboardProcessingContext.ProcessingType.ACTIVATE_DASHBOARD,
					repository);

			// Set the module in the context if available
			populateModuleInContext(dashboard, context);
			
			// Set the favourites service in the context
			context.setFavouritesService(favouritesService);

			// Get the activate dashboard processor chain
			DashboardProcessor processorChain = DashboardProcessorChainFactory.createActivateDashboardChain();

			if (processorChain == null) {
				return false;
			}

			// Process the context through the chain
			DashboardProcessingContext result = processorChain.process(context);

			// Handle results
			return handleActivationResults(result);

		} catch (Exception e) {
			LOGGER.error("Failed to activate dashboard", e);
			return false;
		}
	}

	/**
	 * Validates that the dashboard and repository are in a valid state for processing.
	 */
	private static boolean validateInputs(DashboardExtension dashboard, DefaultRepository repository) {
		return dashboard != null && repository != null;
	}

	/**
	 * Populates the module in the context based on the dashboard's module name.
	 */
	private static void populateModuleInContext(DashboardExtension dashboard, DashboardProcessingContext context) {
		try {
			String moduleName = dashboard.getModuleName();
			if (moduleName != null && !moduleName.trim().isEmpty()) {
				Customer customer = CORE.getCustomer();
				if (customer != null) {
					Module module = customer.getModule(moduleName);
					if (module != null) {
						context.setModule(module);
					}
				}
			}
		} catch (Exception e) {
			LOGGER.error("Could not add module to context", e);
			context.setErrorMessage("Failed to load module: " + e.getMessage());
		}
	}

	/**
	 * Handles the results of processing, including any error reporting or cleanup.
	 */
	private static boolean handleProcessingResults(DashboardProcessingContext context) {
		if (context.hasError()) {
			LOGGER.warn("Processing error: {}",context.getErrorMessage());
			return false;
		}

		if (context.isProcessed()) {
			// Mark the dashboard as loaded
			context.getDashboard().setLoaded(Boolean.TRUE);
			return true;
		}

		return false;
	}

	/**
	 * Handles the results of activation processing, including module and document creation.
	 */
	private static boolean handleActivationResults(DashboardProcessingContext context) {
		if (context.hasError()) {
			LOGGER.warn("Activation error: {}",context.getErrorMessage());
			return false;
		}

		if (context.isProcessed()) {
			// Mark the dashboard as activated if it has an indication flag
			if (context.getDashboard().isIndicateActivated()) {
				context.getDashboard().setActivated(Boolean.TRUE);
			}
			return true;
		}

		return false;
	}
}
