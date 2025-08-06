package modules.admin.Dashboard.actions;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.DashboardService;

/**
 * ServerSideAction to deactivate the Dashboard by removing the HomeDashboard
 * document and views.
 */
public class DeactivateDashboard implements ServerSideAction<DashboardExtension> {
	@Inject
	private transient DashboardService dashboardService;

	// Constants
	private final Logger LOGGER = LoggerFactory.getLogger(getClass());
	private static final String HOME_DASHBOARD = "HomeDashboard";

	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean, WebContext webContext) throws IOException {
		Customer customer = CORE.getCustomer();

		if (Boolean.TRUE.equals(bean.getActivated())) {
			// Find and remove the HomeDashboard document from the repository
			DefaultRepository repository = (DefaultRepository) CORE.getRepository();

			// Try to find the repository that contains the HOME_DASHBOARD document
			try {
				// Find the dynamic repository in the chain
				LockableDynamicRepository dashboardRepository = findRepositoryWithDashboard(repository);

				if (dashboardRepository != null) {
					// Use the existing repository to remove the dashboard
					dashboardRepository.withLock(r -> {
						try {
							// Get the module name from the bean
							String moduleName = bean.getModuleName();

							if (moduleName != null && !moduleName.isEmpty()) {
								// Use targeted cache eviction with specific patterns for this module's dashboard items
								evictSpecificModuleDashboardEntries(r, customer, moduleName);
								LOGGER.info("Successfully removed dashboard metadata for module: {}", moduleName);
							} else {
								LOGGER.warn("Could not remove dashboard - module name is null or empty");
							}
						} catch (Exception e) {
							LOGGER.error("Failed to remove dashboard module", e);
						}
						return null;
					});

					// Reset the menus to show changes
					repository.resetMenus(CORE.getUser());
				}
			} catch (Exception e) {
				LOGGER.error("Error while deactivating dashboard", e);
			}

			bean.setActivated(Boolean.FALSE);

			// Retrieve the MenuView and reset it so that the new menu item can be seen
			dashboardService.resetMenuView();
			LOGGER.info("Menu view reset");
		}

		// Save Dashboard
		DashboardExtension savedBean = CORE.getPersistence()
				.save(bean);

		// Refresh the page
		dashboardService.redirectToHomeUrl();

		return new ServerSideActionResult<>(savedBean);
	}

	/**
	 * Evict specific cache entries related to this module's HomeDashboard
	 * 
	 * @param repo The repository to operate on
	 * @param customer The current customer
	 * @param moduleName The module name to target for eviction
	 */
	private void evictSpecificModuleDashboardEntries(LockableDynamicRepository repo, Customer customer,
			String moduleName) {
		try {
			// Access the cache map using reflection
			java.lang.reflect.Field cacheField = findFieldInHierarchy(LockableDynamicRepository.class, "cache");
			if (cacheField != null) {
				cacheField.setAccessible(true);
				@SuppressWarnings("unchecked")
				Map<String, Optional<?>> cache = (Map<String, Optional<?>>) cacheField.get(repo);

				// Create a list of keys to remove - we can't modify the map while iterating
				List<String> keysToRemove = new ArrayList<>();

				// Find cache keys that match our specific module and HomeDashboard
				for (String key : cache.keySet()) {
					// Check if this key is specific to our module and HomeDashboard
					if ((key.contains("/" + moduleName + "/") || key.endsWith("/" + moduleName)) &&
							(key.contains("/HomeDashboard") || key.endsWith("/HomeDashboard") ||
									key.contains("/HomeDashboard/") || key.endsWith("/HomeDashboard/"))) {
						keysToRemove.add(key);
						LOGGER.debug("Marking for removal: {}", key);
					}

					// Also match module entries (module list, document list, etc.)
					if (key.contains("/modules/" + moduleName) || key.endsWith("/modules/" + moduleName) ||
							key.matches(".*/" + moduleName + "(\\..*)?")) {
						keysToRemove.add(key);
						LOGGER.debug("Marking for removal: {}", key);
					}

					// Match any menu entries for this module
					if ((key.contains("/menu/") || key.endsWith("/menu")) &&
							key.contains("/" + moduleName + "/")) {
						keysToRemove.add(key);
						LOGGER.debug("Marking for removal: {}", key);
					}
				}

				// Now remove the identified keys
				for (String key : keysToRemove) {
					cache.remove(key);
					LOGGER.debug("Removed cache entry: {}", key);
				}

				LOGGER.info("Removed {} cache entries for module {} and HomeDashboard", Integer.valueOf(keysToRemove.size()),
						moduleName);
			} else {
				LOGGER.warn("Could not find cache field in repository");
			}
		} catch (Exception e) {
			LOGGER.error("Failed to evict specific dashboard cache entries", e);
		}
	}

	/**
	 * Find the repository that contains the HOME_DASHBOARD document
	 */
	private LockableDynamicRepository findRepositoryWithDashboard(DefaultRepository repository) {
		try {
			// Get the delegates list from the main repository using reflection
			java.lang.reflect.Field delegatesField = DelegatingProvidedRepositoryChain.class
					.getDeclaredField("delegates");
			delegatesField.setAccessible(true);
			@SuppressWarnings("unchecked")
			List<ProvidedRepository> delegates = (List<ProvidedRepository>) delegatesField.get(repository);

			// First try to find a repository that already has the HOME_DASHBOARD module
			for (ProvidedRepository delegate : delegates) {
				if (delegate instanceof DelegatingProvidedRepositoryChain) {
					// This could be our chain with LockableDynamicRepository
					DelegatingProvidedRepositoryChain delegatingChain = (DelegatingProvidedRepositoryChain) delegate;

					// Get the delegates of this chain
					@SuppressWarnings("unchecked")
					List<ProvidedRepository> subDelegates = (List<ProvidedRepository>) delegatesField
							.get(delegatingChain);

					for (ProvidedRepository subDelegate : subDelegates) {
						if (subDelegate instanceof LockableDynamicRepository) {
							LockableDynamicRepository lockableRepo = (LockableDynamicRepository) subDelegate;

							// Check if this repository contains the HOME_DASHBOARD document
							try {
								// Check all modules in the repository for a HOME_DASHBOARD document
								boolean containsDashboard = checkRepositoryForDashboard(lockableRepo);

								if (containsDashboard) {
									// Found a repository with a HOME_DASHBOARD document!
									return lockableRepo;
								}
							} catch (Exception e) {
								// Continue searching
								LOGGER.debug("Error checking repository for dashboard", e);
							}
						}
					}
				}
			}
		} catch (Exception e) {
			LOGGER.error("Failed to find repository with dashboard", e);
		}

		return null;
	}

	/**
	 * Check if the repository contains a HOME_DASHBOARD document
	 */
	private boolean checkRepositoryForDashboard(LockableDynamicRepository lockableRepo) {
		try {
			// First check the cache using reflection
			java.lang.reflect.Field cacheField = findFieldInHierarchy(LockableDynamicRepository.class, "cache");
			if (cacheField != null) {
				cacheField.setAccessible(true);
				@SuppressWarnings("unchecked")
				Map<String, Optional<?>> cache = (Map<String, Optional<?>>) cacheField.get(lockableRepo);

				// Look through cache entries to find any HOME_DASHBOARD document
				for (Map.Entry<String, Optional<?>> entry : cache.entrySet()) {
					String key = entry.getKey();
					// Check if this key matches patterns like "modules/*/HomeDashboard"
					// or contains HomeDashboard directly
					if ((key.contains("/HomeDashboard") || key.endsWith("/HomeDashboard")) ||
							(key.contains("/HomeDashboard/") || key.endsWith("/HomeDashboard/"))) {
						return true;
					}
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			// Fall back to iterating through all modules
			try {
				// Get all customer names from the repository
				List<String> customerNames = lockableRepo.getAllCustomerNames();
				if (customerNames != null) {
					Customer customer = CORE.getCustomer();
					for (String customerName : customerNames) {
						try {
							if (customer.getName()
									.equals(customerName)) {
								// Check all modules for this customer
								for (org.skyve.metadata.module.Module module : customer.getModules()) {
									if (module.getDocument(customer, HOME_DASHBOARD) != null) {
										return true;
									}
								}
							}
						} catch (@SuppressWarnings("unused") Exception ex) {
							// Continue to next customer
						}
					}
				}
			} catch (Exception ex) {
				// Failed to check modules
				LOGGER.debug("Error checking modules for dashboard", ex);
			}
		}

		return false;
	}

	/**
	 * Utility method to find a field in a class hierarchy by traversing up through
	 * all superclasses
	 * 
	 * @param clazz The starting class to search from
	 * @param fieldName The name of the field to find
	 * @return The Field if found, null otherwise
	 */
	private static java.lang.reflect.Field findFieldInHierarchy(Class<?> clazz, String fieldName) {
		Class<?> currentClass = clazz;
		while (currentClass != null) {
			try {
				java.lang.reflect.Field field = currentClass.getDeclaredField(fieldName);
				return field;
			} catch (@SuppressWarnings("unused") NoSuchFieldException e) {
				currentClass = currentClass.getSuperclass();
			}
		}
		return null; // Field not found in hierarchy
	}
}