package modules.admin.Dashboard.chain.processors;

import static modules.admin.Dashboard.DashboardUtil.HOME_DASHBOARD;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LocalDataStoreRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.ProvidedRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.chain.AbstractDashboardProcessor;
import modules.admin.Dashboard.chain.DashboardProcessingContext;
import modules.admin.domain.Dashboard;

/**
 * Processor responsible for applying changes to the repository.
 * This is the final step in both dashboard loading and activation processes.
 */
public class RepositoryApplicationProcessor extends AbstractDashboardProcessor {

	@Override
	public boolean canHandle(DashboardProcessingContext context) {
		// Handle both load and activation scenarios as the final step
		return (context.isLoadDashboard() && context.getFluentView() != null) ||
				(context.isActivateDashboard() && context.getFluentView() != null);
	}

	@Override
	protected DashboardProcessingContext doProcess(DashboardProcessingContext context) {
		boolean success = false;

		if (context.isLoadDashboard()) {
			success = applyViewToRepository(context);
		} else if (context.isActivateDashboard()) {
			success = applyActivationToRepository(context);
		}

		context.setProcessed(success);
		if (!success) {
			context.setErrorMessage("Failed to apply changes to repository");
		}

		return context;
	}

	/**
	 * Applies the FluentView to the repository for user dashboard loading.
	 */
	private static boolean applyViewToRepository(DashboardProcessingContext context) {
		try {
			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(Dashboard.MODULE_NAME);
			Document dashboardDocument = module.getDocument(customer, Dashboard.DOCUMENT_NAME);

			// Check for existing repository with the view before putting it
			LockableDynamicRepository existingRepository = findExistingRepository(context.getRepository(), context.getDashboard());

			if (existingRepository != null) {
				// Remove the existing repository and replace it
				context.getRepository().removeDelegate(existingRepository);
			}

			LockableDynamicRepository newRepository = new LockableDynamicRepository();
			newRepository.putView(customer, dashboardDocument, context.getFluentView().get());
			context.getRepository().addDelegate(0, newRepository);

			// Reset permissions
			resetUserPermissionsAndMenu(newRepository);

			return true;

		} catch (Exception e) {
			context.setErrorMessage("Failed to apply view to repository: " + e.getMessage());
			return false;
		}
	}

	/**
	 * Applies the complete dashboard activation (view, document, module) to the repository.
	 */
	private static boolean applyActivationToRepository(DashboardProcessingContext context) {
		try {
			Customer customer = CORE.getCustomer();
			DefaultRepository repository = context.getRepository();

			// Remove old module from customer
			Module oldModule = customer.getModule(context.getFluentModule().get().getName());
			customer.getModules().remove(oldModule);

			// Check if we already have a repository for dashboards in the repository chain
			LockableDynamicRepository existingRepository = null;
			DelegatingProvidedRepositoryChain delegatingExistingRepository = null;

			try {
				// Get the delegates list from the main repository using reflection
				Field delegatesField = DelegatingProvidedRepositoryChain.class.getDeclaredField("delegates");
				delegatesField.setAccessible(true);
				@SuppressWarnings("unchecked")
				List<ProvidedRepository> delegates = (List<ProvidedRepository>) delegatesField.get(repository);

				// First try to find a repository that already has the HOME_DASHBOARD module
				for (ProvidedRepository delegate : delegates) {
					try {
						if (delegate instanceof DelegatingProvidedRepositoryChain) {
							// This could be our chain with LockableDynamicRepository
							DelegatingProvidedRepositoryChain delegatingChain = (DelegatingProvidedRepositoryChain) delegate;

							// Get the delegates of this chain
							@SuppressWarnings("unchecked")
							List<ProvidedRepository> subDelegates = (List<ProvidedRepository>) delegatesField.get(delegatingChain);

							for (ProvidedRepository subDelegate : subDelegates) {
								if (subDelegate instanceof LockableDynamicRepository) {
									LockableDynamicRepository lockableRepo = (LockableDynamicRepository) subDelegate;

									// Check if this repository contains HOME_DASHBOARD documents
									if (containsHomeDashboard(lockableRepo)) {
										delegatingExistingRepository = delegatingChain;
										existingRepository = lockableRepo;
										break;
									}
								}
							}

							if (existingRepository != null) {
								break;
							}
						}
					} catch (Exception e) {
						// Continue searching
						logError("Could not check delegate: " + delegate.toString(), e);
					}
				}
			} catch (Exception e) {
				// If reflection or search fails, continue with creating a new repository
				logError("Could not find existing repository, proceeding to create new one", e);
			}

			// Apply changes to repository
			if (delegatingExistingRepository != null && existingRepository != null) {
				// Replace existing repository
				delegatingExistingRepository.removeDelegate(existingRepository);
				LockableDynamicRepository newRepository = new LockableDynamicRepository();

				applyChangesToRepository(newRepository, context);
				delegatingExistingRepository.addDelegate(0, newRepository);
			} else {
				// Create new repository chain
				LockableDynamicRepository newRepository = new LockableDynamicRepository();
				DelegatingProvidedRepositoryChain delegator = new DelegatingProvidedRepositoryChain(
						newRepository, new LocalDataStoreRepository());

				applyChangesToRepository(newRepository, context);
				repository.addDelegate(0, delegator);
			}

			// Reset menu and permissions
			repository.resetMenus(CORE.getUser());
			repository.resetUserPermissions(CORE.getUser());

			return true;

		} catch (Exception e) {
			context.setErrorMessage("Failed to apply activation to repository: " + e.getMessage());
			logError("Repository activation failed", e);
			return false;
		}
	}

	/**
	 * Finds existing repository with the dashboard view.
	 */
	private static LockableDynamicRepository findExistingRepository(DefaultRepository repository, DashboardExtension dashboard) {

		try {
			// Get the delegates list from the main repository using reflection
			Field delegatesField = DelegatingProvidedRepositoryChain.class.getDeclaredField("delegates");
			delegatesField.setAccessible(true);
			@SuppressWarnings("unchecked")
			List<ProvidedRepository> delegates = (List<ProvidedRepository>) delegatesField.get(repository);

			// Search through all delegates for LockableDynamicRepository instances
			for (ProvidedRepository delegate : delegates) {
				try {
					if (delegate instanceof LockableDynamicRepository) {
						// Direct LockableDynamicRepository delegate
						LockableDynamicRepository lockableRepo = (LockableDynamicRepository) delegate;

						try {
							// Check cache for matching view
							Field cacheField = findFieldInHierarchy(LockableDynamicRepository.class, "cache");
							if (cacheField != null) {
								cacheField.setAccessible(true);
								@SuppressWarnings("unchecked")
								Map<String, Optional<?>> cache = (Map<String, Optional<?>>) cacheField.get(lockableRepo);

								// Look through cache entries to find views that match our target
								for (Map.Entry<String, Optional<?>> entry : cache.entrySet()) {
									if (entry.getValue().isPresent()) {
										ViewImpl cachedValue = (ViewImpl) entry.getValue().get();
										String cachedValueDocumentation = cachedValue.getDocumentation();
										// Check if this cached value equals our target view implementation
										if (cachedValueDocumentation != null
												&& cachedValueDocumentation.equals(dashboard.getBizId())) {
											return lockableRepo;
										}
									}
								}
							}
						} catch (@SuppressWarnings("unused") Exception e) {
							// Continue searching other repositories
						}
					}
				} catch (@SuppressWarnings("unused") Exception e) {
					// Something went wrong checking this delegate, continue to next
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			// If reflection or search fails, return null to indicate no existing repository found
		}

		return null; // No existing repository with matching view found
	}

	/**
	 * Resets user permissions and menu after repository changes.
	 */
	private static void resetUserPermissionsAndMenu(LockableDynamicRepository repository) {
		try {
			repository.resetUserPermissions(CORE.getUser());
		} catch (Exception e) {
			LOGGER.warn("Could not reset user permissions during while adding dashboard to repository", e);
		}
	}

	/**
	 * Checks if the repository contains HOME_DASHBOARD documents.
	 */
	private static boolean containsHomeDashboard(LockableDynamicRepository lockableRepo) {
		try {
			// Use helper method to find the cache field
			Field cacheField = findFieldInHierarchy(LockableDynamicRepository.class, "cache");
			if (cacheField != null) {
				cacheField.setAccessible(true);
				@SuppressWarnings("unchecked")
				Map<String, Optional<?>> cache = (Map<String, Optional<?>>) cacheField.get(lockableRepo);

				// Look through cache entries to find any HOME_DASHBOARD document
				for (Map.Entry<String, Optional<?>> entry : cache.entrySet()) {
					String key = entry.getKey();
					if ((key.contains("HomeDashboard") || key.endsWith("/HomeDashboard")) ||
							(key.contains("/HomeDashboard/") || key.endsWith("/HomeDashboard/"))) {
						return true;
					}
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			// Fall back to iterating through all modules
			try {
				List<String> customerNames = lockableRepo.getAllCustomerNames();
				if (customerNames != null) {
					for (String customerName : customerNames) {
						try {
							Customer repoCustomer = lockableRepo.getCustomer(customerName);
							if (repoCustomer != null) {
								for (Module repoModule : repoCustomer.getModules()) {
									try {
										Document doc = lockableRepo.getDocument(repoCustomer, repoModule, HOME_DASHBOARD);
										if (doc != null) {
											return true;
										}
									} catch (@SuppressWarnings("unused") Exception ex) {
										// Document doesn't exist in this module
									}
								}
							}
						} catch (@SuppressWarnings("unused") Exception ex) {
							// Continue to next customer
						}
					}
				}
			} catch (@SuppressWarnings("unused") Exception fallbackException) {
				// Both methods failed, assume no HOME_DASHBOARD
			}
		}
		return false;
	}

	/**
	 * Applies module, document, and view changes to the repository.
	 */
	private static void applyChangesToRepository(LockableDynamicRepository repository, DashboardProcessingContext context) {
		Customer customer = CORE.getCustomer();

		repository.withLock(r -> {
			Module newModule = r.putModule(customer, context.getFluentModule().get());
			Document newDocument = r.putDocument(newModule, context.getFluentDocument().get());
			r.putView(customer, newDocument, context.getFluentView().get());
			return null;
		});
	}

	/**
	 * Logger helper using static reference.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(RepositoryApplicationProcessor.class);

	/**
	 * Helper method for logging errors.
	 */
	private static void logError(String message, Exception e) {
		LOGGER.warn(message, e);
	}

}
