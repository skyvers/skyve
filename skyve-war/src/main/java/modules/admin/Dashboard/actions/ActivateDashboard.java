package modules.admin.Dashboard.actions;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LocalDataStoreRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.fluent.FluentDocument;
import org.skyve.metadata.model.document.fluent.FluentDynamic;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.fluent.FluentCalendarItem;
import org.skyve.metadata.module.fluent.FluentDocumentPrivilege;
import org.skyve.metadata.module.fluent.FluentEditItem;
import org.skyve.metadata.module.fluent.FluentLinkItem;
import org.skyve.metadata.module.fluent.FluentListItem;
import org.skyve.metadata.module.fluent.FluentMapItem;
import org.skyve.metadata.module.fluent.FluentMenu;
import org.skyve.metadata.module.fluent.FluentMenuGroup;
import org.skyve.metadata.module.fluent.FluentModule;
import org.skyve.metadata.module.fluent.FluentModuleDocument;
import org.skyve.metadata.module.fluent.FluentModuleRole;
import org.skyve.metadata.module.fluent.FluentModuleRoleDocumentAggregateAccess;
import org.skyve.metadata.module.fluent.FluentModuleRoleModelAggregateAccess;
import org.skyve.metadata.module.fluent.FluentModuleRoleSingularAccess;
import org.skyve.metadata.module.fluent.FluentTreeItem;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.fluent.FluentActions;
import org.skyve.metadata.view.fluent.FluentBlurb;
import org.skyve.metadata.view.fluent.FluentChart;
import org.skyve.metadata.view.fluent.FluentComponent;
import org.skyve.metadata.view.fluent.FluentCustomAction;
import org.skyve.metadata.view.fluent.FluentHBox;
import org.skyve.metadata.view.fluent.FluentVBox;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.PushMessage;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.DashboardService;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import router.UxUis;

/**
 * ServerSideAction to activate the Dashboard with the configured widgets.
 */
public class ActivateDashboard implements ServerSideAction<DashboardExtension> {
	@Inject
	private transient DashboardService dashboardService;

	// Constants
	private final Logger LOGGER = LoggerFactory.getLogger(getClass());
	public static final String DEFAULT_DASHBOARD_ICON = "fa-solid fa-house";
	private static final String HOME_DASHBOARD_PLURAL_ALIAS = "Home DashBoards";
	public static final String HOME_DASHBOARD_SINGULAR_ALIAS = "Home DashBoard";
	private static final String HOME_DASHBOARD = "HomeDashboard";

	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean, WebContext webContext) throws IOException {
		Customer customer = CORE.getCustomer();

		if (Boolean.TRUE.equals(bean.getLoaded())) {
			// Add default widgets if none exist
			ensureDefaultWidgets(bean);

			// Design the edit view
			DefaultRepository repository = (DefaultRepository) CORE.getRepository();
			FluentDynamic fluentDynamic = new FluentDynamic();
			FluentView designedView = createDashboardView(bean, fluentDynamic);

			// Create a fluent document for the home dashboard
			FluentDocument fluentDocument = createHomeDashboardDocument(bean);
			fluentDynamic.addModel("ModuleFavouritesModel", "modules.admin.Dashboard.models.ModuleFavouritesModel");
			fluentDocument.dynamic(fluentDynamic);

			// Setup the module with fluent builders
			Module module = customer.getModule(bean.getModuleName());
			FluentModule fluentModule = setupModule(bean, module);

			// Apply the changes to the repository
			applyRepositoryChanges(repository, fluentModule, designedView, fluentDocument);

			bean.setLoaded(Boolean.TRUE);
			bean.setActivated(Boolean.TRUE);

			// Retrieve the MenuView and reset it so that the new menu item can be seen
			dashboardService.resetMenuView();
			LOGGER.info("Menu view reset");
		}
		// Save Dashboard
		DashboardExtension savedBean = CORE.getPersistence()
				.save(bean);

		// Inform all users with the roles that can access the dashboard
		DocumentQuery qUsers = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		List<UserExtension> users = qUsers.beanResults();

		for (UserExtension user : users) {
			// loop through the roles that can access the dashboard
			for (UserRole role : savedBean.getRoles()) {
				String[] roleParts = role.getRoleName().split("\\.");
				String roleName = roleParts[roleParts.length - 1];

				// Check if user is in role
				if (user.toMetaDataUser().isInRole(savedBean.getModuleName(), roleName)) {
					DefaultRepository repo = (DefaultRepository) CORE.getRepository();
					repo.resetUserPermissions(user.toMetaDataUser());
					EXT.push(new PushMessage().message(MessageSeverity.info, String.format(
							"A new dashboard has been made available for the module %s. Please log out and log back in to view the change.",
							bean.getModuleName())).user(user.toMetaDataUser()));
				}
			}
		}

		// Refresh the page
		dashboardService.redirectToHomeUrl();

		return new ServerSideActionResult<>(savedBean);
	}

	/**
	 * Ensures the dashboard has default widgets if none are configured
	 */
	private static void ensureDefaultWidgets(DashboardExtension bean) {
		if (bean.getDashboardWidgets()
				.isEmpty()) {
			// Add favourites widget
			DashboardWidgetExtension favouritesWidget = DashboardWidget.newInstance();
			favouritesWidget.setWidgetType(WidgetType.favourites);
			bean.addDashboardWidgetsElement(favouritesWidget);

			// Add system usage pie chart widget
			DashboardWidgetExtension pieChartWidget = DashboardWidget.newInstance();
			pieChartWidget.setWidgetType(WidgetType.mySystemUsageBreakdownPieChart);
			bean.addDashboardWidgetsElement(pieChartWidget);

			// Add system usage line chart widget
			DashboardWidgetExtension lineChartWidget = DashboardWidget.newInstance();
			lineChartWidget.setWidgetType(WidgetType.mySystemUsageLineChart);
			bean.addDashboardWidgetsElement(lineChartWidget);
		}
	}

	/**
	 * Creates the dashboard view with all configured widgets
	 */
	private static FluentView createDashboardView(DashboardExtension bean, FluentDynamic fluentDynamic) {
		FluentView designedView = new FluentView()
				.title("Dashboard")
				.name("edit");

		// Add the welcome banner
		FluentVBox bannerVbox = new FluentVBox()
				.addBlurb(new FluentBlurb().markup("<em>Welcome <strong>{user.userName}</strong> to Skyve</em>"))
				.shrinkWrap(ShrinkWrap.height)
				.border(true);
		designedView.addVBox(bannerVbox);

		if (bean.inDesignMode()) {
			addDesignerComponents(bean, designedView);
		} else {
			addWidgetComponents(bean, designedView, fluentDynamic);
		}

		// Add design and display actions
		addViewActions(designedView);

		return designedView;
	}

	/**
	 * Adds designer components for design mode
	 */
	private static void addDesignerComponents(DashboardExtension bean, FluentView designedView) {
		// Add dataGrid for widget selection
		FluentVBox designerVbox = new FluentVBox()
				.border(true)
				.borderTitle("Design Mode")
				.addComponent(new FluentComponent().name("_designer"));

		// Show custom chart options if applicable
		if (shouldShowCustomChartOptions(bean)) {
			FluentHBox customHbox = new FluentHBox();
			customHbox.addComponent(
					new FluentComponent()
							.name("_customChartDesign")
							.documentName(DashboardWidget.DOCUMENT_NAME)
							.binding(Dashboard.focusItemPropertyName));

			// Include the chart preview if we have enough information
			if (bean.getFocusItem()
					.sufficientInformationToPreview()) {
				FluentChart focusCustomChart = new FluentChart()
						.type(ChartType.valueOf(bean.getFocusItem()
								.getChartType()
								.toCode()))
						.modelName("FocusItemChartModel")
						.responsiveWidth(6);
				customHbox.addChart(focusCustomChart);
			} else {
				FluentBlurb moreInfoBlurb = new FluentBlurb()
						.markup("<em>Preview not yet available - set all starred items</em>");
				customHbox.addBlurb(moreInfoBlurb);
			}
			designerVbox.addHBox(customHbox);
		}

		designedView.addVBox(designerVbox);
	}

	/**
	 * Adds widget components to the view based on configured dashboard widgets
	 */
	private static void addWidgetComponents(DashboardExtension bean, FluentView designedView,
			FluentDynamic fluentDynamic) {
		// Calculate layout columns and responsive width based on widget count
		int widgetCount = bean.getDashboardWidgets()
				.size();
		LayoutSettings layoutSettings = calculateLayoutSettings(widgetCount);

		// Setup widget placement
		int widgetsPlaced = 0;
		int customChartCount = 0;
		FluentHBox widgetHBox = new FluentHBox();

		// Add all widgets
		for (DashboardWidgetExtension widget : bean.getDashboardWidgets()) {
			// Start a new row if needed
			if (widgetsPlaced >= layoutSettings.columns) {
				designedView.addHBox(widgetHBox);
				widgetHBox = new FluentHBox();
				widgetsPlaced = 0;
			}

			if (widget.getWidgetType() != null) {
				widgetsPlaced += addWidgetByType(
						widget,
						widgetHBox,
						layoutSettings.responsiveWidth,
						fluentDynamic,
						customChartCount);

				if (widget.getWidgetType() == WidgetType.customChart) {
					customChartCount++;
				}
			}
		}

		// Add the final row
		designedView.addHBox(widgetHBox);
	}

	/**
	 * Adds a widget to the layout based on its type
	 * 
	 * @return 1 if a widget was added, 0 otherwise
	 */
	private static int addWidgetByType(DashboardWidgetExtension widget,
			FluentHBox widgetHBox,
			int responsiveWidth,
			FluentDynamic fluentDynamic,
			int customChartCount) {
		FluentVBox widgetVBox = null;

		switch (widget.getWidgetType()) {
			case mySystemUsageLineChart:
				widgetVBox = new FluentVBox()
						.border(true)
						.borderTitle(WidgetType.mySystemUsageLineChart.toLocalisedDescription())
						.responsiveWidth(responsiveWidth);

				FluentChart moduleUserActivityChart = new FluentChart()
						.type(ChartType.line)
						.modelName("ModuleUserActivityModel");

				fluentDynamic.addModel("ModuleUserActivityModel",
						"modules.admin.Dashboard.dynamicModels.DynamicModuleUserActivityModel");
				widgetVBox.addChart(moduleUserActivityChart);
				widgetHBox.addVBox(widgetVBox);
				break;

			case mySystemUsageBreakdownPieChart:
				widgetVBox = new FluentVBox()
						.border(true)
						.borderTitle(WidgetType.mySystemUsageBreakdownPieChart.toLocalisedDescription())
						.responsiveWidth(responsiveWidth);

				FluentChart moduleUserActivityContextChart = new FluentChart()
						.type(ChartType.pie)
						.modelName("ModuleUserActivityContextModel");

				fluentDynamic.addModel("ModuleUserActivityContextModel",
						"modules.admin.Dashboard.dynamicModels.DynamicModuleUserActivityContextModel");

				widgetVBox.addChart(moduleUserActivityContextChart);
				widgetHBox.addVBox(widgetVBox);
				break;

			case customChart:
				widgetVBox = new FluentVBox()
						.border(true)
						.borderTitle(widget.getTitle())
						.responsiveWidth(responsiveWidth);

				String customChartModelName = "CustomChartModel" + (customChartCount + 1);

				fluentDynamic.addModel(customChartModelName,
						"modules.admin.Dashboard.dynamicModels." + customChartModelName);

				FluentChart customChart = new FluentChart()
						.type(ChartType.valueOf(widget.getChartType()
								.toCode()))
						.modelName(customChartModelName);

				widgetVBox.addChart(customChart);
				widgetHBox.addVBox(widgetVBox);
				break;

			case myDetails:
				widgetVBox = new FluentVBox()
						.border(true)
						.borderTitle(WidgetType.myDetails.toLocalisedDescription())
						.responsiveWidth(responsiveWidth);

				widgetVBox.addComponent(new FluentComponent().name("_myDetails"));
				widgetHBox.addVBox(widgetVBox);
				break;

			case myJobs:
				widgetVBox = new FluentVBox()
						.border(true)
						.borderTitle(WidgetType.myJobs.toLocalisedDescription())
						.responsiveWidth(responsiveWidth);

				widgetVBox.addComponent(new FluentComponent().name("_myJobs"));
				widgetHBox.addVBox(widgetVBox);
				break;

			case favourites:
				// Favourites widget is handled elsewhere
				return 0;

			default:
				return 0;
		}

		return 1;
	}

	/**
	 * Add update actions to the view
	 */
	private static void addViewActions(FluentView designedView) {
		FluentCustomAction updateMyDetailsAction = new FluentCustomAction()
				.className("UpdateMyDetails")
				.clientValidation(false)
				.displayName("Save")
				.iconStyleClass("fa-solid fa-floppy-disk")
				.inActionPanel(false);

		FluentActions actions = new FluentActions().addCustomAction(updateMyDetailsAction);
		designedView.actions(actions);
	}

	/**
	 * Set up the module with document, menu, and role configurations
	 * 
	 * @param dashboard
	 */
	private static FluentModule setupModule(DashboardExtension dashboard, Module module) {

		// Set up module with role privileges
		FluentModule fluentModule = new FluentModule().from(module);

		List<FluentModuleRole> moduleRoles = new ArrayList<>();
		for (UserRole role : dashboard.getRoles()) {
			// Configure the role with proper access permissions
			FluentModuleRole fluentModuleRole = configureModuleRole(module, role);
			// Configure model access permissions
			configureModelAccess(fluentModuleRole);
			fluentModule = fluentModule.removeRole(fluentModuleRole.get()
					.getName());
			fluentModule = fluentModule.addRole(fluentModuleRole);
			moduleRoles.add(fluentModuleRole);
		}

		// Set up menu
		FluentMenu fluentMenu = createMenu(dashboard, module, moduleRoles);
		fluentModule.menu(fluentMenu);

		// Set up module document reference
		FluentModuleDocument fluentModuleDocument = createModuleDocument(module);
		fluentModule = fluentModule.addDocument(fluentModuleDocument);

		return fluentModule;
	}

	/**
	 * Creates the HomeDashboard document configuration
	 * 
	 * @param dashboard
	 */
	private static FluentDocument createHomeDashboardDocument(DashboardExtension dashboard) {
		FluentDocument fluentDocument = new FluentDocument().name(HOME_DASHBOARD);
		fluentDocument.singularAlias(HOME_DASHBOARD_SINGULAR_ALIAS);
		fluentDocument.pluralAlias(HOME_DASHBOARD_PLURAL_ALIAS);
		fluentDocument.bizKeyExpression(HOME_DASHBOARD)
				.iconStyleClass(
						dashboard.getDashboardIconStyleClass() != null ? dashboard.getDashboardIconStyleClass()
								: DEFAULT_DASHBOARD_ICON);
		return fluentDocument;
	}

	/**
	 * Creates the menu configuration
	 * 
	 * @param dashboard
	 * 
	 * @param moduleRoles
	 */
	private static FluentMenu createMenu(DashboardExtension dashboard, Module module, List<FluentModuleRole> moduleRoles) {
		FluentMenu fluentMenu = new FluentMenu();

		FluentEditItem editItem = new FluentEditItem()
				.documentName(HOME_DASHBOARD)
				.name(dashboard.getDashboardMenuName() != null ? dashboard.getDashboardMenuName() : HOME_DASHBOARD_SINGULAR_ALIAS);

		// Add the roles to the menu item
		moduleRoles.forEach(role -> editItem.addRole(role.get().getName()));

		List<MenuItem> menuItems = module.getMenu()
				.getItems();

		fluentMenu.addEditItem(editItem);
		for (MenuItem item : menuItems) {
			if (item instanceof EditItem) {
				fluentMenu.addEditItem(new FluentEditItem().from((EditItem) item));
			} else if (item instanceof TreeItem) {
				fluentMenu.addTreeItem(new FluentTreeItem().from((TreeItem) item));
			} else if (item instanceof ListItem) {
				fluentMenu.addListItem(new FluentListItem().from((ListItem) item));
			} else if (item instanceof MenuGroup) {
				fluentMenu.addGroup(new FluentMenuGroup().from((MenuGroup) item));
			} else if (item instanceof MapItem) {
				fluentMenu.addMapItem(new FluentMapItem().from((MapItem) item));
			} else if (item instanceof CalendarItem) {
				fluentMenu.addCalendarItem(new FluentCalendarItem().from((CalendarItem) item));
			} else if (item instanceof LinkItem) {
				fluentMenu.addLinkItem(new FluentLinkItem().from((LinkItem) item));
			} else {
				throw new IllegalStateException(item + " not catered for");
			}
		}
		return fluentMenu;
	}

	/**
	 * Configures the module role with proper access permissions
	 * 
	 * @param role
	 */
	private static FluentModuleRole configureModuleRole(Module module, UserRole role) {
		// Create document privilege
		FluentDocumentPrivilege roleDocumentPrivilege = new FluentDocumentPrivilege()
				.documentName(HOME_DASHBOARD)
				.permission(DocumentPermission._R__C);

		// Configure module role
		String[] roleParts = role.getRoleName().split("\\.");
		String roleName = roleParts[roleParts.length - 1];
		FluentModuleRole fluentModuleRole = new FluentModuleRole()
				.from(module.getRole(roleName));

		fluentModuleRole = fluentModuleRole.addPrivilege(roleDocumentPrivilege);

		// Configure document access
		FluentModuleRoleDocumentAggregateAccess documentAccess = fluentModuleRole
				.findDocumentAggregateAccess(HOME_DASHBOARD);
		FluentModuleRoleSingularAccess singularAccess = fluentModuleRole.findSingularAccess(HOME_DASHBOARD);

		if (documentAccess == null) {
			documentAccess = new FluentModuleRoleDocumentAggregateAccess()
					.documentName(HOME_DASHBOARD);
			fluentModuleRole = fluentModuleRole.addDocumentAggregateAccess(documentAccess);
		}

		if (singularAccess == null) {
			singularAccess = new FluentModuleRoleSingularAccess()
					.documentName(HOME_DASHBOARD);
			fluentModuleRole = fluentModuleRole.addSingularAccess(singularAccess);
		}

		return fluentModuleRole;
	}

	/**
	 * Configures model access permissions for all required models
	 */
	private static void configureModelAccess(FluentModuleRole fluentModuleRole) {
		// Add standard model access
		addModelAccess(fluentModuleRole, "ModuleFavouritesModel");
		addModelAccess(fluentModuleRole, "ModuleUserActivityModel");
		addModelAccess(fluentModuleRole, "ModuleUserActivityContextModel");
	}

	/**
	 * Creates a module document reference
	 */
	private static FluentModuleDocument createModuleDocument(Module module) {
		FluentModuleDocument fluentModuleDocument = new FluentModuleDocument();
		DocumentRef documentRef = new DocumentRef();
		documentRef.setOwningModuleName(module.getName());
		documentRef.getModuleNameDotDocumentName(HOME_DASHBOARD);
		fluentModuleDocument.from(HOME_DASHBOARD, documentRef);
		return fluentModuleDocument;
	}

	/**
	 * Apply changes to the repository
	 */
	private static void applyRepositoryChanges(DefaultRepository repository,
			FluentModule finalFluentModule,
			FluentView designedView,
			FluentDocument fluentDocument) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(finalFluentModule.get()
				.getName());
		customer.getModules()
				.remove(module);

		try {
			// Check if we already have a repository for dashboards in the repository chain
			LockableDynamicRepository existingRepository = null;

			// Get the delegates list from the main repository using reflection
			try {
				java.lang.reflect.Field delegatesField = DelegatingProvidedRepositoryChain.class
						.getDeclaredField("delegates");
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
							List<ProvidedRepository> subDelegates = (List<ProvidedRepository>) delegatesField
									.get(delegatingChain);

							for (ProvidedRepository subDelegate : subDelegates) {
								if (subDelegate instanceof LockableDynamicRepository) {
									LockableDynamicRepository lockableRepo = (LockableDynamicRepository) subDelegate;

									// Check if this repository contains any module with a HOME_DASHBOARD document
									try {
										// We need to check ALL modules in the repository for a HOME_DASHBOARD document
										boolean containsDashboard = false;

										try {
											// Use our helper method to find the cache field anywhere in the hierarchy
											java.lang.reflect.Field cacheField = findFieldInHierarchy(
													LockableDynamicRepository.class, "cache");
											if (cacheField != null) {
												cacheField.setAccessible(true);
												@SuppressWarnings("unchecked")
												Map<String, Optional<?>> cache = (Map<String, Optional<?>>) cacheField
														.get(lockableRepo);

												// Look through cache entries to find any HOME_DASHBOARD document
												for (Map.Entry<String, Optional<?>> entry : cache.entrySet()) {
													String key = entry.getKey();
													// Check if this key matches patterns like "modules/*/HomeDashboard"
													// or contains HomeDashboard directly
													if ((key.contains("/HomeDashboard")
															|| key.endsWith("/HomeDashboard")) ||
															(key.contains("/HomeDashboard/")
																	|| key.endsWith("/HomeDashboard/"))) {
														containsDashboard = true;
														break;
													}
												}
											}
										} catch (@SuppressWarnings("unused") Exception e) {
											// Fall back to iterating through all modules
											// Get all customer names from the repository
											List<String> customerNames = lockableRepo.getAllCustomerNames();
											if (customerNames != null) {
												for (String customerName : customerNames) {
													try {
														Customer repoCustomer = lockableRepo.getCustomer(customerName);
														if (repoCustomer != null) {
															for (Module repoModule : repoCustomer.getModules()) {
																try {
																	// Check if this module contains a HOME_DASHBOARD
																	// document
																	Document doc = lockableRepo.getDocument(
																			repoCustomer, repoModule, HOME_DASHBOARD);
																	if (doc != null) {
																		containsDashboard = true;
																		break;
																	}
																} catch (@SuppressWarnings("unused") Exception ex) {
																	// Document doesn't exist in this module
																}
															}
														}
														if (containsDashboard)
															break;
													} catch (@SuppressWarnings("unused") Exception ex) {
														// Continue to next customer
													}
												}
											}
										}

										if (containsDashboard) {
											// Found a repository with a HOME_DASHBOARD document!
											existingRepository = lockableRepo;
											break;
										}
									} catch (@SuppressWarnings("unused") Exception e) {
										// Continue searching
									}
								}
							}

							if (existingRepository != null) {
								break;
							}
						}
					} catch (Exception e) {
						// Something went wrong checking this delegate, continue to next
						e.printStackTrace();
					}
				}
			} catch (Exception e) {
				// If reflection or search fails, just continue with creating a new repository
				e.printStackTrace();
			}

			// If no existing repository was found, create a new one and set up the chain
			if (existingRepository == null) {
				LockableDynamicRepository newRepository = new LockableDynamicRepository();
				DelegatingProvidedRepositoryChain delegator = new DelegatingProvidedRepositoryChain(newRepository,
						new LocalDataStoreRepository());

				// Work with the newly created repository
				newRepository.withLock(r -> {
					Module newModule = r.putModule(customer, finalFluentModule.get());
					Document newDocument = r.putDocument(newModule, fluentDocument.get());
					r.putView(customer, newDocument, designedView.get());
					return null;
				});

				repository.addDelegate(1, delegator);
			} else {
				// Use the existing repository
				existingRepository.withLock(r -> {
					try {
						// Simply put the new module, which will replace any existing one with the same
						// name
						Module newModule = r.putModule(customer, finalFluentModule.get());
						Document newDocument = r.putDocument(newModule, fluentDocument.get());
						r.putView(customer, newDocument, designedView.get());
					} catch (Exception e) {
						e.printStackTrace();
					}
					return null;
				});
			}

			repository.resetMenus(CORE.getUser());
		} catch (Exception e) {
			// Revert to vanilla view if there's an error
			e.printStackTrace();
		}
	}

	/**
	 * Adds model access to the role
	 */
	private static void addModelAccess(FluentModuleRole fluentModuleRole, String modelName) {
		FluentModuleRoleModelAggregateAccess roleModelAccess = fluentModuleRole.findModelAggregateAccess(HOME_DASHBOARD,
				modelName);

		if (roleModelAccess == null) {
			roleModelAccess = new FluentModuleRoleModelAggregateAccess()
					.documentName(HOME_DASHBOARD)
					.modelName(modelName);
			roleModelAccess.addUxUi(UxUis.EXTERNAL.getName());
			fluentModuleRole.addModelAggregateAccess(roleModelAccess);
		}
	}

	/**
	 * Calculate layout columns and responsive width based on widget count
	 */
	private static LayoutSettings calculateLayoutSettings(int widgetCount) {
		LayoutSettings settings = new LayoutSettings();

		if (widgetCount > 4) {
			settings.columns = 3;
			settings.responsiveWidth = 4;
		} else if (widgetCount > 2) {
			settings.columns = 2;
			settings.responsiveWidth = 6;
		} else {
			settings.columns = 1;
			settings.responsiveWidth = 12;
		}

		return settings;
	}

	/**
	 * Check if custom chart options should be shown
	 */
	private static boolean shouldShowCustomChartOptions(DashboardExtension bean) {
		return bean.getFocusItem() != null && bean.getFocusItem()
				.isShowCustomChartOptions();
	}

	/**
	 * Simple class to encapsulate layout settings
	 */
	private static class LayoutSettings {
		public int columns = 1;
		public int responsiveWidth = 12;
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
