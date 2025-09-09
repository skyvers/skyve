package modules.admin.Dashboard.chain.processors;

import static modules.admin.Dashboard.DashboardUtil.DEFAULT_DASHBOARD_ICON;
import static modules.admin.Dashboard.DashboardUtil.HOME_DASHBOARD;
import static modules.admin.Dashboard.DashboardUtil.HOME_DASHBOARD_PLURAL_ALIAS;
import static modules.admin.Dashboard.DashboardUtil.HOME_DASHBOARD_SINGULAR_ALIAS;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.fluent.FluentAssociation;
import org.skyve.metadata.model.document.fluent.FluentCollection;
import org.skyve.metadata.model.document.fluent.FluentDocument;
import org.skyve.metadata.model.document.fluent.FluentDynamic;
import org.skyve.metadata.module.Module;
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
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.Role;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.chain.AbstractDashboardProcessor;
import modules.admin.Dashboard.chain.DashboardProcessingContext;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.DashboardTile;
import modules.admin.domain.DashboardWidget.WidgetType;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import router.UxUis;

/**
 * Processor responsible for creating FluentDocument and FluentModule for dashboard activation.
 * This step only applies to dashboard activation, not loading.
 */
public class DocumentModuleCreationProcessor extends AbstractDashboardProcessor {

	private static final String UPDATE_MY_DETAILS = "UpdateMyDetails";
	private static final String UPDATE_MY_DETAILS_ACTION_CLASSPATH = "modules.admin.Dashboard.actions.DynamicUpdateMyDetails";
	private static final String DYNAMIC_DASHBOARD_BIZLET_CLASSNAME = "modules.admin.Dashboard.DynamicDashboardBizlet";
	private static final Logger LOGGER = LoggerFactory.getLogger(DocumentModuleCreationProcessor.class);

	@Override
	public boolean canHandle(DashboardProcessingContext context) {
		// Only handle activation scenarios
		return context.isActivateDashboard();
	}

	@Override
	protected DashboardProcessingContext doProcess(DashboardProcessingContext context) {
		if (context.isActivateDashboard()) {
			// First clear any existing dashboard artifacts from the module if the dashboard is activated.
			if (context.getDashboard().isIndicateActivated()) {
				Module clearedModule = clearDashboardFromModule(context);
				if (clearedModule != null) {
					context.setModule(clearedModule);
				}
			}

			FluentDocument fluentDocument = createHomeDashboardDocument(context);
			FluentModule fluentModule = createDashboardModule(context);

			if (fluentDocument != null) {
				context.setFluentDocument(fluentDocument);
			}

			if (fluentModule != null) {
				context.setFluentModule(fluentModule);
			}
		}

		return passToNext(context);
	}

	/**
	 * Creates the HomeDashboard document configuration for activation.
	 * Based on DashboardService.createHomeDashboardDocument()
	 */
	private static FluentDocument createHomeDashboardDocument(DashboardProcessingContext context) {
		try {
			FluentDocument fluentDocument = new FluentDocument().name(HOME_DASHBOARD);
			fluentDocument.singularAlias(HOME_DASHBOARD_SINGULAR_ALIAS);
			fluentDocument.pluralAlias(HOME_DASHBOARD_PLURAL_ALIAS);
			fluentDocument.bizKeyExpression(HOME_DASHBOARD)
					.iconStyleClass(
							context.getDashboard().getDashboardIconStyleClass() != null
									? context.getDashboard().getDashboardIconStyleClass()
									: DEFAULT_DASHBOARD_ICON);

			// Add a user attribute
			FluentAssociation userAssociation = new FluentAssociation().name("user")
					.type(AssociationType.aggregation)
					.persistent(false)
					.audited(false)
					.trackChanges(false)
					.displayName("User")
					.documentName(User.DOCUMENT_NAME);
			fluentDocument.addAssociation(userAssociation);
			
			// Add a favourites attribute
			FluentCollection favouritesCollection = new FluentCollection().name("favourites")
					.type(CollectionType.aggregation)
					.persistent(false)
					.audited(false)
					.trackChanges(false)
					.displayName("Favourites")
					.documentName(DashboardTile.DOCUMENT_NAME)
					.minCardinality(0);
			fluentDocument.addCollection(favouritesCollection);
			
			// Add FluentDynamic models for dashboard widgets
			FluentDynamic fluentDynamic = new FluentDynamic();

			// Register standard dynamic models
			fluentDynamic.addModel("ModuleFavouritesModel",
					"modules.admin.Dashboard.models.ModuleFavouritesModel");
			fluentDynamic.addModel("ModuleUserActivityModel",
					"modules.admin.Dashboard.dynamicModels.DynamicModuleUserActivityModel");
			fluentDynamic.addModel("ModuleUserActivityContextModel",
					"modules.admin.Dashboard.dynamicModels.DynamicModuleUserActivityContextModel");

			// Register custom chart models for any custom charts in the dashboard
			int customChartCount = 0;
			for (DashboardWidgetExtension widget : context.getDashboard().getDashboardWidgets()) {
				if (widget.getWidgetType() == WidgetType.customChart) {
					customChartCount++;
					String customChartModelName = "CustomChartModel" + customChartCount;
					fluentDynamic.addModel(customChartModelName,
							"modules.admin.Dashboard.dynamicModels." + customChartModelName);
				}
			}

			// Add action for updating user details
			fluentDynamic.addAction(UPDATE_MY_DETAILS, UPDATE_MY_DETAILS_ACTION_CLASSPATH);

			// Add dynamc bizlet
			fluentDynamic.bizletClassName(DYNAMIC_DASHBOARD_BIZLET_CLASSNAME);

			// Add the FluentDynamic to the document
			fluentDocument.dynamic(fluentDynamic);

			return fluentDocument;

		} catch (Exception e) {
			context.setErrorMessage("Failed to create dashboard document: " + e.getMessage());
			return null;
		}
	}

	/**
	 * Creates the module configuration with document, menu, and role configurations.
	 * Based on DashboardService.setupModule()
	 */
	private static FluentModule createDashboardModule(DashboardProcessingContext context) {
		try {
			Module module = context.getModule();
			if (module == null) {
				context.setErrorMessage("No module found in context for dashboard setup");
				return null;
			}

			// Set up module with role privileges
			FluentModule fluentModule = new FluentModule().from(module);

			List<FluentModuleRole> moduleRoles = new ArrayList<>();
			for (UserRole role : context.getDashboard().getRoles()) {
				// Configure the role with proper access permissions
				FluentModuleRole fluentModuleRole = configureModuleRole(module, role);
				// Configure model access permissions
				configureModelAccess(context.getDashboard(), fluentModuleRole);
				fluentModule = fluentModule.removeRole(fluentModuleRole.get().getName());
				fluentModule = fluentModule.addRole(fluentModuleRole);
				moduleRoles.add(fluentModuleRole);
			}

			// Set up menu
			FluentMenu fluentMenu = createMenu(context.getDashboard(), module, moduleRoles);
			fluentModule.menu(fluentMenu);

			// Set up module document reference
			FluentModuleDocument fluentModuleDocument = createDashboardModuleDocument(module);
			fluentModule = fluentModule.removeDocument(HOME_DASHBOARD);
			fluentModule = fluentModule.addDocument(fluentModuleDocument);

			// Set up module document reference for user document if dashboard contains myDetails widget
			if (context.getDashboard().getDashboardWidgets().stream().anyMatch(d -> d.getWidgetType() == WidgetType.myDetails)) {
				FluentModuleDocument fluentModuleUserDocument = createModuleDocument(User.DOCUMENT_NAME, module.getName(),
						User.MODULE_NAME);
				fluentModule = fluentModule.removeDocument(User.DOCUMENT_NAME);
				fluentModule = fluentModule.addDocument(fluentModuleUserDocument);
			}
			
			// Set up module document reference for DashboardTile document if dashboard contains favourites widget
			if (context.getDashboard().getDashboardWidgets().stream().anyMatch(d -> d.getWidgetType() == WidgetType.favourites)) {
				FluentModuleDocument fluentModuleDashboardTileDocument = createModuleDocument(DashboardTile.DOCUMENT_NAME, module.getName(),
						DashboardTile.MODULE_NAME);
				fluentModule = fluentModule.removeDocument(DashboardTile.DOCUMENT_NAME);
				fluentModule = fluentModule.addDocument(fluentModuleDashboardTileDocument);
			}

			return fluentModule;

		} catch (Exception e) {
			context.setErrorMessage("Failed to create dashboard module: " + e.getMessage());
			return null;
		}
	}

	/**
	 * Creates a module document reference.
	 * 
	 * @param documentName The name of the document to create reference for
	 * @param owningModuleName The name of the module that owns this reference
	 * @param referencedModuleName The name of the module where the document actually exists
	 * @return FluentModuleDocument configured with the specified document and module references
	 */
	private static FluentModuleDocument createModuleDocument(String documentName, String owningModuleName,
			String referencedModuleName) {
		FluentModuleDocument fluentModuleDocument = new FluentModuleDocument();
		Module.DocumentRef documentRef = new Module.DocumentRef();
		documentRef.setOwningModuleName(owningModuleName);
		documentRef.setReferencedModuleName(referencedModuleName);
		documentRef.getModuleNameDotDocumentName(documentName);
		fluentModuleDocument.from(documentName, documentRef);
		return fluentModuleDocument;
	}

	/**
	 * Creates a module document reference using the module for ownership.
	 * 
	 * @param module The module to use for document ownership
	 * @return FluentModuleDocument configured for HOME_DASHBOARD in the same module
	 */
	private static FluentModuleDocument createDashboardModuleDocument(Module module) {
		return createModuleDocument(HOME_DASHBOARD, module.getName(), null);
	}

	/**
	 * Configures the module role with proper access permissions.
	 */
	private static FluentModuleRole configureModuleRole(Module module, UserRole role) {
		// Create document privilege for dashboard
		FluentDocumentPrivilege roleDocumentPrivilege = new FluentDocumentPrivilege()
				.documentName(HOME_DASHBOARD)
				.permission(DocumentPermission._R__C)
				.addActionPrivilege(UPDATE_MY_DETAILS);

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
	 * Configures model access permissions for all required models.
	 * Based on DashboardService.configureModelAccess()
	 */
	private static void configureModelAccess(DashboardExtension dashboard, FluentModuleRole fluentModuleRole) {
		// Add standard model access
		addModelAccess(fluentModuleRole, "ModuleFavouritesModel");
		addModelAccess(fluentModuleRole, "ModuleUserActivityModel");
		addModelAccess(fluentModuleRole, "ModuleUserActivityContextModel");

		// Check for added custom charts and add each of their models
		int customChartCount = 0;
		for (DashboardWidgetExtension widget : dashboard.getDashboardWidgets()) {
			if (widget.getWidgetType() == WidgetType.customChart) {
				customChartCount++;
				addModelAccess(fluentModuleRole, String.format("CustomChartModel%d", Integer.valueOf(customChartCount)));
			}
		}
	}

	/**
	 * Adds model access to the role.
	 * Based on DashboardService.addModelAccess()
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
	 * Creates the menu configuration.
	 * Based on DashboardService.createMenu()
	 */
	private static FluentMenu createMenu(DashboardExtension dashboard, Module module, List<FluentModuleRole> moduleRoles) {
		FluentMenu fluentMenu = new FluentMenu();

		FluentEditItem editItem = new FluentEditItem()
				.documentName(HOME_DASHBOARD)
				.name(dashboard.getDashboardMenuName() != null ? dashboard.getDashboardMenuName() : HOME_DASHBOARD_SINGULAR_ALIAS);

		// Add the roles to the menu item
		moduleRoles.forEach(role -> editItem.addRole(role.get().getName()));

		List<MenuItem> menuItems = module.getMenu().getItems();

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
	 * Clears all dashboard-related artifacts from the module including:
	 * - HOME_DASHBOARD document reference
	 * - Role privileges and permissions for HOME_DASHBOARD
	 * - HOME_DASHBOARD menu item
	 * 
	 * Based on DashboardService.clearDashboardFromModule()
	 * 
	 * @param context The dashboard processing context containing the module to clear
	 * @return The modified module with dashboard artifacts removed, or null if error occurred
	 */
	private static Module clearDashboardFromModule(DashboardProcessingContext context) {
		Module module = context.getModule();
		if (module == null) {
			LOGGER.warn("No module found in context to clear dashboard artifacts from");
			return null;
		}

		LOGGER.info("Clearing existing dashboard artifacts from module: {}", module.getName());

		try {
			// Create a fluent module to work with
			FluentModule fluentModule = new FluentModule().from(module);

			// 1. Remove document reference for HOME_DASHBOARD
			fluentModule = fluentModule.removeDocument(HOME_DASHBOARD);
			LOGGER.debug("Removed HOME_DASHBOARD document reference from module");

			// 2. Remove privileges and access permissions from all roles
			for (Role role : module.getRoles()) {
				FluentModuleRole fluentRole = fluentModule.findRole(role.getName());
				if (fluentRole != null) {
					// Remove document privileges
					fluentRole.removePrivilege(HOME_DASHBOARD);

					// Remove document aggregate access
					fluentRole.removeDocumentAggregateAccess(HOME_DASHBOARD);

					// Remove singular access
					fluentRole.removeSingularAccess(HOME_DASHBOARD);

					// Remove model access for dashboard-related models
					fluentRole.removeModelAggregateAccess(HOME_DASHBOARD, "ModuleFavouritesModel");
					fluentRole.removeModelAggregateAccess(HOME_DASHBOARD, "ModuleUserActivityModel");
					fluentRole.removeModelAggregateAccess(HOME_DASHBOARD, "ModuleUserActivityContextModel");

					// Remove custom chart models (these follow a pattern CustomChartModel1, CustomChartModel2, etc.)
					// We'll attempt to remove a reasonable number of them
					for (int i = 1; i <= 10; i++) {
						fluentRole.removeModelAggregateAccess(HOME_DASHBOARD, "CustomChartModel" + i);
					}

					LOGGER.debug("Removed HOME_DASHBOARD permissions from role: {}", role.getName());
				}
			}

			// 3. Remove HOME_DASHBOARD menu item
			if (module.getMenu() != null) {
				FluentMenu fluentMenu = new FluentMenu().from(module.getMenu());

				// Try to find and remove the menu item by document name
				// Check if there's an edit item with HOME_DASHBOARD document name
				FluentEditItem editItem = fluentMenu.findEditItem(HOME_DASHBOARD);
				if (editItem == null) {
					// Try to find by the dashboard singular alias name
					editItem = fluentMenu.findEditItem(HOME_DASHBOARD_SINGULAR_ALIAS);
				}

				if (editItem != null) {
					// Remove the menu action by name
					fluentMenu.removeMenuAction(editItem.get().getName());
					LOGGER.debug("Removed HOME_DASHBOARD menu item from module menu");
				} else {
					// If we can't find it specifically, try removing any edit item that references HOME_DASHBOARD
					boolean menuItemRemoved = false;
					List<MenuItem> menuItems = new ArrayList<>(module.getMenu().getItems());
					for (MenuItem item : menuItems) {
						if (item instanceof EditItem) {
							EditItem editMenuItem = (EditItem) item;
							if (HOME_DASHBOARD.equals(editMenuItem.getDocumentName()) ||
									HOME_DASHBOARD_SINGULAR_ALIAS.equals(editMenuItem.getName())) {
								fluentMenu.removeMenuAction(editMenuItem.getName());
								menuItemRemoved = true;
								LOGGER.debug("Removed HOME_DASHBOARD edit menu item: {}", editMenuItem.getName());
								break;
							}
						}
					}

					if (!menuItemRemoved) {
						LOGGER.debug("No HOME_DASHBOARD menu item found to remove");
					}
				}

				fluentModule.menu(fluentMenu);
			}
			LOGGER.info("Successfully cleared dashboard artifacts from module: {}", module.getName());

			// Return the modified module by converting the fluent module metadata back to a Module
			return fluentModule.get().convert(module.getName());

		} catch (Exception e) {
			LOGGER.warn("Error occurred while clearing dashboard artifacts from module: {}", module.getName(), e);
			// Don't throw the exception as this is a cleanup operation and shouldn't fail the main process
			// Return the original module if there was an error
			return module;
		}
	}
}
