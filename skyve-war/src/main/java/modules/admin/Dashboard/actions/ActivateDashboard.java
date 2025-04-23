package modules.admin.Dashboard.actions;

import org.skyve.CORE;
import org.skyve.EXT;
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
import org.skyve.metadata.module.fluent.FluentDocumentPrivilege;
import org.skyve.metadata.module.fluent.FluentEditItem;
import org.skyve.metadata.module.fluent.FluentMenu;
import org.skyve.metadata.module.fluent.FluentModule;
import org.skyve.metadata.module.fluent.FluentModuleDocument;
import org.skyve.metadata.module.fluent.FluentModuleRole;
import org.skyve.metadata.module.fluent.FluentModuleRoleDocumentAggregateAccess;
import org.skyve.metadata.module.fluent.FluentModuleRoleModelAggregateAccess;
import org.skyve.metadata.module.fluent.FluentModuleRoleSingularAccess;
import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.fluent.FluentActions;
import org.skyve.metadata.view.fluent.FluentBlurb;
import org.skyve.metadata.view.fluent.FluentChart;
import org.skyve.metadata.view.fluent.FluentComponent;
import org.skyve.metadata.view.fluent.FluentCustomAction;
import org.skyve.metadata.view.fluent.FluentHBox;
import org.skyve.metadata.view.fluent.FluentVBox;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpSession;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;
import router.UxUis;

/**
 * ServerSideAction to activate the Dashboard with the configured widgets.
 */
public class ActivateDashboard implements ServerSideAction<DashboardExtension> {
	
	// Constants
	private final Logger LOGGER = LoggerFactory.getLogger(getClass());
	private static final String FA_FA_HOME = "fa fa-home";
	private static final String HOME_DASHBOARD_PLURAL_ALIAS = "Home DashBoards";
	private static final String HOME_DASHBOARD_SINGULAR_ALIAS = "Home DashBoard";
	private static final String HOME_DASHBOARD = "HomeDashboard";

	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean, WebContext webContext) {
		Customer customer = CORE.getCustomer();

		if (Boolean.TRUE.equals(bean.getLoaded())) {
			// Add default widgets if none exist
			ensureDefaultWidgets(bean);

			// Design the edit view
			DefaultRepository repository = (DefaultRepository) CORE.getRepository();
			FluentDynamic fluentDynamic = new FluentDynamic();
			FluentView designedView = createDashboardView(bean, fluentDynamic);

			// Create a fluent document for the home dashboard
			FluentDocument fluentDocument = createHomeDashboardDocument();
			fluentDynamic.addModel("ModuleFavouritesModel", "modules.admin.Dashboard.models.ModuleFavouritesModel");
			fluentDocument.dynamic(fluentDynamic);

			// Setup the module with fluent builders
			Module module = customer.getModule(bean.getModuleName());
			FluentModule fluentModule = setupModule(module);

			// Apply the changes to the repository
			applyRepositoryChanges(repository, fluentModule, designedView, fluentDocument);

			bean.setLoaded(Boolean.TRUE);
			bean.setActivated(Boolean.TRUE);
			
			// Invalidate the session
			LOGGER.warn("INVALIDATING THE USER'S SESSION AFTER A DASHBOARD UPDATE");
			HttpSession session = EXT.getHttpServletRequest().getSession();
			session.invalidate();
		}
		//Save Dashboard
		DashboardExtension savedBean = CORE.getPersistence().save(bean);

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
	private static void addWidgetComponents(DashboardExtension bean, FluentView designedView, FluentDynamic fluentDynamic) {
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
	 */
	private static FluentModule setupModule(Module module) {

		// Set up module with role privileges
		FluentModule fluentModule = new FluentModule().from(module);

		// Configure the role with proper access permissions
		FluentModuleRole fluentModuleRole = configureModuleRole(module);

		// Configure model access permissions
		configureModelAccess(fluentModuleRole);

		fluentModule = fluentModule.removeRole(fluentModuleRole.get()
				.getName());
		fluentModule = fluentModule.addRole(fluentModuleRole);

		// Set up menu
		FluentMenu fluentMenu = createMenu(module, fluentModuleRole);
		fluentModule.menu(fluentMenu);

		// Set up module document reference
		FluentModuleDocument fluentModuleDocument = createModuleDocument(module);
		fluentModule = fluentModule.addDocument(fluentModuleDocument);

		return fluentModule;
	}

	/**
	 * Creates the HomeDashboard document configuration
	 */
	private static FluentDocument createHomeDashboardDocument() {
		FluentDocument fluentDocument = new FluentDocument().name(HOME_DASHBOARD);
		fluentDocument.singularAlias(HOME_DASHBOARD_SINGULAR_ALIAS);
		fluentDocument.pluralAlias(HOME_DASHBOARD_PLURAL_ALIAS);
		fluentDocument.bizKeyExpression(HOME_DASHBOARD)
				.iconStyleClass(FA_FA_HOME);
		return fluentDocument;
	}

	/**
	 * Creates the menu configuration
	 * 
	 * @param fluentModuleRole
	 */
	private static FluentMenu createMenu(Module module, FluentModuleRole fluentModuleRole) {
		FluentMenu fluentMenu = new FluentMenu().from(module.getMenu());

		FluentEditItem editItem = new FluentEditItem()
				.documentName(HOME_DASHBOARD)
				.name("Home Dashboard")
				.addRole(fluentModuleRole.get()
						.getName());

		fluentMenu.addEditItem(editItem);
		return fluentMenu;
	}

	/**
	 * Configures the module role with proper access permissions
	 */
	private static FluentModuleRole configureModuleRole(Module module) {
		// Create document privilege
		FluentDocumentPrivilege roleDocumentPrivilege = new FluentDocumentPrivilege()
				.documentName(HOME_DASHBOARD)
				.permission(DocumentPermission._R__C);

		// Configure module role
		FluentModuleRole fluentModuleRole = new FluentModuleRole()
				.from(module.getRole(module.getRoles()
						.get(0)
						.getName()));

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
			LockableDynamicRepository newRepository = new LockableDynamicRepository();
			DelegatingProvidedRepositoryChain delegator = new DelegatingProvidedRepositoryChain(newRepository,
					new LocalDataStoreRepository());

			LockableDynamicRepository sessionRepository = (LockableDynamicRepository) repository.getSessionRepository();

			if (sessionRepository == null) {
				throw new IllegalStateException(
						"No session repository - bean should have been set in ModuleRepositorySkyveObserver.login()");
			}

			newRepository.withLock(r -> {
				if (r.getModule(customer, finalFluentModule.get()
						.getName()) == null) {
					Module newModule = r.putModule(customer, finalFluentModule.get());
					Document newDocument = r.putDocument(newModule, fluentDocument.get());
					r.putView(customer, newDocument, designedView.get());
				}
			});

			repository.addDelegate(1, delegator);
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
}
