package modules.admin.Dashboard;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.fluent.FluentActions;
import org.skyve.metadata.view.fluent.FluentBlurb;
import org.skyve.metadata.view.fluent.FluentChart;
import org.skyve.metadata.view.fluent.FluentComponent;
import org.skyve.metadata.view.fluent.FluentCustomAction;
import org.skyve.metadata.view.fluent.FluentDefaultsAction;
import org.skyve.metadata.view.fluent.FluentHBox;
import org.skyve.metadata.view.fluent.FluentListRepeater;
import org.skyve.metadata.view.fluent.FluentVBox;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.metadata.view.model.chart.ChartBuilder;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.ModulesUtil;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.User.UserExtension;
import modules.admin.UserDashboard.Tile;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardTile;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;
import modules.admin.domain.UserRole;

public class DashboardExtension extends Dashboard {
	private static final long serialVersionUID = -1522971002459761943L;

	private static final String DEFAULT_ICON_CLASS = "fa fa-file-o";
	private static final int TILE_COUNT_LIMIT = 6;
	private final Set<Tile> tiles = new HashSet<>();

	// used for 14 day dashboard calculations
	public static final Long TWO_WEEKS_AGO = Long.valueOf(System.currentTimeMillis() - 1209600000L);

	@Inject
	private transient Persistence persistence;
	/*
	 * @Inject
	 * private transient ConversationService conversationService;
	 */

	/**
	 * Loads designed dashboard elements by creating a fluid edit view and replacing
	 * the static edit view
	 */
	public void loadDashboard() {
		if (!Boolean.TRUE.equals(this.getLoaded())) {
			// add a default widget
			if (this.getDashboardWidgets()
					.isEmpty()) {
				DashboardWidgetExtension d1 = DashboardWidget.newInstance();
				d1.setWidgetType(WidgetType.favourites);
				this.addDashboardWidgetsElement(d1);

				DashboardWidgetExtension d2 = DashboardWidget.newInstance();
				d2.setWidgetType(WidgetType.mySystemUsageBreakdownPieChart);
				this.addDashboardWidgetsElement(d2);

				DashboardWidgetExtension d3 = DashboardWidget.newInstance();
				d3.setWidgetType(WidgetType.mySystemUsageLineChart);
				this.addDashboardWidgetsElement(d3);
			}
			
			// Fetch repository
			DefaultRepository r = (DefaultRepository) CORE.getRepository();
			LockableDynamicRepository repository = new LockableDynamicRepository();
			r.addDelegate(0, repository);
			
			// design the edit view
			FluentView designedView = new FluentView().title("Dashboard")
					.name("edit");

			// add the welcome banner component
			FluentVBox bannerVbox = new FluentVBox().addComponent(new FluentComponent().name("_welcomeBanner"))
					.shrinkWrap(ShrinkWrap.height);
			designedView.addVBox(bannerVbox);

			if (this.inDesignMode()) {
				// add dataGrid for widget selection
				FluentVBox designerVbox = new FluentVBox().border(true)
						.borderTitle("Design Mode")
						.addComponent(new FluentComponent().name("_designer"));

				if (this.getFocusItem() != null && this.getFocusItem()
						.isShowCustomChartOptions()) {
					// show a hbox with the custom chart design and the custom chart
					FluentHBox customHbox = new FluentHBox();
					customHbox.addComponent(
							new FluentComponent().name("_customChartDesign")
									.documentName(DashboardWidget.DOCUMENT_NAME)
									.binding(Dashboard.focusItemPropertyName));

					// include the chart if there is enough information
					if (this.getFocusItem()
							.sufficientInformationToPreview()) {
						FluentChart focusCustomChart = new FluentChart()
								.type(ChartType.valueOf(this.getFocusItem()
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
			} else {

				// work out layout
				int responsiveWidth = 12;
				int cols = 1;
				if (this.getDashboardWidgets()
						.size() > 4) {
					cols = 3;
					responsiveWidth = 4;
				} else if (this.getDashboardWidgets()
						.size() > 2) {
					cols = 2;
					responsiveWidth = 6;
				}

				// load all widgets
				int widgetsPlaced = 0;
				int customChartCount = 0;
				FluentHBox widgetHBox = new FluentHBox();
				FluentChart customChart = null;
				StringBuilder customChartModelName = null;
				FluentVBox widgetVBox = null;

				for (DashboardWidgetExtension w : this.getDashboardWidgets()) {

					if (widgetsPlaced < cols) {
						// add to current HBox
					} else {
						// add the preceding HBox and start a new one
						designedView.addHBox(widgetHBox);
						widgetHBox = new FluentHBox();
						widgetsPlaced = 0;
					}

					if (w.getWidgetType() != null) {
						switch (w.getWidgetType()) {
							case mySystemUsageLineChart:
								widgetVBox = new FluentVBox().border(true)
										.borderTitle(WidgetType.mySystemUsageLineChart.toLocalisedDescription())
										.responsiveWidth(responsiveWidth);
								FluentChart moduleUserActivityChart = new FluentChart().type(ChartType.line)
										.modelName("ModuleUserActivityModel");
								widgetVBox.addChart(moduleUserActivityChart);
								widgetHBox.addVBox(widgetVBox);
								break;

							case mySystemUsageBreakdownPieChart:
								widgetVBox = new FluentVBox().border(true)
										.borderTitle(WidgetType.mySystemUsageBreakdownPieChart.toLocalisedDescription())
										.responsiveWidth(responsiveWidth);
								FluentChart moduleUserActivityContextChart = new FluentChart().type(ChartType.pie)
										.modelName("ModuleUserActivityContextModel");
								widgetVBox.addChart(moduleUserActivityContextChart);
								widgetHBox.addVBox(widgetVBox);
								break;

							case customChart:
								widgetVBox = new FluentVBox().border(true)
										.borderTitle(w.getTitle())
										.responsiveWidth(responsiveWidth);
								customChartModelName = new StringBuilder(64);
								customChartModelName.append("CustomChartModel")
										.append(++customChartCount);
								customChart = new FluentChart().type(ChartType.valueOf(w.getChartType()
										.toCode()))
										.modelName(customChartModelName.toString());
								widgetVBox.addChart(customChart);
								widgetHBox.addVBox(widgetVBox);
								break;

							case favourites:
								super.getFavourites().clear();
								createFavourites();
								FluentVBox favVBox = new FluentVBox().border(true)
										.borderTitle("Favourites")
										.responsiveWidth(responsiveWidth);
								FluentListRepeater favouritesRepeater = new FluentListRepeater()
										.modelName("ModuleFavouritesModel");
								favVBox.addListRepeater(favouritesRepeater);
								widgetHBox.addVBox(favVBox);
								break;
							case myDetails:
								widgetVBox = new FluentVBox().border(true)
										.borderTitle(WidgetType.myDetails.toLocalisedDescription())
										.responsiveWidth(responsiveWidth);
								widgetVBox.addComponent(
										new FluentComponent().name("_myDetails"));
								widgetHBox.addVBox(widgetVBox);
								break;
							case myJobs:
								widgetVBox = new FluentVBox().border(true)
										.borderTitle(WidgetType.myJobs.toLocalisedDescription())
										.responsiveWidth(responsiveWidth);
								widgetVBox.addComponent(
										new FluentComponent().name("_myJobs"));
								widgetHBox.addVBox(widgetVBox);
								break;

							default:
								break;
						}
					}

					widgetsPlaced++;
				}
				designedView.addHBox(widgetHBox);
				widgetHBox = new FluentHBox();

			}

			// add design and display actions
			FluentCustomAction switchToDesignAction = new FluentCustomAction().className("SwitchDashboardMode")
					.name("SwitchToDesign")
					.clientValidation(false)
					.displayName("Design")
					.iconStyleClass("fa fa-paintbrush")
					.invisibleConditionName("inDesignMode")
					.inActionPanel(false);
			FluentCustomAction switchToDisplayAction = new FluentCustomAction().className("SwitchDashboardMode")
					.name("SwitchToDisplay")
					.clientValidation(false)
					.displayName("Display")
					.iconStyleClass("fa fa-eye")
					.invisibleConditionName("notInDesignMode")
					.inActionPanel(false);
			FluentCustomAction goToSelectorAction = new FluentCustomAction().className("GoToSelector")
					.clientValidation(false)
					.displayName("Done")
					.iconStyleClass("fa fa-thumbs-up")
					.invisibleConditionName("notShowGoToSelector")
					.inActionPanel(false);
			FluentCustomAction addWidgetAction = new FluentCustomAction().className("AddWidget")
					.invisibleConditionName("notShowAddWidgetAction")
					.clientValidation(false)
					.displayName("Add")
					.iconStyleClass("fa fa-plus")
					.inActionPanel(false);
			FluentCustomAction updateMyDetailsAction = new FluentCustomAction().className("UpdateMyDetails")
					.clientValidation(false)
					.displayName("Save")
					.iconStyleClass("fa-solid fa-floppy-disk")
					.inActionPanel(false);

			FluentCustomAction activateDashboardAction;
			FluentCustomAction deactivateDashboardAction;

			if (isIndicateActivated()) {
				activateDashboardAction = new FluentCustomAction().className("ActivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Updating the dashboard will add it to other users' menus. Do you wish to continue?")
						.displayName("Update Dashboard")
						.iconStyleClass("fa-solid fa-rotate-right")
						.inActionPanel(true);

				deactivateDashboardAction = new FluentCustomAction().className("DeactivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Deactivating the dashboard will remove it from other users' menus. Do you wish to continue?")
						.displayName("Remove Dashboard")
						.iconStyleClass("fa-solid fa-toggle-off")
						.inActionPanel(true);
			} else {
				activateDashboardAction = new FluentCustomAction().className("ActivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Activating the dashboard will add it to other users' menus. Do you wish to continue?")
						.displayName("Activate Dashboard")
						.iconStyleClass("fa-solid fa-toggle-on")
						.inActionPanel(true);

				deactivateDashboardAction = new FluentCustomAction().className("DeactivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Deactivating the dashboard will remove it from other users' menus. Do you wish to continue?")
						.displayName("Remove Dashboard")
						.iconStyleClass("fa-solid fa-toggle-off")
						.inActionPanel(true)
						.invisibleConditionName("notIndicateActivated");
			}

			FluentDefaultsAction defaultsAction = new FluentDefaultsAction();
			FluentActions actions = new FluentActions().addCustomAction(switchToDesignAction)
					.addCustomAction(switchToDisplayAction)
					.addCustomAction(goToSelectorAction)
					.addCustomAction(addWidgetAction)
					.addCustomAction(updateMyDetailsAction)
					.addCustomAction(activateDashboardAction)
					.addCustomAction(deactivateDashboardAction)
					.addDefaultsAction(defaultsAction);
			designedView.actions(actions);

			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(Dashboard.MODULE_NAME);
			Document dashboardDocument = module.getDocument(customer, Dashboard.DOCUMENT_NAME);
			
			// Put the View in the repository
			repository.putView(CORE.getCustomer(), dashboardDocument, designedView.get());
			// Reset permissions
			repository.resetUserPermissions(CORE.getUser());
			this.setLoaded(Boolean.TRUE);
		}
	}

	/**
	 * Condition for whether the user is currently designing their Dashboard
	 * dashboard
	 * 
	 * @return
	 */
	public boolean inDesignMode() {
		return Boolean.TRUE.equals(this.getDesignMode());
	}

	/**
	 * Show the custom chart design options if a custom chart is selected
	 * 
	 * @return
	 */
	public boolean showCustomChartOptions() {
		return this.getFocusItem() != null && this.getFocusItem()
				.isShowCustomChartOptions();
	}

	/**
	 * Generic chart model for custom charts
	 * The user may create up to 9 custom charts on their Dashboard page
	 * The model responds to the specific widget and applies the settings of that
	 * widget
	 * in the creation of the chart data.
	 * 0
	 * 
	 * @param widget
	 * @return
	 */
	public static ChartData customChartModel(DashboardWidgetExtension widget) {
		if (widget != null && widget.getModuleEntity() != null && widget.getDashboardModule() != null) {
			DocumentQuery q = CORE.getPersistence()
					.newDocumentQuery(widget.getDashboardModule(), widget.getModuleEntity());

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);

			cb.category(widget.getCategoryBinding());
			// cb.category(widget.getCategoryBinding(), new
			// TemporalBucket(TemporalBucketType.dayMonthYear));
			if (widget.getAggregateFunction() != null) {
				cb.value(widget.getValueBinding(), AggregateFunction.valueOf(widget.getAggregateFunction()
						.toCode()));
			} else {
				cb.value(widget.getValueBinding());
			}
			if (widget.getColTop() != null) {
				cb.top(widget.getColTop()
						.intValue(),
						OrderBy.valueOf(widget.getTopOrderBy()
								.toCode()),
						SortDirection.valueOf(widget.getTopSortDirection()
								.toCode()),
						Boolean.TRUE.equals(widget.getIncludeOthers()));
			}
			if (widget.getOrderBy() != null) {
				cb.orderBy(OrderBy.valueOf(widget.getOrderBy()
						.toCode()),
						SortDirection.valueOf(widget.getSortDirection()
								.toCode()));
			}

			ChartData chartData = cb.build(widget.getTitle(), widget.getDataLabel());
			return chartData;
		}
		return null;
	}

	/**
	 * Locate a widget in the collection based on the widget number
	 * 
	 * The widget number relates to the custom items in the selected widgets
	 * 
	 * @param widgetNumber
	 * @return
	 */
	public DashboardWidgetExtension findWidget(int widgetNumber) {
		DashboardWidgetExtension result = null;
		// find the widget that corresponds to this model
		int countFound = 0;
		for (DashboardWidgetExtension w : this.getDashboardWidgets()) {
			if (WidgetType.customChart.equals(w.getWidgetType()) && ++countFound == widgetNumber) {
				return w;
			}
		}
		return result;
	}

	/**
	 * Whether to show the button to add an item to the collection
	 * AddWidget should only show if the focus item is not already in the collection
	 * 
	 * @return
	 */
	public boolean showAddWidgetAction() {
		if (this.getFocusItem() != null) {
			if (this.getFocusItem()
					.getWidgetType() == null) {
				return false;
			}
			// check if it is already in the collection
			return !this.getDashboardWidgets()
					.contains(this.getFocusItem());
		}
		return false;
	}

	/**
	 * Whether to show the button to go back to the widget selector page
	 * This should only show if the item is already in the collection
	 * 
	 * @return
	 */
	public boolean showGoToSelector() {
		if (this.getFocusItem() != null) {
			// check if it is already in the collection
			return this.getDashboardWidgets()
					.contains(this.getFocusItem());
		}
		return false;
	}

	/**
	 * Create markup for shortcut links to favourite actions for this user
	 * 
	 * @return The HTML markup for the favourites
	 */
	private void createFavourites() {

		UserExtension currentUser = ModulesUtil.currentAdminUser();

		// temporarily elevate user permissions to view Audit records
		this.persistence.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			// favourites for the most common record saved by me (which hasn't been deleted)
			if (this.tiles.size() < TILE_COUNT_LIMIT) {
				createTilesCommon(popularUpdates(currentUser), Operation.update, 1, "Popular by me");
			}

			// favourite for the most recent record saved by me (which hasn't been deleted)
			if (this.tiles.size() < TILE_COUNT_LIMIT) {
				createTilesRecent(recentUpdates(currentUser), Operation.update, 1, "Recent by me");
			}

			if (this.tiles.size() < TILE_COUNT_LIMIT) {
				createTilesRecent(recentInsertDocuments(currentUser), Operation.insert, 1, "Recently created");
			}

			if (this.tiles.size() < TILE_COUNT_LIMIT) {
				// add favourites to Dashboard documents for all modules the user has access to
				Customer customer = this.persistence.getUser()
						.getCustomer();
				Module module = customer.getModule(Dashboard.MODULE_NAME);
				// check if user has access to the Dashboard document
				Document document = module.getDocument(customer, module.getHomeDocumentName());
				if (ViewType.list.equals(module.getHomeRef())) {
					if (CORE.getUser()
							.canCreateDocument(document)) {
						String reason = "Suggested for creation";
						addTile(createTile(Operation.insert, module.getName(), module.getHomeDocumentName(), null,
								reason));
					}
				} else {
					// exclude user dashboard - we are already here
					if (!Dashboard.DOCUMENT_NAME.equals(document.getName()) && CORE.getUser()
							.canAccessDocument(document)) {
						String reason = "Suggested for viewing";
						addTile(createTile(Operation.update, module.getName(), module.getHomeDocumentName(), null,
								reason));
					}
				}
			}
		});

		// render the tiles for display
		for (Tile tile : this.tiles) {
			DashboardTile g = DashboardTile.newInstance();
			g.setTileMarkup(tile.toMarkup());
			super.getFavourites().add(g);
		}
	}

	/**
	 * Records most popularly updated by the filter user
	 * 
	 * @return
	 */
	private List<Bean> popularUpdates(UserExtension filterUser) {

		DocumentQuery q = this.persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter()
				.addEquals(Audit.auditModuleNamePropertyName, Dashboard.MODULE_NAME); // filter for this module only

		q.getFilter()
				.addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter()
				.addNotEquals(Audit.operationPropertyName, Operation.delete);
		if (filterUser != null) {
			q.getFilter()
					.addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundGrouping(Audit.auditModuleNamePropertyName);
		q.addBoundGrouping(Audit.auditDocumentNamePropertyName);
		q.addBoundGrouping(Audit.auditBizIdPropertyName);

		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "Score");

		q.addExpressionOrdering("4", SortDirection.descending); // sort by the 4th column
		q.setMaxResults(10);

		return q.projectedResults();
	}

	/**
	 * Documents most recently created by the filter user
	 * 
	 * @param filterUser
	 * @return
	 */
	private List<Bean> recentInsertDocuments(UserExtension filterUser) {
		DocumentQuery q = this.persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter()
				.addEquals(Audit.auditModuleNamePropertyName, Dashboard.MODULE_NAME); // filter for this module only

		q.getFilter()
				.addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter()
				.addEquals(Audit.operationPropertyName, Operation.insert);
		q.getFilter()
				.addNotEquals(Audit.auditModuleNamePropertyName, Audit.MODULE_NAME);
		if (filterUser != null) {
			q.getFilter()
					.addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundProjection(Audit.timestampPropertyName);
		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundOrdering(Audit.timestampPropertyName, SortDirection.descending);
		q.setMaxResults(10);

		return q.projectedResults();
	}

	/**
	 * Construct a list of tile shortcuts to perform the operation on the audited
	 * beans
	 * 
	 * @param audits
	 * @param operation
	 * @param top
	 * @param reason
	 */
	private void createTilesCommon(List<Bean> audits, Operation operation, int top, String reason) {

		try {
			int count = 0;
			for (Bean audit : audits) {
				String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
				String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(moduleName);
				Document document = module.getDocument(customer, documentName);

				if (CORE.getUser()
						.canAccessDocument(document)) {
					String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
					if (id != null) {
						Bean exists = this.persistence.retrieve(moduleName, documentName, id);
						if (exists != null) {
							boolean added = addTile(
									createTile(Operation.update, moduleName, documentName, exists, reason));
							if (added) {
								count++;
							}
						}
					}
				}
				if (count == top) {
					break;
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			LOGGER.warn("Failed to create " + reason + " tile.");

		}
	}

	/**
	 * When two actions happen at a similar timestamp, the latest will be the most
	 * senior
	 * 
	 * @param audits
	 * @param operation
	 */
	private void createTilesRecent(List<Bean> audits, Operation operation, int top, String reason) {

		int count = 0;
		Set<String> documents = new HashSet<>();
		Timestamp lastTime = null;
		for (Bean audit : audits) {
			Timestamp timestamp = (Timestamp) Binder.get(audit, Audit.timestampPropertyName);
			String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
			String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
			if (checkModuleDocumentCanBeRead(moduleName, documentName)) {
				if (Operation.update.equals(operation)) {
					String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
					Bean exists = this.persistence.retrieve(moduleName, documentName, id);
					if (exists != null) {
						if ((lastTime == null || lastTime.before(timestamp))
								&& !documents.contains(documentName)) {
							boolean added = addTile(createTile(operation, moduleName, documentName, exists, reason));
							lastTime = timestamp;
							if (added) {
								count++;
							}
						}
					}
				} else {
					if ((lastTime == null || lastTime.before(timestamp))
							&& !documents.contains(documentName)) {
						boolean added = addTile(createTile(operation, moduleName, documentName, null, reason));
						lastTime = timestamp;
						if (added) {
							count++;
						}
					}
				}
				if (count == top) {
					break;
				}
			}
		}
	}

	/**
	 * Adds a tile to the working set of tiles if it hasn't already been
	 * added, and the tile limit has not already been reached.
	 * 
	 * @param tile The {@link Tile} to add
	 * @return True if a tile was added
	 */
	private boolean addTile(final Tile tile) {
		if (tile == null) {
			return false;
		}
		int size = this.tiles.size();
		if (this.tiles.size() < TILE_COUNT_LIMIT) {
			this.tiles.add(tile);
		}

		return size != this.tiles.size();
	}

	/**
	 * create a clickable tile markup for the action
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param reason
	 * @param action
	 * @return
	 */
	private static Tile createTile(Operation operation, String moduleName, String documentName, Bean bean,
			String reason) {

		if (!checkModuleDocumentCanBeRead(moduleName, documentName)) {
			return null;
		}

		if (bean != null
				&& !CORE.getUser()
						.canReadBean(bean.getBizId(), bean.getBizModule(), bean.getBizDocument(), bean.getBizCustomer(),
								bean.getBizDataGroupId(), bean.getBizUserId())) {
			return null;
		}

		StringBuilder link = new StringBuilder();
		link.append(Util.getHomeUrl());
		link.append("?a=e&m=")
				.append(moduleName)
				.append("&d=")
				.append(documentName);
		if (bean != null) {
			link.append("&i=")
					.append(bean.getBizId());
		}

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		String action;
		String actionClass = null;
		String iconClass = (document.getIconStyleClass() == null ? DEFAULT_ICON_CLASS : document.getIconStyleClass());
		String singularAlias = document.getLocalisedSingularAlias();

		Tile.Operation tileOperation = Tile.Operation.view;

		switch (operation) {
			case delete:
				action = "Delete ";
				actionClass = "fa-times";
				tileOperation = Tile.Operation.delete;

				// clear the link if the user does not have delete permission
				if (!user.canDeleteDocument(document)) {
					link.setLength(0);
				}
				break;
			case insert:
				action = "Create a new ";
				actionClass = "fa-plus";
				tileOperation = Tile.Operation.insert;

				// clear the link if the user does not have create permission
				if (!user.canCreateDocument(document)) {
					link.setLength(0);
				}
				break;
			case update:
				// check if the document is persistent for "view" or "edit"
				if (document.getPersistent() == null) {
					action = "View ";
					actionClass = "fa-chevron-right";

					// clear the link if the user does not have read permission
					if (!user.canReadDocument(document)) {
						link.setLength(0);
					}
				} else {
					action = operation.toLocalisedDescription();
					actionClass = "fa-angle-up";
					tileOperation = Tile.Operation.update;

					// clear the link if the user does not have update permission
					if (!user.canUpdateDocument(document)) {
						link.setLength(0);
					}
				}
				break;
			default:
				action = operation.toLocalisedDescription();
				actionClass = "fa-chevron-right";

				// clear the link if the user does not have read permission
				if (!user.canReadDocument(document)) {
					link.setLength(0);
				}
		}

		// set the document icon
		String icon = String.format(""
				+ "<span class='icon'>"
				+ "  <i class='%1$s' style=\"font-size:24px;\"></i>"
				+ "</span>", iconClass);

		if (bean != null) {
			// provide a thumbnail for the first image or content attribute type
			for (Attribute a : document.getAllAttributes(customer)) {
				if (AttributeType.content.equals(a.getAttributeType())
						|| AttributeType.image.equals(a.getAttributeType())) {
					String cId = (String) Binder.get(bean, a.getName());
					if (cId != null) {
						String imgSrc = "content?_n=" + cId + "&_doc=" + moduleName + "." + documentName + "&_b="
								+ a.getName()
								+ "&_w=24&_h=24";
						icon = String.format("<span class='icon'>"
								+ "  <img src='%1$s'/>"
								+ "</span>", imgSrc);
					}
					break;
				}
			}
		}

		StringBuilder tileText = new StringBuilder();
		tileText.append(action)
				.append(" ")
				.append(singularAlias);
		if (bean != null && bean.getBizKey() != null) {
			tileText.append(" - ")
					.append(OWASP.sanitise(Sanitisation.relaxed, bean.getBizKey()));
		}

		Tile tile = new Tile.Builder().action(action)
				.actionClass(actionClass)
				.icon(icon)
				.link(link.toString())
				.operation(tileOperation)
				.reason(reason)
				.title(tileText.toString())
				.build();

		return tile;
	}

	/**
	 * Since we are generating favourites from the audit history, it could be the
	 * case that:
	 * - the referenced module no longer exists, or can no longer be accessed by the
	 * user
	 * - the referenced document no longer exists, or can no longer be accessed by
	 * the user
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	private static boolean checkModuleDocumentCanBeRead(String moduleName, String documentName) {
		Customer customer = CORE.getCustomer();
		Module module = null;
		Document document = null;
		boolean found = false;
		for (Module modl : customer.getModules()) {
			if (modl.getName()
					.equals(moduleName)) {
				for (String docName : modl.getDocumentRefs()
						.keySet()) {
					if (docName.equals(documentName)) {
						module = customer.getModule(moduleName);
						document = module.getDocument(customer, documentName);
						found = CORE.getUser()
								.canReadDocument(document);
						break;
					}
				}
				break;
			}
		}
		return found;
	}

	/**
	 * Queries the 20 most recently updated audit records, filtered by the specified
	 * user if provided.
	 * 
	 * @param The user to filter the audits by
	 * @return The last 20 audits in the system
	 */
	private List<Bean> recentUpdates(UserExtension filterUser) {

		DocumentQuery q = this.persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter()
				.addEquals(Audit.auditModuleNamePropertyName, Dashboard.MODULE_NAME); // filter for this module only

		q.getFilter()
				.addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter()
				.addNotEquals(Audit.operationPropertyName, Operation.delete);
		if (filterUser != null) {
			q.getFilter()
					.addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundProjection(Audit.timestampPropertyName);
		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addBoundOrdering(Audit.timestampPropertyName, SortDirection.descending);
		q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
		q.setMaxResults(20);

		return q.projectedResults();
	}

	@Override
	public String getDashboardIconMarkup() {
		if (getDashboardIconStyleClass() != null) {
			return iconMarkup(getDashboardIconStyleClass());
		}
		return super.getDashboardIconMarkup();
	}

	/**
	 * return the markup for an icon
	 * 
	 * @param icon
	 * @return
	 */
	public static String iconMarkup(String icon) {
		return "<i class='" + icon + "' style='font-size:200%'></i>";
	}

	/**
	 * Retrieves the list of roles for the current module as DomainValue objects.
	 * Each DomainValue contains the role's bizId and a display name combining the role name and its description.
	 * If the role description is longer than 50 characters, it is truncated.
	 * Only roles for the module specified by getModuleName() are included.
	 *
	 * @return a list of DomainValue objects representing the module's roles
	 */
	public List<DomainValue> getModuleRoles() {
		List<DomainValue> result = new ArrayList<>(48);
		if (getModuleName() != null) {
			String moduleName = getModuleName();
			Module module = CORE.getCustomer().getModule(moduleName);
			if (module != null) {
				for (Role role : module.getRoles()) {
					String roleName = role.getName();
					String roleDescription = role.getLocalisedDescription();

					DocumentQuery qUserRoles = CORE.getPersistence().newDocumentQuery(UserRole.MODULE_NAME, UserRole.DOCUMENT_NAME);
					qUserRoles.getFilter().addEquals(UserRole.roleNamePropertyName, String.format("%s.%s", moduleName, roleName));
					UserRole userRole = qUserRoles.beanResult();

					if (roleDescription != null) {
						if (roleDescription.length() > 50) {
							roleDescription = roleDescription.substring(0, 47) + "...";
						}
						result.add(new DomainValue(userRole.getBizId(),
								String.format("%s (%s)", roleName, roleDescription)));
					} else {
						result.add(new DomainValue(userRole.getBizId(),
								roleName));
					}
				}
			}
		}
		return result;
	}
}
