package modules.admin.Dashboard.chain.processors;

import org.skyve.CORE;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.metadata.view.fluent.FluentActions;
import org.skyve.metadata.view.fluent.FluentBlurb;
import org.skyve.metadata.view.fluent.FluentChart;
import org.skyve.metadata.view.fluent.FluentComponent;
import org.skyve.metadata.view.fluent.FluentCustomAction;
import org.skyve.metadata.view.fluent.FluentDefaultsAction;
import org.skyve.metadata.view.fluent.FluentForm;
import org.skyve.metadata.view.fluent.FluentFormColumn;
import org.skyve.metadata.view.fluent.FluentFormItem;
import org.skyve.metadata.view.fluent.FluentFormRow;
import org.skyve.metadata.view.fluent.FluentHBox;
import org.skyve.metadata.view.fluent.FluentListRepeater;
import org.skyve.metadata.view.fluent.FluentVBox;
import org.skyve.metadata.view.fluent.FluentView;

import modules.admin.Dashboard.chain.AbstractDashboardProcessor;
import modules.admin.Dashboard.chain.DashboardProcessingContext;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;

/**
 * Processor responsible for creating the FluentView from dashboard widgets.
 * This applies to both dashboard loading and activation, but with different view styles.
 */
public class ViewCreationProcessor extends AbstractDashboardProcessor {

	@Override
	public boolean canHandle(DashboardProcessingContext context) {
		// Handle both load and activation scenarios for view creation
		return context.isLoadDashboard() || context.isActivateDashboard();
	}

	@Override
	protected DashboardProcessingContext doProcess(DashboardProcessingContext context) {
		FluentView fluentView = null;

		if (context.isLoadDashboard()) {
			fluentView = createUserDashboardView(context);
		} else if (context.isActivateDashboard()) {
			fluentView = createActivationDashboardView(context);
		}

		if (fluentView != null) {
			context.setFluentView(fluentView);
		}

		return passToNext(context);
	}

	/**
	 * Creates a FluentView for user dashboard loading.
	 * This includes design mode components and user-specific actions.
	 */
	private static FluentView createUserDashboardView(DashboardProcessingContext context) {
		try {
			// Create the basic view
			FluentView designedView = new FluentView()
					.title("Dashboard")
					.name("edit");

			// Add the welcome banner component
			FluentVBox bannerVbox = new FluentVBox()
					.addComponent(new FluentComponent().name("_welcomeBanner"))
					.shrinkWrap(ShrinkWrap.height);
			designedView.addVBox(bannerVbox);

			if (context.getDashboard().inDesignMode()) {
				addDesignModeComponents(designedView, context);
			} else {
				addWidgetComponents(designedView, context);
			}

			addViewActions(designedView, context);

			// Set documentation to dashboard bizId for finding
			designedView.documentation(context.getDashboard().getBizId());

			return designedView;

		} catch (Exception e) {
			context.setErrorMessage("Failed to create user dashboard view: " + e.getMessage());
			return null;
		}
	}

	/**
	 * Creates a FluentView for dashboard activation.
	 * This creates a clean view for system-wide dashboard activation.
	 */
	private static FluentView createActivationDashboardView(DashboardProcessingContext context) {
		try {
			FluentView designedView = new FluentView()
					.title("Dashboard")
					.name("edit");

			// Add the welcome banner
			FluentVBox bannerVbox = new FluentVBox().shrinkWrap(ShrinkWrap.height)
					.border(true);
			FluentForm bannerForm = new FluentForm().addColumn(new FluentFormColumn())
					.addRow(new FluentFormRow().addItem(new FluentFormItem()
							.blurb(new FluentBlurb().markup("<em>Welcome <strong>{user.userName}</strong> to Skyve</em>"))));
			bannerVbox.addForm(bannerForm);

			designedView.addVBox(bannerVbox);

			// Add widget components for activation
			addWidgetComponents(designedView, context);

			// Add activation-specific actions
			addActivationViewActions(designedView, context);

			// Set documentation to dashboard bizId for finding
			designedView.documentation(context.getDashboard().getBizId());

			return designedView;

		} catch (Exception e) {
			context.setErrorMessage("Failed to create activation dashboard view: " + e.getMessage());
			return null;
		}
	}

	/**
	 * Adds widget components to the view based on dashboard configuration.
	 */
	private static void addWidgetComponents(FluentView view, DashboardProcessingContext context) {
		try {
			// Calculate layout columns and responsive width based on widget count
			int widgetCount = context.getDashboard().getDashboardWidgets().size();
			int responsiveWidth = 12;
			int cols = 1;

			if (widgetCount > 4) {
				cols = 3;
				responsiveWidth = 4;
			} else if (widgetCount > 2) {
				cols = 2;
				responsiveWidth = 6;
			}

			// Setup widget placement
			int widgetsPlaced = 0;
			FluentHBox widgetHBox = new FluentHBox();

			// Add all widgets
			for (DashboardWidgetExtension widget : context.getDashboard().getDashboardWidgets()) {

				// Start a new row if needed
				if (widgetsPlaced >= cols) {
					view.addHBox(widgetHBox);
					widgetHBox = new FluentHBox();
					widgetsPlaced = 0;
				}

				if (widget.getWidgetType() != null) {
					if (addWidget(widget, widgetHBox, responsiveWidth, context)) {
						widgetsPlaced++;
					}
				}
			}

			// Add the final row
			view.addHBox(widgetHBox);

		} catch (

		Exception e) {
			context.setErrorMessage("Failed to add widget components: " + e.getMessage());
		}
	}

	/**
	 * Adds design mode components for user dashboard editing.
	 */
	private static void addDesignModeComponents(FluentView view, DashboardProcessingContext context) {
		try {
			// add dataGrid for widget selection
			FluentVBox designerVbox = new FluentVBox()
					.border(true)
					.borderTitle("Design Mode")
					.addComponent(new FluentComponent().name("_designer"));

			if (context.getDashboard().getFocusItem() != null &&
					context.getDashboard().getFocusItem().isShowCustomChartOptions()) {

				// show a hbox with the custom chart design and the custom chart
				FluentHBox customHbox = new FluentHBox();

				customHbox.addComponent(
						new FluentComponent()
								.name("_customChartDesign")
								.documentName(DashboardWidget.DOCUMENT_NAME)
								.binding(Dashboard.focusItemPropertyName));

				// include the chart if there is enough information
				if (context.getDashboard().getFocusItem().sufficientInformationToPreview()) {
					FluentChart focusCustomChart = new FluentChart()
							.type(ChartType.valueOf(
									context.getDashboard()
											.getFocusItem()
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

			view.addVBox(designerVbox);

		} catch (Exception e) {
			context.setErrorMessage("Failed to add design mode components: " + e.getMessage());
		}
	}

	/**
	 * Adds user dashboard specific actions.
	 */
	private static void addUserDashboardActions(FluentView view, DashboardProcessingContext context) {
		try {
			FluentCustomAction switchToDesignAction = new FluentCustomAction()
					.className("SwitchDashboardMode")
					.name("SwitchToDesign")
					.clientValidation(false)
					.displayName("Design")
					.iconStyleClass("fa fa-paintbrush")
					.invisibleConditionName("inDesignMode")
					.inActionPanel(false);

			FluentCustomAction switchToDisplayAction = new FluentCustomAction()
					.className("SwitchDashboardMode")
					.name("SwitchToDisplay")
					.clientValidation(false)
					.displayName("Display")
					.iconStyleClass("fa fa-eye")
					.invisibleConditionName("notInDesignMode")
					.inActionPanel(false);

			FluentCustomAction goToSelectorAction = new FluentCustomAction()
					.className("GoToSelector")
					.clientValidation(false)
					.displayName("Done")
					.iconStyleClass("fa fa-thumbs-up")
					.invisibleConditionName("notShowGoToSelector")
					.inActionPanel(false);

			FluentCustomAction addWidgetAction = new FluentCustomAction()
					.className("AddWidget")
					.invisibleConditionName("notShowAddWidgetAction")
					.clientValidation(false)
					.displayName("Add")
					.iconStyleClass("fa fa-plus")
					.inActionPanel(false);

			FluentCustomAction updateMyDetailsAction = new FluentCustomAction()
					.className("UpdateMyDetails")
					.clientValidation(false)
					.displayName("Save")
					.iconStyleClass("fa-solid fa-floppy-disk")
					.inActionPanel(false);

			FluentCustomAction activateDashboardAction;
			FluentCustomAction deactivateDashboardAction;

			if (context.getDashboard().isIndicateActivated()) {
				activateDashboardAction = new FluentCustomAction()
						.className("ActivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Updating the dashboard will add it to other users' menus. Do you wish to continue?")
						.displayName("Update Dashboard")
						.iconStyleClass("fa-solid fa-rotate-right")
						.inActionPanel(true);

				deactivateDashboardAction = new FluentCustomAction()
						.className("DeactivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Deactivating the dashboard will remove it from other users' menus. Do you wish to continue?")
						.displayName("Remove Dashboard")
						.iconStyleClass("fa-solid fa-toggle-off")
						.inActionPanel(true);
			} else {
				activateDashboardAction = new FluentCustomAction()
						.className("ActivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Activating the dashboard will add it to other users' menus. Do you wish to continue?")
						.displayName("Activate Dashboard")
						.iconStyleClass("fa-solid fa-toggle-on")
						.inActionPanel(true);

				deactivateDashboardAction = new FluentCustomAction()
						.className("DeactivateDashboard")
						.clientValidation(true)
						.confirmationText(
								"Deactivating the dashboard will remove it from other users' menus. Do you wish to continue?")
						.displayName("Remove Dashboard")
						.iconStyleClass("fa-solid fa-toggle-off")
						.inActionPanel(true)
						.invisibleConditionName("notIndicateActivated");
			}

			FluentDefaultsAction defaultsAction = new FluentDefaultsAction();

			FluentActions actions = new FluentActions()
					.addCustomAction(switchToDesignAction)
					.addCustomAction(switchToDisplayAction)
					.addCustomAction(goToSelectorAction)
					.addCustomAction(addWidgetAction)
					.addCustomAction(updateMyDetailsAction)
					.addCustomAction(activateDashboardAction)
					.addCustomAction(deactivateDashboardAction)
					.addDefaultsAction(defaultsAction);

			view.actions(actions);

		} catch (Exception e) {
			context.setErrorMessage("Failed to add user dashboard actions: " + e.getMessage());
		}
	}

	/**
	 * Creates and adds actions to the view based on processing type.
	 */
	private static void addViewActions(FluentView view, DashboardProcessingContext context) {
		try {
			if (context.isLoadDashboard()) {
				addUserDashboardActions(view, context);
			}
			// Activation actions will be added when we implement activation
		} catch (Exception e) {
			context.setErrorMessage("Failed to add view actions: " + e.getMessage());
		}
	}

	/**
	 * Adds a widget to the layout
	 * 
	 * @return true if a widget was added, false otherwise
	 */
	private static boolean addWidget(DashboardWidgetExtension widget,
			FluentHBox widgetHBox,
			int responsiveWidth,
			DashboardProcessingContext context) {
		FluentVBox widgetVBox = null;
		StringBuilder customChartModelName;
		FluentChart customChart;

		// Use context to track custom chart count across calls
		int customChartCount = context.getCustomChartCount();

		try {
			if (widget.getWidgetType() != null) {
				switch (widget.getWidgetType()) {
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
								.borderTitle(widget.getTitle())
								.responsiveWidth(responsiveWidth);
						customChartModelName = new StringBuilder(64);
						customChartModelName.append("CustomChartModel")
								.append(++customChartCount);
						customChart = new FluentChart().type(ChartType.valueOf(widget.getChartType()
								.toCode()))
								.modelName(customChartModelName.toString());
						widgetVBox.addChart(customChart);
						widgetHBox.addVBox(widgetVBox);
						context.setCustomChartCount(customChartCount); // Update context
						break;

					case favourites:
						context.getDashboard().getFavourites().clear();
						createFavourites(context);
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
								new FluentComponent().name("_myDetails")
										.moduleName(Dashboard.MODULE_NAME)
										.documentName(Dashboard.DOCUMENT_NAME));
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
					case myUserLoginHistory:
						widgetVBox = new FluentVBox().border(true)
								.borderTitle(WidgetType.myUserLoginHistory.toLocalisedDescription())
								.responsiveWidth(responsiveWidth);
						FluentChart myUserLoginHistoryChart = new FluentChart().type(ChartType.lineArea)
								.modelName("MyUserLoginHistoryModel");
						widgetVBox.addChart(myUserLoginHistoryChart);
						widgetHBox.addVBox(widgetVBox);
						break;
					case usersLoginHistory:
						widgetVBox = new FluentVBox().border(true)
						.borderTitle(WidgetType.usersLoginHistory.toLocalisedDescription())
						.responsiveWidth(responsiveWidth);
						FluentChart usersLoginHistoryChart = new FluentChart().type(ChartType.lineArea)
								.modelName("UsersLoginHistoryModel");
						widgetVBox.addChart(usersLoginHistoryChart);
						widgetHBox.addVBox(widgetVBox);
						break;

					default:
						return false;
				}
			}

			return true;
		} catch (Exception e) {
			context.setErrorMessage("Failed to add widget of type " + widget.getWidgetType() + ": " + e.getMessage());
			return false;
		}
	}

	/**
	 * Adds activation-specific actions to the view.
	 * These are simplified actions without design mode functionality.
	 */
	private static void addActivationViewActions(FluentView view, DashboardProcessingContext context) {
		try {
			FluentCustomAction updateMyDetailsAction = new FluentCustomAction()
					.className("UpdateMyDetails")
					.clientValidation(false)
					.displayName("Save")
					.iconStyleClass("fa-solid fa-floppy-disk")
					.inActionPanel(false);

			FluentDefaultsAction defaultsAction = new FluentDefaultsAction();

			FluentActions actions = new FluentActions()
					.addCustomAction(updateMyDetailsAction)
					.addDefaultsAction(defaultsAction);

			view.actions(actions);

		} catch (Exception e) {
			context.setErrorMessage("Failed to add activation view actions: " + e.getMessage());
		}
	}

	/**
	 * Create markup for shortcut links to favourite actions for this user
	 * 
	 * @param context
	 */
	private static void createFavourites(DashboardProcessingContext context) {
		try {
			// Use the FavouritesService from context to create favourites
			if (context.getFavouritesService() != null) {
				context.getFavouritesService().createFavourites(context.getDashboard(), CORE.getPersistence());
			}
		} catch (Exception e) {
			context.setErrorMessage("Failed to create favourites: " + e.getMessage());
		}
	}
}
