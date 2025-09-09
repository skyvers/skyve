package modules.admin.Dashboard;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.model.chart.ChartBuilder;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import jakarta.inject.Inject;
import modules.admin.Dashboard.chain.DashboardChainService;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget.WidgetType;
import modules.admin.domain.UserRole;

public class DashboardExtension extends Dashboard {
	private static final long serialVersionUID = -1522971002459761943L;

	@Inject
	private transient DashboardChainService dashboardChainService;

	/**
	 * Loads designed dashboard elements by creating a fluid edit view and replacing
	 * the static edit view
	 */
	public void loadDashboard() {
		if (!Boolean.TRUE.equals(this.getLoaded())) {
			// Fetch repository
			DefaultRepository r = (DefaultRepository) CORE.getRepository();

			// Use the chain service to process the dashboard loading
			boolean success = dashboardChainService.loadDashboard(this, r);

			if (!success) {
				// Handle failure case - perhaps log an error or set a flag
				this.setLoaded(Boolean.FALSE);
			}
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
