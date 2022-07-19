package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;

public class ViewImpl extends Container implements View {
	private static final long serialVersionUID = -2621201277538515637L;

	private String name;
	private String icon32x32RelativeFileName;
	private String iconStyleClass;
	private String helpRelativeFileName;
	private String helpURL;
	private String title;
	private String actionsWidgetId;
	private LinkedHashMap<String, Action> actions = new LinkedHashMap<>();
	private Integer refreshTimeInSeconds;
	private String refreshConditionName;
	private String refreshActionName;
	private List<ViewParameter> parameters = new ArrayList<>();
	private String documentation;
	// map of modelId -> model metadata used to instantiate models on the server-side
	private List<ModelMetaData> inlineModels = new ArrayList<>();
	private String overriddenCustomerName;
	private String overriddenUxUiName;
	private Map<String, String> properties = new TreeMap<>();
	
	@Override
	public String getRefreshConditionName() {
		return refreshConditionName;
	}

	public void setRefreshConditionName(String refreshConditionName) {
		this.refreshConditionName = refreshConditionName;
	}

	@Override
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	@Override
	public String getRefreshActionName() {
		return refreshActionName;
	}

	public void setRefreshActionName(String refreshActionName) {
		this.refreshActionName = refreshActionName;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = icon32x32RelativeFileName;
	}

	@Override
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = iconStyleClass;
	}

	@Override
	public String getHelpRelativeFileName() {
		return helpRelativeFileName;
	}

	public void setHelpRelativeFileName(String helpRelativeFileName) {
		this.helpRelativeFileName = helpRelativeFileName;
	}

	@Override
	public String getHelpURL() {
		return helpURL;
	}

	public void setHelpURL(String helpURL) {
		this.helpURL = helpURL;
	}

	@Override
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public String getActionsWidgetId() {
		return actionsWidgetId;
	}

	public void setActionsWidgetId(String actionsWidgetId) {
		this.actionsWidgetId = actionsWidgetId;
	}

	@Override
	public Action getAction(String actionName) {
		return actions.get(actionName);
	}

	public void putAction(Action action) {
		actions.put(action.getName(), action);
	}

	@Override
	public Collection<Action> getActions() {
		return actions.values();
	}

	/**
	 * These represent parameters that are allowed to be populated when creating a new record.
	 */
	@Override
	public List<ViewParameter> getParameters() {
		return parameters;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
	
	/**
	 * Get an implicit inlined model - ie a chart model from a given modelId.
	 * @param modelIndex	The index of the model to get
	 * @return	The model.
	 */
	@Override
	public ModelMetaData getInlineModel(int modelIndex) {
		return inlineModels.get(modelIndex);
	}
	
	@Override
	public String getOverriddenCustomerName() {
		return overriddenCustomerName;
	}

	public void setOverriddenCustomerName(String overriddenCustomerName) {
		this.overriddenCustomerName = overriddenCustomerName;
	}

	@Override
	public String getOverriddenUxUiName() {
		return overriddenUxUiName;
	}

	public void setOverriddenUxUiName(String overriddenUxUiName) {
		this.overriddenUxUiName = overriddenUxUiName;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Ensure that any component loaded matches the given UX/UI.
	 * Ensure that inlined metadata model definitions are added to the implicit models map.
	 */
	public void resolve(String uxui, Customer customer, Document document) {
		Module module = customer.getModule(document.getOwningModuleName());
		new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, this) {
			// Overrides visitComponent standard behaviour to load the component
			// Note that the component children are not traversed here.
			@Override
			public void visitComponent(Component component, boolean parentVisible, boolean parentEnabled) {
				component.setContained(uxui, customer, module, document, name);
			}
			
			@Override
			public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
				ChartBuilderMetaData model = chart.getModel();
				if (model != null) {
					inlineModels.add(model);
					model.setModelIndex(inlineModels.size() - 1);
				}
			}
		}.visit();
	}
}
