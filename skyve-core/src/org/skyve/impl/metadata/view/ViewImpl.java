package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.widget.bound.Parameter;

public class ViewImpl extends Container implements View {
	private static final long serialVersionUID = -2621201277538515637L;

	private ViewType type;
	private String icon32x32RelativeFileName;
	private String iconStyleClass;
	private String title;
	private String actionsWidgetId;
	private LinkedHashMap<String, Action> actions = new LinkedHashMap<>();
	private Integer refreshTimeInSeconds;
	private String refreshConditionName;
	private String refreshActionName;
	private List<Parameter> parameters = new ArrayList<>();
	private String documentation;
	
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
	public ViewType getType() {
		return type;
	}

	public void setType(ViewType type) {
		this.type = type;
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
	public List<Parameter> getParameters() {
		return parameters;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
}
