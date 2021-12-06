package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.view.View;

public class FluentView extends FluentContainer<FluentView> {
	private ViewMetaData view = null;

	public FluentView() {
		view = new ViewMetaData();
	}

	public FluentView(ViewMetaData view) {
		this.view = view;
	}

	public FluentView from(@SuppressWarnings("hiding") View view) {
		name(view.getName());
		title(view.getTitle());
		iconStyleClass(view.getIconStyleClass());
		icon32x32RelativeFileName(view.getIcon32x32RelativeFileName());
		documentation(view.getDocumentation());
		helpRelativeFileName(view.getHelpRelativeFileName());
		helpURL(view.getHelpURL());
		refreshTimeInSeconds(view.getRefreshTimeInSeconds());
		refreshConditionName(view.getRefreshConditionName());
		refreshActionName(view.getRefreshActionName());

		actions(new FluentActions().from(view.getActionsWidgetId(), view.getActions()));

		view.getParameters().forEach(p -> addParameter(new FluentViewParameter().from(p)));

		super.from(((ViewImpl) view));

		return this;
	}

	public FluentView name(String name) {
		view.setName(name);
		return this;
	}

	public FluentView title(String title) {
		view.setTitle(title);
		return this;
	}

	public FluentView iconStyleClass(String iconStyleClass) {
		view.setIconStyleClass(iconStyleClass);
		return this;
	}

	public FluentView icon32x32RelativeFileName(String icon32x32RelativeFileName) {
		view.setIcon32x32RelativeFileName(icon32x32RelativeFileName);
		return this;
	}

	public FluentView documentation(String documentation) {
		view.setDocumentation(documentation);
		return this;
	}

	public FluentView helpRelativeFileName(String helpRelativeFileName) {
		view.setHelpRelativeFileName(helpRelativeFileName);
		return this;
	}

	public FluentView helpURL(String helpURL) {
		view.setHelpURL(helpURL);
		return this;
	}

	public FluentView refreshTimeInSeconds(int refreshTimeInSeconds) {
		view.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}

	public FluentView refreshConditionName(String refreshConditionName) {
		view.setRefreshConditionName(refreshConditionName);
		return this;
	}

	public FluentView refreshActionName(String refreshActionName) {
		view.setRefreshActionName(refreshActionName);
		return this;
	}

	public FluentView actions(FluentActions actions) {
		view.setActions(actions.get());
		return this;
	}

	public FluentView addParameter(FluentViewParameter parameter) {
		view.getParameters().add(parameter.get());
		return this;
	}

	@Override
	public ViewMetaData get() {
		return view;
	}
}
