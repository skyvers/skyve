package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewDocumentAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewPreviousCompleteUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewQueryAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewParameter;

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
		Integer i = view.getRefreshTimeInSeconds();
		if (i != null) {
			refreshTimeInSeconds(i.intValue());
		}
		refreshConditionName(view.getRefreshConditionName());
		refreshActionName(view.getRefreshActionName());

		Sidebar sidebar = view.getSidebar();
		if (sidebar != null) {
			sidebar(new FluentSidebar().from(sidebar));
		}
		actions(new FluentActions().from(view.getActionsWidgetId(), view.getActions()));

		view.getParameters().forEach(p -> addParameter(new FluentViewParameter().from(p)));

		// Note accesses are not stored on the View implementation but just computed there,
		// So we can't generate access definitions from this object.

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
	
	/**
	 * Adds a new {@link FluentViewDocumentAggregateAccess} to this view.
	 */
	public FluentView addDocumentAggregateAccess(FluentViewDocumentAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view document aggregate access in this view's list of accesses.
	 */
	public FluentViewDocumentAggregateAccess findDocumentAggregateAccess(final String documentName) {
		ViewUserAccessMetaData result = view.getAccesses().getAccesses().stream()
											.filter(a -> ((a instanceof ViewDocumentAggregateUserAccessMetaData) &&
															((ViewDocumentAggregateUserAccessMetaData) a).getDocumentName().equals(documentName)))
											.findFirst()
											.orElse(null);
		return (result != null) ? new FluentViewDocumentAggregateAccess((ViewDocumentAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewDocumentAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeDocumentAggregateAccess(final String documentName) {
		view.getAccesses().getAccesses().removeIf(a -> ((a instanceof ViewDocumentAggregateUserAccessMetaData) &&
															((ViewDocumentAggregateUserAccessMetaData) a).getDocumentName().equals(documentName)));
		return this;
	}

	/**
	 * Adds a new {@link FluentViewModelAggregateAccess} to this view.
	 */
	public FluentView addModelAggregateAccess(FluentViewModelAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view model aggregate access in this view's list of accesses.
	 */
	public FluentViewModelAggregateAccess findModelAggregateAccess(final String modelName) {
		ViewUserAccessMetaData result = view.getAccesses().getAccesses().stream()
												.filter(a -> ((a instanceof ViewModelAggregateUserAccessMetaData) &&
																((ViewModelAggregateUserAccessMetaData) a).getModelName().equals(modelName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewModelAggregateAccess((ViewModelAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewModelAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeModelAggregateAccess(final String modelName) {
		view.getAccesses().getAccesses().removeIf(a -> ((a instanceof ViewModelAggregateUserAccessMetaData) &&
															((ViewModelAggregateUserAccessMetaData) a).getModelName().equals(modelName)));
		return this;
	}

	/**
	 * Adds a new {@link FluentViewPreviousCompleteAccess} to this view.
	 */
	public FluentView addPreviousCompleteAccess(FluentViewPreviousCompleteAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view previous complete access in this view's list of accesses.
	 */
	public FluentViewPreviousCompleteAccess findPreviousCompleteAccess(final String binding) {
		ViewUserAccessMetaData result = view.getAccesses().getAccesses().stream()
												.filter(a -> ((a instanceof ViewPreviousCompleteUserAccessMetaData) &&
																((ViewPreviousCompleteUserAccessMetaData) a).getBinding().equals(binding)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewPreviousCompleteAccess((ViewPreviousCompleteUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewPreviousCompleteUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removePreviousCompleteAccess(final String binding) {
		view.getAccesses().getAccesses().removeIf(a -> ((a instanceof ViewPreviousCompleteUserAccessMetaData) &&
															((ViewPreviousCompleteUserAccessMetaData) a).getBinding().equals(binding)));
		return this;
	}

	/**
	 * Adds a new {@link FluentViewQueryAggregateAccess} to this view.
	 */
	public FluentView addQueryAggregateAccess(FluentViewQueryAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view query access in this view's list of accesses.
	 */
	public FluentViewQueryAggregateAccess findQueryAggregateAccess(final String queryName) {
		ViewUserAccessMetaData result = view.getAccesses().getAccesses().stream()
												.filter(a -> ((a instanceof ViewQueryAggregateUserAccessMetaData) &&
																((ViewQueryAggregateUserAccessMetaData) a).getQueryName().equals(queryName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewQueryAggregateAccess((ViewQueryAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewQueryAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeQueryAggregateAccess(final String queryName) {
		view.getAccesses().getAccesses().removeIf(a -> ((a instanceof ViewQueryAggregateUserAccessMetaData) &&
															((ViewQueryAggregateUserAccessMetaData) a).getQueryName().equals(queryName)));
		return this;
	}

	/**
	 * Adds a new {@link FluentViewSingularAccess} to this view.
	 */
	public FluentView addSingularAccess(FluentViewSingularAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view singular access in this view's list of accesses.
	 */
	public FluentViewSingularAccess findSingularAccess(final String documentName) {
		ViewUserAccessMetaData result = view.getAccesses().getAccesses().stream()
												.filter(a -> ((a instanceof ViewSingularUserAccessMetaData) &&
																((ViewSingularUserAccessMetaData) a).getDocumentName().equals(documentName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewSingularAccess((ViewSingularUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewSingularUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeSingularAccess(final String documentName) {
		view.getAccesses().getAccesses().removeIf(a -> ((a instanceof ViewSingularUserAccessMetaData) &&
															((ViewSingularUserAccessMetaData) a).getDocumentName().equals(documentName)));
		return this;
	}

	/**
	 * Clears all the accesses for this module role.
	 */
	public FluentView clearAccesses() {
		view.getAccesses().getAccesses().clear();
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

	public FluentView sidebar(FluentSidebar sidebar) {
		view.setSidebar(sidebar.get());
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

	public FluentView removeParameter(String binding) {
		view.getParameters().removeIf(p -> (binding.equals(p.getBoundTo()) || binding.equals(p.getFromBinding())));
		return this;
	}

	public FluentView clearParameters() {
		view.getParameters().clear();
		return this;
	}

	public FluentViewParameter findParameter(String binding) {
		ViewParameter result = view.getParameters().stream()
				.filter(p -> (binding.equals(p.getBoundTo()) || binding.equals(p.getFromBinding()))).findAny().orElse(null);
		if (result != null) {
			return new FluentViewParameter(result);
		}
		return null;
	}

	@Override
	public ViewMetaData get() {
		return view;
	}

	/**
	 * Adds a new {@link FluentViewUserAccess} to this module role.
	 */
	private <T extends FluentViewUserAccess<?, ?>> FluentView addAccess(T access) {
		view.getAccesses().getAccesses().add(access.get());
		return this;
	}
}
