package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewContentUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewDocumentAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewDynamicImageUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewPreviousCompleteUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewQueryAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewReportUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewParameter;

import jakarta.annotation.Nonnull;

/**
 * Fluent builder for top-level {@link View} metadata.
 */
public class FluentView extends FluentContainer<FluentView> {
	private ViewMetaData view = null;

	/**
	 * Creates a builder backed by a new {@link ViewMetaData} instance.
	 */
	public FluentView() {
		view = new ViewMetaData();
	}

	/**
	 * Creates a builder backed by the supplied view metadata instance.
	 *
	 * @param view
	 *            the view metadata to mutate
	 */
	public FluentView(ViewMetaData view) {
		this.view = view;
	}

	/**
	 * Copies view metadata into this builder.
	 *
	 * <p>Side effects: replaces identity, presentation, refresh settings, sidebar/actions,
	 * parameters, and all contained widgets.
	 *
	 * @param view
	 *            the source view metadata
	 * @return this builder
	 */
	public FluentView from(@SuppressWarnings("hiding") View view) {
		name(view.getName());
		title(view.getTitle());
		escapeTitle(((ViewImpl) view).getEscapeTitle());
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

	/**
	 * Sets the view name.
	 *
	 * @param name
	 *            the view identifier
	 * @return this builder
	 */
	public FluentView name(String name) {
		view.setName(name);
		return this;
	}

	/**
	 * Sets the view title.
	 *
	 * @param title
	 *            the display title
	 * @return this builder
	 */
	public FluentView title(String title) {
		view.setTitle(title);
		return this;
	}

	/**
	 * Sets whether the view title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentView escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the view title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentView escapeTitle(Boolean escapeTitle) {
		view.setEscapeTitle(escapeTitle);
		return this;
	}

	/**
	 * Sets the style class for the view icon.
	 *
	 * @param iconStyleClass
	 *            icon style class
	 * @return this builder
	 */
	public FluentView iconStyleClass(String iconStyleClass) {
		view.setIconStyleClass(iconStyleClass);
		return this;
	}

	/**
	 * Sets the relative path to the 32x32 icon.
	 *
	 * @param icon32x32RelativeFileName
	 *            relative icon file path
	 * @return this builder
	 */
	public FluentView icon32x32RelativeFileName(String icon32x32RelativeFileName) {
		view.setIcon32x32RelativeFileName(icon32x32RelativeFileName);
		return this;
	}

	/**
	 * Sets the documentation text for this view.
	 *
	 * @param documentation
	 *            documentation content
	 * @return this builder
	 */
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
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewDocumentAggregateUserAccessMetaData vda) &&
																vda.getDocumentName().equals(documentName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewDocumentAggregateAccess((ViewDocumentAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewDocumentAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeDocumentAggregateAccess(final String documentName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewDocumentAggregateUserAccessMetaData vda) &&
													vda.getDocumentName().equals(documentName)));
		}
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
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewModelAggregateUserAccessMetaData vma) &&
																vma.getModelName().equals(modelName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewModelAggregateAccess((ViewModelAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewModelAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeModelAggregateAccess(final String modelName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewModelAggregateUserAccessMetaData vma) &&
													vma.getModelName().equals(modelName)));
		}
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
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewPreviousCompleteUserAccessMetaData vpc) &&
																vpc.getBinding().equals(binding)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewPreviousCompleteAccess((ViewPreviousCompleteUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewPreviousCompleteUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removePreviousCompleteAccess(final String binding) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewPreviousCompleteUserAccessMetaData vpc) &&
													vpc.getBinding().equals(binding)));
		}
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
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewQueryAggregateUserAccessMetaData vqa) &&
																vqa.getQueryName().equals(queryName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewQueryAggregateAccess((ViewQueryAggregateUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewQueryAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeQueryAggregateAccess(final String queryName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewQueryAggregateUserAccessMetaData vqa) &&
													vqa.getQueryName().equals(queryName)));
		}
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
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewSingularUserAccessMetaData vs) &&
																vs.getDocumentName().equals(documentName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewSingularAccess((ViewSingularUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewSingularUserAccessMetaData} with the specified
	 * document name if one is defined for this view.
	 */
	public FluentView removeSingularAccess(final String documentName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewSingularUserAccessMetaData vs) &&
													vs.getDocumentName().equals(documentName)));
		}
		return this;
	}

	/**
	 * Adds a new {@link FluentViewReportAccess} to this view.
	 */
	public FluentView addReportAccess(FluentViewReportAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view report access in this view's list of accesses.
	 */
	public FluentViewReportAccess findReportAccess(final String moduleName, final String documentName, final String reportName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewReportUserAccessMetaData vr) &&
																vr.getModuleName().equals(moduleName) &&
																vr.getDocumentName().equals(documentName) &&
																vr.getReportName().equals(reportName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewReportAccess((ViewReportUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewReportUserAccessMetaData} with the specified
	 * module name, document name and report name if one is defined for this view.
	 */
	public FluentView removeReportAccess(final String moduleName, final String documentName, final String reportName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewReportUserAccessMetaData vr) &&
													vr.getModuleName().equals(moduleName) &&
													vr.getDocumentName().equals(documentName) &&
													vr.getReportName().equals(reportName)));
		}
		return this;
	}

	/**
	 * Adds a new {@link FluentViewDynamicImageAccess} to this view.
	 */
	public FluentView addDynamicImageAccess(FluentViewDynamicImageAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view dynamic image access in this view's list of accesses.
	 */
	public FluentViewDynamicImageAccess findDynamicImageAccess(final String imageName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewDynamicImageUserAccessMetaData vdi) &&
																vdi.getImageName().equals(imageName)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewDynamicImageAccess((ViewDynamicImageUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewDynamicImageUserAccessMetaData} with the specified
	 * image name if one is defined for this view.
	 */
	public FluentView removeDynamicImageAccess(final String imageName) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewDynamicImageUserAccessMetaData vdi) &&
													vdi.getImageName().equals(imageName)));
		}
		return this;
	}

	/**
	 * Adds a new {@link FluentViewContentAccess} to this view.
	 */
	public FluentView addContentAccess(FluentViewContentAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the view content access in this view's list of accesses.
	 */
	public FluentViewContentAccess findContentAccess(final String binding) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses == null) {
			return null;
		}
		ViewUserAccessMetaData result = accesses.getAccesses().stream()
												.filter(a -> ((a instanceof ViewContentUserAccessMetaData vc) &&
																vc.getBinding().equals(binding)))
												.findFirst()
												.orElse(null);
		return (result != null) ? new FluentViewContentAccess((ViewContentUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link ViewContentUserAccessMetaData} with the specified
	 * binding if one is defined for this view.
	 */
	public FluentView removeContentAccess(final String binding) {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().removeIf(a -> ((a instanceof ViewContentUserAccessMetaData vc) &&
													vc.getBinding().equals(binding)));
		}
		return this;
	}

	/**
	 * Clears all the accesses for this module role.
	 */
	public FluentView clearAccesses() {
		ViewUserAccessesMetaData accesses = view.getAccesses();
		if (accesses != null) {
			accesses.getAccesses().clear();
		}
		return this;
	}
	
	/**
	 * Removes the access metadata block from this view.
	 *
	 * <p>Side effects: drops all existing access entries and resets generate-access behaviour
	 * to framework defaults.
	 *
	 * @return this builder
	 */
	public FluentView removeAccesses() {
		view.setAccesses(null);
		return this;
	}
	
	/**
	 * Enables or disables generated access metadata for this view.
	 *
	 * <p>Side effects: creates the access metadata container when absent.
	 *
	 * @param generate
	 *            {@code true} to enable generated access metadata
	 * @return this builder
	 */
	public FluentView generateAccesses(boolean generate) {
		ViewUserAccessesMetaData accesses = establishViewAccesses();
		accesses.setGenerate(generate);
		return this;
	}
	
	/**
	 * Sets the relative help file path for this view.
	 *
	 * @param helpRelativeFileName
	 *            relative help file path
	 * @return this builder
	 */
	public FluentView helpRelativeFileName(String helpRelativeFileName) {
		view.setHelpRelativeFileName(helpRelativeFileName);
		return this;
	}

	/**
	 * Sets the external help URL for this view.
	 *
	 * @param helpURL
	 *            help URL
	 * @return this builder
	 */
	public FluentView helpURL(String helpURL) {
		view.setHelpURL(helpURL);
		return this;
	}

	/**
	 * Sets the automatic refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds
	 *            refresh interval in seconds
	 * @return this builder
	 */
	public FluentView refreshTimeInSeconds(int refreshTimeInSeconds) {
		view.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}

	/**
	 * Sets the condition name controlling automatic refresh.
	 *
	 * @param refreshConditionName
	 *            refresh condition identifier
	 * @return this builder
	 */
	public FluentView refreshConditionName(String refreshConditionName) {
		view.setRefreshConditionName(refreshConditionName);
		return this;
	}

	/**
	 * Sets the action name invoked during automatic refresh.
	 *
	 * @param refreshActionName
	 *            refresh action identifier
	 * @return this builder
	 */
	public FluentView refreshActionName(String refreshActionName) {
		view.setRefreshActionName(refreshActionName);
		return this;
	}

	/**
	 * Sets the sidebar for this view.
	 *
	 * @param sidebar
	 *            sidebar builder
	 * @return this builder
	 */
	public FluentView sidebar(FluentSidebar sidebar) {
		view.setSidebar(sidebar.get());
		return this;
	}

	/**
	 * Sets the actions metadata for this view.
	 *
	 * @param actions
	 *            actions builder
	 * @return this builder
	 */
	public FluentView actions(FluentActions actions) {
		view.setActions(actions.get());
		return this;
	}

	/**
	 * Appends a view parameter.
	 *
	 * @param parameter
	 *            parameter builder to append
	 * @return this builder
	 */
	public FluentView addParameter(FluentViewParameter parameter) {
		view.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Removes parameters matching the supplied binding.
	 *
	 * <p>A parameter is removed when {@code boundTo} or {@code fromBinding} equals the
	 * supplied binding.
	 *
	 * @param binding
	 *            binding to remove
	 * @return this builder
	 */
	public FluentView removeParameter(String binding) {
		view.getParameters().removeIf(p -> (binding.equals(p.getBoundTo()) || binding.equals(p.getFromBinding())));
		return this;
	}

	/**
	 * Removes all parameters from this view.
	 *
	 * @return this builder
	 */
	public FluentView clearParameters() {
		view.getParameters().clear();
		return this;
	}

	/**
	 * Finds a parameter that is bound to or sourced from the supplied binding.
	 *
	 * @param binding
	 *            the binding to match against {@code boundTo} and {@code fromBinding}
	 * @return the matching parameter wrapper, or {@code null} when no parameter matches
	 */
	public FluentViewParameter findParameter(String binding) {
		ViewParameter result = view.getParameters().stream()
				.filter(p -> (binding.equals(p.getBoundTo()) || binding.equals(p.getFromBinding()))).findAny().orElse(null);
		if (result != null) {
			return new FluentViewParameter(result);
		}
		return null;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable view metadata
	 */
	@Override
	public ViewMetaData get() {
		return view;
	}

	private @Nonnull ViewUserAccessesMetaData establishViewAccesses() {
		ViewUserAccessesMetaData result = view.getAccesses();
		if (result == null) {
			result = new ViewUserAccessesMetaData();
			result.setGenerate(false);
			view.setAccesses(result);
		}
		
		return result;
	}
	
	/**
	 * Adds a new {@link FluentViewUserAccess} to this module role.
	 */
	private <T extends FluentViewUserAccess<?, ?>> FluentView addAccess(T access) {
		ViewUserAccessesMetaData accesses = establishViewAccesses();
		accesses.getAccesses().add(access.get());
		return this;
	}
}
