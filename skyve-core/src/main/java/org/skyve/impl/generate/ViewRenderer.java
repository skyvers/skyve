package org.skyve.impl.generate;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.MetadataIconResolver;
import org.skyve.impl.metadata.MetadataIconResolver.ResolvedIcon;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractListWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.BeanValidator;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Icons;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base renderer that converts a {@link org.skyve.metadata.view.View}
 * into a client-specific representation.
 *
 * <p>Extends {@link org.skyve.impl.metadata.view.ViewVisitor} to walk the
 * view tree and emit renderer-specific output. Final visitor callbacks maintain
 * traversal state and route each metadata element to an abstract
 * {@code render*} or {@code rendered*} hook. Opening hooks run before nested
 * metadata; closing hooks run after it.
 *
 * <p>Renderer instances are stateful and single-use for one traversal. Context
 * getters expose the active form, data widget, column, binding target, and list
 * model only while the corresponding callback is executing. Nullable strings
 * passed to hooks represent optional authored or localised metadata, not errors.
 *
 * <p>Threading: not thread-safe; one instance per rendered view.
 */
public abstract class ViewRenderer extends ViewVisitor {
	/** User whose permissions and locale govern the rendered output. */
	protected @Nonnull User user;

	/** Whether all forms should render labels above their inputs. */
	protected boolean forceTopFormLabelAlignment = false;

	// Stack of containers sent in to render methods
	private final Deque<Container> currentContainers = new ArrayDeque<>(24); // non-null elements

	/**
	 * Returns the active container stack used during view traversal.
	 *
	 * <p>Top of stack is the current rendering container.
	 *
	 * @return mutable traversal stack used by renderer hooks
	 */
	public @Nonnull Deque<Container> getCurrentContainers() {
		return currentContainers;
	}
	
	// Attributes pushed and popped during internal processing
	private final Deque<String> renderAttributes = new LinkedList<>(); // nullable elements
	
	/**
	 * Creates a renderer bound to the supplied view metadata context.
	 *
	 * @param user the current user context used for visibility and localisation
	 * @param module the module owning the rendered view
	 * @param document the document owning the rendered view
	 * @param view the view metadata to traverse
	 * @param uxui the target UX/UI profile key
	 */
	protected ViewRenderer(@Nonnull User user,
							@Nonnull Module module,
							@Nonnull Document document,
							@Nonnull View view,
							@Nonnull String uxui) {
		super((CustomerImpl) user.getCustomer(), (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view, uxui);
		this.user = user;
		resolvedViewIcon = MetadataIconResolver.resolve(document, view);
	}

	/**
	 * Resolves nullable metadata escape flags to the renderer escape decision.
	 *
	 * @param escape {@code Boolean.FALSE} to allow trusted markup; {@code null} or
	 *        {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return {@code false} only when {@code escape} is {@code Boolean.FALSE}
	 */
	public static boolean shouldEscape(@Nullable Boolean escape) {
		return ! Boolean.FALSE.equals(escape);
	}

	/**
	 * Forces form rendering to use top-aligned labels for the current traversal.
	 *
	 * <p>Side effects: mutates renderer state and affects later form-render decisions.
	 *
	 * @return this renderer for fluent configuration
	 */
	public @Nonnull ViewRenderer forceTopFormLabelAlignment() {
		this.forceTopFormLabelAlignment = true;
		return this;
	}

	private final @Nonnull ResolvedIcon resolvedViewIcon;

	/**
	 * Begins rendering of the root view and pushes it onto the container stack.
	 */
	@Override
	public final void visitView() {
		renderView(resolvedViewIcon);
		currentContainers.push(view);
	}

	// NB View titles are evaluated dynamically for a view

	/**
	 * Renders the opening portion of the root view with its renderer-neutral icon selection.
	 *
	 * @param resolvedIcon icon selected from the view and document metadata; must not be {@code null}
	 */
	public abstract void renderView(@Nonnull ResolvedIcon resolvedIcon);
	
	/**
	 * Finalises rendering of the root view and pops it from the container stack.
	 */
	@Override
	public final void visitedView() {
		renderedView(resolvedViewIcon);
		currentContainers.pop();
	}

	// NB View titles are evaluated dynamically for a view
	/**
	 * Renders the closing portion of the root view with its renderer-neutral icon selection.
	 *
	 * @param resolvedIcon icon selected from the view and document metadata; must not be {@code null}
	 */
	public abstract void renderedView(@Nonnull ResolvedIcon resolvedIcon);
	
	private @Nullable TabPane currentTabPane;

	/**
	 * Returns the tab pane currently being rendered.
	 *
	 * @return current tab pane, or {@code null} when traversal is outside a tab pane
	 */
	public @Nullable TabPane getCurrentTabPane() {
		return currentTabPane;
	}
	
	/**
	 * Begins rendering of a tab pane.
	 *
	 * @param tabPane the tab-pane metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@Override
	public final void visitTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		renderTabPane(tabPane);
		currentTabPane = tabPane;
	}
	
	/**
	 * Renders the opening portion of a tab pane.
	 *
	 * @param tabPane the tab-pane metadata; must not be null
	 */
	public abstract void renderTabPane(@Nonnull TabPane tabPane);
	
	/**
	 * Finalises rendering of a tab pane.
	 *
	 * @param tabPane the tab-pane metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@Override
	public final void visitedTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		renderedTabPane(tabPane);
		currentTabPane = null;
	}

	/**
	 * Renders the closing portion of a tab pane.
	 *
	 * @param tabPane the tab-pane metadata; must not be null
	 */
	public abstract void renderedTabPane(@Nonnull TabPane tabPane);
	
	/**
	 * Begins rendering of a tab and pushes it onto the container stack.
	 *
	 * @param tab the tab metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@Override
	public final void visitTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		String title = tab.getLocalisedTitle();
		String icon16x16Url = iconToUrl(tab.getIcon16x16RelativeFileName());
		renderAttributes.push(icon16x16Url);
		renderAttributes.push(title);
		renderTab(title, icon16x16Url, tab);
		currentContainers.push(tab);
	}

	/**
	 * Renders the opening portion of a tab.
	 *
	 * @param title the localised title, or null when none is configured
	 * @param icon16x16Url the small-icon URL, or null when no image icon is configured
	 * @param tab the tab metadata; must not be null
	 */
	public abstract void renderTab(@Nullable String title, @Nullable String icon16x16Url, @Nonnull Tab tab);
	
	/**
	 * Finalises rendering of a tab and pops it from the container stack.
	 *
	 * @param tab the tab metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@Override
	public final void visitedTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		renderedTab(renderAttributes.pop(), renderAttributes.pop(), tab);
		currentContainers.pop();
	}
	
	/**
	 * Renders the closing portion of a tab.
	 *
	 * @param title the localised title, or null when none is configured
	 * @param icon16x16Url the small-icon URL, or null when no image icon is configured
	 * @param tab the tab metadata; must not be null
	 */
	public abstract void renderedTab(@Nullable String title, @Nullable String icon16x16Url, @Nonnull Tab tab);

	/** {@inheritDoc} */
	@Override
	public final void visitVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		String borderTitle = vbox.getLocalisedBorderTitle();
		renderAttributes.push(borderTitle);
		renderVBox(borderTitle, vbox);
		currentContainers.push(vbox);
	}

	/**
	 * Renders the opening portion of a vertical box container.
	 *
	 * @param borderTitle the localised border title, or null when none is configured
	 * @param vbox the vertical-box metadata; must not be null
	 */
	public abstract void renderVBox(@Nullable String borderTitle, @Nonnull VBox vbox);
	
	/** {@inheritDoc} */
	@Override
	public final void visitedVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		renderedVBox(renderAttributes.pop(), vbox);
		currentContainers.pop();
	}

	/**
	 * Renders the closing portion of a vertical box container.
	 *
	 * @param borderTitle the localised border title, or null when none is configured
	 * @param vbox the vertical-box metadata; must not be null
	 */
	public abstract void renderedVBox(@Nullable String borderTitle, @Nonnull VBox vbox);

	/** {@inheritDoc} */
	@Override
	public final void visitSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		renderSidebar(sidebar);
		currentContainers.push(sidebar);
	}

	/**
	 * Renders the opening portion of a sidebar container.
	 *
	 * @param sidebar the sidebar metadata; must not be null
	 */
	public abstract void renderSidebar(@Nonnull Sidebar sidebar);
	
	/** {@inheritDoc} */
	@Override
	public final void visitedSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		renderedSidebar(sidebar);
		currentContainers.pop();
	}

	/**
	 * Renders the closing portion of a sidebar container.
	 *
	 * @param sidebar the sidebar metadata; must not be null
	 */
	public abstract void renderedSidebar(@Nonnull Sidebar sidebar);
	
	/** {@inheritDoc} */
	@Override
	public final void visitHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		String borderTitle = hbox.getLocalisedBorderTitle();
		renderAttributes.push(borderTitle);
		renderHBox(borderTitle, hbox);
		currentContainers.push(hbox);
	}

	/**
	 * Renders the opening portion of a horizontal box container.
	 *
	 * @param borderTitle the localised border title, or null when none is configured
	 * @param hbox the horizontal-box metadata; must not be null
	 */
	public abstract void renderHBox(@Nullable String borderTitle, @Nonnull HBox hbox);
	
	/** {@inheritDoc} */
	@Override
	public final void visitedHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		renderedHBox(renderAttributes.pop(), hbox);
		currentContainers.pop();
	}

	/**
	 * Renders the closing portion of a horizontal box container.
	 *
	 * @param title the localised title, or null when none is configured
	 * @param hbox the horizontal-box metadata; must not be null
	 */
	public abstract void renderedHBox(@Nullable String title, @Nonnull HBox hbox);
	
	private @Nullable Form currentForm;
	
	/**
	 * Returns the form currently being rendered.
	 *
	 * @return current form, or {@code null} when traversal is outside a form
	 */
	public @Nullable Form getCurrentForm() {
		return currentForm;
	}
	
	private @Nullable String currentFormBorderTitle;
	
	// Is this form defined with top labels or side labels (by module default or form setting)
	private boolean currentFormAuthoredTopLabels = false;
	
	/**
	 * Indicates whether the authored form label layout resolves to top labels.
	 *
	 * @return {@code true} when authored/default form metadata resolves to top labels
	 */
	public boolean isCurrentFormAuthoredTopLabels() {
		return currentFormAuthoredTopLabels;
	}

	// Should this form be rendered with top labels or side labels
	private boolean currentFormRenderTopLabels = false;
	
	/**
	 * Indicates whether the current form should be rendered with top labels.
	 *
	 * @return {@code true} when current rendering should use top-aligned labels
	 */
	public boolean isCurrentFormRenderTopLabels() {
		return forceTopFormLabelAlignment || currentFormRenderTopLabels;
	}

	/** {@inheritDoc} */
	@Override
	public final void visitForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		// If explicitly defined on the form, use that
		FormLabelLayout layout = form.getLabelLayout();
		if (layout != null) {
			currentFormAuthoredTopLabels = (layout == FormLabelLayout.top);
			currentFormRenderTopLabels = currentFormAuthoredTopLabels;
		}
		else {
			// Use the module definition (defaults to side)
			currentFormAuthoredTopLabels = (module.getFormLabelLayout() == FormLabelLayout.top);
			currentFormRenderTopLabels = currentFormAuthoredTopLabels;

			// Use the customer override to render if defined
			if (! currentFormRenderTopLabels) {
				layout = customer.getModuleEntries().get(module.getName());
				currentFormRenderTopLabels = (layout == FormLabelLayout.top);
			}
		}
		
		currentForm = form;
		currentFormBorderTitle = form.getLocalisedBorderTitle();
		currentFormColumnIndex = 0;
		renderForm(currentFormBorderTitle, form);
	}
	
	/**
	 * Renders the form for the active traversal context.
	 *
	 * @param borderTitle the localised border title, or null when none is configured
	 * @param form the form metadata; must not be null
	 */
	public abstract void renderForm(@Nullable String borderTitle, @Nonnull Form form);

	/** {@inheritDoc} */
	@Override
	public final void visitedForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		renderedForm(currentFormBorderTitle, form);
		
		currentForm = null;
		currentFormBorderTitle = null;
		currentFormAuthoredTopLabels = false;
		currentFormRenderTopLabels = false;
	}

	/**
	 * Completes rendering of the form after its nested metadata.
	 *
	 * @param borderTitle the localised border title, or null when none is configured
	 * @param form the form metadata; must not be null
	 */
	public abstract void renderedForm(@Nullable String borderTitle, @Nonnull Form form);

	/** {@inheritDoc} */
	@Override
	public final void visitFormColumn(@Nonnull FormColumn column, boolean parentVisible, boolean parentEnabled) {
		renderFormColumn(column);
	}
	
	/**
	 * Renders the form column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderFormColumn(@Nonnull FormColumn column);

	private @Nullable FormRow currentFormRow;

	/**
	 * Returns the current form row for the current rendering context.
	 * @return the current form row, or null when no matching traversal context is active
	 */
	public @Nullable FormRow getCurrentFormRow() {
		return currentFormRow;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitFormRow(@Nonnull FormRow row, boolean parentVisible, boolean parentEnabled) {
		currentFormRow = row;
		currentFormColumnIndex = 0;
		renderFormRow(row);
	}

	/**
	 * Renders the form row for the active traversal context.
	 *
	 * @param row the row metadata; must not be null
	 */
	public abstract void renderFormRow(@Nonnull FormRow row);

	private int currentFormColumnIndex = 0;

	/**
	 * Advances the active form-column index, wrapping at the end of the form.
	 */
	public void incrementFormColumn() {
		if (currentForm != null) {
			List<FormColumn> formColumns = currentForm.getColumns();
			currentFormColumnIndex++;
			if (currentFormColumnIndex >= formColumns.size()) {
				currentFormColumnIndex = 0;
			}
		}
	}
	/**
	 * Returns the current form column for the current rendering context.
	 * @return the current form column, or null when no matching traversal context is active
	 */
	public @Nullable FormColumn getCurrentFormColumn() {
		if (currentForm != null) {
			return currentForm.getColumns().get(currentFormColumnIndex);
		}
		return null;
	}
	
	private @Nullable FormItem currentFormItem;

	/**
	 * Returns the current form item for the current rendering context.
	 * @return the current form item, or null when no matching traversal context is active
	 */
	public @Nullable FormItem getCurrentFormItem() {
		return currentFormItem;
	}
	
	private @Nullable String currentWidgetLabel;

	/**
	 * Returns the current widget label for the current rendering context.
	 * @return the current widget label, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentWidgetLabel() {
		return currentWidgetLabel;
	}

	private boolean currentWidgetEscapeLabel = true;
	
	/**
	 * Returns whether the resolved current form-item label should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted label markup
	 */
	public boolean getCurrentWidgetEscapeLabel() {
		return currentWidgetEscapeLabel;
	}
	
	private boolean currentWidgetShowLabel;

	/**
	 * Indicates whether the current widget show label applies to the current rendering context.
	 *
	 * @return {@code true} when the current form item resolves to a visible label
	 */
	public boolean isCurrentWidgetShowLabel() {
		return currentWidgetShowLabel;
	}
	
	private boolean currentWidgetRequired;
	private @Nullable String currentWidgetRequiredMessage;

	/**
	 * Returns the current widget required message for the current rendering context.
	 * @return the current widget required message, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentWidgetRequiredMessage() {
		return currentWidgetRequiredMessage;
	}
	
	private boolean currentWidgetEscapeRequiredMessage = true;
	
	/**
	 * Returns whether the resolved current form-item required message should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted message markup
	 */
	public boolean getCurrentWidgetEscapeRequiredMessage() {
		return currentWidgetEscapeRequiredMessage;
	}
	
	private @Nullable String currentWidgetHelp;

	/**
	 * Returns the current widget help for the current rendering context.
	 * @return the current widget help, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentWidgetHelp() {
		return currentWidgetHelp;
	}
	
	private boolean currentWidgetEscapeHelp = true;
	
	/**
	 * Returns whether the resolved current form-item help text should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted help markup
	 */
	public boolean getCurrentWidgetEscapeHelp() {
		return currentWidgetEscapeHelp;
	}
	
	private int currentWidgetColspan = 1;

	/**
	 * Returns the current widget colspan for the current rendering context.
	 *
	 * @return the resolved form-column span; defaults to {@code 1}
	 */
	public int getCurrentWidgetColspan() {
		return currentWidgetColspan;
	}
	
	private @Nullable TargetMetaData currentTarget;

	/**
	 * Returns the current target for the current rendering context.
	 * @return the current target, or null when no matching traversal context is active
	 */
	public @Nullable TargetMetaData getCurrentTarget() {
		return currentTarget;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitFormItem(@Nonnull FormItem item, boolean parentVisible, boolean parentEnabled) {
		currentFormItem = item;
	}

	/**
	 * Renders the form item for the active traversal context.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param requiredMessage the localised required message, or null when the widget is not required
	 * @param help the localised help text, or null when none is configured
	 * @param showsLabel whether the widget type supports a form-item label
	 * @param colspan the resolved number of form columns occupied
	 * @param item the item metadata; must not be null
	 */
	public abstract void renderFormItem(@Nullable String label,
											@Nullable String requiredMessage,
											@Nullable String help,
											boolean showsLabel,
											int colspan,
											@Nonnull FormItem item);
	
	/**
	 * Prepares renderer state for the widget before dispatching a render hook.
	 *
	 * @param binding the widget binding, or null for an unbound widget
	 * @param showsLabelByDefault whether the widget type displays a label when metadata does not override it
	 */
	private void preProcessWidget(@Nullable String binding, boolean showsLabelByDefault) {
		currentWidgetLabel = null;
		currentWidgetEscapeLabel = true;
		currentWidgetShowLabel = false;
		currentWidgetRequired = false;
		currentWidgetRequiredMessage = null;
		currentWidgetEscapeRequiredMessage = true;
		currentWidgetHelp = null;
		currentWidgetEscapeHelp = true;
		currentWidgetColspan = 1;
		currentTarget = null;
		
		String ultimateBinding = binding;
		
		AbstractDataWidget dataWidget = currentDataWidget;
		if (dataWidget != null) {
			if (binding == null) {
				ultimateBinding = dataWidget.getBinding();
			}
			else {
				ultimateBinding = BindUtil.createCompoundBinding(dataWidget.getBinding(), binding);
			}
		}
		if (ultimateBinding != null) {
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, ultimateBinding);
			Document targetDocument = target.getDocument();
			Attribute targetAttribute = target.getAttribute();
			currentTarget = target;
			if (ultimateBinding.endsWith(Bean.BIZ_KEY)) {
				currentWidgetLabel = targetDocument.getLocalisedSingularAlias();
				currentWidgetHelp = targetDocument.getLocalisedDescription();
			}
			else if (ultimateBinding.endsWith(Bean.ORDINAL_NAME)) {
				org.skyve.impl.metadata.model.document.field.Integer bizOrdinalAttribute = DocumentImpl.getBizOrdinalAttribute();
				currentWidgetLabel = bizOrdinalAttribute.getLocalisedDisplayName();
				currentWidgetRequired = bizOrdinalAttribute.isRequired();
				currentWidgetRequiredMessage = bizOrdinalAttribute.getLocalisedRequiredMessage();
				currentWidgetHelp = bizOrdinalAttribute.getLocalisedDescription();
			}
			
			if (targetAttribute != null) {
				currentWidgetLabel = targetAttribute.getLocalisedDisplayName();
				currentWidgetRequired = targetAttribute.isRequired();
				currentWidgetRequiredMessage = targetAttribute.getLocalisedRequiredMessage();
				currentWidgetHelp = targetAttribute.getLocalisedDescription();
			}
			preProcessWidget(false, showsLabelByDefault);
		}
	}
	
	/**
	 * Prepares renderer state for the widget before dispatching a render hook.
	 *
	 * @param clearState whether previously resolved widget state is cleared first
	 * @param showsLabelByDefault whether the widget type displays a label when metadata does not override it
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void preProcessWidget(boolean clearState, boolean showsLabelByDefault) {
		if (clearState) {
			currentWidgetLabel = null;
			currentWidgetEscapeLabel = true;
			currentWidgetHelp = null;
			currentWidgetEscapeHelp = true;
			currentWidgetRequired = false;
			currentWidgetRequiredMessage = null;
			currentWidgetEscapeRequiredMessage = true;
			currentWidgetColspan = 1;
		}
		FormItem formItem = currentFormItem;
		if (formItem != null) {
			String label = formItem.getLocalisedLabel();
			if (label != null) {
				currentWidgetLabel = label;
				currentWidgetEscapeLabel = shouldEscape(formItem.getEscapeLabel());
			}
			String help = formItem.getLocalisedHelp();
			if (help != null) {
				currentWidgetHelp = help;
				currentWidgetEscapeHelp = shouldEscape(formItem.getEscapeHelp());
			}
			Boolean required = formItem.getRequired();
			if (required != null) {
				currentWidgetRequired = required.booleanValue();
			}
			// Check for an overridden message if the current widget is required
			if (currentWidgetRequired) {
				String requiredMessage = formItem.getLocalisedRequiredMessage();
				if (requiredMessage != null) {
					currentWidgetRequiredMessage = requiredMessage;
					currentWidgetEscapeRequiredMessage = shouldEscape(formItem.getEscapeRequiredMessage());
				}
			}
			// Ensure required message is set to the default if widget input is required and there is no message
			if (currentWidgetRequired && (currentWidgetRequiredMessage == null)) {
				currentWidgetRequiredMessage = Util.nullSafeI18n(BeanValidator.VALIDATION_REQUIRED_KEY, currentWidgetLabel);
			}
			
			Integer colspan = formItem.getColspan();
			if (colspan != null) {
				currentWidgetColspan = colspan.intValue();
			}

			Boolean showLabel = formItem.getShowLabel();
			currentWidgetShowLabel = (showLabel == null) ? showsLabelByDefault : showLabel.booleanValue();

			// If showing label and we're rendering top for a side authored form,
			// increment the colspan so we assume the size of the side label column too.
			if (currentWidgetShowLabel && isCurrentFormRenderTopLabels() && (! currentFormAuthoredTopLabels)) {
				currentWidgetColspan++;
			}
			renderFormItem(currentWidgetLabel,
							currentWidgetRequiredMessage,
							currentWidgetHelp,
							currentWidgetShowLabel,
							currentWidgetColspan,
							formItem);
		}
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitedFormItem(@Nonnull FormItem item, boolean parentVisible, boolean parentEnabled) {
		renderedFormItem(currentWidgetLabel,
							currentWidgetRequiredMessage,
							currentWidgetHelp,
							currentWidgetShowLabel,
							currentWidgetColspan,
							item);
		currentFormItem = null;
		currentWidgetRequired = false;
		currentWidgetRequiredMessage = null;
		currentWidgetEscapeRequiredMessage = true;
		currentWidgetLabel = null;
		currentWidgetEscapeLabel = true;
		currentWidgetShowLabel = false;
		currentWidgetHelp = null;
		currentWidgetEscapeHelp = true;
		currentWidgetColspan = 1;
		currentTarget = null;
	}

	/**
	 * Completes rendering of the form item after its nested metadata.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param requiredMessage the localised required message, or null when the widget is not required
	 * @param help the localised help text, or null when none is configured
	 * @param showLabel whether the resolved form-item label is rendered
	 * @param colspan the resolved number of form columns occupied
	 * @param item the item metadata; must not be null
	 */
	public abstract void renderedFormItem(@Nullable String label,
											@Nullable String requiredMessage,
											@Nullable String help,
											boolean showLabel,
											int colspan,
											@Nonnull FormItem item);
	
	/** {@inheritDoc} */
	@Override
	public final void visitedFormRow(@Nonnull FormRow row, boolean parentVisible, boolean parentEnabled) {
		renderedFormRow(row);
		currentFormRow = null;
	}

	/**
	 * Completes rendering of the form row after its nested metadata.
	 *
	 * @param row the row metadata; must not be null
	 */
	public abstract void renderedFormRow(@Nonnull FormRow row);

	private @Nullable String actionName;
	private @Nullable String actionLabel;
	private @Nullable String actionIconUrl;
	private @Nullable String actionIconStyleClass;
	private @Nullable String actionToolTip;
	private @Nullable String actionConfirmationText;
	private boolean actionEscapeDisplayName = true;
	private boolean actionEscapeToolTip = true;
	private boolean actionEscapeConfirm = true;

	/**
	 * Returns whether the resolved current action label should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted label markup
	 */
	public boolean getActionEscapeDisplayName() {
		return actionEscapeDisplayName;
	}

	/**
	 * Returns whether the resolved current action tooltip should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted tooltip markup
	 */
	public boolean getActionEscapeToolTip() {
		return actionEscapeToolTip;
	}

	/**
	 * Returns whether the resolved current action confirmation text should be escaped.
	 *
	 * @return {@code true} to escape at the renderer boundary; {@code false} to allow trusted confirmation markup
	 */
	public boolean getActionEscapeConfirm() {
		return actionEscapeConfirm;
	}
	
	/**
	 * @param action
	 * @return	false if the user does not have privileges to execute the action, otherwise true.
	 * @param implicitName the implicit action name, or null for a custom action
	 * @param showOverride the widget-specific display override, or null to use the action setting
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // Complexity OK
	private boolean preProcessAction(@Nullable ImplicitActionName implicitName, @Nonnull Action action, @Nullable ActionShow showOverride) {
		boolean result = true;
		DocumentImpl actionDocument = document;
		
		String resourceName = action.getResourceName();
		String displayName = action.getLocalisedDisplayName();
		actionEscapeDisplayName = true;
		actionEscapeToolTip = true;
		actionEscapeConfirm = true;
		// Note that the " " result is for SC
		if (displayName != null) {
			actionLabel = displayName;
			actionEscapeDisplayName = shouldEscape(action.getEscapeDisplayName());
		}
		else if (implicitName == null) {
			actionLabel = " ";
		}
		else {
			actionLabel = implicitName.getLocalisedDisplayName();
		}
		String relativeIconFileName = action.getRelativeIconFileName();
		actionIconStyleClass = action.getIconStyleClass();
		actionConfirmationText = action.getConfirmationText(); // NB localised later with the param
		if (actionConfirmationText != null) {
			actionEscapeConfirm = shouldEscape(action.getEscapeConfirm());
		}
		String actionConfirmationParam = null;
		
		if (implicitName == null) {
			if (! user.canExecuteAction(actionDocument, resourceName)) {
				result = false;
			}
			actionName = resourceName;
		}
		else {
			actionName = implicitName.name();
			switch (implicitName) {
				case Add:
					if (! user.canCreateDocument(actionDocument)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_ADD;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_ADD;
					}
					break;
				case BizExport:
					if (! user.canExecuteAction(actionDocument, resourceName)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_BIZ_EXPORT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_BIZ_EXPORT;
					}
					actionName = resourceName;
					break;
				case BizImport:
					if (! user.canExecuteAction(actionDocument, resourceName)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_BIZ_IMPORT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_BIZ_IMPORT;
					}
					actionName = resourceName;
					break;
				case Download:
					if (! user.canExecuteAction(actionDocument, resourceName)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_DOWNLOAD;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_DOWNLOAD;
					}
					actionName = resourceName;
					break;
				case Upload:
					if (! user.canExecuteAction(actionDocument, resourceName)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_UPLOAD;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_UPLOAD;
					}
					actionName = resourceName;
					break;
				case Cancel:
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_CANCEL;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_CANCEL;
					}
					break;
				case Delete:
					if (! user.canDeleteDocument(actionDocument)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_DELETE;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_DELETE;
					}
					if (actionConfirmationText == null) {
						actionConfirmationText = "ui.delete.confirmation";
						actionConfirmationParam = actionDocument.getLocalisedSingularAlias();
					}
					break;
				case Edit:
					if (! user.canReadDocument(actionDocument)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_EDIT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_EDIT;
					}
					break;
				case New:
					if (! user.canCreateDocument(actionDocument)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_NEW;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_NEW;
					}
					break;
				case OK:
					if ((! user.canUpdateDocument(actionDocument)) &&
							(! user.canCreateDocument(actionDocument))) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_OK;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_OK;
					}
					break;
				case Remove:
					if (! user.canDeleteDocument(actionDocument)) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_REMOVE;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_REMOVE;
					}
					if (actionConfirmationText == null) {
						actionConfirmationText = "ui.remove.confirmation";
					}
					break;
				case Report:
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_REPORT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_REPORT;
					}
					break;
				case Save:
					if ((! user.canUpdateDocument(actionDocument)) &&
							(! user.canCreateDocument(actionDocument))) {
						result = false;
					}
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_SAVE;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_SAVE;
					}
					break;
				case ZoomOut:
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_ZOOM_OUT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_ZOOM_OUT;
					}
					break;
				case Print:
					if (relativeIconFileName == null) {
						relativeIconFileName = Icons.IMAGE_PRINT;
					}
					if (actionIconStyleClass == null) {
						actionIconStyleClass = Icons.FONT_PRINT;
					}
					break;
				default:
					throw new IllegalArgumentException(implicitName + " not catered for");
			}
		}

		// remove the icon state or label state if its not meant to be shown
		ActionShow show = (showOverride == null) ? action.getShow() : showOverride;
		if (ActionShow.text == show) {
			relativeIconFileName = null;
			actionIconStyleClass = null;
		}
		else if (ActionShow.icon == show) {
			actionLabel = null;
		}

		actionIconUrl = iconToUrl(relativeIconFileName);
		actionToolTip = action.getLocalisedToolTip();
		if (actionToolTip != null) {
			actionEscapeToolTip = shouldEscape(action.getEscapeToolTip());
		}
		if (actionConfirmationParam != null) {
			actionConfirmationText = Util.i18n(actionConfirmationText, actionConfirmationParam);
		}
		else {
			actionConfirmationText = Util.i18n(actionConfirmationText);
		}
		
		return result;
	}
		
	/** {@inheritDoc} */
	@Override
	public final void visitButton(@Nonnull Button button, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, button.showsLabelByDefault());
		Action action = view.getAction(button.getActionName());
		if (preProcessAction(action.getImplicitName(), action, button.getShow())) {
			if (currentFormItem != null) {
				renderFormButton(actionName,
									actionLabel,
									actionIconUrl,
									actionIconStyleClass,
									actionToolTip,
									actionConfirmationText,
									action,
									button);
			}
			else {
				renderButton(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action,
								button);
			}
		}
		else if (currentForm != null) { // no access to the action but we're in a form
			renderFormSpacer(new Spacer());
		}
	}

	/**
	 * Renders the form button for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 * @param button the button metadata; must not be null
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract void renderFormButton(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull Action action,
											@Nonnull Button button);

	/**
	 * Renders the button for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 * @param button the button metadata; must not be null
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract void renderButton(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull Action action,
										@Nonnull Button button);

	/** {@inheritDoc} */
	@Override
	public final void visitZoomIn(@Nonnull ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(zoomIn.getBinding(), zoomIn.showsLabelByDefault());
		String label = zoomIn.getLocalisedDisplayName();
		String relativeIconFileName = zoomIn.getRelativeIconFileName();
		String iconStyleClass = zoomIn.getIconStyleClass();
		if ((relativeIconFileName == null) && (iconStyleClass == null)) {
			iconStyleClass = Icons.FONT_ZOOM_IN;
		}
		String toolTip = zoomIn.getLocalisedToolTip();
		
		// remove the icon state or label state if its not meant to be rendered
		ActionShow show = zoomIn.getShow();
		if (ActionShow.text == show) {
			relativeIconFileName = null;
			iconStyleClass = null;
		}
		else if (ActionShow.icon == show) {
			label = null;
		}
		
		String iconUrl = iconToUrl(relativeIconFileName);
		if (currentFormItem != null) {
			renderFormZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
		}
		else {
			renderZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
		}
	}

	/**
	 * Renders the form zoom in for the active traversal context.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param zoomIn the zoom in metadata; must not be null
	 */
	public abstract void renderFormZoomIn(@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nonnull ZoomIn zoomIn);

	/**
	 * Renders a zoom-in control outside a form item.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param zoomIn the zoom-in metadata; must not be null
	 */
	public abstract void renderZoomIn(@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nonnull ZoomIn zoomIn);

	/** {@inheritDoc} */
	@Override
	public final void visitMap(@Nonnull MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		renderMap(map);
	}
	
	/**
	 * Renders the map for the active traversal context.
	 *
	 * @param map the map metadata; must not be null
	 */
	public abstract void renderMap(@Nonnull MapDisplay map);
	
	/** {@inheritDoc} */
	@Override
	public final void visitChart(@Nonnull Chart chart, boolean parentVisible, boolean parentEnabled) {
		renderChart(chart);
	}
	
	/**
	 * Renders the chart for the active traversal context.
	 *
	 * @param chart the chart metadata; must not be null
	 */
	public abstract void renderChart(@Nonnull Chart chart);
	

	/** {@inheritDoc} */
	@Override
	public final void visitGeometry(@Nonnull Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(geometry.getBinding(), geometry.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnGeometry(geometry);
		}
		else {
			renderFormGeometry(geometry);
		}
	}

	/**
	 * Renders the bound column geometry for the active traversal context.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderBoundColumnGeometry(@Nonnull Geometry geometry);

	/**
	 * Renders the form geometry for the active traversal context.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderFormGeometry(@Nonnull Geometry geometry);

	/**
	 * Completes rendering of the bound column geometry after its nested metadata.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderedBoundColumnGeometry(@Nonnull Geometry geometry);

	/**
	 * Completes rendering of the form geometry after its nested metadata.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderedFormGeometry(@Nonnull Geometry geometry);

	/** {@inheritDoc} */
	@Override
	public final void visitedGeometry(@Nonnull Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnGeometry(geometry);
		}
		else {
			renderedFormGeometry(geometry);
		}
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitGeometryMap(@Nonnull GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(geometry.getBinding(), geometry.showsLabelByDefault());
		renderFormGeometryMap(geometry);
	}
	
	/**
	 * Renders the form geometry map for the active traversal context.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderFormGeometryMap(@Nonnull GeometryMap geometry);

	/**
	 * Completes rendering of the form geometry map after its nested metadata.
	 *
	 * @param geometry the geometry metadata; must not be null
	 */
	public abstract void renderedFormGeometryMap(@Nonnull GeometryMap geometry);
	
	/** {@inheritDoc} */
	@Override
	public final void visitedGeometryMap(@Nonnull GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		renderedFormGeometryMap(geometry);
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitDialogButton(@Nonnull DialogButton button, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, button.showsLabelByDefault());
		String label = button.getLocalisedDisplayName();
		if (currentFormItem != null) {
			renderFormDialogButton(label, button);
		}
		else {
			renderDialogButton(label, button);
		}
	}

	/**
	 * Renders the form dialog button for the active traversal context.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param button the button metadata; must not be null
	 */
	public abstract void renderFormDialogButton(@Nullable String label, @Nonnull DialogButton button);

	/**
	 * Renders the dialog button for the active traversal context.
	 *
	 * @param label the localised label, or null when no label is rendered
	 * @param button the button metadata; must not be null
	 */
	public abstract void renderDialogButton(@Nullable String label, @Nonnull DialogButton button);
	
	/** {@inheritDoc} */
	@Override
	public final void visitSpacer(@Nonnull Spacer spacer) {
		preProcessWidget(true, spacer.showsLabelByDefault());
		if (currentFormItem != null) {
			renderFormSpacer(spacer);
		}
		else {
			renderSpacer(spacer);
		}
	}
	
	/**
	 * Renders the form spacer for the active traversal context.
	 *
	 * @param spacer the spacer metadata; must not be null
	 */
	public abstract void renderFormSpacer(@Nonnull Spacer spacer);

	/**
	 * Renders the spacer for the active traversal context.
	 *
	 * @param spacer the spacer metadata; must not be null
	 */
	public abstract void renderSpacer(@Nonnull Spacer spacer);

	/** {@inheritDoc} */
	@Override
	public final void visitStaticImage(@Nonnull StaticImage image, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, image.showsLabelByDefault());
		String fileUrl = staticImageToUrl(image.getRelativeFile());
		if (currentFormItem != null) {
			renderFormStaticImage(fileUrl, image);
		}
		else if (currentContainerColumn != null) {
			renderContainerColumnStaticImage(fileUrl, image);
		}
		else {
			renderStaticImage(fileUrl, image);
		}
	}

	/**
	 * Renders the form static image for the active traversal context.
	 *
	 * @param fileUrl the resolved image URL; must not be null
	 * @param image the image metadata; must not be null
	 */
	public abstract void renderFormStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image);

	/**
	 * Renders the static image for the active traversal context.
	 *
	 * @param fileUrl the resolved image URL; must not be null
	 * @param image the image metadata; must not be null
	 */
	public abstract void renderStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image);

	/**
	 * Renders the container column static image for the active traversal context.
	 *
	 * @param fileUrl the resolved image URL; must not be null
	 * @param image the image metadata; must not be null
	 */
	public abstract void renderContainerColumnStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image);
	
	/** {@inheritDoc} */
	@Override
	public final void visitDynamicImage(@Nonnull DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		if (currentContainerColumn != null) {
			renderContainerColumnDynamicImage(image);
		}
		else {
			renderDynamicImage(image);
		}
	}
	
	/**
	 * Renders the container column dynamic image for the active traversal context.
	 *
	 * @param image the image metadata; must not be null
	 */
	public abstract void renderContainerColumnDynamicImage(@Nonnull DynamicImage image);

	/**
	 * Renders the dynamic image for the active traversal context.
	 *
	 * @param image the image metadata; must not be null
	 */
	public abstract void renderDynamicImage(@Nonnull DynamicImage image);
	
	/** {@inheritDoc} */
	@Override
	public final void visitLink(@Nonnull Link link, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, link.showsLabelByDefault());
		String value = link.getLocalisedValue();
		if (currentFormItem != null) {
			renderFormLink(value, link);
		}
		else if (currentContainerColumn != null) {
			renderContainerColumnLink(value, link);
		}
		else {
			renderLink(value, link);
		}
	}
	
	/**
	 * Renders the form link for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param link the link metadata; must not be null
	 */
	public abstract void renderFormLink(@Nullable String value, @Nonnull Link link);

	/**
	 * Renders the container column link for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param link the link metadata; must not be null
	 */
	public abstract void renderContainerColumnLink(@Nullable String value, @Nonnull Link link);

	/**
	 * Renders the link for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param link the link metadata; must not be null
	 */
	public abstract void renderLink(@Nullable String value, @Nonnull Link link);

	/** {@inheritDoc} */
	@Override
	public final void visitBlurb(@Nonnull Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, blurb.showsLabelByDefault());
		String markup = blurb.getLocalisedMarkup();
		if (currentFormItem != null) {
			renderFormBlurb(markup, blurb);
		}
		else if (currentContainerColumn != null) {
			renderContainerColumnBlurb(markup, blurb);
		}
		else {
			renderBlurb(markup, blurb);
		}
	}

	/**
	 * Renders the form blurb for the active traversal context.
	 *
	 * @param markup the localised markup, or null when absent
	 * @param blurb the blurb metadata; must not be null
	 */
	public abstract void renderFormBlurb(@Nullable String markup, @Nonnull Blurb blurb);

	/**
	 * Renders the container column blurb for the active traversal context.
	 *
	 * @param markup the localised markup, or null when absent
	 * @param blurb the blurb metadata; must not be null
	 */
	public abstract void renderContainerColumnBlurb(@Nullable String markup, @Nonnull Blurb blurb);

	/**
	 * Renders the blurb for the active traversal context.
	 *
	 * @param markup the localised markup, or null when absent
	 * @param blurb the blurb metadata; must not be null
	 */
	public abstract void renderBlurb(@Nullable String markup, @Nonnull Blurb blurb);

	/** {@inheritDoc} */
	@Override
	public final void visitLabel(@Nonnull Label label, boolean parentVisible, boolean parentEnabled) {
		String value = null;
		boolean boundValue = false; 

		String binding = label.getBinding();
		String faw = label.getFor();
		if (faw != null) {
			preProcessWidget(faw, label.showsLabelByDefault());
			value = currentWidgetLabel;
			if (value == null) {
				value = "Label";
			}
			value += (currentWidgetRequired ? " *:" : " :");
		}
		else if (binding != null) {
			preProcessWidget(binding, label.showsLabelByDefault());
		}
		else {
			preProcessWidget(true, label.showsLabelByDefault());
			value = label.getLocalisedValue();
			boundValue = (value != null) && BindUtil.containsSkyveExpressions(value);
			currentTarget = null;
		}
		if (currentFormItem != null) {
			renderFormLabel(value, boundValue, label);
		}
		else if (currentContainerColumn != null) {
			renderContainerColumnLabel(value, label);
		}
		else {
			renderLabel(value, boundValue, label);
		}
	}

	/**
	 * Renders the form label for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param boundValue whether the value contains a Skyve binding expression
	 * @param label the localised label, or null when no label is rendered
	 */
	public abstract void renderFormLabel(@Nullable String value, boolean boundValue, @Nonnull Label label);

	/**
	 * Renders the container column label for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param label the localised label, or null when no label is rendered
	 */
	public abstract void renderContainerColumnLabel(@Nullable String value, @Nonnull Label label);

	/**
	 * Renders the label for the active traversal context.
	 *
	 * @param value the resolved or authored value, or null when absent
	 * @param boundValue whether the value contains a Skyve binding expression
	 * @param label the localised label, or null when no label is rendered
	 */
	public abstract void renderLabel(@Nullable String value, boolean boundValue, @Nonnull Label label);

	/** {@inheritDoc} */
	@Override
	public final void visitProgressBar(@Nonnull ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(progressBar.getBinding(), progressBar.showsLabelByDefault());
		renderFormProgressBar(progressBar);
	}
	
	/**
	 * Renders the form progress bar for the active traversal context.
	 *
	 * @param progressBar the progress bar metadata; must not be null
	 */
	public abstract void renderFormProgressBar(@Nonnull ProgressBar progressBar);
	
	private @Nullable String currentTabularTitle;

	private @Nullable String currentListWidgetModelName;

	/**
	 * Returns the current list widget model name for the current rendering context.
	 * @return the current list widget model name, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentListWidgetModelName() {
		return currentListWidgetModelName;
	}
	
	private @Nullable String currentListWidgetModelDocumentName;

	/**
	 * Returns the current list widget model document name for the current rendering context.
	 * @return the current list widget model document name, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentListWidgetModelDocumentName() {
		return currentListWidgetModelDocumentName;
	}
	
	private @Nullable ListModel<Bean> currentListWidgetModel;

	/**
	 * Returns the current list widget model for the current rendering context.
	 * @return the current list widget model, or null when no matching traversal context is active
	 */
	public @Nullable ListModel<Bean> getCurrentListWidgetModel() {
		return currentListWidgetModel;
	}
	
	private boolean currentListWidgetAggregateQuery;
	
	/**
	 * Prepares renderer state for the list widget before dispatching a render hook.
	 *
	 * @param widget the widget metadata; must not be null
	 * @return the resolved runtime list model; never null
	 */
	private @Nonnull ListModel<Bean> preProcessListWidget(@Nonnull AbstractListWidget widget) {
		currentTabularTitle = widget.getLocalisedTitle();

		String queryName = widget.getQueryName();
		String modelName = widget.getModelName();
		ListModel<Bean> listModel;
		
		if (queryName == null) {
			if (modelName == null) {
				throw new MetaDataException("Abstract List Widget " + widget.getTitle() + " does not have a query or model");
			}

			currentListWidgetModelName = modelName;
			DocumentImpl listDocument = document;
			currentListWidgetModelDocumentName = listDocument.getName();
			listModel = listDocument.getListModel(customer, modelName, true);
			currentListWidgetModel = listModel;
			currentListWidgetAggregateQuery = false;
		}
		else {
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, queryName);
			}
			currentListWidgetModelName = queryName;
			currentListWidgetModelDocumentName = query.getDocumentName();
			// Don't need the runtime list model here and EXT is not available.
			DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>(query);
	        queryModel.postConstruct(customer, false);
	        listModel = queryModel;
	        currentListWidgetModel = listModel;
	        currentListWidgetAggregateQuery = query.isAggregate();
		}

		return listModel;
	}
	
	/**
	 * Clears renderer state retained for the list widget.
	 */
	private void postProcessListWidget() {
		currentTabularTitle = null;
		currentListWidgetModelName = null;
		currentListWidgetModelDocumentName = null;
		currentListWidgetModel = null;
		currentListWidgetAggregateQuery = false;
	}

	/** {@inheritDoc} */
	@Override
	public final void visitListGrid(@Nonnull ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		ListModel<Bean> listModel = preProcessListWidget(grid);
		renderListGrid(currentTabularTitle, currentListWidgetAggregateQuery, grid);
		
		for (MetaDataQueryColumn column : listModel.getColumns()) {
			if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
				renderListGridProjectedColumn(projectedColumn);
			}
			else {
				renderListGridContentColumn((MetaDataQueryContentColumn) column);
			}
		}
	}

	/**
	 * Renders the list grid for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param aggregateQuery whether the backing query contains aggregate projections
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderListGrid(@Nullable String title, boolean aggregateQuery, @Nonnull ListGrid grid);

	/**
	 * Renders the list grid projected column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderListGridProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column);

	/**
	 * Renders the list grid content column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderListGridContentColumn(@Nonnull MetaDataQueryContentColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitedListGrid(@Nonnull ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		renderedListGrid(currentTabularTitle, currentListWidgetAggregateQuery, grid);
		postProcessListWidget();
	}

	/**
	 * Completes rendering of the list grid after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param aggregateQuery whether the backing query contains aggregate projections
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderedListGrid(@Nullable String title, boolean aggregateQuery, @Nonnull ListGrid grid);

	/** {@inheritDoc} */
	@Override
	public final void visitListRepeater(@Nonnull ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		ListModel<Bean> listModel = preProcessListWidget(repeater);
		renderListRepeater(currentTabularTitle, repeater);

		for (MetaDataQueryColumn column : listModel.getColumns()) {
			if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
				renderListRepeaterProjectedColumn(projectedColumn);
			}
			else {
				renderListRepeaterContentColumn((MetaDataQueryContentColumn) column);
			}
		}
	}

	/**
	 * Renders the list repeater for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param repeater the repeater metadata; must not be null
	 */
	public abstract void renderListRepeater(@Nullable String title, @Nonnull ListRepeater repeater);

	/**
	 * Renders the list repeater projected column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderListRepeaterProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column);

	/**
	 * Renders the list repeater content column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderListRepeaterContentColumn(@Nonnull MetaDataQueryContentColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitedListRepeater(@Nonnull ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		renderedListRepeater(currentTabularTitle, repeater);
		postProcessListWidget();
	}

	/**
	 * Completes rendering of the list repeater after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param repeater the repeater metadata; must not be null
	 */
	public abstract void renderedListRepeater(@Nullable String title, @Nonnull ListRepeater repeater);

	/** {@inheritDoc} */
	@Override
	public final void visitTreeGrid(@Nonnull TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		ListModel<Bean> listModel = preProcessListWidget(grid);
		renderTreeGrid(currentTabularTitle, grid);

		for (MetaDataQueryColumn column : listModel.getColumns()) {
			if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
				renderTreeGridProjectedColumn(projectedColumn);
			}
			else {
				renderTreeGridContentColumn((MetaDataQueryContentColumn) column);
			}
		}
	}

	/**
	 * Renders the tree grid for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderTreeGrid(@Nullable String title, @Nonnull TreeGrid grid);

	/**
	 * Renders the tree grid projected column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderTreeGridProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column);

	/**
	 * Renders the tree grid content column for the active traversal context.
	 *
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderTreeGridContentColumn(@Nonnull MetaDataQueryContentColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitedTreeGrid(@Nonnull TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		renderedTreeGrid(currentTabularTitle, grid);
		postProcessListWidget();
	}

	/**
	 * Completes rendering of the tree grid after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderedTreeGrid(@Nullable String title, @Nonnull TreeGrid grid);

	private @Nullable AbstractDataWidget currentDataWidget;

	/**
	 * Returns the current data widget for the current rendering context.
	 * @return the current data widget, or null when no matching traversal context is active
	 */
	public @Nullable AbstractDataWidget getCurrentDataWidget() {
		return currentDataWidget;
	}
	
	private @Nullable TargetMetaData currentDataWidgetTarget;

	/** {@inheritDoc} */
	@Override
	public final void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(grid.getBinding(), false);
		currentDataWidgetTarget = currentTarget;
		currentTabularTitle = grid.getLocalisedTitle();
		currentDataWidget = grid;
		renderDataGrid(currentTabularTitle, grid);
	}

	/**
	 * Renders the data grid for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderDataGrid(@Nullable String title, @Nonnull DataGrid grid);

	/** {@inheritDoc} */
	@Override
	public final void visitedDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		currentTarget = currentDataWidgetTarget;
		renderedDataGrid(currentTabularTitle, grid);
		currentDataWidgetTarget = null;
		currentTabularTitle = null;
		currentDataWidget = null;
	}

	/**
	 * Completes rendering of the data grid after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param grid the grid metadata; must not be null
	 */
	public abstract void renderedDataGrid(@Nullable String title, @Nonnull DataGrid grid);

	/** {@inheritDoc} */
	@Override
	public final void visitDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(repeater.getBinding(), false);
		currentDataWidgetTarget = currentTarget;
		currentTabularTitle = repeater.getLocalisedTitle();
		currentDataWidget = repeater;
		renderDataRepeater(currentTabularTitle, repeater);
	}

	/**
	 * Renders the data repeater for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param repeater the repeater metadata; must not be null
	 */
	public abstract void renderDataRepeater(@Nullable String title, @Nonnull DataRepeater repeater);

	/** {@inheritDoc} */
	@Override
	public final void visitedDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		currentTarget = currentDataWidgetTarget;
		renderedDataRepeater(currentTabularTitle, repeater);
		currentDataWidgetTarget = null;
		currentTabularTitle = null;
		currentDataWidget = null;
	}

	/**
	 * Completes rendering of the data repeater after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param repeater the repeater metadata; must not be null
	 */
	public abstract void renderedDataRepeater(@Nullable String title, @Nonnull DataRepeater repeater);

	private @Nullable String currentColumnTitle;

	/**
	 * Returns the current column title for the current rendering context.
	 * @return the current column title, or null when no matching traversal context is active
	 */
	public @Nullable String getCurrentColumnTitle() {
		return currentColumnTitle;
	}
	private @Nullable DataGridBoundColumn currentBoundColumn;

	/**
	 * Returns the current bound column for the current rendering context.
	 * @return the current bound column, or null when no matching traversal context is active
	 */
	public @Nullable DataGridBoundColumn getCurrentBoundColumn() {
		return currentBoundColumn;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		currentColumnTitle = column.getLocalisedTitle();
		preProcessWidget(column.getBinding(), false);
		if (currentColumnTitle == null) {
			currentColumnTitle = currentWidgetLabel;
		}
		currentBoundColumn = column;
		if (currentDataWidget instanceof DataGrid) {
			renderDataGridBoundColumn(currentColumnTitle, column);
		}
		else {
			renderDataRepeaterBoundColumn(currentColumnTitle, column);
		}
	}

	/**
	 * Renders the data repeater bound column for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderDataRepeaterBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column);

	/**
	 * Renders the data grid bound column for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderDataGridBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitedDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		if (currentDataWidget instanceof DataGrid) {
			renderedDataGridBoundColumn(currentColumnTitle, column);
		}
		else {
			renderedDataRepeaterBoundColumn(currentColumnTitle, column);
		}
		currentColumnTitle = null;
		currentBoundColumn = null;
	}

	/**
	 * Completes rendering of the data repeater bound column after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderedDataRepeaterBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column);

	/**
	 * Completes rendering of the data grid bound column after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderedDataGridBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column);

	private @Nullable DataGridContainerColumn currentContainerColumn;

	/**
	 * Returns the current container column for the current rendering context.
	 * @return the current container column, or null when no matching traversal context is active
	 */
	public @Nullable DataGridContainerColumn getCurrentContainerColumn() {
		return currentContainerColumn;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visitDataGridContainerColumn(@Nonnull DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		currentColumnTitle = column.getLocalisedTitle();
		currentContainerColumn = column;
		if (currentDataWidget instanceof DataGrid) {
			renderDataGridContainerColumn(currentColumnTitle, column);
		}
		else {
			renderDataRepeaterContainerColumn(currentColumnTitle, column);
		}
	}

	/**
	 * Renders the data repeater container column for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderDataRepeaterContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column);

	/**
	 * Renders the data grid container column for the active traversal context.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderDataGridContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitedDataGridContainerColumn(@Nonnull DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		if (currentDataWidget instanceof DataGrid) {
			renderedDataRepeaterContainerColumn(currentColumnTitle, column);
		}
		else {
			renderedDataGridContainerColumn(currentColumnTitle, column);
		}
		currentColumnTitle = null;
		currentContainerColumn = null;
	}

	/**
	 * Completes rendering of the data repeater container column after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderedDataRepeaterContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column);

	/**
	 * Completes rendering of the data grid container column after its nested metadata.
	 *
	 * @param title the localised title, or null when no title is configured
	 * @param column the column metadata; must not be null
	 */
	public abstract void renderedDataGridContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column);

	/** {@inheritDoc} */
	@Override
	public final void visitCheckBox(@Nonnull CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(checkBox.getBinding(), checkBox.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnCheckBox(checkBox);
		}
		else {
			renderFormCheckBox(checkBox);
		}
	}

	/**
	 * Renders the bound column check box for the active traversal context.
	 *
	 * @param checkBox the check box metadata; must not be null
	 */
	public abstract void renderBoundColumnCheckBox(@Nonnull CheckBox checkBox);

	/**
	 * Renders the form check box for the active traversal context.
	 *
	 * @param checkBox the check box metadata; must not be null
	 */
	public abstract void renderFormCheckBox(@Nonnull CheckBox checkBox);

	/** {@inheritDoc} */
	@Override
	public final void visitedCheckBox(@Nonnull CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnCheckBox(checkBox);
		}
		else {
			renderedFormCheckBox(checkBox);
		}
	}

	/**
	 * Completes rendering of the bound column check box after its nested metadata.
	 *
	 * @param checkBox the check box metadata; must not be null
	 */
	public abstract void renderedBoundColumnCheckBox(@Nonnull CheckBox checkBox);

	/**
	 * Completes rendering of the form check box after its nested metadata.
	 *
	 * @param checkBox the check box metadata; must not be null
	 */
	public abstract void renderedFormCheckBox(@Nonnull CheckBox checkBox);

	/** {@inheritDoc} */
	@Override
	public final void visitCheckMembership(@Nonnull CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(membership.getBinding(), false);
		renderCheckMembership(membership);
	}

	/**
	 * Renders the check membership for the active traversal context.
	 *
	 * @param membership the membership metadata; must not be null
	 */
	public abstract void renderCheckMembership(@Nonnull CheckMembership membership);

	/** {@inheritDoc} */
	@Override
	public final void visitedCheckMembership(@Nonnull CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		renderedCheckMembership(membership);
	}

	/**
	 * Completes rendering of the check membership after its nested metadata.
	 *
	 * @param membership the membership metadata; must not be null
	 */
	public abstract void renderedCheckMembership(@Nonnull CheckMembership membership);

	/** {@inheritDoc} */
	@Override
	public final void visitColourPicker(@Nonnull ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(colour.getBinding(), colour.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnColourPicker(colour);
		}
		else {
			renderFormColourPicker(colour);
		}
	}

	/**
	 * Renders the bound column colour picker for the active traversal context.
	 *
	 * @param colour the colour metadata; must not be null
	 */
	public abstract void renderBoundColumnColourPicker(@Nonnull ColourPicker colour);

	/**
	 * Renders the form colour picker for the active traversal context.
	 *
	 * @param colour the colour metadata; must not be null
	 */
	public abstract void renderFormColourPicker(@Nonnull ColourPicker colour);

	/** {@inheritDoc} */
	@Override
	public final void visitedColourPicker(@Nonnull ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnColourPicker(colour);
		}
		else {
			renderedFormColourPicker(colour);
		}
	}

	/**
	 * Completes rendering of the bound column colour picker after its nested metadata.
	 *
	 * @param colour the colour metadata; must not be null
	 */
	public abstract void renderedBoundColumnColourPicker(@Nonnull ColourPicker colour);

	/**
	 * Completes rendering of the form colour picker after its nested metadata.
	 *
	 * @param colour the colour metadata; must not be null
	 */
	public abstract void renderedFormColourPicker(@Nonnull ColourPicker colour);

	/** {@inheritDoc} */
	@Override
	public final void visitCombo(@Nonnull Combo combo, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(combo.getBinding(), combo.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnCombo(combo);
		}
		else {
			renderFormCombo(combo);
		}
	}
	
	/**
	 * Renders the bound column combo for the active traversal context.
	 *
	 * @param combo the combo metadata; must not be null
	 */
	public abstract void renderBoundColumnCombo(@Nonnull Combo combo);

	/**
	 * Renders the form combo for the active traversal context.
	 *
	 * @param combo the combo metadata; must not be null
	 */
	public abstract void renderFormCombo(@Nonnull Combo combo);

	/** {@inheritDoc} */
	@Override
	public final void visitedCombo(@Nonnull Combo combo, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnCombo(combo);
		}
		else {
			renderedFormCombo(combo);
		}
	}

	/**
	 * Completes rendering of the bound column combo after its nested metadata.
	 *
	 * @param combo the combo metadata; must not be null
	 */
	public abstract void renderedBoundColumnCombo(@Nonnull Combo combo);

	/**
	 * Completes rendering of the form combo after its nested metadata.
	 *
	 * @param combo the combo metadata; must not be null
	 */
	public abstract void renderedFormCombo(@Nonnull Combo combo);
	
	/**
	 * Dispatches a content upload to the active rendering context.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 * @param parentVisible whether ancestor metadata is visible
	 * @param parentEnabled whether ancestor metadata is enabled
	 */
	@Override
	public final void visitContent(@Nonnull ContentUpload content, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(content.getBinding(), content.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnContent(content);
		}
		else if (currentContainerColumn != null) {
			renderContainerColumnContent(content);
		}
		else {
			renderFormContent(content);
		}
	}
	
	/**
	 * Renders content for a bound data-grid column.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 * @throws MetaDataException until a concrete renderer implements content rendering
	 */
	@SuppressWarnings("static-method")
	public void renderBoundColumnContent(@Nonnull ContentUpload content) {
		throw new MetaDataException("Content upload rendering is not implemented for this renderer: " + content.getBinding());
	}

	/**
	 * Renders content for a container data-grid column.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 * @throws MetaDataException until a concrete renderer implements content rendering
	 */
	@SuppressWarnings("static-method")
	public void renderContainerColumnContent(@Nonnull ContentUpload content) {
		throw new MetaDataException("Content upload rendering is not implemented for this renderer: " + content.getBinding());
	}

	/**
	 * Renders content for a form item.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 * @throws MetaDataException until a concrete renderer implements content rendering
	 */
	@SuppressWarnings("static-method")
	public void renderFormContent(@Nonnull ContentUpload content) {
		throw new MetaDataException("Content upload rendering is not implemented for this renderer: " + content.getBinding());
	}

	/** {@inheritDoc} */
	@Override
	public final void visitContentSignature(@Nonnull ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(signature.getBinding(), signature.showsLabelByDefault());
		renderFormContentSignature(signature);
	}

	/**
	 * Renders the form content signature for the active traversal context.
	 *
	 * @param signature the signature metadata; must not be null
	 */
	public abstract void renderFormContentSignature(@Nonnull ContentSignature signature);

	/** {@inheritDoc} */
	@Override
	public final void visitHTML(@Nonnull HTML html, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(html.getBinding(), html.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnHTML(html);
		}
		else {
			renderFormHTML(html);
		}
	}
	
	/**
	 * Renders the bound column html for the active traversal context.
	 *
	 * @param html the HTML-widget metadata; must not be null
	 */
	public abstract void renderBoundColumnHTML(@Nonnull HTML html);

	/**
	 * Renders the form html for the active traversal context.
	 *
	 * @param html the HTML-widget metadata; must not be null
	 */
	public abstract void renderFormHTML(@Nonnull HTML html);

	private @Nullable String listMembershipCandidatesHeading;
	private @Nullable String listMembershipMembersHeading;
	
	/** {@inheritDoc} */
	@Override
	public final void visitListMembership(@Nonnull ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(membership.getBinding(), false);
		listMembershipCandidatesHeading = membership.getLocalisedCandidatesHeading();
		listMembershipMembersHeading = membership.getLocalisedMembersHeading();
		renderListMembership(listMembershipCandidatesHeading,
								listMembershipMembersHeading,
								membership);
	}

	/**
	 * Renders the list membership for the active traversal context.
	 *
	 * @param candidatesHeading the localised candidates heading, or null when not configured
	 * @param membersHeading the localised members heading, or null when not configured
	 * @param membership the membership metadata; must not be null
	 */
	public abstract void renderListMembership(@Nullable String candidatesHeading,
												@Nullable String membersHeading,
												@Nonnull ListMembership membership);

	/** {@inheritDoc} */
	@Override
	public final void visitedListMembership(@Nonnull ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		renderedListMembership(listMembershipCandidatesHeading,
								listMembershipMembersHeading,
								membership);
		
		listMembershipCandidatesHeading = null;
		listMembershipMembersHeading = null;
	}

	/**
	 * Completes rendering of the list membership after its nested metadata.
	 *
	 * @param candidatesHeading the localised candidates heading, or null when not configured
	 * @param membersHeading the localised members heading, or null when not configured
	 * @param membership the membership metadata; must not be null
	 */
	public abstract void renderedListMembership(@Nullable String candidatesHeading,
													@Nullable String membersHeading,
													@Nonnull ListMembership membership);

	/** {@inheritDoc} */
	@Override
	public final void visitComparison(@Nonnull Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(comparison.getBinding(), false);
		renderComparison(comparison);
	}

	/**
	 * Renders the comparison for the active traversal context.
	 *
	 * @param comparison the comparison metadata; must not be null
	 */
	public abstract void renderComparison(@Nonnull Comparison comparison);
	
	private @Nullable MetaDataQueryDefinition currentLookupQuery;
	private boolean currentLookupCanCreate;
	private boolean currentLookupCanUpdate;
	
	/**
	 * Prepares renderer state for the lookup widget before dispatching a render hook.
	 *
	 * @param binding the widget binding, or null for an unbound widget
	 * @param widgetQueryName the explicitly configured query name, or null to derive one from the relation
	 * @param widgetModelName the enclosing document's list model, or null for query resolution
	 * @param showsLabelByDefault whether the widget type displays a label when metadata does not override it
	 */
	private void preProcessLookupWidget(@Nullable String binding, @Nullable String widgetQueryName, @Nullable String widgetModelName, boolean showsLabelByDefault) {
		preProcessWidget(binding, showsLabelByDefault);
		if (widgetModelName != null) {
			ListModel<Bean> model = document.getListModel(customer, widgetModelName, true);
			Document drivingDocument = model.getDrivingDocument();
			if (drivingDocument == null) {
				throw new MetaDataException("Lookup model " + widgetModelName + " has no driving document.");
			}
			currentLookupQuery = null;
			currentLookupCanCreate = user.canCreateDocument(drivingDocument);
			currentLookupCanUpdate = user.canUpdateDocument(drivingDocument);
			return;
		}

		String queryName = widgetQueryName;
		// Use reference query name if none provided in the widget
		TargetMetaData target = currentTarget;
		if (target == null) {
			throw new MetaDataException("Lookup widget " + binding + " has no target metadata.");
		}
		Attribute targetAttribute = target.getAttribute();
		if ((queryName == null) && (targetAttribute instanceof Reference reference)) {
			queryName = reference.getQueryName();
		}
		// Use the default query if none is defined, else get the named query.
		if ((queryName == null) && (targetAttribute instanceof Relation relation)) {
			currentLookupQuery = module.getDocumentDefaultQuery(customer, relation.getDocumentName());
		}
		else if (queryName != null) {
			currentLookupQuery = module.getMetaDataQuery(queryName);
		}

		if (currentLookupQuery == null) {
			throw new MetaDataException("Lookup widget " + binding + " has no query to use.");
		}
		
		Document queryDocument = module.getDocument(customer, currentLookupQuery.getDocumentName());
		currentLookupCanCreate = user.canCreateDocument(queryDocument);
		currentLookupCanUpdate = user.canUpdateDocument(queryDocument);
	}

	private @Nullable String currentLookupDescriptionBinding;

	/** {@inheritDoc} */
	@Override
	public final void visitLookupDescription(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		preProcessLookupWidget(lookup.getBinding(), lookup.getQuery(), lookup.getModelName(), lookup.showsLabelByDefault());

		MetaDataQueryDefinition lookupQuery = currentLookupQuery;
		if ((lookupQuery == null) && (lookup.getModelName() == null)) {
			throw new MetaDataException("Lookup description has no query to use.");
		}
		String descriptionBinding = lookup.getDescriptionBinding();
		if (descriptionBinding == null) {
			descriptionBinding = Bean.BIZ_KEY;
		}
		currentLookupDescriptionBinding = descriptionBinding;

		if (currentBoundColumn != null) {
			renderBoundColumnLookupDescription(lookupQuery,
												currentLookupCanCreate,
												currentLookupCanUpdate,
												descriptionBinding,
												lookup);
		}
		else {
			renderFormLookupDescription(lookupQuery,
											currentLookupCanCreate,
											currentLookupCanUpdate,
											descriptionBinding,
											lookup);
		}
	}

	/**
	 * Renders the bound column lookup description for the active traversal context.
	 *
	 * @param query the resolved lookup query, or null for a model lookup
	 * @param canCreate whether the user may create records through the lookup
	 * @param canUpdate whether the user may update records through the lookup
	 * @param descriptionBinding the resolved lookup description binding; must not be null
	 * @param lookup the lookup metadata; must not be null
	 */
	public abstract void renderBoundColumnLookupDescription(@Nullable MetaDataQueryDefinition query,
																boolean canCreate,
																boolean canUpdate,
																@Nonnull String descriptionBinding,
																@Nonnull LookupDescription lookup);

	/**
	 * Renders a lookup-description widget in a form item.
	 *
	 * @param query the resolved lookup query, or null for a model lookup
	 * @param canCreate whether the user may create records through the lookup
	 * @param canUpdate whether the user may update records through the lookup
	 * @param descriptionBinding the resolved lookup description binding; must not be null
	 * @param lookup the lookup metadata; must not be null
	 */
	public abstract void renderFormLookupDescription(@Nullable MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														@Nonnull String descriptionBinding,
														@Nonnull LookupDescription lookup);

	/** {@inheritDoc} */
	@Override
	public final void visitedLookupDescription(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		MetaDataQueryDefinition lookupQuery = currentLookupQuery;
		if ((lookupQuery == null) && (lookup.getModelName() == null)) {
			throw new MetaDataException("Lookup description has no query to use.");
		}
		String descriptionBinding = currentLookupDescriptionBinding;
		if (descriptionBinding == null) {
			throw new MetaDataException("Lookup description has no description binding to use.");
		}

		if (currentBoundColumn != null) {
			renderedBoundColumnLookupDescription(lookupQuery,
													currentLookupCanCreate,
													currentLookupCanUpdate,
													descriptionBinding,
													lookup);
		}
		else {
			renderedFormLookupDescription(lookupQuery,
												currentLookupCanCreate,
												currentLookupCanUpdate,
												descriptionBinding,
												lookup);
		}
		
		currentLookupQuery = null;
		currentLookupCanCreate = false;
		currentLookupCanUpdate = false;
		currentLookupDescriptionBinding = null;
	}

	/**
	 * Completes rendering of the bound column lookup description after its nested metadata.
	 *
	 * @param query the resolved lookup query, or null for a model lookup
	 * @param canCreate whether the user may create records through the lookup
	 * @param canUpdate whether the user may update records through the lookup
	 * @param descriptionBinding the resolved lookup description binding; must not be null
	 * @param lookup the lookup metadata; must not be null
	 */
	public abstract void renderedBoundColumnLookupDescription(@Nullable MetaDataQueryDefinition query,
																boolean canCreate,
																boolean canUpdate,
																@Nonnull String descriptionBinding,
																@Nonnull LookupDescription lookup);

	/**
	 * Completes rendering of a form lookup-description widget.
	 *
	 * @param query the resolved lookup query, or null for a model lookup
	 * @param canCreate whether the user may create records through the lookup
	 * @param canUpdate whether the user may update records through the lookup
	 * @param descriptionBinding the resolved lookup description binding; must not be null
	 * @param lookup the lookup metadata; must not be null
	 */
	public abstract void renderedFormLookupDescription(@Nullable MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														@Nonnull String descriptionBinding,
														@Nonnull LookupDescription lookup);

	/** {@inheritDoc} */
	@Override
	public final void visitPassword(@Nonnull Password password, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(password.getBinding(), password.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnPassword(password);
		}
		else {
			renderFormPassword(password);
		}
	}

	/**
	 * Renders the bound column password for the active traversal context.
	 *
	 * @param password the password metadata; must not be null
	 */
	public abstract void renderBoundColumnPassword(@Nonnull Password password);

	/**
	 * Renders the form password for the active traversal context.
	 *
	 * @param password the password metadata; must not be null
	 */
	public abstract void renderFormPassword(@Nonnull Password password);

	/** {@inheritDoc} */
	@Override
	public final void visitedPassword(@Nonnull Password password, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnPassword(password);
		}
		else {
			renderedFormPassword(password);
		}
	}

	/**
	 * Completes rendering of the bound column password after its nested metadata.
	 *
	 * @param password the password metadata; must not be null
	 */
	public abstract void renderedBoundColumnPassword(@Nonnull Password password);

	/**
	 * Completes rendering of the form password after its nested metadata.
	 *
	 * @param password the password metadata; must not be null
	 */
	public abstract void renderedFormPassword(@Nonnull Password password);

	/** {@inheritDoc} */
	@Override
	public final void visitRadio(@Nonnull Radio radio, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(radio.getBinding(), radio.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnRadio(radio);
		}
		else {
			renderFormRadio(radio);
		}
	}

	/**
	 * Renders the bound column radio for the active traversal context.
	 *
	 * @param radio the radio metadata; must not be null
	 */
	public abstract void renderBoundColumnRadio(@Nonnull Radio radio);

	/**
	 * Renders the form radio for the active traversal context.
	 *
	 * @param radio the radio metadata; must not be null
	 */
	public abstract void renderFormRadio(@Nonnull Radio radio);

	/** {@inheritDoc} */
	@Override
	public final void visitedRadio(@Nonnull Radio radio, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnRadio(radio);
		}
		else {
			renderedFormRadio(radio);
		}
	}

	/**
	 * Completes rendering of the bound column radio after its nested metadata.
	 *
	 * @param radio the radio metadata; must not be null
	 */
	public abstract void renderedBoundColumnRadio(@Nonnull Radio radio);

	/**
	 * Completes rendering of the form radio after its nested metadata.
	 *
	 * @param radio the radio metadata; must not be null
	 */
	public abstract void renderedFormRadio(@Nonnull Radio radio);

	/** {@inheritDoc} */
	@Override
	public final void visitRichText(@Nonnull RichText text, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(text.getBinding(), text.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnRichText(text);
		}
		else {
			renderFormRichText(text);
		}
	}
	
	/**
	 * Renders the bound column rich text for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderBoundColumnRichText(@Nonnull RichText text);

	/**
	 * Renders the form rich text for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderFormRichText(@Nonnull RichText text);

	/** {@inheritDoc} */
	@Override
	public final void visitedRichText(@Nonnull RichText text, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnRichText(text);
		}
		else {
			renderedFormRichText(text);
		}
	}

	/**
	 * Completes rendering of the bound column rich text after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedBoundColumnRichText(@Nonnull RichText text);

	/**
	 * Completes rendering of the form rich text after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedFormRichText(@Nonnull RichText text);

	/** {@inheritDoc} */
	@Override
	public final void visitSlider(@Nonnull Slider slider, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(slider.getBinding(), slider.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnSlider(slider);
		}
		else {
			renderFormSlider(slider);
		}
	}

	/**
	 * Renders the bound column slider for the active traversal context.
	 *
	 * @param slider the slider metadata; must not be null
	 */
	public abstract void renderBoundColumnSlider(@Nonnull Slider slider);

	/**
	 * Renders the form slider for the active traversal context.
	 *
	 * @param slider the slider metadata; must not be null
	 */
	public abstract void renderFormSlider(@Nonnull Slider slider);

	/** {@inheritDoc} */
	@Override
	public final void visitedSlider(@Nonnull Slider slider, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnSlider(slider);
		}
		else {
			renderedFormSlider(slider);
		}
	}

	/**
	 * Completes rendering of the bound column slider after its nested metadata.
	 *
	 * @param slider the slider metadata; must not be null
	 */
	public abstract void renderedBoundColumnSlider(@Nonnull Slider slider);

	/**
	 * Completes rendering of the form slider after its nested metadata.
	 *
	 * @param slider the slider metadata; must not be null
	 */
	public abstract void renderedFormSlider(@Nonnull Slider slider);

	/** {@inheritDoc} */
	@Override
	public final void visitSpinner(@Nonnull Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(spinner.getBinding(), spinner.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnSpinner(spinner);
		}
		else {
			renderFormSpinner(spinner);
		}
	}

	/**
	 * Renders the bound column spinner for the active traversal context.
	 *
	 * @param spinner the spinner metadata; must not be null
	 */
	public abstract void renderBoundColumnSpinner(@Nonnull Spinner spinner);

	/**
	 * Renders the form spinner for the active traversal context.
	 *
	 * @param spinner the spinner metadata; must not be null
	 */
	public abstract void renderFormSpinner(@Nonnull Spinner spinner);

	/** {@inheritDoc} */
	@Override
	public final void visitedSpinner(@Nonnull Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnSpinner(spinner);
		}
		else {
			renderedFormSpinner(spinner);
		}
	}

	/**
	 * Completes rendering of the bound column spinner after its nested metadata.
	 *
	 * @param spinner the spinner metadata; must not be null
	 */
	public abstract void renderedBoundColumnSpinner(@Nonnull Spinner spinner);

	/**
	 * Completes rendering of the form spinner after its nested metadata.
	 *
	 * @param spinner the spinner metadata; must not be null
	 */
	public abstract void renderedFormSpinner(@Nonnull Spinner spinner);

	/** {@inheritDoc} */
	@Override
	public final void visitTextArea(@Nonnull TextArea text, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(text.getBinding(), text.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnTextArea(text);
		}
		else {
			renderFormTextArea(text);
		}
	}

	/**
	 * Renders the bound column text area for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderBoundColumnTextArea(@Nonnull TextArea text);

	/**
	 * Renders the form text area for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderFormTextArea(@Nonnull TextArea text);

	/** {@inheritDoc} */
	@Override
	public final void visitedTextArea(@Nonnull TextArea text, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnTextArea(text);
		}
		else {
			renderedFormTextArea(text);
		}
	}

	/**
	 * Completes rendering of the bound column text area after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedBoundColumnTextArea(@Nonnull TextArea text);

	/**
	 * Completes rendering of the form text area after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedFormTextArea(@Nonnull TextArea text);
	
	/** {@inheritDoc} */
	@Override
	public final void visitTextField(@Nonnull TextField text, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(text.getBinding(), text.showsLabelByDefault());
		if (currentBoundColumn != null) {
			renderBoundColumnTextField(text);
		}
		else {
			renderFormTextField(text);
		}
	}

	/**
	 * Renders the bound column text field for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderBoundColumnTextField(@Nonnull TextField text);

	/**
	 * Renders the form text field for the active traversal context.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderFormTextField(@Nonnull TextField text);

	/** {@inheritDoc} */
	@Override
	public final void visitedTextField(@Nonnull TextField text, boolean parentVisible, boolean parentEnabled) {
		if (currentBoundColumn != null) {
			renderedBoundColumnTextField(text);
		}
		else {
			renderedFormTextField(text);
		}
	}

	/**
	 * Completes rendering of the bound column text field after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedBoundColumnTextField(@Nonnull TextField text);

	/**
	 * Completes rendering of the form text field after its nested metadata.
	 *
	 * @param text the text metadata; must not be null
	 */
	public abstract void renderedFormTextField(@Nonnull TextField text);
	
	/** {@inheritDoc} */
	@Override
	public final void visitInject(@Nonnull Inject inject, boolean parentVisible, boolean parentEnabled) {
		preProcessWidget(true, false);
		if (currentFormItem != null) {
			renderFormInject(inject);
		}
		else {
			renderInject(inject);
		}
	}
	
	/**
	 * Renders the form inject for the active traversal context.
	 *
	 * @param inject the inject metadata; must not be null
	 */
	public abstract void renderFormInject(@Nonnull Inject inject);

	/**
	 * Renders the inject for the active traversal context.
	 *
	 * @param inject the inject metadata; must not be null
	 */
	public abstract void renderInject(@Nonnull Inject inject);


	/** {@inheritDoc} */
	@Override
	public final void visitCustomAction(@Nonnull ActionImpl action) {
		if (preProcessAction(null, action, null)) {
			renderCustomAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the custom action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderCustomAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitAddAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Add, action, null)) {
			renderAddAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}
	
	/**
	 * Renders the add action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderAddAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitRemoveAction(@Nonnull ActionImpl action) {
		boolean canDelete = preProcessAction(ImplicitActionName.Remove, action, null);
		renderRemoveAction(actionName,
							actionLabel,
							actionIconUrl,
							actionIconStyleClass,
							actionToolTip,
							actionConfirmationText,
							action,
							canDelete);
	}

	/**
	 * Renders the remove action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 * @param canDelete whether the user may delete records through the action
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract void renderRemoveAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action,
												boolean canDelete);

	/** {@inheritDoc} */
	@Override
	public final void visitZoomOutAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.ZoomOut, action, null)) {
			renderZoomOutAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the zoom out action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderZoomOutAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);
	
	/** {@inheritDoc} */
	@Override
	public final void visitNavigateAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Navigate, action, null)) {
			renderNavigateAction(actionName,
									actionLabel,
									actionIconUrl,
									actionIconStyleClass,
									actionToolTip,
									actionConfirmationText,
									action);
		}
	}

	/**
	 * Renders the navigate action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderNavigateAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitOKAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.OK, action, null)) {
			renderOKAction(actionName,
							actionLabel,
							actionIconUrl,
							actionIconStyleClass,
							actionToolTip,
							actionConfirmationText,
							action);
		}
	}

	/**
	 * Renders the OK action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderOKAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitSaveAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Save, action, null)) {
			renderSaveAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the save action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderSaveAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitCancelAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Cancel, action, null)) {
			renderCancelAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the cancel action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderCancelAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitDeleteAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Delete, action, null)) {
			renderDeleteAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the delete action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderDeleteAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitReportAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Report, action, null)) {
			renderReportAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the report action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderReportAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitBizExportAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.BizExport, action, null)) {
			renderBizExportAction(actionName,
									actionLabel,
									actionIconUrl,
									actionIconStyleClass,
									actionToolTip,
									actionConfirmationText,
									action);
		}
	}

	/**
	 * Renders the business-export action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderBizExportAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitBizImportAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.BizImport, action, null)) {
			renderBizImportAction(actionName,
									actionLabel,
									actionIconUrl,
									actionIconStyleClass,
									actionToolTip,
									actionConfirmationText,
									action);
		}
	}

	/**
	 * Renders the business-import action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderBizImportAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitDownloadAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Download, action, null)) {
			renderDownloadAction(actionName,
									actionLabel,
									actionIconUrl,
									actionIconStyleClass,
									actionToolTip,
									actionConfirmationText,
									action);
		}
	}

	/**
	 * Renders the download action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderDownloadAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitUploadAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Upload, action, null)) {
			renderUploadAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the upload action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderUploadAction(@Nullable String name,
												@Nullable String label,
												@Nullable String iconUrl,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText,
												@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitNewAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.New, action, null)) {
			renderNewAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the new action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderNewAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitEditAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Edit, action, null)) {
			renderEditAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}

	/**
	 * Renders the edit action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderEditAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitPrintAction(@Nonnull ActionImpl action) {
		if (preProcessAction(ImplicitActionName.Print, action, null)) {
			renderPrintAction(actionName,
								actionLabel,
								actionIconUrl,
								actionIconStyleClass,
								actionToolTip,
								actionConfirmationText,
								action);
		}
	}
	
	/**
	 * Renders the print action for the active traversal context.
	 *
	 * @param name the resolved action name, or null when metadata supplies none
	 * @param label the localised label, or null when no label is rendered
	 * @param iconUrl the resolved action icon URL, or null when no image icon is rendered
	 * @param iconStyleClass the action font-icon classes, or null when no font icon is rendered
	 * @param toolTip the localised tooltip, or null when none is configured
	 * @param confirmationText the localised confirmation text, or null when confirmation is not required
	 * @param action the action metadata; must not be null
	 */
	public abstract void renderPrintAction(@Nullable String name,
											@Nullable String label,
											@Nullable String iconUrl,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText,
											@Nonnull ActionImpl action);

	/** {@inheritDoc} */
	@Override
	public final void visitServerSideActionEventAction(@Nonnull ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		Action action = view.getAction(server.getActionName());
		visitServerSideActionEventAction(action, server);
	}
	
	/**
	 * Visits and renders the server side action event action in the current renderer context.
	 *
	 * @param action the action metadata; must not be null
	 * @param server the server metadata; must not be null
	 */
	public abstract void visitServerSideActionEventAction(@Nonnull Action action, @Nonnull ServerSideActionEventAction server);

	/**
	 * Resolves the default pixel width for the supplied attribute type.
	 *
	 * @param attributeType the attribute type whose default width is required; must not be null
	 * @return the default width in pixels, or null when the type has no renderer default
	 */
	@SuppressWarnings("static-method")
	public @Nullable Integer determineDefaultColumnWidth(@Nonnull AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType)) {
			return Integer.valueOf(100);
		}
		if (AttributeType.dateTime.equals(attributeType)) {
			return Integer.valueOf(125);
		}
		if (AttributeType.time.equals(attributeType)) {
			return Integer.valueOf(75);
		}
		if (AttributeType.timestamp.equals(attributeType)) {
			return Integer.valueOf(125);
		}
		if (AttributeType.bool.equals(attributeType)) {
			return Integer.valueOf(75);
		}

		return null;
	}

	/**
	 * Converts an optional document-relative icon name into its renderer URL.
	 *
	 * @param icon the document-relative icon name, or null when no image icon is configured
	 * @return the renderer URL, or null when {@code icon} is null
	 */
	private @Nullable String iconToUrl(@Nullable String icon) {
		if (icon == null) {
			return null;
		}
		return String.format("resources?_doc=%s.%s&_n=%s", module.getName(), document.getName(), icon);
	}
	
	/**
	 * Converts an image-relative path into its static-image URL.
	 *
	 * @param imagePath the image-relative path; must not be null
	 * @return the static-image URL; never null
	 */
	private static @Nonnull String staticImageToUrl(@Nonnull String imagePath) {
		return "images/" + imagePath;
	}
}
