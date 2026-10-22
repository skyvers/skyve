package org.skyve.impl.generate.client;

import java.util.Deque;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.MetadataIconResolver.ResolvedIcon;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.ReferenceProcessor;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
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
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base view renderer for client-code generation targets.
 *
 * <p>
 * Bridges generic {@link org.skyve.impl.generate.ViewRenderer} traversal to
 * client-specific component/layout renderers.
 */
public class ClientViewRenderer extends ViewRenderer {
	/** Renderer for leaf and action components. */
	protected ComponentRenderer cr;
	/** Renderer for container/layout structures. */
	protected LayoutRenderer lr;
	/** Indicates whether the current view is the create variant. */
	protected boolean createView;

	private RenderedComponent current; // current component being constructed
	private RenderedComponent result; // the result of construction
	private List<RenderedComponent> toolbarLayouts; // the toolbar layouts

	/**
	 * Creates a client-oriented renderer for the supplied Skyve view metadata.
	 *
	 * @param user     the current user context
	 * @param module   the module owning the rendered view
	 * @param document the document owning the rendered view
	 * @param view     the view metadata to render
	 * @param uxui     the target UX/UI profile
	 */
	public ClientViewRenderer(@Nonnull User user, @Nonnull Module module, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui) {
		super(user, module, document, view, uxui);
		createView = ViewType.create.toString().equals(view.getName());
	}

	/**
	 * Sets concrete component and layout renderers used for this traversal.
	 *
	 * @param cr component renderer implementation
	 * @param lr layout renderer implementation
	 */
	protected void setRenderers(@Nonnull ComponentRenderer cr, @Nonnull LayoutRenderer lr) {
		this.cr = cr;
		this.lr = lr;
	}

	/**
	 * Returns the rendered root component produced by {@link #visitView()}
	 * traversal.
	 *
	 * @return rendered root component, or {@code null} before rendering starts
	 */
	public @Nullable RenderedComponent getResult() {
		return result;
	}

	/**
	 * Returns the rendered root component once the view has been visited.
	 *
	 * @return rendered root component; never {@code null}
	 * @throws IllegalStateException when the view has not been rendered yet
	 */
	public @Nonnull RenderedComponent getNullSafeResult() {
		if (result == null) {
			throw new IllegalStateException("The view has not been rendered");
		}
		return result;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 *
	 * @param resolvedIcon resolved root-view icon metadata
	 */
	@Override
	public void renderView(@Nonnull ResolvedIcon resolvedIcon) {
		// Ensure visibility is set for both create and edit views
		current = cr.view(null, createView ? "created" : "notCreated");
		result = current;

		// Create the toolbar(s)
		toolbarLayouts = lr.toolbarLayouts(null);

		// Add the view layout if defined
		RenderedComponent layout = lr.viewLayout(null);
		if (layout != null) {
			current.addChild(layout);
			current = layout;
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 *
	 * @param resolvedIcon resolved root-view icon metadata
	 */
	@Override
	public void renderedView(@Nonnull ResolvedIcon resolvedIcon) {
		// Add the toolbar(s) if it/they has/have contents
    	if ((toolbarLayouts != null) && (! toolbarLayouts.isEmpty()) && (! toolbarLayouts.get(0).isLeaf())) {
			// If we get any toolbars back, add the toolbar layouts to it
			List<RenderedComponent> toolbars = cr.toolbars(null, view.getActionsWidgetId());
			if (toolbars != null) {
				if (toolbars.size() != toolbarLayouts.size()) {
					throw new IllegalStateException(String.format("The component Builder %s yielded %d toolbars but Layout Builder %s yielded %d toolbar layouts", 
																	cr.getClass().getName(),
																	Integer.valueOf(toolbars.size()), 
																	lr.getClass().getName(),
																	Integer.valueOf(toolbarLayouts.size())));
				}
				lr.addToolbarLayouts(toolbars, toolbarLayouts);
				lr.addToolbarsOrLayouts(result, toolbars);
			}
			else {
				lr.addToolbarsOrLayouts(result, toolbarLayouts);
			}
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderTabPane(@Nonnull TabPane tabPane) {
		RenderedComponent component = cr.tabPane(null, tabPane);
		addToContainer(component, 
        				tabPane.getPixelWidth(), 
        				tabPane.getResponsiveWidth(), 
        				tabPane.getPercentageWidth(),
        				tabPane.getInvisibleCondition());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedTabPane(@Nonnull TabPane tabPane) {
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderTab(@Nullable String title, @Nullable String icon16x16Url, @Nonnull Tab tab) {
		RenderedComponent component = cr.tab(null, title, tab);
		lr.addTab(current, component);
		current = component;
		RenderedComponent layout = lr.tabLayout(null);
		if (layout != null) {
			current = lr.addTabLayout(null, component, layout);
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedTab(@Nullable String title, @Nullable String icon16x16Url, @Nonnull Tab tab) {
		current = lr.addedTab(null, current);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderVBox(@Nullable String borderTitle, @Nonnull VBox vbox) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			border = cr.border(null, borderTitle, vbox.getInvisibleCondition(), vbox.getPixelWidth());
			addToContainer(border, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth(),
							vbox.getInvisibleCondition());
		}

		RenderedComponent layout = lr.vboxLayout(null, vbox);

		// Cater for border if defined
		if (border != null) {
			lr.addBorderLayout(border, layout);
		}
		else {
			addToContainer(layout, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth(),
							vbox.getInvisibleCondition());
		}
		current = layout;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedVBox(@Nullable String borderTitle, @Nonnull VBox vbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderHBox(@Nullable String borderTitle, @Nonnull HBox hbox) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			border = cr.border(null, borderTitle, hbox.getInvisibleCondition(), hbox.getPixelWidth());
			addToContainer(border, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth(),
							hbox.getInvisibleCondition());
		}

		RenderedComponent layout = lr.hboxLayout(null, hbox);

		// Cater for border if defined
		if (border != null) {
			lr.addBorderLayout(border, layout);
		}
		else {
			addToContainer(layout, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth(),
							hbox.getInvisibleCondition());
		}
		current = layout;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedHBox(@Nullable String title, @Nonnull HBox hbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderForm(@Nullable String borderTitle, @Nonnull Form form) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(form.getBorder())) {
			border = cr.border(null, borderTitle, form.getInvisibleCondition(), form.getPixelWidth());
			addToContainer(border, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth(),
							form.getInvisibleCondition());
		}

		RenderedComponent layout = lr.formLayout(null, form);

		// Cater for border if defined
		if (border != null) {
			lr.addBorderLayout(border, layout);
		}
		else {
			addToContainer(layout, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth(),
							form.getInvisibleCondition());
		}
		current = layout;
// TODO form.getDisabledCondition() form.getLabelDefaultHorizontalAlignment()
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedForm(@Nullable String borderTitle, @Nonnull Form form) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(form.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormColumn(@Nonnull FormColumn column) {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	private RenderedComponent formRowLayout = null;

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderFormRow(@Nonnull FormRow row) {
		formRowLayout = lr.formRowLayout(null, row);
		if (formRowLayout != null) {
			current = lr.addFormRowLayout(null, current, formRowLayout);
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormItem(@Nullable String label,
								@Nullable String requiredMessage,
								@Nullable String help,
								boolean showsLabel,
								int colspan,
								@Nonnull FormItem item) {
		// TODO not implemented yet
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormItem(@Nullable String label,
									@Nullable String requiredMessage,
									@Nullable String help,
									boolean showLabel,
									int colspan,
									@Nonnull FormItem item) {
		// TODO not implemented yet
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormRow(@Nonnull FormRow row) {
		if (formRowLayout != null) {
			current = lr.addedFormRowLayout(null, formRowLayout);
		}
		formRowLayout = null;
	}

	/**
	 * Returns the data grid or data repeater whose columns are being rendered.
	 *
	 * @return the current data widget; never {@code null}
	 * @throws IllegalStateException when no data widget is being rendered
	 */
	private @Nonnull AbstractDataWidget getNullSafeCurrentDataWidget() {
		AbstractDataWidget dataWidget = getCurrentDataWidget();
		if (dataWidget == null) { // cannot happen as data widget columns are only visited within a data widget
			throw new IllegalStateException("Data widget column is rendered outside of a data widget");
		}
		return dataWidget;
	}

	@SuppressWarnings({"java:S107", "java:S3776"}) // Long parameter list preserves the existing framework/API contract; complexity OK.
	private void addComponent(@Nullable String widgetLabel,
								int formColspan,
								@Nullable String widgetRequiredMessage,
								@Nullable String widgetInvisible,
								@Nullable String helpText,
								@Nullable RenderedComponent component,
								@Nullable Integer pixelWidth,
								@Nullable Integer responsiveWidth,
								@Nullable Integer percentageWidth) {
		if (component == null) {
			return;
		}

		DataGridBoundColumn currentBoundColumn = getCurrentBoundColumn();
		if (currentBoundColumn != null) { // bound column in a data grid or data repeater
			// Add editing component if we have an inline data grid and the current column is editable
			boolean columnEditable = ! Boolean.FALSE.equals(currentBoundColumn.getEditable());
			if (columnEditable) { // NB short circuit test
				AbstractDataWidget currentDataWidget = getCurrentDataWidget();
				boolean inline = (currentDataWidget instanceof DataGrid grid) ? 
									Boolean.TRUE.equals(grid.getInline()) :
									true;
				if (inline) {
					current.addChild(component);
				}
			}
		}
		else { // not a bound column in a data grid or data repeater
			Form currentForm = getCurrentForm();
			if (currentForm == null) { // not a form item
				DataGridContainerColumn currentContainerColumn = getCurrentContainerColumn();
				if (currentContainerColumn != null) { // container column in a data grid or data repeater
					// add a spacer, if required
					if (! current.isLeaf()) {
						current.addChild(cr.label(null, " "));
					}
					current.addChild(component);
				}
				else {  // This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, responsiveWidth, percentageWidth, widgetInvisible);
					addedToContainer();
				}
			}
			else { // a form item
				FormItem formItem = getCurrentFormItem();
				FormColumn formColumn = getCurrentFormColumn();
				if ((formItem == null) || (formColumn == null)) { // cannot happen as a form widget is always within a form item and column
					throw new IllegalStateException("Form widget " + widgetLabel + " is not within a form item and column");
				}
				if (isCurrentWidgetShowLabel()) {
					lr.layoutFormItemLabel(current,
											component,
											currentForm, 
											formItem, 
											formColumn,
											widgetLabel,
											widgetRequiredMessage,
											widgetInvisible,
											helpText);
					incrementFormColumn();
				}
				lr.layoutFormItemWidget(current,
											component,
											currentForm, 
											formItem, 
											formColumn,
											widgetLabel,
											formColspan,
											widgetRequiredMessage,
											widgetInvisible,
											helpText);
				for (int i = 0, l = formColspan; i< l; i++) {
					incrementFormColumn();
				}
			}
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormButton(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull Action action,
									@Nonnull Button button) {
		renderButton(name,
						label,
						getCurrentWidgetColspan(),
						iconUrl,
						iconStyleClass,
						toolTip,
						confirmationText,
						action,
						button);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderButton(@Nullable String name,
								@Nullable String label,
								@Nullable String iconUrl,
								@Nullable String iconStyleClass,
								@Nullable String toolTip,
								@Nullable String confirmationText,
								@Nonnull Action action,
								@Nonnull Button button) {
		renderButton(name, label, 0, iconUrl, iconStyleClass, toolTip, confirmationText, action, button);
	}

	@SuppressWarnings({"java:S107", "unused"}) // Long parameter list preserves the existing framework/API contract.
	private void renderButton(@Nullable String name,
								@Nullable String label,
								int formColspan,
								@Nullable String iconUrl,
								@Nullable String iconStyleClass,
								@Nullable String toolTip,
								@Nullable String confirmationText,
								@Nonnull Action action,
								@Nonnull Button button) {
		ImplicitActionName type = action.getImplicitName();
		RenderedComponent c = null;
		if (ImplicitActionName.Report.equals(type)) {
			c = cr.reportButton(null, button, action);
		}
		else if (ImplicitActionName.Download.equals(type)) {
			c = cr.downloadButton(null, button, action, module.getName(), document.getName());
		}
		else {
			c = cr.actionButton(null, dataWidgetBinding, dataWidgetVar, button, action);
		}
	    addComponent(null, 
	    				formColspan,
	    				null, 
	    				action.getInvisibleCondition(), 
	    				null,
	    				c, 
	    				button.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormZoomIn(@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nonnull ZoomIn zoomIn) {
		renderZoomIn(label, getCurrentWidgetColspan(), iconUrl, iconStyleClass, toolTip, zoomIn);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderZoomIn(@Nullable String label,
								@Nullable String iconUrl,
								@Nullable String iconStyleClass,
								@Nullable String toolTip,
								@Nonnull ZoomIn zoomIn) {
		renderZoomIn(label, 0, iconUrl, iconStyleClass, toolTip, zoomIn);
	}

	@SuppressWarnings("unused")
	private void renderZoomIn(@Nullable String label,
								int formColspan,
								@Nullable String iconUrl,
								@Nullable String iconStyleClass,
								@Nullable String toolTip,
								@Nonnull ZoomIn zoomIn) {
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "zoomIn " + label); // TODO geometry
		eventSource = c;
	    addComponent(null, 
	    				formColspan,
	    				null, 
	    				zoomIn.getInvisibleCondition(), 
	    				null,
	    				c, 
	    				zoomIn.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderBoundColumnGeometry(@Nonnull Geometry geometry) {
		renderGeometry(0, geometry);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderFormGeometry(@Nonnull Geometry geometry) {
		renderGeometry(getCurrentWidgetColspan(), geometry);
	}

	/**
	 * Renders a geometry placeholder component for either form or bound-column
	 * contexts.
	 *
	 * @param formColspan the form column span to apply when rendering inside a form
	 * @param geometry    the geometry widget metadata being rendered
	 */
	public void renderGeometry(int formColspan, @Nonnull Geometry geometry) {
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "geometry"); // TODO geometry
		eventSource = c;
	    addComponent(null,
	    				formColspan,
	    				null, 
	    				geometry.getInvisibleCondition(), 
	    				null,
	    				c, 
	    				geometry.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnGeometry(@Nonnull Geometry geometry) {
		renderedFormGeometry(geometry);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormGeometry(@Nonnull Geometry geometry) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormGeometryMap(@Nonnull GeometryMap geometry) {
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "geometryMap"); // TODO geometryMap
		eventSource = c;
	    addComponent(null, 
	    				getCurrentWidgetColspan(),
	    				null, 
	    				geometry.getInvisibleCondition(), 
	    				null,
	    				c, 
	    				geometry.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormGeometryMap(@Nonnull GeometryMap geometry) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderMap(@Nonnull MapDisplay map) {
		RenderedComponent l = cr.label(null, "map"); // TODO map
	    addComponent(null, 
	    				0,
	    				null, 
	    				map.getInvisibleCondition(), 
	    				null,
	    				l, 
	    				map.getPixelWidth(), 
	    				map.getResponsiveWidth(),
	    				map.getPercentageWidth());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderChart(@Nonnull Chart chart) {
		RenderedComponent l = cr.label(null, "chart"); // TODO chart
	    addComponent(null, 
	    				0,
	    				null, 
	    				chart.getInvisibleCondition(), 
	    				null,
	    				l, 
	    				chart.getPixelWidth(), 
	    				chart.getResponsiveWidth(),
	    				chart.getPercentageWidth());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormDialogButton(@Nullable String label, @Nonnull DialogButton button) {
		renderDialogButton(label, getCurrentWidgetColspan(), button);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderDialogButton(@Nullable String label, @Nonnull DialogButton button) {
		renderDialogButton(label, 0, button);
	}

	/**
	 * Renders a dialog-button placeholder with explicit form colspan handling.
	 */
	private void renderDialogButton(@SuppressWarnings("unused") @Nullable String label, int formColspan, @Nonnull DialogButton button) {
		RenderedComponent bn = cr.label(null, "dialogButton"); // TODO dialog button
	    addComponent(null, 
	    				formColspan,
	    				null, 
	    				button.getInvisibleCondition(), 
	    				null,
	    				bn, 
	    				null, 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderContainerColumnDynamicImage(@Nonnull DynamicImage image) {
		renderDynamicImage(image);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDynamicImage(@Nonnull DynamicImage image) {
		RenderedComponent i = cr.dynamicImage(null, image, module.getName(), document.getName());
		addComponent(null, 
						0,
						null, 
						image.getInvisibleCondition(), 
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormSpacer(@Nonnull Spacer spacer) {
		renderSpacer(getCurrentWidgetColspan(), spacer);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderSpacer(@Nonnull Spacer spacer) {
		renderSpacer(0, spacer);
	}

	/**
	 * Renders a spacer component with explicit form colspan handling.
	 */
	private void renderSpacer(int formColspan, @Nonnull Spacer spacer) {
		RenderedComponent component = cr.spacer(null, spacer);
		if (component != null) {
			addComponent(null, 
							formColspan,
							null, 
							spacer.getInvisibleCondition(), 
							null,
							component, 
							spacer.getPixelWidth(), 
							null, 
							null);
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image) {
		renderStaticImage(fileUrl, getCurrentWidgetColspan(), image);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderContainerColumnStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image) {
		renderStaticImage(fileUrl, 0, image);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderStaticImage(@Nonnull String fileUrl, @Nonnull StaticImage image) {
		renderStaticImage(fileUrl, 0, image);
	}

	/**
	 * Renders a static image using an explicit form colspan.
	 *
	 * @param fileUrl     the resolved static-image URL
	 * @param formColspan the form column span to apply when rendering inside a form
	 * @param image       the static-image metadata being rendered
	 */
	public void renderStaticImage(@Nonnull String fileUrl, int formColspan, @Nonnull StaticImage image) {
		RenderedComponent i = cr.staticImage(null, fileUrl, image);
		addComponent(null, 
						formColspan,
						null, 
						image.getInvisibleCondition(), 
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormBlurb(@Nullable String markup, @Nonnull Blurb blurb) {
		renderBlurb(markup, getCurrentWidgetColspan(), blurb);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderContainerColumnBlurb(@Nullable String markup, @Nonnull Blurb blurb) {
		renderBlurb(markup, 0, blurb);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderBlurb(@Nullable String markup, @Nonnull Blurb blurb) {
		renderBlurb(markup, 0, blurb);
	}

	/**
	 * Renders a blurb using either literal markup or a bound expression.
	 */
	private void renderBlurb(@Nullable String markup, int formColspan, @Nonnull Blurb blurb) {
		if (markup == null) { // validated when the view is validated
			throw new MetaDataException("A Blurb has no markup specified.");
		}
		String value = null;
		String binding = null;
		if (markup.indexOf('{') > -1) {
			binding = markup;
		}
		else {
			value = markup;
		}
		RenderedComponent c = cr.blurb(null, dataWidgetVar, value, binding, blurb);
		addComponent(null, 
						formColspan,
						null, 
						blurb.getInvisibleCondition(), 
						null,
						c, 
						blurb.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormLink(@Nullable String value, @Nonnull Link link) {
		renderLink(value, getCurrentWidgetColspan(), link);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderContainerColumnLink(@Nullable String value, @Nonnull Link link) {
		renderLink(value, 0, link);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderLink(@Nullable String value, @Nonnull Link link) {
		renderLink(value, 0, link);
	}

	/**
	 * Renders a link by resolving its reference type into a target client
	 * component.
	 */
	private void renderLink(@SuppressWarnings("unused") @Nullable String value, int formColspan, @Nonnull Link link) {
		org.skyve.impl.metadata.view.reference.Reference outerReference = link.getReference();
		@SuppressWarnings("unused")
		final ReferenceTarget target = link.getTarget();
		final AtomicReference<RenderedComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processResourceReference(@Nonnull ResourceReference reference) {
				c.set(cr.label(null, "resource link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processReportReference(@Nonnull ReportReference reference) {
				c.set(cr.label(null, "report link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processQueryListViewReference(@Nonnull QueryListViewReference reference) {
				c.set(cr.label(null, "list view link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processImplicitActionReference(@Nonnull ImplicitActionReference reference) {
				c.set(cr.label(null, "implicit action link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processExternalReference(@Nonnull ExternalReference reference) {
				c.set(cr.label(null, "external link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processEditViewReference(@Nonnull EditViewReference reference) {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');
				c.set(cr.label(null, "external link")); // TODO link
				//c.set(cr.outputLink(listVar, value, href.toString(), link.getInvisibleCondition(), target));
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processDefaultListViewReference(@Nonnull DefaultListViewReference reference) {
				c.set(cr.label(null, "default list view link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processContentReference(@Nonnull ContentReference reference) {
				c.set(cr.label(null, "content link")); // TODO link
			}

			/**
			 * Executes this renderer lifecycle override for the current view context.
			 */
			@Override
			public void processActionReference(@Nonnull ActionReference reference) {
/* TODO we need the dataWidget binding to make this call
				Action action = obtainActionForActionReference(reference, customer, module, document, dataWidgetBinding, cr.userAgentType);
				if (action != null) {
					c.set(cr.actionLink(null, dataWidgetBinding, dataWidgetVar, link, action));
				}
*/
			}
		}.process(outerReference);

		addComponent(null, 
						formColspan,
						null, 
						link.getInvisibleCondition(), 
						null,
						c.get(), 
						link.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormLabel(@Nullable String value, boolean boundValue, @Nonnull Label label) {
		renderLabel(value, getCurrentWidgetColspan(), boundValue, label);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderContainerColumnLabel(@Nullable String value, @Nonnull Label label) {
		renderLabel(value, 0, false, label);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderLabel(@Nullable String value, boolean boundValue, @Nonnull Label label) {
		renderLabel(value, 0, boundValue, label);
	}

	/**
	 * Renders a label with optional bound-value semantics and explicit form colspan
	 * handling.
	 */
	private void renderLabel(@Nullable String value, int formColspan, boolean boundValue, @Nonnull Label label) {
		String ultimateValue = value;
		String binding = label.getBinding();
		if (boundValue) {
			binding = value;
			ultimateValue = null;
		}
		RenderedComponent c = cr.label(null, dataWidgetVar, ultimateValue, binding, label);
	    addComponent(null, 
	    				formColspan,
	    				null, 
	    				label.getInvisibleCondition(), 
	    				null,
	    				c, 
	    				label.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormProgressBar(@Nonnull ProgressBar progressBar) {
		RenderedComponent p = cr.label(null, "progressBar"); // TODO progress bar
	    addComponent(null,
	    				getCurrentWidgetColspan(),
	    				null, 
	    				progressBar.getInvisibleCondition(), 
	    				null,
	    				p, 
	    				progressBar.getPixelWidth(), 
	    				null, 
	    				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListGrid(@Nullable String title, boolean aggregateQuery, @Nonnull ListGrid grid) {
		String modelDocumentName = getCurrentListWidgetModelDocumentName();
		String modelName = getCurrentListWidgetModelName();
		ListModel<Bean> model = getCurrentListWidgetModel();
		if ((modelDocumentName == null) || (modelName == null) || (model == null)) { // cannot happen as the list widget is pre-processed
			throw new IllegalStateException("List grid " + title + " is rendered without a list model");
		}
		RenderedComponent l = cr.listGrid(null,
											modelDocumentName,
											modelName,
											model,
											title,
											grid,
											aggregateQuery);
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleCondition());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListGridProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListGridContentColumn(@Nonnull MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedListGrid(@Nullable String title, boolean aggregateQuery, @Nonnull ListGrid grid) {
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListRepeater(@Nullable String title, @Nonnull ListRepeater repeater) {
		String modelDocumentName = getCurrentListWidgetModelDocumentName();
		String modelName = getCurrentListWidgetModelName();
		ListModel<Bean> model = getCurrentListWidgetModel();
		if ((modelDocumentName == null) || (modelName == null) || (model == null)) { // cannot happen as the list widget is pre-processed
			throw new IllegalStateException("List repeater " + title + " is rendered without a list model");
		}
		RenderedComponent r = cr.listRepeater(null,
												modelDocumentName, 
												modelName, 
												model, 
												repeater.getFilterParameters(),
												repeater.getParameters(),
												title,
												Boolean.TRUE.equals(repeater.getShowColumnHeaders()),
												Boolean.TRUE.equals(repeater.getShowGrid()));
		addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleCondition());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListRepeaterProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListRepeaterContentColumn(@Nonnull MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedListRepeater(@Nullable String title, @Nonnull ListRepeater repeater) {
		addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderTreeGrid(@Nullable String title, @Nonnull TreeGrid grid) {
		RenderedComponent l = cr.label(null, "treeGrid");
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleCondition()); // TODO tree grid
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderTreeGridProjectedColumn(@Nonnull MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderTreeGridContentColumn(@Nonnull MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedTreeGrid(@Nullable String title, @Nonnull TreeGrid grid) {
		addedToContainer();
	}

	private String dataWidgetBinding;
	private String dataWidgetVar;

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataGrid(@Nullable String title, @Nonnull DataGrid grid) {
		// Determine if the grid collection is ordered
		dataWidgetBinding = grid.getBinding();
		boolean ordered = false;
		final TargetMetaData target = getCurrentTarget();
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation instanceof Collection collection) {
				ordered = Boolean.TRUE.equals(collection.getOrdered());
			}
		}

		// Create the datagrid faces component
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		RenderedComponent g = cr.dataGrid(null, dataWidgetVar, ordered, title, grid);
        addToContainer(g, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleCondition());
		gridColumnExpression = new StringBuilder(512);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataRepeater(@Nullable String title, @Nonnull DataRepeater repeater) {
		// Create the data repeater faces component
		dataWidgetBinding = repeater.getBinding();
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		RenderedComponent r = cr.dataRepeater(null, dataWidgetVar, title, repeater);
        addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleCondition());
		gridColumnExpression = new StringBuilder(512);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataGrid(@Nullable String title, @Nonnull DataGrid grid) {
		renderedDataWidget(grid);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataRepeater(@Nullable String title, @Nonnull DataRepeater repeater) {
		renderedDataWidget(repeater);
	}

	/**
	 * Finalises rendering for a data-grid style widget and clears row-scoped state.
	 */
	private void renderedDataWidget(@Nonnull AbstractDataWidget widget) {
		// Determine the document alias
		String alias = null;
		TargetMetaData target = getCurrentTarget();
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation != null) {
				alias = module.getDocument(customer, targetRelation.getDocumentName()).getLocalisedSingularAlias();
			}
		}

		if (widget instanceof DataGrid grid) {
			current = cr.addDataGridActionColumn(null,
													current, 
													grid,
													dataWidgetVar,
													gridColumnExpression.toString(), 
													alias, 
													Boolean.TRUE.equals(grid.getInline()));
		}
	    dataWidgetBinding = null;
	    dataWidgetVar = null;
	    gridColumnExpression = null;
	    addedToContainer();
	}

	private StringBuilder gridColumnExpression;

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataRepeaterBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column) {
		renderDataGridBoundColumn(title, column);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataGridBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column) {
		String binding = column.getBinding();
		if (binding == null) {
			binding = Bean.BIZ_KEY;
		}
		else {
			TargetMetaData target = getCurrentTarget();
			if (target != null) {
				Attribute targetAttribute = target.getAttribute();
				if (targetAttribute instanceof Association) {
					binding = BindUtil.createCompoundBinding(binding, Bean.BIZ_KEY);
				}
			}
		}
		current = cr.addDataGridBoundColumn(null,
												current, 
												getNullSafeCurrentDataWidget(),
												column, 
												dataWidgetVar,
												title, 
												binding, 
												gridColumnExpression);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataRepeaterBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column) {
		renderedDataGridBoundColumn(title, column);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataGridBoundColumn(@Nullable String title, @Nonnull DataGridBoundColumn column) {
		current = cr.addedDataGridBoundColumn(null, current);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataRepeaterContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDataGridContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column) {
		current = cr.addDataGridContainerColumn(null, current, getNullSafeCurrentDataWidget(), title, column);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataRepeaterContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedDataGridContainerColumn(@Nullable String title, @Nonnull DataGridContainerColumn column) {
		current = cr.addedDataGridContainerColumn(null, current);
	}

	// A reference to the current widget that is the source of events
	private RenderedComponent eventSource = null;

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnCheckBox(@Nonnull CheckBox checkBox) {
		renderCheckBox(0, checkBox);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormCheckBox(@Nonnull CheckBox checkBox) {
		renderCheckBox(getCurrentWidgetColspan(), checkBox);
	}

	/**
	 * Renders a checkbox input with explicit form colspan handling.
	 */
	private void renderCheckBox(int formColspan, @Nonnull CheckBox checkBox) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.checkBox(null, dataWidgetVar, checkBox, title, requiredMessage);
		eventSource = c;
		addComponent(title,
						formColspan,
						requiredMessage,
						checkBox.getInvisibleCondition(), 
						getCurrentWidgetHelp(),
						c, 
						checkBox.getPixelWidth(), 
						null,
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnCheckBox(@Nonnull CheckBox checkBox) {
		renderedFormCheckBox(checkBox);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormCheckBox(@Nonnull CheckBox checkBox) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderCheckMembership(@Nonnull CheckMembership membership) {
		RenderedComponent c = cr.label(null, "checkMembership"); // TODO check membership
        eventSource = c;
        addToContainer(c, null, null, null, membership.getInvisibleCondition());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedCheckMembership(@Nonnull CheckMembership membership) {
	    addedToContainer();
	    eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnColourPicker(@Nonnull ColourPicker colour) {
		renderColourPicker(0, colour);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormColourPicker(@Nonnull ColourPicker colour) {
		renderColourPicker(getCurrentWidgetColspan(), colour);
	}

	/**
	 * Renders a colour-picker input with explicit form colspan handling.
	 */
	private void renderColourPicker(int formColspan, @Nonnull ColourPicker colour) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.colourPicker(null, dataWidgetVar, colour, title, requiredMessage);
		eventSource = c;
		addComponent(title, 
						formColspan,
						requiredMessage, 
						colour.getInvisibleCondition(), 
						getCurrentWidgetHelp(),
						c, 
						colour.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnColourPicker(@Nonnull ColourPicker colour) {
		renderedFormColourPicker(colour);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormColourPicker(@Nonnull ColourPicker colour) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnCombo(@Nonnull Combo combo) {
		renderCombo(0, combo);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormCombo(@Nonnull Combo combo) {
		renderCombo(getCurrentWidgetColspan(), combo);
	}

	/**
	 * Renders a combo input with explicit form colspan handling.
	 */
	private void renderCombo(int formColspan, @Nonnull Combo combo) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent s = cr.combo(null, dataWidgetVar, combo, title, requiredMessage);
		eventSource = s;
		addComponent(title, 
						formColspan,
						requiredMessage, 
						combo.getInvisibleCondition(), 
						getCurrentWidgetHelp(),
						s, 
						combo.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnCombo(@Nonnull Combo combo) {
		renderedFormCombo(combo);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormCombo(@Nonnull Combo combo) {
		eventSource = null;
	}

	/**
	 * Renders a content upload in a bound-column context.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 */
	@Override
	public void renderBoundColumnContent(@Nonnull ContentUpload content) {
		renderContent(0, content);
	}

	/**
	 * Renders a content upload in a container-column context.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 */
	@Override
	public void renderContainerColumnContent(@Nonnull ContentUpload content) {
		renderContent(0, content);
	}

	/**
	 * Renders a content upload in a form context.
	 *
	 * @param content the content upload to render; must not be {@code null}
	 */
	@Override
	public void renderFormContent(@Nonnull ContentUpload content) {
		renderContent(getCurrentWidgetColspan(), content);
	}

	/**
	 * Creates and registers the generated client component for a content upload.
	 *
	 * @param formColspan form colspan to apply when rendering inside a form
	 * @param content the content upload metadata; must not be {@code null}
	 */
	private void renderContent(int formColspan, @Nonnull ContentUpload content) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.content(null, dataWidgetVar, content, title, requiredMessage);
		addComponent(title,
						formColspan,
						null,
						content.getInvisibleCondition(),
						getCurrentWidgetHelp(),
						c,
						content.getPixelWidth(),
						null,
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormContentSignature(@Nonnull ContentSignature signature) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.contentSignature(null, dataWidgetVar, signature, title, requiredMessage);
        addComponent(title, 
        				getCurrentWidgetColspan(),
        				null, 
        				signature.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				signature.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnHTML(@Nonnull HTML html) {
		renderHTML(0, html);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormHTML(@Nonnull HTML html) {
		renderHTML(getCurrentWidgetColspan(), html);
	}

	/**
	 * Renders an HTML input with explicit form colspan handling.
	 */
	private void renderHTML(int formColspan, @Nonnull HTML html) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.html(null, dataWidgetVar, html, title, requiredMessage);
        addComponent(title, 
        				formColspan,
        				requiredMessage, 
        				html.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				html.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderListMembership(@Nullable String candidatesHeading, @Nullable String membersHeading, @Nonnull ListMembership membership) {
		RenderedComponent c = cr.listMembership(null, membership);
		eventSource = c;
		addToContainer(c, membership.getPixelWidth(), null, null, membership.getInvisibleCondition());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedListMembership(@Nullable String candidatesHeading, @Nullable String membersHeading, @Nonnull ListMembership membership) {
		addedToContainer();
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderComparison(@Nonnull Comparison comparison) {
		RenderedComponent c = cr.label(null, "comparison"); // TODO comparison
        addToContainer(c, comparison.getPixelWidth(), comparison.getResponsiveWidth(), comparison.getPercentageWidth(), comparison.getInvisibleCondition());
        addedToContainer();
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnLookupDescription(@Nullable MetaDataQueryDefinition query,
													boolean canCreate,
													boolean canUpdate,
													@Nonnull String descriptionBinding,
													@Nonnull LookupDescription lookup) {
		renderLookupDescription(0, query, canCreate, canUpdate, descriptionBinding, lookup);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormLookupDescription(@Nullable MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												@Nonnull String descriptionBinding,
												@Nonnull LookupDescription lookup) {
		renderLookupDescription(getCurrentWidgetColspan(), query, canCreate, canUpdate, descriptionBinding, lookup);
	}
	
	private void renderLookupDescription(int formColspan,
											@Nullable MetaDataQueryDefinition query,
											@SuppressWarnings("unused") boolean canCreate,
											@SuppressWarnings("unused") boolean canUpdate,
											@Nonnull String descriptionBinding,
											@Nonnull LookupDescription lookup) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.lookupDescription(null,
													dataWidgetVar, 
													lookup, 
													title, 
													requiredMessage,
													descriptionBinding,
													query);
        eventSource = c;
        addComponent(title,
        				formColspan,
        				requiredMessage, 
        				lookup.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				lookup.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnLookupDescription(@Nullable MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														@Nonnull String descriptionBinding,
														@Nonnull LookupDescription lookup) {
		renderedFormLookupDescription(query, canCreate, canUpdate, descriptionBinding, lookup);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormLookupDescription(@Nullable MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												@Nonnull String descriptionBinding,
												@Nonnull LookupDescription lookup) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnPassword(@Nonnull Password password) {
		renderPassword(0, password);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormPassword(@Nonnull Password password) {
		renderPassword(getCurrentWidgetColspan(), password);
	}

	/**
	 * Renders a password input with explicit form colspan handling.
	 */
	private void renderPassword(int formColspan, @Nonnull Password password) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.password(null, dataWidgetVar, password, title, requiredMessage);
        eventSource = c;
        addComponent(title,
        				formColspan,
        				requiredMessage, 
        				password.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				password.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnPassword(@Nonnull Password password) {
		renderedFormPassword(password);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormPassword(@Nonnull Password password) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnRadio(@Nonnull Radio radio) {
		renderRadio(0, radio);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormRadio(@Nonnull Radio radio) {
		renderRadio(getCurrentWidgetColspan(), radio);
	}

	/**
	 * Renders a radio input with explicit form colspan handling.
	 */
	private void renderRadio(int formColspan, @Nonnull Radio radio) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.radio(null, dataWidgetVar, radio, title, requiredMessage);
		eventSource = c;
		addComponent(title, 
						formColspan,
						requiredMessage, 
						radio.getInvisibleCondition(), 
						getCurrentWidgetHelp(),
						c, 
						radio.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnRadio(@Nonnull Radio radio) {
		renderedFormRadio(radio);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormRadio(@Nonnull Radio radio) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnRichText(@Nonnull RichText text) {
		renderRichText(0, text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormRichText(@Nonnull RichText text) {
		renderRichText(getCurrentWidgetColspan(), text);
	}

	/**
	 * Renders a rich-text input with explicit form colspan handling.
	 */
	private void renderRichText(int formColspan, @Nonnull RichText text) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.richText(null, dataWidgetVar, text, title, requiredMessage);
        eventSource = c;
        addComponent(title, 
        				formColspan,
        				requiredMessage, 
        				text.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				text.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnRichText(@Nonnull RichText text) {
		renderedFormRichText(text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormRichText(@Nonnull RichText text) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnSlider(@Nonnull Slider slider) {
		renderSlider(0, slider);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormSlider(@Nonnull Slider slider) {
		renderSlider(getCurrentWidgetColspan(), slider);
	}

	/**
	 * Renders a slider input placeholder with explicit form colspan handling.
	 */
	private void renderSlider(int formColspan, @Nonnull Slider slider) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.label(null, "slider"); // TODO slider
        eventSource = c;
        addComponent(title, 
        				formColspan,
        				requiredMessage, 
        				slider.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				slider.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnSlider(@Nonnull Slider slider) {
		renderedFormSlider(slider);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormSlider(@Nonnull Slider slider) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnSpinner(@Nonnull Spinner spinner) {
		renderSpinner(0, spinner);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormSpinner(@Nonnull Spinner spinner) {
		renderSpinner(getCurrentWidgetColspan(), spinner);
	}

	/**
	 * Renders a spinner input with explicit form colspan handling.
	 */
	private void renderSpinner(int formColspan, @Nonnull Spinner spinner) {
		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.spinner(null, dataWidgetVar, spinner, title, requiredMessage);
        eventSource = c;
        addComponent(title, 
        				formColspan,
        				requiredMessage, 
        				spinner.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				spinner.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnSpinner(@Nonnull Spinner spinner) {
		renderedFormSpinner(spinner);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormSpinner(@Nonnull Spinner spinner) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnTextArea(@Nonnull TextArea text) {
		renderTextArea(0, text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormTextArea(@Nonnull TextArea text) {
		renderTextArea(getCurrentWidgetColspan(), text);
	}

	/**
	 * Renders a text-area input with attribute-derived length and explicit form
	 * colspan handling.
	 */
	private void renderTextArea(int formColspan, @Nonnull TextArea text) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		Integer length = null;
		if (attribute instanceof LengthField field) {
			length = Integer.valueOf(field.getLength());
		}

		String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
		RenderedComponent c = cr.textArea(null, dataWidgetVar, text, title, requiredMessage, length);
        eventSource = c;
        addComponent(title,
        				formColspan,
        				requiredMessage, 
        				text.getInvisibleCondition(), 
        				getCurrentWidgetHelp(),
        				c, 
        				text.getPixelWidth(), 
        				null, 
        				null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnTextArea(@Nonnull TextArea text) {
		renderedFormTextArea(text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormTextArea(@Nonnull TextArea text) {
		eventSource = null;
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBoundColumnTextField(@Nonnull TextField text) {
		renderTextField(0, text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormTextField(@Nonnull TextField text) {
		renderTextField(getCurrentWidgetColspan(), text);
	}

	/**
	 * Renders a text field with attribute-derived converter, format, and length
	 * metadata.
	 *
	 * @param formColspan the form column span to apply when rendering inside a form
	 * @param text        the text-field metadata being rendered
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public void renderTextField(int formColspan, @Nonnull TextField text) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		TextFormat textFormat = (attribute instanceof Text textAttribute) ? textAttribute.getFormat() : null;
		Format<?> format = (textFormat == null) ? null : textFormat.getFormat();
		Integer length = null;
		if (attribute instanceof LengthField field) {
			length = Integer.valueOf(field.getLength());
		}
		Converter<?> converter = null;
        if (attribute instanceof ConvertibleField field) {
            converter = field.getConverter();
        }
        if (AttributeType.date.equals(type)) {
        	if (converter == null) {
                converter = customer.getDefaultDateConverter();
            }
        }
        else if (AttributeType.dateTime.equals(type)) {
            if (converter == null) {
                converter = customer.getDefaultDateTimeConverter();
            }
        }
        else if (AttributeType.timestamp.equals(type)) {
            if (converter == null) {
                converter = customer.getDefaultTimestampConverter();
            }
        }
        else if (AttributeType.time.equals(type)) {
            if (converter == null) {
                converter = customer.getDefaultTimeConverter();
            }
        }

        String title = getCurrentWidgetLabel();
		String requiredMessage = getCurrentWidgetRequiredMessage();
        RenderedComponent c = cr.text(null,
										dataWidgetVar, 
										text, 
										title, 
										requiredMessage,
										length,
										converter,
										format);
        eventSource = c;
		addComponent(title, 
						formColspan,
						requiredMessage, 
						text.getInvisibleCondition(), 
						getCurrentWidgetHelp(),
						c, 
						text.getPixelWidth(), 
						null, 
						null);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderFormInject(@Nonnull Inject inject) {
		// TODO
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderInject(@Nonnull Inject inject) {
		// TODO
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedBoundColumnTextField(@Nonnull TextField text) {
		renderedFormTextField(text);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedFormTextField(@Nonnull TextField text) {
		eventSource = null;
	}

	private void addToContainer(@Nonnull RenderedComponent component, 
									@Nullable Integer pixelWidth, 
									@Nullable Integer responsiveWidth,
									@Nullable Integer percentageWidth,
									@Nullable String invisibleCondition) {
		Deque<Container> currentContainers = getCurrentContainers();
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();

		current = lr.addToContainer(null,
										currentContainer, 
										current, 
										component, 
										pixelWidth, 
										responsiveWidth, 
										percentageWidth,
										invisibleCondition);
	}

	/**
	 * Finalises the most recent container insertion and updates the active layout
	 * pointer.
	 */
	private void addedToContainer() {
		Deque<Container> currentContainers = getCurrentContainers();
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to complete the add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();
		current = lr.addedToContainer(null, currentContainer, current);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnChangedEventHandler(@Nonnull Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = changeable.getBinding();
		List<EventAction> changedActions = changeable.getChangedActions();
		cr.addAjaxBehavior(eventSource, "change", dataWidgetBinding, dataWidgetVar, binding, changedActions);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnChangedEventHandler(@Nonnull Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnFocusEventHandler(@Nonnull Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (blurable instanceof Bound bound) ? bound.getBinding() : null;
		cr.addAjaxBehavior(eventSource, "focus", dataWidgetBinding, dataWidgetVar, binding, blurable.getFocusActions());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnFocusEventHandler(@Nonnull Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnBlurEventHandler(@Nonnull Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (blurable instanceof Bound bound) ? bound.getBinding() : null;
		cr.addAjaxBehavior(eventSource, "blur", dataWidgetBinding, dataWidgetVar, binding, blurable.getBlurActions());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnBlurEventHandler(@Nonnull Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnAddedEventHandler(@Nonnull Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnAddedEventHandler(@Nonnull Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnEditedEventHandler(@Nonnull Editable editable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnEditedEventHandler(@Nonnull Editable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnRemovedEventHandler(@Nonnull Removable removable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnRemovedEventHandler(@Nonnull Removable removable,
												boolean parentVisible,
												boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnSelectedEventHandler(@Nonnull Selectable selectable,
												boolean parentVisible,
												boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnSelectedEventHandler(@Nonnull Selectable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnPickedEventHandler(@Nonnull LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemSelect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getPickedActions());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnPickedEventHandler(@Nonnull LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitOnClearedEventHandler(@Nonnull LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemUnselect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getClearedActions());
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitedOnClearedEventHandler(@Nonnull LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitRerenderEventAction(@Nonnull RerenderEventAction rerender,
											@Nonnull EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitServerSideActionEventAction(@Nonnull Action action, @Nonnull ServerSideActionEventAction server) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitSetDisabledEventAction(@Nonnull SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitSetInvisibleEventAction(@Nonnull SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitToggleDisabledEventAction(@Nonnull ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitToggleVisibilityEventAction(@Nonnull ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderCustomAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (RenderedComponent toolbarLayout : toolbarLayouts) {
					toolbarLayout.addChild(cr.action(null,
														dataWidgetBinding,
														dataWidgetVar,
														action,
														null,
														label));
				}
			}
		}
	}

	/**
	 * Processes an implicit action and appends its generated component to each
	 * toolbar layout.
	 *
	 * @param action the action metadata instance
	 * @param name   the implicit action type to render
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void processImplicitAction(@Nonnull ActionImpl action, @Nonnull ImplicitActionName name) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (RenderedComponent toolbarLayout : toolbarLayouts) {
					if (ImplicitActionName.Report.equals(name)) {
						toolbarLayout.addChild(cr.report(null, action));
					}
					else if (ImplicitActionName.Download.equals(name)) {
						toolbarLayout.addChild(cr.download(null,
															action,
															module.getName(),
															document.getName()));
					}
					else if (ImplicitActionName.Upload.equals(name)) {
						toolbarLayout.addChild(cr.upload(null, action));
					}
					else {
						String displayName = action.getLocalisedDisplayName();
						if (displayName == null) {
							displayName = name.getLocalisedDisplayName();
						}
						toolbarLayout.addChild(cr.action(null,
															dataWidgetBinding,
															dataWidgetVar,
															action,
															name,
															displayName));
					}
				}
			}
		}
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderAddAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Add);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderRemoveAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action,
									boolean canDelete) {
		processImplicitAction(action, ImplicitActionName.Remove);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderZoomOutAction(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.ZoomOut);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderNavigateAction(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Navigate);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderOKAction(@Nullable String name,
								@Nullable String label,
								@Nullable String iconUrl,
								@Nullable String iconStyleClass,
								@Nullable String toolTip,
								@Nullable String confirmationText,
								@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.OK);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderSaveAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Save);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderCancelAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Cancel);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDeleteAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Delete);
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/report/Bum.html?_f=html&_c=<webId>&_id=<id>&wee=poo&_n=Bum&_mod=<module>&_doc=<document>
	 * 
	 * @param action
	 */
	@Override
	public void renderReportAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Report);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBizExportAction(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizExport);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderBizImportAction(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizImport);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderDownloadAction(@Nullable String name,
										@Nullable String label,
										@Nullable String iconUrl,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Download);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderUploadAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Upload);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderNewAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.New);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderEditAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Edit);
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderPrintAction(@Nullable String name,
									@Nullable String label,
									@Nullable String iconUrl,
									@Nullable String iconStyleClass,
									@Nullable String toolTip,
									@Nullable String confirmationText,
									@Nonnull ActionImpl action) {
		// TODO implement
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitParameter(@Nonnull Parameter parameter,
								boolean parentVisible,
								boolean parentEnabled) {
		// nothing to see here
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void visitFilterParameter(@Nonnull FilterParameter parameter,
										boolean parentVisible,
										boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */

	@Override
	public void renderSidebar(@Nonnull Sidebar sidebar) {
		// TODO Auto-generated method stub
	}

	/**
	 * Executes this renderer lifecycle override for the current view context.
	 */
	@Override
	public void renderedSidebar(@Nonnull Sidebar sidebar) {
		// TODO Auto-generated method stub
	}
}
