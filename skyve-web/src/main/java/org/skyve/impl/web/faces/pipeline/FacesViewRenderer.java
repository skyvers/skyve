package org.skyve.impl.web.faces.pipeline;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import org.primefaces.component.picklist.PickList;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.date.MMM_DD_YYYY;
import org.skyve.domain.types.converters.date.MM_DD_YYYY;
import org.skyve.domain.types.converters.date.YYYY_MM_DD;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.container.Collapsible;
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
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
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
import org.skyve.impl.web.faces.converters.date.DD_MMM_YYYY;
import org.skyve.impl.web.faces.converters.date.DD_MM_YYYY;
import org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD_HH_MI;
import org.skyve.impl.web.faces.converters.decimal.Decimal10Converter;
import org.skyve.impl.web.faces.converters.decimal.Decimal10TwoDecimalPlaces;
import org.skyve.impl.web.faces.converters.decimal.Decimal2Converter;
import org.skyve.impl.web.faces.converters.decimal.Decimal2Integer;
import org.skyve.impl.web.faces.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.impl.web.faces.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.impl.web.faces.converters.decimal.Decimal5Converter;
import org.skyve.impl.web.faces.converters.decimal.Decimal5Integer;
import org.skyve.impl.web.faces.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.impl.web.faces.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TimeDuration;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal10DollarsAndCents;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.impl.web.faces.converters.geometry.GeometryConverter;
import org.skyve.impl.web.faces.converters.integer.IntegerConverter;
import org.skyve.impl.web.faces.converters.integer.IntegerSeparator;
import org.skyve.impl.web.faces.converters.integer.LongIntegerConverter;
import org.skyve.impl.web.faces.converters.integer.LongIntegerSeparator;
import org.skyve.impl.web.faces.converters.integer.SimplePercentage;
import org.skyve.impl.web.faces.converters.time.HH24_MI;
import org.skyve.impl.web.faces.converters.time.HH24_MI_SS;
import org.skyve.impl.web.faces.converters.time.HH_MI;
import org.skyve.impl.web.faces.converters.time.HH_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Customisations;
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
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;

public class FacesViewRenderer extends ViewRenderer {
	private ComponentBuilder cb;
	private LayoutBuilder lb;
	private boolean createView;
	private String widgetId;
	private UIComponent fragment; // if we have a widgetId to render, this holds a reference to that component

	private UIComponent current; // current component being constructed
	private UIComponent facesSidebar; // the result of sidebar construction
	private UIComponent facesView; // the result of view construction
	private List<UIComponent> toolbarLayouts; // the toolbar layouts

	// A reference to the current widget that is the source of events
	private UIComponentBase eventSource = null;

	public FacesViewRenderer(User user,
								Module module,
								Document document,
								View view,
								String uxui,
								String widgetId,
								ComponentBuilder cb,
								LayoutBuilder lb) {
		super(user, module, document, view, uxui);
		String viewName = view.getName();
		createView = ViewType.create.toString().equals(viewName);
		this.widgetId = widgetId;
		this.cb = cb;
		this.lb = lb;
	}

	public UIComponent getFacesView() {
		return facesView;
	}
	
	public UIComponent getSidebar() {
		return facesSidebar;
	}

	@Override
	public void renderView(String icon16x16Url, String icon32x32Url) {
		// Ensure visibility is set for both create and edit views
		current = cb.view(null, createView);
		facesView = current;

		// Create the toolbar(s)
		toolbarLayouts = lb.toolbarLayouts(null);

		// Add the view layout if defined
		UIComponent layout = lb.viewLayout(null);
		if (layout != null) {
			if (widgetId == null) {
				current.getChildren().add(layout);
			}
			current = layout;
		}
	}

	@Override
	public void renderedView(String icon16x16Url, String icon32x32Url) {
		// Add the toolbar(s) if this is a full view render or
		// a view with a widgetId = actions widgetId
		if ((widgetId == null) || widgetId.equals(view.getActionsWidgetId())) {
			// Add the toolbar(s) if it/they has/have contents
			if ((toolbarLayouts != null) && 
					(! toolbarLayouts.isEmpty()) && 
					(! toolbarLayouts.get(0).getChildren().isEmpty())) {
				// If we get any toolbars back, add the toolbar layouts to it
				List<UIComponent> toolbars = cb.toolbars(null, view.getActionsWidgetId());
				if (toolbars != null) {
					if (toolbars.size() != toolbarLayouts.size()) {
						throw new IllegalStateException(String.format("The component Builder %s yielded %d toolbars but Layout Builder %s yielded %d toolbar layouts",
																		cb.getClass().getName(),
																		Integer.valueOf(toolbars.size()),
																		lb.getClass().getName(),
																		Integer.valueOf(toolbarLayouts.size())));
					}
					lb.addToolbarLayouts(toolbars, toolbarLayouts);
					lb.addToolbarsOrLayouts(facesView, toolbars);
				}
				else {
					lb.addToolbarsOrLayouts(facesView, toolbarLayouts);
				}
			}
		}

		// Add any script UIComponents created at the end (in order)
		facesView.getChildren().addAll(scripts);
	}

	// NB this is a list of UI component so that script visibility can be controlled
	// - ie rendered="<condition>"
	private List<UIComponent> scripts = new ArrayList<>();

	@Override
	public void renderTabPane(TabPane tabPane) {
		UIComponent component = cb.tabPane(null, tabPane, module.getName(), document.getName());
		addToContainer(component,
						tabPane.getPixelWidth(),
						tabPane.getResponsiveWidth(),
						tabPane.getPercentageWidth(),
						tabPane.getSm(),
						tabPane.getMd(),
						tabPane.getLg(),
						tabPane.getXl(),
						tabPane.getInvisibleConditionName());

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			fragment = component;
		}

		// These are added in the order they are encountered to ensure rendering works for nested tab panes correctly
		// Add scripts if we are rendering the whole view or we are within the fragment being rendered
		if ((widgetId == null) || ((widgetId != null) && (fragment != null))) {
			scripts.add(cb.tabPaneScript(null, tabPane, module.getName(), document.getName(), current.getId()));
		}
	}

	@Override
	public void renderedTabPane(TabPane tabPane) {
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
			// Add the scripts required for this fragment here (and not in renderedView()
			facesView.getChildren().addAll(scripts);
		}
	}

	@Override
	public void renderTab(String title, String icon16x16Url, Tab tab) {
		UIComponent component = cb.tab(null, title, tab);
		lb.addTab(current, component);
		current = component;
		UIComponent layout = lb.tabLayout(null);
		if (layout != null) {
			current = lb.addTabLayout(null, component, layout);
		}
	}

	@Override
	public void renderedTab(String title, String icon16x16Url, Tab tab) {
		current = lb.addedTab(null, current);
	}

	@Override
	public void renderVBox(String borderTitle, VBox vbox) {
		UIComponent layout = lb.vboxLayout(null, vbox);
		addToContainerWithPotentialBorder(layout, vbox.getBorder(), borderTitle, vbox, vbox.getInvisibleConditionName(), vbox.getCollapsible(), vbox.getWidgetId());
	}

	@Override
	public void renderedVBox(String borderTitle, VBox vbox) {
		addedToContainerWithPotentialBorder(vbox.getBorder(), vbox.getCollapsible(), vbox.getWidgetId());
	}

	@Override
	public void renderHBox(String borderTitle, HBox hbox) {
		UIComponent layout = lb.hboxLayout(null, hbox);
		addToContainerWithPotentialBorder(layout, hbox.getBorder(), borderTitle, hbox, hbox.getInvisibleConditionName(), hbox.getCollapsible(), hbox.getWidgetId());
	}

	@Override
	public void renderedHBox(String borderTitle, HBox hbox) {
		addedToContainerWithPotentialBorder(hbox.getBorder(), hbox.getCollapsible(), hbox.getWidgetId());
	}

	@Override
	public void renderForm(String borderTitle, Form form) {
		UIComponent layout = lb.formLayout(null, form);
		addToContainerWithPotentialBorder(layout, form.getBorder(), borderTitle, form, form.getInvisibleConditionName(), form.getCollapsible(), form.getWidgetId());
// TODO form.getDisabledConditionName() form.getLabelDefaultHorizontalAlignment()
	}

	@Override
	public void renderedForm(String borderTitle, Form form) {
		addedToContainerWithPotentialBorder(form.getBorder(), form.getCollapsible(), form.getWidgetId());
	}

	@Override
	public void renderFormColumn(FormColumn column) {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	private UIComponent formRowLayout = null;

	@Override
	public void renderFormRow(FormRow row) {
		formRowLayout = lb.formRowLayout(null, row);
		if (formRowLayout != null) {
			current = lb.addFormRowLayout(null, current, formRowLayout);
		}
	}

	@Override
	public void renderFormItem(String label, boolean required, String help, boolean showLabel, int colspan, FormItem item) {
		// nothing to do here
	}

	@Override
	public void renderedFormItem(String label, boolean required, String help, boolean showLabel, int colspan, FormItem item) {
		// nothing to do here
	}

	@Override
	public void renderedFormRow(FormRow row) {
		if (formRowLayout != null) {
			current = lb.addedFormRowLayout(null, formRowLayout);
		}
		formRowLayout = null;
	}

	private void addComponent(String widgetLabel,
								int formColspan,
								boolean widgetRequired,
								String widgetInvisible,
								String helpText,
								UIComponent component,
								Integer pixelWidth,
								Integer responsiveWidth,
								Integer percentageWidth,
								Integer sm,
								Integer md,
								Integer lg,
								Integer xl) {
		if (component == null) {
			return;
		}

		DataGridBoundColumn currentBoundColumn = getCurrentBoundColumn();
		if (currentBoundColumn != null) { // bound column in a data grid or data repeater
			// Add editing component if we have an inline data grid and the current column is editable
			boolean columnEditable = ! Boolean.FALSE.equals(currentBoundColumn.getEditable());
			if (columnEditable) { // NB short circuit test
				AbstractDataWidget currentDataWidget = getCurrentDataWidget();
				boolean inline = (currentDataWidget instanceof DataGrid) ?
									Boolean.TRUE.equals(((DataGrid) currentDataWidget).getInline()) :
									false;
				if (inline) {
					current.getChildren().add(component);
				}
			}
		}
		else { // not a bound column in a data grid or data repeater
			Form currentForm = getCurrentForm();
			if (currentForm == null) { // not a form item
				DataGridContainerColumn currentContainerColumn = getCurrentContainerColumn();
				if (currentContainerColumn != null) { // container column in a data grid or data repeater
					// add a spacer, if required
					List<UIComponent> children = current.getChildren();
					if (! children.isEmpty()) {
						children.add(cb.label(null, " "));
					}
					children.add(component);
				}
				else { // This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, responsiveWidth, percentageWidth, sm, md, lg, xl, widgetInvisible);
					addedToContainer();
				}
			}
			else { // a form item
				FormItem formItem = getCurrentFormItem();
				FormColumn formColumn = getCurrentFormColumn();
				if (isCurrentWidgetShowLabel() && (! isCurrentFormRenderTopLabels())) {
					lb.layoutFormItemLabel(current,
											component,
											currentForm,
											formItem,
											formColumn,
											widgetLabel,
											widgetRequired,
											widgetInvisible,
											helpText);
					incrementFormColumn();
				}

				lb.layoutFormItemWidget(current,
											component,
											currentForm,
											formItem,
											formColumn,
											widgetLabel,
											formColspan,
											widgetRequired,
											widgetInvisible,
											helpText,
											pixelWidth,
											isCurrentWidgetShowLabel(),
											isCurrentFormRenderTopLabels());
				for (int i = 0, l = formColspan; i < l; i++) {
					incrementFormColumn();
				}
			}
		}
	}

	@Override
	public void renderFormButton(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									Action action,
									Button button) {
		Form currentForm = getCurrentForm();
		renderButton(label,
						getCurrentWidgetColspan(),
						iconStyleClass,
						toolTip,
						confirmationText,
						action,
						button,
						(currentForm == null) ? null : currentForm.getDisabledConditionName());
	}

	@Override
	public void renderButton(String name,
								String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								Action action,
								Button button) {
		renderButton(label, 0, iconStyleClass, toolTip, confirmationText, action, button, null);
	}

	private void renderButton(String label,
								int formColspan,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								Action action,
								Button button,
								String formDisabledConditionName) {
		ImplicitActionName name = action.getImplicitName();
		UIComponent c = null;
		if (ImplicitActionName.Report.equals(name)) {
			c = cb.reportButton(null,
									label,
									iconStyleClass,
									toolTip,
									confirmationText,
									button,
									formDisabledConditionName,
									action);
		}
		else if (ImplicitActionName.Download.equals(name)) {
			c = cb.downloadButton(null,
									dataWidgetBinding,
									dataWidgetVar,
									label,
									iconStyleClass,
									toolTip,
									confirmationText,
									button,
									formDisabledConditionName,
									action);
		}
		else if (ImplicitActionName.Upload.equals(name)) {
			c = cb.uploadButton(null,
									label,
									iconStyleClass,
									toolTip,
									confirmationText,
									button,
									formDisabledConditionName,
									action);
		}
		else {
			c = cb.actionButton(null,
									dataWidgetBinding,
									dataWidgetVar,
									label,
									iconStyleClass,
									toolTip,
									confirmationText,
									button,
									formDisabledConditionName,
									action);
		}
		addComponent(null,
						formColspan,
						false,
						action.getInvisibleConditionName(),
						null,
						c,
						button.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderFormZoomIn(String label, String iconUrl, String iconStyleClass, String toolTip, ZoomIn zoomIn) {
		Form currentForm = getCurrentForm();
		String formDisabledConditionName = (currentForm == null) ? null : currentForm.getDisabledConditionName();
		renderZoomIn(label, getCurrentWidgetColspan(), iconStyleClass, toolTip, zoomIn, formDisabledConditionName);
	}

	@Override
	public void renderZoomIn(String label, String iconUrl, String iconStyleClass, String toolTip, ZoomIn zoomIn) {
		renderZoomIn(label, 0, iconStyleClass, toolTip, zoomIn, null);
	}

	protected void renderZoomIn(String label,
									int formColspan,
									String iconStyleClass,
									String toolTip,
									ZoomIn zoomIn,
									String formDisabledConditionName) {
		UIComponent z = cb.zoomIn(null, label, iconStyleClass, toolTip, zoomIn, formDisabledConditionName);
		addComponent(null,
						formColspan,
						false,
						zoomIn.getInvisibleConditionName(),
						null,
						z,
						zoomIn.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderMap(MapDisplay map) {
		UIComponent m = cb.map(null, map, map.getModelName());
		addComponent(null,
						0,
						false,
						map.getInvisibleConditionName(),
						null,
						m,
						map.getPixelWidth(),
						map.getResponsiveWidth(),
						map.getPercentageWidth(),
						map.getSm(),
						map.getMd(),
						map.getLg(),
						map.getXl());
	}

	@Override
	public void renderChart(Chart chart) {
		UIComponent c = cb.chart(null, chart);
		addComponent(null,
						0,
						false,
						chart.getInvisibleConditionName(),
						null,
						c,
						chart.getPixelWidth(),
						chart.getResponsiveWidth(),
						chart.getPercentageWidth(),
						chart.getSm(),
						chart.getMd(),
						chart.getLg(),
						chart.getXl());
	}

	@Override
	public void renderBoundColumnGeometry(Geometry geometry) {
		renderGeometry(0, geometry);
	}

	@Override
	public void renderFormGeometry(Geometry geometry) {
		renderGeometry(getCurrentWidgetColspan(), geometry);
	}

	private void renderGeometry(int formColspan, Geometry geometry) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.geometry(null,
												dataWidgetVar,
												geometry,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required,
												CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, AttributeType.geometry));
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						false,
						geometry.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						geometry.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnGeometry(Geometry geometry) {
		renderedFormGeometry(geometry);
	}

	@Override
	public void renderedFormGeometry(Geometry geometry) {
		eventSource = null;
	}

	@Override
	public void renderFormGeometryMap(GeometryMap geometry) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.geometryMap(null,
													geometry,
													(currentForm == null) ? null : currentForm.getDisabledConditionName(),
													title,
													required);
		eventSource = c.getEventSource();
		addComponent(title,
						getCurrentWidgetColspan(),
						false,
						geometry.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						geometry.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedFormGeometryMap(GeometryMap geometry) {
		eventSource = null;
	}

	@Override
	public void renderFormDialogButton(String label, DialogButton button) {
		renderDialogButton(label, getCurrentWidgetColspan(), button);
	}

	@Override
	public void renderDialogButton(String label, DialogButton button) {
		renderDialogButton(label, 0, button);
	}

	private void renderDialogButton(String label, int formColspan, DialogButton button) {
		UIComponent bn = cb.label(null, "dialogButton " + label); // TODO dialog button
		addComponent(null,
						formColspan,
						false,
						button.getInvisibleConditionName(),
						null,
						bn,
						null,
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderFormSpacer(Spacer spacer) {
		renderSpacer(getCurrentWidgetColspan(), spacer);
	}

	@Override
	public void renderSpacer(Spacer spacer) {
		renderSpacer(0, spacer);
	}

	private void renderSpacer(int formColspan, Spacer spacer) {
		UIComponent component = cb.spacer(null, spacer);
		if (component != null) {
			addComponent(null,
							formColspan,
							false,
							spacer.getInvisibleConditionName(),
							null,
							component,
							spacer.getPixelWidth(),
							null,
							null,
							null,
							null,
							null,
							null);
		}
	}

	@Override
	public void renderFormStaticImage(String fileUrl, StaticImage image) {
		renderStaticImage(fileUrl, getCurrentWidgetColspan(), image);
	}

	@Override
	public void renderStaticImage(String fileUrl, StaticImage image) {
		renderStaticImage(fileUrl, 0, image);
	}

	private void renderStaticImage(String fileUrl, int formColspan, StaticImage image) {
		UIComponent i = cb.staticImage(null, fileUrl, image);
		addComponent(null,
						formColspan,
						false,
						image.getInvisibleConditionName(),
						null,
						i,
						image.getPixelWidth(),
						image.getResponsiveWidth(),
						image.getPercentageWidth(),
						image.getSm(),
						image.getMd(),
						image.getLg(),
						image.getXl());
	}

	@Override
	public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
		renderStaticImage(fileUrl, image);
	}

	@Override
	public void renderContainerColumnDynamicImage(DynamicImage image) {
		renderDynamicImage(image);
	}

	@Override
	public void renderDynamicImage(DynamicImage image) {
		UIComponent i = cb.dynamicImage(null, image, module.getName(), document.getName());
		addComponent(null,
						0,
						false,
						image.getInvisibleConditionName(),
						null,
						i,
						image.getPixelWidth(),
						image.getResponsiveWidth(),
						image.getPercentageWidth(),
						image.getSm(),
						image.getMd(),
						image.getLg(),
						image.getXl());
	}

	@Override
	public void renderFormLink(String value, Link link) {
		renderLink(value, getCurrentWidgetColspan(), link);
	}

	@Override
	public void renderContainerColumnLink(String value, Link link) {
		renderLink(value, 0, link);
	}

	@Override
	public void renderLink(String value, Link link) {
		renderLink(value, 0, link);
	}

	private void renderLink(String value, int formColspan, Link link) {
		org.skyve.impl.metadata.view.reference.Reference outerReference = link.getReference();
		final ReferenceTarget target = link.getTarget();
		final AtomicReference<UIComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			@Override
			public void processResourceReference(ResourceReference reference) {
				c.set(cb.label(null, "resource link")); // TODO link
			}

			@Override
			public void processReportReference(ReportReference reference) {
				c.set(cb.label(null, "report link")); // TODO link
			}

			@Override
			public void processQueryListViewReference(QueryListViewReference reference) {
				c.set(cb.label(null, "list view link")); // TODO link
			}

			@Override
			public void processImplicitActionReference(ImplicitActionReference reference) {
				c.set(cb.label(null, "implicit action link")); // TODO link
			}

			@Override
			public void processExternalReference(ExternalReference reference) {
				c.set(cb.label(null, "external link")); // TODO link
			}

			@Override
			public void processEditViewReference(EditViewReference reference) {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');

				c.set(cb.outputLink(dataWidgetVar, value, href.toString(), link.getInvisibleConditionName(), target));
			}

			@Override
			public void processDefaultListViewReference(DefaultListViewReference reference) {
				c.set(cb.label(null, "default list view link")); // TODO link
			}

			@Override
			public void processContentReference(ContentReference reference) {
				c.set(cb.label(null, "content link")); // TODO link
			}

			@Override
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference) {
				Action action = obtainActionForActionReference(reference, customer, module, document, dataWidgetBinding, cb.userAgentType);
				if (action != null) {
					ActionShow show = action.getShow();
					String iconStyleClass = action.getIconStyleClass();
					if (ActionShow.text == show) {
						iconStyleClass = null;
					}
					c.set(cb.actionLink(null,
											dataWidgetBinding,
											dataWidgetVar,
											value,
											iconStyleClass,
											action.getLocalisedToolTip(),
											action.getLocalisedConfirmationText(),
											link,
											action));
				}
			}
		}.process(outerReference);

		UIComponent component = c.get();
		if (component != null) {
			addComponent(null,
							formColspan,
							false,
							link.getInvisibleConditionName(),
							null,
							component,
							link.getPixelWidth(),
							null,
							null,
							null,
							null,
							null,
							null);
		}
	}

	@Override
	public void renderFormBlurb(String markup, Blurb blurb) {
		renderBlurb(markup, getCurrentWidgetColspan(), blurb);
	}

	@Override
	public void renderContainerColumnBlurb(String markup, Blurb blurb) {
		renderBlurb(markup, 0, blurb);
	}

	@Override
	public void renderBlurb(String markup, Blurb blurb) {
		renderBlurb(markup, 0, blurb);
	}

	private void renderBlurb(String markup, int formColspan, Blurb blurb) {
		String value = null;
		String binding = null;
		if (markup.indexOf('{') > -1) {
			binding = markup;
		}
		else {
			value = markup;
		}
		UIComponent c = cb.blurb(null, dataWidgetVar, value, binding, blurb);
		addComponent(null,
						formColspan,
						false,
						blurb.getInvisibleConditionName(),
						null,
						c,
						blurb.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderFormLabel(String value, boolean boundValue, Label label) {
		renderLabel(value, getCurrentWidgetColspan(), boundValue, label);
	}

	@Override
	public void renderContainerColumnLabel(String value, Label label) {
		renderLabel(value, 0, false, label);
	}

	@Override
	public void renderLabel(String value, boolean boundValue, Label label) {
		renderLabel(value, 0, boundValue, label);
	}

	private void renderLabel(String value, int formColspan, boolean boundValue, Label label) {
		String ultimateValue = value;
		String binding = label.getBinding();
		if (boundValue) {
			binding = value;
			ultimateValue = null;
		}

		UIComponent c = cb.label(null, dataWidgetVar, ultimateValue, binding, label);
		addComponent(null,
						formColspan,
						false,
						label.getInvisibleConditionName(),
						null,
						c,
						label.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderFormProgressBar(ProgressBar progressBar) {
		UIComponent p = cb.label(null, "progressBar"); // TODO progress bar
		addComponent(null,
						getCurrentWidgetColspan(),
						false,
						progressBar.getInvisibleConditionName(),
						null,
						p,
						progressBar.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		// Use the component builder specified by the listGrid property if it exists
		String componentBuilderClass = grid.getProperties().get(ComponentBuilder.COMPONENT_BUILDER_CLASS_KEY);
		ComponentBuilder componentBuilder = cb;
		if (componentBuilderClass != null) {
			componentBuilder = org.skyve.impl.web.faces.components.ListGrid.newComponentBuilder(componentBuilderClass);
	        componentBuilder.setManagedBeanName(cb.managedBeanName);
	    	componentBuilder.setProcess(cb.process);
	    	componentBuilder.setUpdate(cb.update);
	    	componentBuilder.setUserAgentType(cb.userAgentType);
		}
		
		UIComponent component = componentBuilder.listGrid(null,
															module.getName(),
															getCurrentListWidgetModelDocumentName(),
															getCurrentListWidgetModelName(),
															currentUxUi,
															getCurrentListWidgetModel(),
															document,
															grid,
															aggregateQuery);

		addToContainerWithPotentialBorder(component,
											(title == null) ? Boolean.FALSE : Boolean.TRUE,
											title,
											grid,
											grid.getInvisibleConditionName(),
											null,
											null);
		
		if ((! aggregateQuery) && (! grid.getContinueConversation()) && (! Boolean.FALSE.equals(grid.getShowZoom()))) {
			// Add as a sibling to the list grid
			component.getParent().getChildren().add(componentBuilder.listGridContextMenu(null, component.getId(), grid));
		}
	}

	@Override
	public void renderListGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderListGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderedListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		addedToContainerWithPotentialBorder((title == null) ? Boolean.FALSE : Boolean.TRUE, null, null);
	}

	@Override
	public void renderListRepeater(String title, ListRepeater repeater) {
		UIComponent component = cb.listRepeater(null,
													getCurrentListWidgetModelDocumentName(),
													getCurrentListWidgetModelName(),
													currentUxUi,
													getCurrentListWidgetModel(),
													repeater.getFilterParameters(),
													repeater.getParameters(),
													Boolean.TRUE.equals(repeater.getShowColumnHeaders()),
													Boolean.TRUE.equals(repeater.getShowGrid()));
		addToContainerWithPotentialBorder(component,
											(title == null) ? Boolean.FALSE : Boolean.TRUE,
											title,
											repeater,
											repeater.getInvisibleConditionName(),
											null,
											null);
	}

	@Override
	public void renderListRepeaterProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderListRepeaterContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderedListRepeater(String title, ListRepeater repeater) {
		addedToContainerWithPotentialBorder((title == null) ? Boolean.FALSE : Boolean.TRUE, null, null);
	}

	@Override
	public void renderTreeGrid(String title, TreeGrid grid) {
		UIComponent l = cb.label(null, "treeGrid");
		addToContainer(l,
						grid.getPixelWidth(),
						grid.getResponsiveWidth(),
						grid.getPercentageWidth(),
						grid.getSm(),
						grid.getMd(),
						grid.getLg(),
						grid.getXl(),
						grid.getInvisibleConditionName()); // TODO tree grid
	}

	@Override
	public void renderTreeGridProjectedColumn(MetaDataQueryProjectedColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderTreeGridContentColumn(MetaDataQueryContentColumn column) {
		// TODO Auto-generated method stub
	}

	@Override
	public void renderedTreeGrid(String title, TreeGrid grid) {
		addedToContainer();
	}

	private String dataWidgetBinding;
	private String dataWidgetVar;

	@Override
	public void renderDataGrid(String title, DataGrid grid) {
		// Determine if the grid collection is ordered
		dataWidgetBinding = grid.getBinding();
		boolean ordered = false;
		final TargetMetaData target = getCurrentTarget();
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation instanceof Collection) {
				ordered = Boolean.TRUE.equals(((Collection) targetRelation).getOrdered());
			}
		}

		// Create the datagrid faces component
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		UIComponent component = cb.dataGrid(null, dataWidgetVar, ordered, grid);
		addToContainerWithPotentialBorder(component,
											(title == null) ? Boolean.FALSE : Boolean.TRUE,
											title,
											grid,
											grid.getInvisibleConditionName(),
											null,
											grid.getWidgetId());

		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			fragment = component;
		}
	}

	@Override
	public void renderedDataGrid(String title, DataGrid grid) {
		renderedDataWidget(title, grid);
	}

	@Override
	public void renderDataRepeater(String title, DataRepeater repeater) {
		// Create the data repeater faces component
		dataWidgetBinding = repeater.getBinding();
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		UIComponent component = cb.dataRepeater(null, dataWidgetVar, repeater);
		addToContainerWithPotentialBorder(component,
											(title == null) ? Boolean.FALSE : Boolean.TRUE,
											title,
											repeater,
											repeater.getInvisibleConditionName(),
											null,
											repeater.getWidgetId());
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(repeater.getWidgetId()))) {
			fragment = component;
		}
	}

	@Override
	public void renderedDataRepeater(String title, DataRepeater repeater) {
		renderedDataWidget(title, repeater);
	}

	private void renderedDataWidget(String title, AbstractDataWidget widget) {
		// Determine the document alias
		String alias = null;
		boolean canCreate = false;
		boolean canDelete = false;
		TargetMetaData target = getCurrentTarget();
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation != null) {
				final Document targetDocument = module.getDocument(customer, targetRelation.getDocumentName());
				alias = targetDocument.getLocalisedSingularAlias();
				canCreate = user.canCreateDocument(targetDocument);
				canDelete = user.canDeleteDocument(targetDocument);
			}
		}

		if (widget instanceof DataGrid) {
			DataGrid grid = (DataGrid) widget;
			current = cb.addDataGridActionColumn(null,
													current,
													grid,
													dataWidgetVar,
													gridColumnExpression.toString(),
													alias,
													Boolean.TRUE.equals(grid.getInline()),
													canCreate,
													canDelete);
		}
		dataWidgetBinding = null;
		dataWidgetVar = null;
		gridColumnExpression = null;

		addedToContainerWithPotentialBorder((title == null) ? Boolean.FALSE : Boolean.TRUE, null, widget.getWidgetId());
	}

	private StringBuilder gridColumnExpression;

	@Override
	public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderDataGridBoundColumn(title, column);
	}

	@Override
	public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
		String binding = column.getBinding();
		HorizontalAlignment alignment = column.getAlignment();
		Integer pixelWidth = column.getPixelWidth();

		TargetMetaData target = getCurrentTarget();

		if (binding == null) {
			binding = Bean.BIZ_KEY;
		}
		else {
			if (target != null) {
				Attribute targetAttribute = target.getAttribute();
				if (targetAttribute != null) {
					AttributeType attributeType = targetAttribute.getAttributeType();
					Customisations customisations = CORE.getCustomisations();
					if (alignment == null) {
						alignment = customisations.determineDefaultTextAlignment(currentUxUi, attributeType);
					}
					if (pixelWidth == null) {
						pixelWidth = customisations.determineDefaultColumnWidth(currentUxUi, attributeType);
					}

					if (targetAttribute instanceof Association) {
						binding = BindUtil.createCompoundBinding(binding, Bean.BIZ_KEY);
					}
				}
			}
		}

		current = cb.addDataGridBoundColumn(null,
												current,
												getCurrentDataWidget(),
												column,
												dataWidgetVar,
												title,
												binding,
												gridColumnExpression,
												alignment,
												pixelWidth);
	}

	@Override
	public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderedDataGridBoundColumn(title, column);
	}

	@Override
	public void renderedDataGridBoundColumn(String title, DataGridBoundColumn column) {
		current = cb.addedDataGridBoundColumn(null, current);
	}

	@Override
	public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	@Override
	public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
		TargetMetaData target = getCurrentTarget();
		HorizontalAlignment alignment = column.getAlignment();
		if ((alignment == null) && (target != null)) {
			Attribute targetAttribute = target.getAttribute();
			if (targetAttribute != null) {
				alignment = CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, targetAttribute.getAttributeType());
			}
		}

		current = cb.addDataGridContainerColumn(null, current, getCurrentDataWidget(), title, column, alignment);
	}

	@Override
	public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	@Override
	public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
		current = cb.addedDataGridContainerColumn(null, current);
	}

	@Override
	public void renderBoundColumnCheckBox(CheckBox checkBox) {
		renderCheckBox(0, checkBox);
	}

	@Override
	public void renderFormCheckBox(CheckBox checkBox) {
		renderCheckBox(getCurrentWidgetColspan(), checkBox);
	}

	private void renderCheckBox(int formColspan, CheckBox checkBox) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.checkBox(null,
												dataWidgetVar,
												checkBox,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						checkBox.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						checkBox.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnCheckBox(CheckBox checkBox) {
		renderedFormCheckBox(checkBox);
	}

	@Override
	public void renderedFormCheckBox(CheckBox checkBox) {
		eventSource = null;
	}

	@Override
	public void renderCheckMembership(CheckMembership membership) {
		UIComponentBase c = (UIComponentBase) cb.label(null, "checkMembership"); // TODO check membership
		eventSource = c;
		addToContainer(c, null, null, null, null, null, null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void renderedCheckMembership(CheckMembership membership) {
		addedToContainer();
		eventSource = null;
	}

	@Override
	public void renderBoundColumnColourPicker(ColourPicker colour) {
		renderColourPicker(0, colour);
	}

	@Override
	public void renderFormColourPicker(ColourPicker colour) {
		renderColourPicker(getCurrentWidgetColspan(), colour);
	}

	private void renderColourPicker(int formColspan, ColourPicker colour) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		EventSourceComponent c = cb.colourPicker(null,
													dataWidgetVar,
													colour,
													(currentForm == null) ? null : currentForm.getDisabledConditionName(),
													title,
													required,
													(attribute != null) ?
														CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
														null);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						colour.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						colour.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnColourPicker(ColourPicker colour) {
		renderedFormColourPicker(colour);
	}

	@Override
	public void renderedFormColourPicker(ColourPicker colour) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnCombo(Combo combo) {
		renderCombo(0, combo);
	}

	@Override
	public void renderFormCombo(Combo combo) {
		renderCombo(getCurrentWidgetColspan(), combo);
	}

	private void renderCombo(int formColspan, Combo combo) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.combo(null,
											dataWidgetVar,
											combo,
											(currentForm == null) ? null : currentForm.getDisabledConditionName(),
											title,
											required);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						combo.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						combo.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnCombo(Combo combo) {
		renderedFormCombo(combo);
	}

	@Override
	public void renderedFormCombo(Combo combo) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnContentImage(ContentImage image) {
		renderContentImage(0, image);
	}

	@Override
	public void renderContainerColumnContentImage(ContentImage image) {
		renderContentImage(0, image);
	}

	@Override
	public void renderFormContentImage(ContentImage image) {
		renderContentImage(getCurrentWidgetColspan(), image);
	}

	private void renderContentImage(int formColspan, ContentImage image) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		UIComponent c = cb.contentImage(null,
											dataWidgetVar,
											image,
											(currentForm == null) ? null : currentForm.getDisabledConditionName(),
											title,
											required);
		addComponent(title,
						formColspan,
						false,
						image.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c,
						image.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderBoundColumnContentLink(String value, ContentLink link) {
		renderContentLink(0, link);
	}

	@Override
	public void renderFormContentLink(String value, ContentLink link) {
		renderContentLink(getCurrentWidgetColspan(), link);
	}

	private void renderContentLink(int formColspan, ContentLink link) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		UIComponent c = cb.contentLink(null,
										dataWidgetVar,
										link,
										(currentForm == null) ? null : currentForm.getDisabledConditionName(),
										title,
										required,
										(attribute != null) ?
											CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
											null);
		addComponent(title,
						formColspan,
						required,
						link.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c,
						link.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderFormContentSignature(ContentSignature signature) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		UIComponent c = lb.contentSignatureLayout(null, signature);
		addComponent(title,
						getCurrentWidgetColspan(),
						false,
						signature.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c,
						signature.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
		cb.addContentSignature(null,
								c,
								signature,
								(currentForm == null) ? null : currentForm.getDisabledConditionName(),
								title,
								required);
	}

	@Override
	public void renderBoundColumnHTML(HTML html) {
		renderHTML(0, html);
	}

	@Override
	public void renderFormHTML(HTML html) {
		renderHTML(getCurrentWidgetColspan(), html);
	}

	private void renderHTML(int formColspan, HTML html) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		UIComponent c = cb.html(null,
									dataWidgetVar,
									html,
									(currentForm == null) ? null : currentForm.getDisabledConditionName(),
									title,
									required);
		addComponent(title,
						formColspan,
						required,
						html.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c,
						html.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		EventSourceComponent c = cb.listMembership(null, candidatesHeading, membersHeading, membership);
		eventSource = c.getEventSource();
		Integer pixelWidth = membership.getPixelWidth();
		addToContainer(c.getComponent(),
						pixelWidth,
						null,
						null,
						null,
						null,
						null,
						null,
						membership.getInvisibleConditionName());
	}

	@Override
	public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		addedToContainer();
		eventSource = null;
	}

	@Override
	public void renderComparison(Comparison comparison) {
		UIComponent c = cb.label(null, "comparison"); // TODO comparison
		addToContainer(c,
						comparison.getPixelWidth(),
						comparison.getResponsiveWidth(),
						comparison.getPercentageWidth(),
						comparison.getSm(),
						comparison.getMd(),
						comparison.getLg(),
						comparison.getXl(),
						comparison.getInvisibleConditionName());
		addedToContainer();
	}

	@Override
	public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
													boolean canCreate,
													boolean canUpdate,
													String descriptionBinding,
													LookupDescription lookup) {
		renderLookupDescription(query, 0, canCreate, canUpdate, descriptionBinding, lookup);
	}

	@Override
	public void renderFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		renderLookupDescription(query, getCurrentWidgetColspan(), canCreate, canUpdate, descriptionBinding, lookup);
	}

	public void renderLookupDescription(MetaDataQueryDefinition query,
											int formColspan,
											// No zooming in PF
											@SuppressWarnings("unused") boolean canCreate,
											// No zooming in PF
											@SuppressWarnings("unused") boolean canUpdate,
											String descriptionBinding,
											LookupDescription lookup) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		EventSourceComponent c = cb.lookupDescription(null,
														dataWidgetVar,
														lookup,
														(currentForm == null) ? null : currentForm.getDisabledConditionName(),
														title,
														required,
														(attribute != null) ?
															CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
															null,
														descriptionBinding,
														query);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						lookup.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						lookup.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnLookupDescription(MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														String descriptionBinding,
														LookupDescription lookup) {
		renderedFormLookupDescription(query, canCreate, canUpdate, descriptionBinding, lookup);
	}

	@Override
	public void renderedFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnPassword(Password password) {
		renderPassword(0, password);
	}

	@Override
	public void renderFormPassword(Password password) {
		renderPassword(getCurrentWidgetColspan(), password);
	}

	private void renderPassword(int formColspan, Password password) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		EventSourceComponent c = cb.password(null,
												dataWidgetVar,
												password,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required,
												(attribute != null) ?
													CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
													null);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						password.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						password.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnPassword(Password password) {
		renderedFormPassword(password);
	}

	@Override
	public void renderedFormPassword(Password password) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnRadio(Radio radio) {
		renderRadio(0, radio);
	}

	@Override
	public void renderFormRadio(Radio radio) {
		renderRadio(getCurrentWidgetColspan(), radio);
	}

	private void renderRadio(int formColspan, Radio radio) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.radio(null,
											dataWidgetVar,
											radio,
											(currentForm == null) ? null : currentForm.getDisabledConditionName(),
											title,
											required);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						radio.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						radio.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnRadio(Radio radio) {
		renderedFormRadio(radio);
	}

	@Override
	public void renderedFormRadio(Radio radio) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnRichText(RichText text) {
		renderRichText(0, text);
	}

	@Override
	public void renderFormRichText(RichText text) {
		renderRichText(getCurrentWidgetColspan(), text);
	}

	private void renderRichText(int formColspan, RichText text) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.richText(null,
												dataWidgetVar,
												text,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						text.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						text.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnRichText(RichText text) {
		renderedFormRichText(text);
	}

	@Override
	public void renderedFormRichText(RichText text) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnSlider(Slider slider) {
		renderSlider(0, slider);
	}

	@Override
	public void renderFormSlider(Slider slider) {
		renderSlider(getCurrentWidgetColspan(), slider);
	}

	private void renderSlider(int formColspan, Slider slider) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		Converter<?> converter = null;
		if (attribute instanceof ConvertibleField) {
			converter = ((ConvertibleField) attribute).getConverter();
		}

		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.slider(null,
												dataWidgetVar,
												slider,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required,
												convertConverter(converter, type));
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						slider.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						slider.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnSlider(Slider slider) {
		renderedFormSlider(slider);
	}

	@Override
	public void renderedFormSlider(Slider slider) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnSpinner(Spinner spinner) {
		renderSpinner(0, spinner);
	}

	@Override
	public void renderFormSpinner(Spinner spinner) {
		renderSpinner(getCurrentWidgetColspan(), spinner);
	}

	private void renderSpinner(int formColspan, Spinner spinner) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		Converter<?> converter = null;
		if (attribute instanceof ConvertibleField) {
			converter = ((ConvertibleField) attribute).getConverter();
		}

		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.spinner(null,
												dataWidgetVar,
												spinner,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required,
												(attribute != null) ?
													CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
													null,
												convertConverter(converter, type));
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						spinner.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						spinner.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnSpinner(Spinner spinner) {
		renderedFormSpinner(spinner);
	}

	@Override
	public void renderedFormSpinner(Spinner spinner) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnTextArea(TextArea text) {
		renderTextArea(0, text);
	}

	@Override
	public void renderFormTextArea(TextArea text) {
		renderTextArea(getCurrentWidgetColspan(), text);
	}

	private void renderTextArea(int formColspan, TextArea text) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		Integer length = null;
		if (attribute instanceof LengthField) {
			length = Integer.valueOf(((LengthField) attribute).getLength());
		}

		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.textArea(null,
												dataWidgetVar,
												text,
												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
												title,
												required,
												(attribute != null) ?
													CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, attribute.getAttributeType()) :
													null,
												length);
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						text.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						text.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnTextArea(TextArea text) {
		renderedFormTextArea(text);
	}

	@Override
	public void renderedFormTextArea(TextArea text) {
		eventSource = null;
	}

	@Override
	public void renderBoundColumnTextField(TextField text) {
		renderTextField(0, text);
	}

	@Override
	public void renderFormTextField(TextField text) {
		renderTextField(getCurrentWidgetColspan(), text);
	}

	private void renderTextField(int formColspan, TextField text) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		TextFormat textFormat = (attribute instanceof Text) ? ((Text) attribute).getFormat() : null;
		Format<?> format = (textFormat == null) ? null : textFormat.getFormat();
		Integer length = null;
		if (attribute instanceof LengthField) {
			length = Integer.valueOf(((LengthField) attribute).getLength());
		}
		Converter<?> converter = null;
		if (attribute instanceof ConvertibleField) {
			converter = ((ConvertibleField) attribute).getConverter();
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
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		EventSourceComponent c = cb.text(null,
											dataWidgetVar,
											text,
											(currentForm == null) ? null : currentForm.getDisabledConditionName(),
											title,
											required,
											CORE.getCustomisations().determineDefaultTextAlignment(currentUxUi, type),
											length,
											converter,
											format,
											convertConverter(converter, type));
		eventSource = c.getEventSource();
		addComponent(title,
						formColspan,
						required,
						text.getInvisibleConditionName(),
						getCurrentWidgetHelp(),
						c.getComponent(),
						text.getPixelWidth(),
						null,
						null,
						null,
						null,
						null,
						null);
	}

	@Override
	public void renderedBoundColumnTextField(TextField text) {
		renderedFormTextField(text);
	}

	@Override
	public void renderedFormTextField(TextField text) {
		eventSource = null;
	}

	private static jakarta.faces.convert.Converter<?> convertConverter(Converter<?> converter, AttributeType type) {
		jakarta.faces.convert.Converter<?> result = null;
		if (converter != null) {
			// Date
			if (converter instanceof org.skyve.domain.types.converters.date.DD_MM_YYYY) {
				result = new DD_MM_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.date.DD_MMM_YYYY) {
				result = new DD_MMM_YYYY();
			}
			else if (converter instanceof MM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.date.MM_DD_YYYY();
			}
			else if (converter instanceof MMM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.date.MMM_DD_YYYY();
			}
			else if (converter instanceof YYYY_MM_DD) {
				result = new org.skyve.impl.web.faces.converters.date.YYYY_MM_DD();
			}
			// Date/Time
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI) {
				result = new DD_MM_YYYY_HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI) {
				result = new DD_MM_YYYY_HH24_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MM_YYYY) {
				result = new org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI) {
				result = new DD_MMM_YYYY_HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI) {
				result = new DD_MMM_YYYY_HH24_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MMM_YYYY) {
				result = new org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH_MI) {
				result = new MM_DD_YYYY_HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH24_MI) {
				result = new MM_DD_YYYY_HH24_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.datetime.MM_DD_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH_MI) {
				result = new MMM_DD_YYYY_HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH24_MI) {
				result = new MMM_DD_YYYY_HH24_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.MMM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.datetime.MMM_DD_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH_MI) {
				result = new YYYY_MM_DD_HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH24_MI) {
				result = new YYYY_MM_DD_HH24_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.datetime.YYYY_MM_DD) {
				result = new org.skyve.impl.web.faces.converters.datetime.YYYY_MM_DD();
			}
			// Currency
			else if (converter instanceof org.skyve.domain.types.converters.decimal.currency.Decimal10DollarsAndCents) {
				result = new Decimal10DollarsAndCents();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents) {
				result = new Decimal2DollarsAndCents();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute) {
				result = new Decimal2DollarsAndCentsAbsolute();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents) {
				result = new Decimal5DollarsAndCents();
			}
			// Decimal
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal10Converter) {
				result = new Decimal10Converter();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal10TwoDecimalPlaces) {
				result = new Decimal10TwoDecimalPlaces();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal2Converter) {
				result = new Decimal2Converter();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal2Integer) {
				result = new Decimal2Integer();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage) {
				result = new Decimal2IntegerPercentage();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace) {
				result = new Decimal2OneDecimalPlace();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5Converter) {
				result = new Decimal5Converter();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5Integer) {
				result = new Decimal5Integer();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage) {
				result = new Decimal5IntegerPercentage();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace) {
				result = new Decimal5OneDecimalPlace();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5TimeDuration) {
				result = new Decimal5TimeDuration();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces) {
				result = new Decimal5TwoDecimalPlaces();
			}
			else if (converter instanceof org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage) {
				result = new Decimal5TwoDecimalPlacesPercentage();
			}
			// No Faces DynamicEnumerationConverter
			// Geometry
			else if (converter instanceof org.skyve.domain.types.converters.geometry.GeometryConverter) {
				result = new GeometryConverter();
			}
			// Integer
			else if (converter instanceof org.skyve.domain.types.converters.integer.IntegerConverter) {
				result = new IntegerConverter();
			}
			else if (converter instanceof org.skyve.domain.types.converters.integer.IntegerSeparator) {
				result = new IntegerSeparator();
			}
			else if (converter instanceof org.skyve.domain.types.converters.integer.LongIntegerConverter) {
				result = new LongIntegerConverter();
			}
			else if (converter instanceof org.skyve.domain.types.converters.integer.LongIntegerSeparator) {
				result = new LongIntegerSeparator();
			}
			else if (converter instanceof org.skyve.domain.types.converters.integer.SimplePercentage) {
				result = new SimplePercentage();
			}
			// No corresponding Skyve lang converters for Boolean and String as these coercions are implicit
			// No select converters required as these are faces specific - no Skyve converters for these
			// Time
			else if (converter instanceof org.skyve.domain.types.converters.time.HH_MI_SS) {
				result = new HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.time.HH_MI) {
				result = new HH_MI();
			}
			else if (converter instanceof org.skyve.domain.types.converters.time.HH24_MI_SS) {
				result = new HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.time.HH24_MI) {
				result = new HH24_MI();
			}
			// Timestamp
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS) {
				result = new DD_MM_YYYY_HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS) {
				result = new DD_MM_YYYY_HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MM_YYYY) {
				result = new org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS) {
				result = new DD_MMM_YYYY_HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS) {
				result = new DD_MMM_YYYY_HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY) {
				result = new org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH_MI_SS) {
				result = new MM_DD_YYYY_HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH24_MI_SS) {
				result = new MM_DD_YYYY_HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.timestamp.MM_DD_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH_MI_SS) {
				result = new MMM_DD_YYYY_HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS) {
				result = new MMM_DD_YYYY_HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY) {
				result = new org.skyve.impl.web.faces.converters.timestamp.MMM_DD_YYYY();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH_MI_SS) {
				result = new org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD_HH_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH24_MI_SS) {
				result = new org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD_HH24_MI_SS();
			}
			else if (converter instanceof org.skyve.domain.types.converters.timestamp.YYYY_MM_DD) {
				result = new org.skyve.impl.web.faces.converters.timestamp.YYYY_MM_DD();
			}
			else {
				throw new IllegalArgumentException(converter + " cannot be converted to a faces converter");
			}
		}
		else {
			// Set default faces numeric converters if none is set
			if (AttributeType.decimal2.equals(type)) {
				result = new Decimal2Converter();
			}
			else if (AttributeType.decimal5.equals(type)) {
				result = new Decimal5Converter();
			}
			else if (AttributeType.decimal10.equals(type)) {
				result = new Decimal10Converter();
			}
			else if (AttributeType.integer.equals(type)) {
				result = new IntegerConverter();
			}
			else if (AttributeType.longInteger.equals(type)) {
				result = new LongIntegerConverter();
			}
		}

		return result;
	}

	@Override
	public void renderFormInject(Inject inject) {
		// do nothing - this is for web 2 ux uis only
	}

	@Override
	public void renderInject(Inject inject) {
		// do nothing - this is for web 2 ux uis only
	}

	private void addToContainer(UIComponent component,
									Integer pixelWidth,
									Integer responsiveWidth,
									Integer percentageWidth,
									Integer sm,
									Integer md,
									Integer lg,
									Integer xl,
									String invisibleConditionName) {
		Stack<Container> currentContainers = getCurrentContainers();
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();

		current = lb.addToContainer(null,
										currentContainer,
										current,
										component,
										pixelWidth,
										responsiveWidth,
										percentageWidth,
										sm,
										md,
										lg,
										xl,
										invisibleConditionName);
	}

	private void addedToContainer() {
		Stack<Container> currentContainers = getCurrentContainers();
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to complete the add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();
		current = lb.addedToContainer(null, currentContainer, current);
	}
	
	private void addToContainerWithPotentialBorder(UIComponent component,
													Boolean border,
													String borderTitle,
													RelativeSize size,
													String invisibleConditionName,
													Collapsible collapsible,
													String thisWidgetId) {
		boolean bordered = (collapsible != null) || Boolean.TRUE.equals(border);
		
		validateCollapsible(collapsible, borderTitle);
		// Cater for a border if this thing has a border
		if (bordered) {
			UIComponent borderComponent = cb.border(null, borderTitle, invisibleConditionName, size.getPixelWidth(), collapsible);
			addToContainer(borderComponent,
							size.getPixelWidth(),
							size.getResponsiveWidth(),
							size.getPercentageWidth(),
							size.getSm(),
							size.getMd(),
							size.getLg(),
							size.getXl(),
							invisibleConditionName);
	
			lb.addBorderLayout(borderComponent, component);
	
			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(thisWidgetId))) {
				fragment = borderComponent;
			}
		}
		else {
			addToContainer(component,
							size.getPixelWidth(),
							size.getResponsiveWidth(),
							size.getPercentageWidth(),
							size.getSm(),
							size.getMd(),
							size.getLg(),
							size.getXl(),
							invisibleConditionName);
	
			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(thisWidgetId))) {
				fragment = component;
			}
		}
		
		current = component;
	}

	private void addedToContainerWithPotentialBorder(Boolean border, Collapsible collapsible, String thisWidgetId) {
		// Cater for border, if one was added
		if ((collapsible != null) || Boolean.TRUE.equals(border)) {
			current = lb.addedBorderLayout(null, current);
		}
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(thisWidgetId))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}
	
	@Override
	public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void renderCustomAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (UIComponent toolbarLayout : toolbarLayouts) {
					toolbarLayout.getChildren().add(cb.action(null,
																dataWidgetBinding,
																dataWidgetVar,
																label,
																iconStyleClass,
																toolTip,
																confirmationText,
																null,
																action));
				}
			}
		}
	}

	@Override
	public void renderAddAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
//		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Add);
	}

	@Override
	public void renderRemoveAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action,
									boolean canDelete) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Remove, canDelete);
	}

	@Override
	public void renderZoomOutAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.ZoomOut, false);
	}

	@Override
	public void renderNavigateAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
//		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Navigate, false);
	}

	@Override
	public void renderOKAction(String name,
								String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.OK, false);
	}

	@Override
	public void renderSaveAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Save, false);
	}

	@Override
	public void renderCancelAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Cancel, false);
	}

	@Override
	public void renderDeleteAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Delete, false);
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/report/Bum.html?_f=html&_c=<webId>&_id=<id>&wee=poo&_n=Bum&_mod=<module>&_doc=<document>
	 * 
	 * @param action
	 */
	@Override
	public void renderReportAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Report, false);
	}

	@Override
	public void renderBizExportAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.BizExport, false);
	}

	@Override
	public void renderBizImportAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.BizImport, false);
	}

	@Override
	public void renderDownloadAction(String name,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Download, false);
	}

	@Override
	public void renderUploadAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.Upload, false);
	}

	@Override
	public void renderNewAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
//		processImplicitAction(label, iconStyleClass, toolTip, confirmationText, action, ImplicitActionName.New, false);
	}

	@Override
	public void renderEditAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Edit);
	}

	@Override
	public void renderPrintAction(String name,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									ActionImpl action) {
		// TODO implement
	}

	private void processImplicitAction(String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										Action action,
										ImplicitActionName name,
										boolean canDelete) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (UIComponent toolbarLayout : toolbarLayouts) {
					if (ImplicitActionName.Report.equals(name)) {
						toolbarLayout.getChildren().add(cb.report(null, label, iconStyleClass, toolTip, confirmationText, action));
					}
					else if (ImplicitActionName.Download.equals(name)) {
						toolbarLayout.getChildren().add(cb.download(null,
																		dataWidgetBinding,
																		dataWidgetVar,
																		label,
																		iconStyleClass,
																		toolTip,
																		confirmationText,
																		action));
					}
					else if (ImplicitActionName.Upload.equals(name)) {
						toolbarLayout.getChildren().add(cb.upload(null, label, iconStyleClass, toolTip, confirmationText, action));
					}
					else if (ImplicitActionName.Remove.equals(name)) {
						toolbarLayout.getChildren().add(cb.remove(null, label, iconStyleClass, toolTip, confirmationText, action, canDelete));
					}
					else {
						toolbarLayout.getChildren().add(cb.action(null,
																	dataWidgetBinding,
																	dataWidgetVar,
																	label,
																	iconStyleClass,
																	toolTip,
																	confirmationText,
																	name,
																	action));
					}
				}
			}
		}
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		String binding = changeable.getBinding();
		List<EventAction> changedActions = changeable.getChangedActions();

		if (eventSource instanceof PickList) {
			cb.addAjaxBehavior(eventSource, "transfer", dataWidgetBinding, dataWidgetVar, binding, changedActions);
		}
		else {
			cb.addAjaxBehavior(eventSource, "change", dataWidgetBinding, dataWidgetVar, binding, changedActions);
			// No need to add dateSelect event to datePicker as "changed" fires on the input field when selected
/*
			if (eventSource instanceof DatePicker) {
				cb.addAjaxBehavior(eventSource, "dateSelect", dataWidgetBinding, dataWidgetVar, binding, changedActions);
			}
*/
		}
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		String binding = (blurable instanceof Bound) ? ((Bound) blurable).getBinding() : null;
		cb.addAjaxBehavior(eventSource, "focus", dataWidgetBinding, dataWidgetVar, binding, blurable.getFocusActions());
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		String binding = (blurable instanceof Bound) ? ((Bound) blurable).getBinding() : null;
		cb.addAjaxBehavior(eventSource, "blur", dataWidgetBinding, dataWidgetVar, binding, blurable.getBlurActions());
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		cb.addAjaxBehavior(eventSource, "itemSelect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(),
				lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		cb.addAjaxBehavior(eventSource,
							"itemUnselect",
							dataWidgetBinding,
							dataWidgetVar,
							lookup.getBinding(),
							lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// nothing to see here
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	private static void validateCollapsible(Collapsible collapsible, String borderTitle) {
		if (collapsible != null && borderTitle == null) {
			throw new MetaDataException("Border title must be defined if the collapsible attribute is present");
		}
	}

	@Override
	public void renderSidebar(Sidebar sidebar) {
		UIComponent layout = lb.sidebarLayout(null, sidebar, createView);
		
		addToContainer(layout,
						sidebar.getPixelWidth(),
						sidebar.getResponsiveWidth(),
						sidebar.getPercentageWidth(),
						null,
						null,
						null,
						null,
						sidebar.getInvisibleConditionName());

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(sidebar.getWidgetId()))) {
			fragment = layout;
		}
		
		facesSidebar = layout;
		
		current = layout.getChildren().get(0);
		
		if ((widgetId == null) || ((widgetId != null) && (fragment != null))) {
			scripts.add(cb.sidebarScript(null, sidebar, createView, facesSidebar.getId()));
		}
	}

	@Override
	public void renderedSidebar(Sidebar sidebar) {
		addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(sidebar.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}				
	}
}
