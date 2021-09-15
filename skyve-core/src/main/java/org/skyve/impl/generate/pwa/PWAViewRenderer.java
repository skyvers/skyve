package org.skyve.impl.generate.pwa;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.container.HBox;
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
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

public class PWAViewRenderer extends ViewRenderer {
	protected ComponentRenderer cr;
	protected LayoutRenderer lr;
	protected boolean createView;

	private RenderedComponent current; // current component being constructed
	private RenderedComponent result; // the result of construction
	private List<RenderedComponent> toolbarLayouts; // the toolbar layouts

	public PWAViewRenderer(User user, Module module, Document document, View view) {
		super(user, module, document, view);
		createView = ViewType.create.toString().equals(view.getName());
	}
	
	protected void setRenderers(ComponentRenderer cr, LayoutRenderer lr) {
		this.cr = cr;
		this.lr = lr;
	}
	
	public RenderedComponent getResult() {
		return result;
	}

	@Override
	public void renderView(String icon16x16Url, String icon32x32Url) {
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

	@Override
	public void renderedView(String icon16x16Url, String icon32x32Url) {
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

	@Override
	public void renderTabPane(TabPane tabPane) {
		RenderedComponent component = cr.tabPane(null, tabPane);
		addToContainer(component, 
        				tabPane.getPixelWidth(), 
        				tabPane.getResponsiveWidth(), 
        				tabPane.getPercentageWidth(),
        				tabPane.getInvisibleConditionName());
	}

	@Override
	public void renderedTabPane(TabPane tabPane) {
		addedToContainer();
	}
	
	@Override
	public void renderTab(String title, String icon16x16Url, Tab tab) {
		RenderedComponent component = cr.tab(null, title, tab);
		lr.addTab(current, component);
		current = component;
		RenderedComponent layout = lr.tabLayout(null);
		if (layout != null) {
			current = lr.addTabLayout(null, component, layout);
		}
	}

	@Override
	public void renderedTab(String title, String icon16x16Url, Tab tab) {
		current = lr.addedTab(null, current);
	}

	@Override
	public void renderVBox(String borderTitle, VBox vbox) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			border = cr.border(null, borderTitle, vbox.getInvisibleConditionName(), vbox.getPixelWidth());
			addToContainer(border, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth(),
							vbox.getInvisibleConditionName());
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
							vbox.getInvisibleConditionName());
		}
		current = layout;
	}

	@Override
	public void renderedVBox(String borderTitle, VBox vbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	@Override
	public void renderHBox(String borderTitle, HBox hbox) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			border = cr.border(null, borderTitle, hbox.getInvisibleConditionName(), hbox.getPixelWidth());
			addToContainer(border, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth(),
							hbox.getInvisibleConditionName());
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
							hbox.getInvisibleConditionName());
		}
		current = layout;
	}

	@Override
	public void renderedHBox(String title, HBox hbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	@Override
	public void renderForm(String borderTitle, Form form) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(form.getBorder())) {
			border = cr.border(null, borderTitle, form.getInvisibleConditionName(), form.getPixelWidth());
			addToContainer(border, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth(),
							form.getInvisibleConditionName());
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
							form.getInvisibleConditionName());
		}
		current = layout;
// TODO form.getDisabledConditionName() form.getLabelDefaultHorizontalAlignment()
	}

	@Override
	public void renderedForm(String borderTitle, Form form) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(form.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
	}

	@Override
	public void renderFormColumn(FormColumn column) {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	private RenderedComponent formRowLayout = null;
	
	@Override
	public void renderFormRow(FormRow row) {
		formRowLayout = lr.formRowLayout(null, row);
		if (formRowLayout != null) {
			current = lr.addFormRowLayout(null, current, formRowLayout);
		}
	}
	
	@Override
	public void renderFormItem(String label, boolean required, String help, boolean showsLabel, FormItem item) {
	}

	@Override
	public void renderedFormItem(String label, boolean required, String help, boolean showLabel, FormItem item) {
	}

	@Override
	public void renderedFormRow(FormRow row) {
		if (formRowLayout != null) {
			current = lr.addedFormRowLayout(null, formRowLayout);
		}
		formRowLayout = null;
	}

	private void addComponent(String widgetLabel,
								boolean widgetRequired,
								String widgetInvisible,
								String helpText,
								RenderedComponent component,
								Integer pixelWidth,
								Integer responsiveWidth,
								Integer percentageWidth) {
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
				if (isCurrentWidgetShowLabel()) {
					lr.layoutFormItemLabel(current,
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
				lr.layoutFormItemWidget(current,
											component,
											currentForm, 
											formItem, 
											formColumn,
											widgetLabel,
											widgetRequired,
											widgetInvisible,
											helpText);
				Integer colspan = formItem.getColspan();
				if (colspan == null) {
					incrementFormColumn();
				}
				else {
					for (int i = 0, l = colspan.intValue(); i< l; i++) {
						incrementFormColumn();
					}
				}
			}
		}
	}
	
	@Override
	public void renderFormButton(Action action,
									String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									Button button) {
		renderButton(action, label, iconUrl, iconStyleClass, toolTip, confirmationText, type, button);
	}
	
	@Override
	public void renderButton(Action action,
								String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								char type,
								Button button) {
		ImplicitActionName name = action.getImplicitName();
		RenderedComponent c = null;
		if (ImplicitActionName.Report.equals(name)) {
			c = cr.reportButton(null, button, action);
		}
		else if (ImplicitActionName.Download.equals(name)) {
			c = cr.downloadButton(null, button, action, module.getName(), document.getName());
		}
		else {
			c = cr.actionButton(null, dataWidgetBinding, dataWidgetVar, button, action);
		}
	    addComponent(null, 
	    				false, 
	    				action.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				button.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void renderFormZoomIn(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									ZoomIn zoomIn) {
		renderFormZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
	}
	
	@Override
	public void renderZoomIn(String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								ZoomIn zoomIn) {
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "zoomIn " + label); // TODO geometry
		eventSource = c;
	    addComponent(null, 
	    				false, 
	    				zoomIn.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				zoomIn.getPixelWidth(), 
	    				null, 
	    				null);
	}
	
	@Override
	public void renderBoundColumnGeometry(Geometry geometry) {
		renderFormGeometry(geometry);
	}
	
	@Override
	public void renderFormGeometry(Geometry geometry) {
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "geometry"); // TODO geometry
		eventSource = c;
	    addComponent(null, 
	    				false, 
	    				geometry.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				geometry.getPixelWidth(), 
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
//		String title = getCurrentWidgetLabel();
//		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "geometryMap"); // TODO geometryMap
		eventSource = c;
	    addComponent(null, 
	    				false, 
	    				geometry.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				geometry.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void renderedFormGeometryMap(GeometryMap geometry) {
		eventSource = null;
	}
	
	@Override
	public void renderMap(MapDisplay map) {
		RenderedComponent l = cr.label(null, "map"); // TODO map
	    addComponent(null, 
	    				false, 
	    				map.getInvisibleConditionName(), 
	    				null,
	    				l, 
	    				map.getPixelWidth(), 
	    				map.getResponsiveWidth(),
	    				map.getPercentageWidth());
	}

	@Override
	public void renderChart(Chart chart) {
		RenderedComponent l = cr.label(null, "chart"); // TODO chart
	    addComponent(null, 
	    				false, 
	    				chart.getInvisibleConditionName(), 
	    				null,
	    				l, 
	    				chart.getPixelWidth(), 
	    				chart.getResponsiveWidth(),
	    				chart.getPercentageWidth());
	}

	@Override
	public void renderFormDialogButton(String label, DialogButton button) {
		renderDialogButton(label, button);
	}
	
	@Override
	public void renderDialogButton(String label, DialogButton button) {
		RenderedComponent bn = cr.label(null, "dialogButton"); // TODO dialog button
	    addComponent(null, 
	    				false, 
	    				button.getInvisibleConditionName(), 
	    				null,
	    				bn, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void renderContainerColumnDynamicImage(DynamicImage image) {
		renderDynamicImage(image);
	}
	
	@Override
	public void renderDynamicImage(DynamicImage image) {
		RenderedComponent i = cr.dynamicImage(null, image, module.getName(), document.getName());
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	@Override
	public void renderFormSpacer(Spacer spacer) {
		renderSpacer(spacer);
	}

	@Override
	public void renderSpacer(Spacer spacer) {
		RenderedComponent component = cr.spacer(null, spacer);
		if (component != null) {
			addComponent(null, 
							false, 
							spacer.getInvisibleConditionName(), 
							null,
							component, 
							spacer.getPixelWidth(), 
							null, 
							null);
		}
	}

	@Override
	public void renderFormStaticImage(String fileUrl, StaticImage image) {
		renderStaticImage(fileUrl, image);
	}
	
	@Override
	public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
		renderStaticImage(fileUrl, image);
	}
	
	@Override
	public void renderStaticImage(String fileUrl, StaticImage image) {
		RenderedComponent i = cr.staticImage(null, fileUrl, image);
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	@Override
	public void renderFormBlurb(String markup, Blurb blurb) {
		renderBlurb(markup, blurb);
	}

	@Override
	public void renderContainerColumnBlurb(String markup, Blurb blurb) {
		renderBlurb(markup, blurb);
	}
	
	@Override
	public void renderBlurb(String markup, Blurb blurb) {
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
						false, 
						blurb.getInvisibleConditionName(), 
						null,
						c, 
						blurb.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void renderFormLink(String value, Link link) {
		renderLink(value, link);
	}
	
	@Override
	public void renderContainerColumnLink(String value, Link link) {
		renderLink(value, link);
	}

	@Override
	public void renderLink(String value, Link link) {
		org.skyve.impl.metadata.view.reference.Reference outerReference = link.getReference();
		final ReferenceTarget target = link.getTarget();
		final AtomicReference<RenderedComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			@Override
			public void processResourceReference(ResourceReference reference) {
				c.set(cr.label(null, "resource link")); // TODO link
			}
			
			@Override
			public void processReportReference(ReportReference reference) {
				c.set(cr.label(null, "report link")); // TODO link
			}
			
			@Override
			public void processQueryListViewReference(QueryListViewReference reference) {
				c.set(cr.label(null, "list view link")); // TODO link
			}
			
			@Override
			public void processImplicitActionReference(ImplicitActionReference reference) {
				c.set(cr.label(null, "implicit action link")); // TODO link
			}
			
			@Override
			public void processExternalReference(ExternalReference reference) {
				c.set(cr.label(null, "external link")); // TODO link
			}
			
			@Override
			public void processEditViewReference(EditViewReference reference) {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');
				c.set(cr.label(null, "external link")); // TODO link
				//c.set(cr.outputLink(listVar, value, href.toString(), link.getInvisibleConditionName(), target));
			}
			
			@Override
			public void processDefaultListViewReference(DefaultListViewReference reference) {
				c.set(cr.label(null, "default list view link")); // TODO link
			}
			
			@Override
			public void processContentReference(ContentReference reference) {
				c.set(cr.label(null, "content link")); // TODO link
			}
			
			@Override
			public void processActionReference(ActionReference reference) {
/* TODO we need the dataWidget binding to make this call
				Action action = obtainActionForActionReference(reference, customer, module, document, dataWidgetBinding, cr.userAgentType);
				if (action != null) {
					c.set(cr.actionLink(null, dataWidgetBinding, dataWidgetVar, link, action));
				}
*/
			}
		}.process(outerReference);

		addComponent(null, 
						false, 
						link.getInvisibleConditionName(), 
						null,
						c.get(), 
						link.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void renderFormLabel(String value, Label label) {
		renderLabel(value, label);
	}
	
	@Override
	public void renderContainerColumnLabel(String value, Label label) {
		renderLabel(value, label);
	}
	
	@Override
	public void renderLabel(String value, Label label) {
		String ultimateValue = label.getLocalisedValue();
		String binding = label.getBinding();
		if ((ultimateValue == null) && (binding == null)) { // using the Label.for attribute
			ultimateValue = "Label";
			TargetMetaData target = getCurrentTarget();
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					ultimateValue = String.format("%s %s:", value, attribute.isRequired() ? "*" : ""); 
				}
			}
		}
		else if ((value != null) && value.indexOf('{') > -1) { // label value with binding expression
			binding = value;
			ultimateValue = null;
		}
		else { // boilerplate value or a binding
			ultimateValue = value;
		}
		RenderedComponent c = cr.label(null, dataWidgetVar, value, binding, label);
	    addComponent(null, 
	    				false, 
	    				label.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				label.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void renderFormProgressBar(ProgressBar progressBar) {
		RenderedComponent p = cr.label(null, "progressBar"); // TODO progress bar
	    addComponent(null, 
	    				false, 
	    				progressBar.getInvisibleConditionName(), 
	    				null,
	    				p, 
	    				progressBar.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
		RenderedComponent l = cr.listGrid(null,
											getCurrentListWidgetModelDocumentName(),
											getCurrentListWidgetModelName(),
											getCurrentListWidgetModel(),
											title,
											grid,
											(user == null) ? true : user.canCreateDocument(getCurrentListWidgetDrivingDocument()),
											aggregateQuery);
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName());
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
		addedToContainer();
	}

	@Override
	public void renderListRepeater(String title, ListRepeater repeater) {
		RenderedComponent r = cr.listRepeater(null,
												getCurrentListWidgetModelDocumentName(), 
												getCurrentListWidgetModelName(), 
												getCurrentListWidgetModel(), 
												repeater.getFilterParameters(),
												repeater.getParameters(),
												title,
												Boolean.TRUE.equals(repeater.getShowColumnHeaders()),
												Boolean.TRUE.equals(repeater.getShowGrid()));
		addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleConditionName());
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
		addedToContainer();
	}
	
	@Override
	public void renderTreeGrid(String title, TreeGrid grid) {
		RenderedComponent l = cr.label(null, "treeGrid");
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName()); // TODO tree grid
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
		RenderedComponent g = cr.dataGrid(null, dataWidgetVar, ordered, title, grid);
        addToContainer(g, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName());
		gridColumnExpression = new StringBuilder(512);
	}

	@Override
	public void renderDataRepeater(String title, DataRepeater repeater) {
		// Create the data repeater faces component
		dataWidgetBinding = repeater.getBinding();
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		RenderedComponent r = cr.dataRepeater(null, dataWidgetVar, title, repeater);
        addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleConditionName());
		gridColumnExpression = new StringBuilder(512);
	}
	
	@Override
	public void renderedDataGrid(String title, DataGrid grid) {
		renderedDataWidget(grid);
	}
	
	@Override
	public void renderedDataRepeater(String title, DataRepeater repeater) {
		renderedDataWidget(repeater);
	}

	private void renderedDataWidget(AbstractDataWidget widget) {
		// Determine the document alias
		String alias = null;
		TargetMetaData target = getCurrentTarget();
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation != null) {
				alias = module.getDocument(customer, targetRelation.getDocumentName()).getLocalisedSingularAlias();
			}
		}

		if (widget instanceof DataGrid) {
			DataGrid grid = (DataGrid) widget;
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

	@Override
	public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderDataGridBoundColumn(title, column);
	}

	@Override
	public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
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
												getCurrentDataWidget(),
												column, 
												dataWidgetVar,
												title, 
												binding, 
												gridColumnExpression);
	}

	@Override
	public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
		renderedDataGridBoundColumn(title, column);
	}

	@Override
	public void renderedDataGridBoundColumn(String title, DataGridBoundColumn column) {
		current = cr.addedDataGridBoundColumn(null, current);
	}

	@Override
	public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	@Override
	public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
		current = cr.addDataGridContainerColumn(null, current, getCurrentDataWidget(), title, column);
	}

	@Override
	public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	@Override
	public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
		current = cr.addedDataGridContainerColumn(null, current);
	}

	// A reference to the current widget that is the source of events
	private RenderedComponent eventSource = null;
	
	@Override
	public void renderBoundColumnCheckBox(CheckBox checkBox) {
		renderFormCheckBox(checkBox);
	}
	
	@Override
	public void renderFormCheckBox(CheckBox checkBox) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.checkBox(null, dataWidgetVar, checkBox, title, required);
		eventSource = c;
		addComponent(title,
						required,
						checkBox.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						c, 
						checkBox.getPixelWidth(), 
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
		RenderedComponent c = cr.label(null, "checkMembership"); // TODO check membership
        eventSource = c;
        addToContainer(c, null, null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void renderedCheckMembership(CheckMembership membership) {
	    addedToContainer();
	    eventSource = null;
	}

	@Override
	public void renderBoundColumnColourPicker(ColourPicker colour) {
		renderFormColourPicker(colour);
	}

	@Override
	public void renderFormColourPicker(ColourPicker colour) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.colourPicker(null, dataWidgetVar, colour, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						colour.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						c, 
						colour.getPixelWidth(), 
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
		renderFormCombo(combo);
	}

	@Override
	public void renderFormCombo(Combo combo) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent s = cr.combo(null, dataWidgetVar, combo, title, required);
		eventSource = s;
		addComponent(title, 
						required, 
						combo.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						s, 
						combo.getPixelWidth(), 
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
		renderFormContentImage(image);
	}

	@Override
	public void renderContainerColumnContentImage(ContentImage image) {
		renderFormContentImage(image);
	}

	@Override
	public void renderFormContentImage(ContentImage image) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.contentImage(null, dataWidgetVar, image, title, required);
        addComponent(title, 
        				false, 
        				image.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				image.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void renderFormContentSignature(ContentSignature signature) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.contentSignature(null, dataWidgetVar, signature, title, required);
        addComponent(title, 
        				false, 
        				signature.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				signature.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void renderBoundColumnContentLink(String value, ContentLink link) {
		renderFormContentLink(value, link);
	}

	@Override
	public void renderFormContentLink(String value, ContentLink link) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.contentLink(null, dataWidgetBinding, link, title, required);
		addComponent(title, 
						required, 
						link.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						c, 
						link.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void renderBoundColumnHTML(HTML html) {
		renderFormHTML(html);
	}

	@Override
	public void renderFormHTML(HTML html) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.html(null, dataWidgetVar, html, title, required);
        addComponent(title, 
        				required, 
        				html.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				html.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void renderListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		RenderedComponent c = cr.listMembership(null, membership);
		eventSource = c;
		addToContainer(c, membership.getPixelWidth(), null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		addedToContainer();
		eventSource = null;
	}

	@Override
	public void renderComparison(Comparison comparison) {
		RenderedComponent c = cr.label(null, "comparison"); // TODO comparison
        addToContainer(c, comparison.getPixelWidth(), comparison.getResponsiveWidth(), comparison.getPercentageWidth(), comparison.getInvisibleConditionName());
        addedToContainer();
	}

	@Override
	public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
													boolean canCreate,
													boolean canUpdate,
													String descriptionBinding,
													LookupDescription lookup) {
		renderFormLookupDescription(query, canCreate, canUpdate, descriptionBinding, lookup);
	}

	@Override
	public void renderFormLookupDescription(MetaDataQueryDefinition query,
												boolean canCreate,
												boolean canUpdate,
												String descriptionBinding,
												LookupDescription lookup) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.lookupDescription(null,
													dataWidgetVar, 
													lookup, 
													title, 
													required,
													descriptionBinding,
													query);
        eventSource = c;
        addComponent(title, 
        				required, 
        				lookup.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				lookup.getPixelWidth(), 
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
		renderFormPassword(password);
	}

	@Override
	public void renderFormPassword(Password password) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.password(null, dataWidgetVar, password, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				password.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				password.getPixelWidth(), 
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
		renderFormRadio(radio);
	}

	@Override
	public void renderFormRadio(Radio radio) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.radio(null, dataWidgetVar, radio, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						radio.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						c, 
						radio.getPixelWidth(), 
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
		renderFormRichText(text);
	}

	@Override
	public void renderFormRichText(RichText text) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.richText(null, dataWidgetVar, text, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				text.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				text.getPixelWidth(), 
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
		renderFormSlider(slider);		
	}

	@Override
	public void renderFormSlider(Slider slider) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.label(null, "slider"); // TODO slider
        eventSource = c;
        addComponent(title, 
        				required, 
        				slider.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				slider.getPixelWidth(), 
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
		renderFormSpinner(spinner);
	}

	@Override
	public void renderFormSpinner(Spinner spinner) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.spinner(null, dataWidgetVar, spinner, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				spinner.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				spinner.getPixelWidth(), 
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
		renderFormTextArea(text);
	}

	@Override
	public void renderFormTextArea(TextArea text) {
		TargetMetaData target = getCurrentTarget();
		Attribute attribute = (target == null) ? null : target.getAttribute();
		Integer length = null;
		if (attribute instanceof LengthField) {
			length = Integer.valueOf(((LengthField) attribute).getLength());
		}

		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		RenderedComponent c = cr.textArea(null, dataWidgetVar, text, title, required, length);
        eventSource = c;
        addComponent(title, 
        				required, 
        				text.getInvisibleConditionName(), 
        				getCurrentWidgetHelp(),
        				c, 
        				text.getPixelWidth(), 
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
		renderFormTextField(text);
	}

	@Override
	public void renderFormTextField(TextField text) {
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
        if (attribute instanceof ConvertableField) {
            converter = ((ConvertableField) attribute).getConverter();
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
        RenderedComponent c = cr.text(null,
										dataWidgetVar, 
										text, 
										title, 
										required,
										length,
										converter,
										format);
        eventSource = c;
		addComponent(title, 
						required, 
						text.getInvisibleConditionName(), 
						getCurrentWidgetHelp(),
						c, 
						text.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void renderFormInject(Inject inject) {
		// TODO
	}

	@Override
	public void renderInject(Inject inject) {
		// TODO
	}
	
	@Override
	public void renderedBoundColumnTextField(TextField text) {
		renderedFormTextField(text);
	}

	@Override
	public void renderedFormTextField(TextField text) {
		eventSource = null;
	}

	private void addToContainer(RenderedComponent component, 
									Integer pixelWidth, 
									Integer responsiveWidth,
									Integer percentageWidth,
									String invisibleConditionName) {
		Stack<Container> currentContainers = getCurrentContainers();
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
										invisibleConditionName);
	}
	
	private void addedToContainer() {
		Stack<Container> currentContainers = getCurrentContainers();
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to complete the add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();
		current = lr.addedToContainer(null, currentContainer, current);
	}
	
	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = changeable.getBinding();
		List<EventAction> changedActions = changeable.getChangedActions();
		cr.addAjaxBehavior(eventSource, "change", dataWidgetBinding, dataWidgetVar, binding, changedActions);
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (blurable instanceof Bound) ? ((Bound) blurable).getBinding() : null;
		cr.addAjaxBehavior(eventSource, "focus", dataWidgetBinding, dataWidgetVar, binding, blurable.getFocusActions());
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		String binding = (blurable instanceof Bound) ? ((Bound) blurable).getBinding() : null;
		cr.addAjaxBehavior(eventSource, "blur", dataWidgetBinding, dataWidgetVar, binding, blurable.getBlurActions());
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable,
											boolean parentVisible,
											boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable,
												boolean parentVisible,
												boolean parentEnabled) {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
		// TODO - need to account for data/list grids in here
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable,
												boolean parentVisible,
												boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// TODO - need to account for data/list/tree grids in here
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemSelect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemUnselect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup,
												boolean parentVisible,
												boolean parentEnabled) {
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
	public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
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
	public void renderCustomAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
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

	private void processImplicitAction(ActionImpl action, ImplicitActionName name) {
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
	
	@Override
	public void renderAddAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Add);
	}

	@Override
	public void renderRemoveAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type, 
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Remove);
	}

	@Override
	public void renderZoomOutAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.ZoomOut);
	}

	@Override
	public void renderNavigateAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Navigate);
	}

	@Override
	public void renderOKAction(String label,
								String iconUrl,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								char type,
								ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.OK);
	}

	@Override
	public void renderSaveAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Save);
	}

	@Override
	public void renderCancelAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Cancel);
	}

	@Override
	public void renderDeleteAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Delete);
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/report/Bum.html?_f=html&_c=<webId>&_id=<id>&wee=poo&_n=Bum&_mod=<module>&_doc=<document>
	 * 
	 * @param action
	 */
	@Override
	public void renderReportAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Report);
	}

	@Override
	public void renderBizExportAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizExport);
	}

	@Override
	public void renderBizImportAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizImport);
	}

	@Override
	public void renderDownloadAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Download);
	}

	@Override
	public void renderUploadAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Upload);
	}

	@Override
	public void renderNewAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.New);
	}

	@Override
	public void renderEditAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Edit);
	}

	@Override
	public void renderPrintAction(String label,
									String iconUrl,
									String iconStyleClass,
									String toolTip,
									String confirmationText,
									char type,
									ActionImpl action) {
		// TODO implement
	}

	@Override
	public void visitParameter(Parameter parameter,
								boolean parentVisible,
								boolean parentEnabled) {
		// nothing to see here
	}
	
	@Override
	public void visitFilterParameter(FilterParameter parameter,
										boolean parentVisible,
										boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
}
