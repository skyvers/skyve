package org.skyve.impl.generate.pwa;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientDataGridFieldDefinition;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientFieldDefinition;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientLookupDefinition;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
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
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.GeoLocator;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
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
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

public class PWAViewVisitor extends ViewVisitor {
	protected User user;
	protected ComponentRenderer cr;
	protected LayoutRenderer lr;
	protected boolean createView;
	protected String widgetId; // the widgetId to render
	private RenderedComponent fragment; // if we have a widgetId to render, this holds a reference to that component

	private RenderedComponent current; // current component being constructed
	private Stack<Container> currentContainers = new Stack<>(); // used to determine how to add widgets to containers
	private RenderedComponent result; // the result of construction
	private List<RenderedComponent> toolbarLayouts; // the toolbar layouts

	public PWAViewVisitor(Customer customer,
							Module module,
							Document document,
							View view,
							String widgetId) {
		super((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view);
		this.user = CORE.getUser();
		String viewName = view.getName();
		createView = ViewType.create.toString().equals(viewName);
		this.widgetId = widgetId;
		this.view = (ViewImpl) view;
	}
	
	protected void setRenderers(ComponentRenderer cr, LayoutRenderer lr) {
		this.cr = cr;
		this.lr = lr;
	}
	
	public RenderedComponent getResult() {
		return result;
	}

	@Override
	public void visitView() {
	    // Ensure visibility is set for both create and edit views
        current = cr.view(null, createView ? "created" : "notCreated");
        result = current;
        
		// Create the toolbar(s)
    	toolbarLayouts = lr.toolbarLayouts(null);

        // Add the view layout if defined
    	RenderedComponent layout = lr.viewLayout(null);
    	if (layout != null) {
			if (widgetId == null) {
				current.addChild(layout);
			}
	        current = layout;
        }
        
        currentContainers.push(view);
	}

	@Override
	public void visitedView() {
        currentContainers.pop();

        // Add the toolbar(s) if this is a full view render or a view with a widgetId = actions widgetId
        if ((widgetId == null) || widgetId.equals(view.getActionsWidgetId()))  {
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
	}

	private StringBuilder stickyTabScript = new StringBuilder(128);
	
	@Override
	public void visitTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
		RenderedComponent component = cr.tabPane(null, tabPane, module.getName(), document.getName(), stickyTabScript);
		addToContainer(component, 
        				tabPane.getPixelWidth(), 
        				tabPane.getResponsiveWidth(), 
        				tabPane.getPercentageWidth(),
        				tabPane.getInvisibleConditionName());

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			fragment = component;
		}
	}

	@Override
	public void visitedTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			current.removeChild(fragment);
			result.addChild(fragment);
			fragment = null;
		}
	}
	
	@Override
	public void visitTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
System.out.println(tab.getTitle());
		RenderedComponent component = cr.tab(null, tab);
		lr.addTab(current, component);
		current = component;
		RenderedComponent layout = lr.tabLayout(null);
		if (layout != null) {
			current = lr.addTabLayout(null, component, layout);
		}

		currentContainers.push(tab);
	}

	@Override
	public void visitedTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
		currentContainers.pop();
		current = lr.addedTab(null, current);
	}

	@Override
	public void visitVBox(VBox vbox,
							boolean parentVisible,
							boolean parentEnabled) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			border = cr.border(null, vbox.getBorderTitle(), vbox.getInvisibleConditionName(), vbox.getPixelWidth());
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

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth(),
							vbox.getInvisibleConditionName());

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;

		currentContainers.push(vbox);
	}

	@Override
	public void visitedVBox(VBox vbox,
								boolean parentVisible,
								boolean parentEnabled) {
		currentContainers.pop();

		// Cater for border, if one was added
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
			current.removeChild(fragment);
			result.addChild(fragment);
			fragment = null;
		}
	}

	@Override
	public void visitHBox(HBox hbox,
							boolean parentVisible,
							boolean parentEnabled) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			border = cr.border(null, hbox.getBorderTitle(), hbox.getInvisibleConditionName(), hbox.getPixelWidth());
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

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth(),
							hbox.getInvisibleConditionName());
			
			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;

		currentContainers.push(hbox);
	}

	@Override
	public void visitedHBox(HBox hbox,
								boolean parentVisible,
								boolean parentEnabled) {
		currentContainers.pop();

		// Cater for border, if one was added
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
			current.removeChild(fragment);
			result.addChild(fragment);
			fragment = null;
		}
	}

	private Form currentForm; // for columns and disabled state
	private int currentFormColumn;
	
	@Override
	public void visitForm(Form form,
							boolean parentVisible,
							boolean parentEnabled) {
		// Cater for a border if this thing has a border
		RenderedComponent border = null;
		if (Boolean.TRUE.equals(form.getBorder())) {
			border = cr.border(null, form.getBorderTitle(), form.getInvisibleConditionName(), form.getPixelWidth());
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

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth(),
							form.getInvisibleConditionName());

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;
		currentForm = form;
		currentFormColumn = 0;
// TODO form.getDisabledConditionName() form.getLabelDefaultHorizontalAlignment()
	}

	@Override
	public void visitedForm(Form form,
								boolean parentVisible,
								boolean parentEnabled) {
		currentForm = null; // reset form

		// Cater for border, if one was added
		if (Boolean.TRUE.equals(form.getBorder())) {
			current = lr.addedBorderLayout(null, current);
		}
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
			current.removeChild(fragment);
			result.addChild(fragment);
			fragment = null;
		}
	}

	@Override
	public void visitFormColumn(FormColumn column,
									boolean parentVisible,
									boolean parentEnabled) {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	private RenderedComponent formRowLayout = null;
	
	@Override
	public void visitFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		formRowLayout = lr.formRowLayout(null, row);
		if (formRowLayout != null) {
			current = lr.addFormRowLayout(null, current, formRowLayout);
		}
		currentFormColumn = 0;
	}

	private FormItem currentFormItem;
	
	@Override
	public void visitFormItem(FormItem item,
								boolean parentVisible,
								boolean parentEnabled) {
		currentFormItem = item;
	}

	@Override
	public void visitedFormItem(FormItem item,
									boolean parentVisible,
									boolean parentEnabled) {
		currentFormItem = null;
	}

	@Override
	public void visitedFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		if (formRowLayout != null) {
			current = lr.addedFormRowLayout(null, formRowLayout);
		}
		formRowLayout = null;
	}

	private void addComponent(String widgetLabel,
								boolean widgetRequired,
								String widgetInvisible,
								boolean showsLabelByDefault,
								String helpText,
								RenderedComponent component,
								Integer pixelWidth,
								Integer responsiveWidth,
								Integer percentageWidth) {
		if (component == null) {
			return;
		}

		if (currentDataGridBoundColumn != null) { // bound column in a datagrid
			// Add editing component if we have an inline data grid and the current column is editable
			boolean columnEditable = ! Boolean.FALSE.equals(currentDataGridBoundColumn.getEditable());
			if (columnEditable) { // NB short circuit test
				boolean inline = (currentGrid instanceof DataGrid) ? 
									Boolean.TRUE.equals(((DataGrid) currentGrid).getInline()) :
									true;
				if (inline) {
					current.addChild(component);
				}
			}
		}
		else { // not a bound column in a datagrid
			if (currentFormItem == null) { // not a form item
				if (currentGrid == null) { // not a container column in a datagrid
					// This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, responsiveWidth, percentageWidth, widgetInvisible);
					addedToContainer();
				}
				else {
					// This must be a data grid container column
					// add a spacer, if required
					if (! current.isLeaf()) {
						current.addChild(cr.label(null, " "));
					}
					current.addChild(component);
				}
			}
			else { // a form item
				lr.layoutFormItem(current,
									component,
									currentForm, 
									currentFormItem, 
									currentFormColumn,
									widgetLabel,
									widgetRequired,
									widgetInvisible,
									showsLabelByDefault,
									helpText);
			}
		}
	}
	
	@Override
	public void visitButton(Button button,
								boolean parentVisible,
								boolean parentEnabled) {
		org.skyve.metadata.view.Action action = view.getAction(button.getActionName());
		ImplicitActionName name = action.getImplicitName();
		RenderedComponent c = null;
		if (ImplicitActionName.Report.equals(name)) {
			c = cr.reportButton(null, button, action);
		}
		else if (ImplicitActionName.Download.equals(name)) {
			c = cr.downloadButton(null, button, action, module.getName(), document.getName());
		}
		else {
			c = cr.actionButton(null, listBinding, listVar, button, action);
		}
	    addComponent(null, 
	    				false, 
	    				action.getInvisibleConditionName(), 
	    				button.showsLabelByDefault(), 
	    				null,
	    				c, 
	    				button.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitGeoLocator(GeoLocator locator,
									boolean parentVisible,
									boolean parentEnabled) {
		RenderedComponent l = cr.label(null, "geoLocator"); // TODO geolocator
	    addComponent(null, 
	    				false, 
	    				locator.getInvisibleConditionName(), 
	    				locator.showsLabelByDefault(), 
	    				null,
	    				l, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void visitGeometry(Geometry geometry,
									boolean parentVisible,
									boolean parentEnabled) {
		RenderedComponent l = cr.label(null, "geometry"); // TODO geometry
	    addComponent(null, 
	    				false, 
	    				geometry.getInvisibleConditionName(), 
	    				geometry.showsLabelByDefault(), 
	    				null,
	    				l, 
	    				geometry.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitMap(MapDisplay map,
							boolean parentVisible,
							boolean parentEnabled) {
		RenderedComponent l = cr.label(null, "map"); // TODO map
	    addComponent(null, 
	    				false, 
	    				map.getInvisibleConditionName(), 
	    				false,
	    				null,
	    				l, 
	    				map.getPixelWidth(), 
	    				map.getResponsiveWidth(),
	    				map.getPercentageWidth());
	}

	@Override
	public void visitDialogButton(DialogButton button,
									boolean parentVisible,
									boolean parentEnabled) {
		RenderedComponent bn = cr.label(null, "dialogButton"); // TODO dialog button
	    addComponent(null, 
	    				false, 
	    				button.getInvisibleConditionName(), 
	    				button.showsLabelByDefault(),
	    				null,
	    				bn, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void visitDynamicImage(DynamicImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		RenderedComponent i = cr.dynamicImage(null, image, module.getName(), document.getName());
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						false,
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		RenderedComponent component = cr.spacer(null, spacer);
		if (component != null) {
			addComponent(null, 
							false, 
							spacer.getInvisibleConditionName(), 
							spacer.showsLabelByDefault(),
							null,
							component, 
							spacer.getPixelWidth(), 
							null, 
							null);
		}
	}

	@Override
	public void visitStaticImage(StaticImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		RenderedComponent i = cr.staticImage(null, image);
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						image.showsLabelByDefault(),
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	@Override
	public void visitBlurb(Blurb blurb,
							boolean parentVisible,
							boolean parentEnabled) {
		String value = null;
		String binding = null;
		String markup = blurb.getMarkup();
		if (markup.indexOf('{') > -1) {
			binding = markup;
		}
		else {
			value = markup;
		}
		RenderedComponent c = cr.blurb(null, listVar, value, binding, blurb);
		addComponent(null, 
						false, 
						blurb.getInvisibleConditionName(), 
						blurb.showsLabelByDefault(),
						null,
						c, 
						blurb.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitLink(final Link link,
	                        boolean parentVisible,
	                        boolean parentEnabled) {
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
				//c.set(cr.outputLink(listVar, link.getValue(), href.toString(), link.getInvisibleConditionName(), target));
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
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference) {
				final TargetMetaData listTarget = BindUtil.getMetaDataForBinding(customer, module, document, listBinding);

				final Document listDocument;
				// Figure out the document type of the relation.
				if (listTarget.getAttribute() instanceof Relation) {
					final String documentName = ((Relation) listTarget.getAttribute()).getDocumentName();
					listDocument = module.getDocument(customer, documentName);
				} else {
					listDocument = listTarget.getDocument();
				}

				final ViewType[] viewTypesToSearch = new ViewType[] { ViewType.edit, ViewType.create };
				Action action = null;
				for (ViewType viewType : viewTypesToSearch) {
					final View listDocumentView = listDocument.getView(cr.userAgentType.name(), customer, viewType.name());
					if (listDocumentView == null) {
						continue;
					}
					action = listDocumentView.getAction(reference.getActionName());
					if (action != null) {
						// Found the action, we can stop looking.
						break;
					}
				}

				if (action != null) {
					c.set(cr.actionLink(null, listBinding, listVar, link, action.getName()));
				} else {
					c.set(cr.actionLink(null, listBinding, listVar, link, reference.getActionName()));
				}
			}
		}.process(outerReference);

		addComponent(null, 
						false, 
						link.getInvisibleConditionName(), 
						link.showsLabelByDefault(),
						null,
						c.get(), 
						link.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitLabel(Label label,
	                        boolean parentVisible,
	                        boolean parentEnabled) {
		String value = label.getValue();
		String binding = label.getBinding();
		if ((value == null) && (binding == null)) {
			// Find the display name if applicable
			value = "Label";
			String displayBinding = label.getFor();
			if (displayBinding != null) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, displayBinding);
				if (target != null) {
					Attribute attribute = target.getAttribute();
					if (attribute != null) {
						value = String.format("%s %s:", attribute.getDisplayName(), attribute.isRequired() ? "*" : ""); 
					}
				}
			}
		}
		else if ((value != null) && value.indexOf('{') > -1) {
			binding = value;
			value = null;
		}
		RenderedComponent c = cr.label(null, listVar, value, binding, label);
	    addComponent(null, 
	    				false, 
	    				label.getInvisibleConditionName(), 
	    				label.showsLabelByDefault(),
	    				null,
	    				c, 
	    				label.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		RenderedComponent p = cr.label(null, "progressBar"); // TODO progress bar
	    addComponent(null, 
	    				false, 
	    				progressBar.getInvisibleConditionName(), 
	    				progressBar.showsLabelByDefault(),
	    				null,
	    				p, 
	    				progressBar.getPixelWidth(), 
	    				null, 
	    				null);
	}

	private MetaData currentGrid;

	private String listWidgetModelDocumentName;
	private String listWidgetModelName;
	private ListModel<? extends Bean> listWidgetModel;
	private Document listWidgetDrivingDocument;
	
	@Override
	public void visitListGrid(ListGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		visitListWidget(grid);
		RenderedComponent l = cr.listGrid(null,
										listWidgetModelDocumentName,
										listWidgetModelName,
										listWidgetModel,
										grid,
										(user == null) ? true : user.canCreateDocument(listWidgetDrivingDocument));
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName());
	}
	
	@Override
	public void visitListRepeater(ListRepeater repeater,
									boolean parentVisible,
									boolean parentEnabled) {
		visitListWidget(repeater);
		RenderedComponent r = cr.listRepeater(null,
											listWidgetModelDocumentName, 
											listWidgetModelName, 
											listWidgetModel, 
											repeater.getParameters(), 
											repeater.getTitle(),
											Boolean.TRUE.equals(repeater.getShowColumnHeaders()),
											Boolean.TRUE.equals(repeater.getShowGrid()));
		addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleConditionName());
	}

	private void visitListWidget(AbstractListWidget widget) {
		String queryName = widget.getQueryName();
		String modelName = widget.getModelName();
		
		if ((queryName == null) && (modelName != null)) {
			listWidgetModelName = modelName;
			listWidgetModelDocumentName = document.getName();
			listWidgetModel = CORE.getRepository().getListModel(customer, document, listWidgetModelName, user != null);
			listWidgetDrivingDocument = listWidgetModel.getDrivingDocument();
		}
		else {
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, queryName);
			}
			listWidgetModelName = queryName;
			listWidgetModelDocumentName = query.getDocumentName();
			listWidgetDrivingDocument = query.getDocumentModule(customer).getDocument(customer, listWidgetModelDocumentName);
	        DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>();
	        queryModel.setQuery(query);
	        listWidgetModel = queryModel;
		}
		currentGrid = widget;		
	}
	
	@Override
	public void visitedListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		visitedListWidget();
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater,
										boolean parentVisible,
										boolean parentEnabled) {
		visitedListWidget();
	}
	
	private void visitedListWidget() {
		currentGrid = null;
		listWidgetModelDocumentName = null;
		listWidgetModelName = null;
		listWidgetModel = null;
		listWidgetDrivingDocument = null;
		addedToContainer();
	}

	@Override
	public void visitTreeGrid(TreeGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		RenderedComponent l = cr.label(null, "treeGrid");
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName()); // TODO tree grid
		currentGrid = grid;
	}

	@Override
	public void visitedTreeGrid(TreeGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		currentGrid = null;
		addedToContainer();
	}

	private String listBinding;
	private String listVar;
	
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Determine if the grid collection is ordered
		listBinding = grid.getBinding();
		boolean ordered = false;
		final TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, listBinding);
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation instanceof Collection) {
				ordered = Boolean.TRUE.equals(((Collection) targetRelation).getOrdered());
			}
		}
		
		// Create the datagrid faces component
		listVar = BindUtil.sanitiseBinding(listBinding) + "Row";
		RenderedComponent g = cr.dataGrid(null, listVar, ordered, grid);
        addToContainer(g, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName());
		currentGrid = grid;
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			fragment = g;
		}
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// Create the data repeater faces component
		listBinding = repeater.getBinding();
		listVar = BindUtil.sanitiseBinding(listBinding) + "Row";
		RenderedComponent r = cr.dataRepeater(null, listVar, repeater);
        addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleConditionName());
		currentGrid = repeater;
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(repeater.getWidgetId()))) {
			fragment = r;
		}
	}
	
	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitedDataWidget(grid);
	}
	
	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		visitedDataWidget(repeater);
	}

	private void visitedDataWidget(AbstractDataWidget widget) {
		// Determine the document alias
		String alias = null;
		TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, widget.getBinding());
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation != null) {
				alias = module.getDocument(customer, targetRelation.getDocumentName()).getSingularAlias();
			}
		}

		if (widget instanceof DataGrid) {
			DataGrid grid = (DataGrid) widget;
			current = cr.addDataGridActionColumn(null,
													current, 
													grid,
													listVar,
													gridColumnExpression.toString(), 
													alias, 
													Boolean.TRUE.equals(grid.getInline()));
		}
	    currentGrid = null;
	    listBinding = null;
	    listVar = null;
	    gridColumnExpression = null;
	    addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(widget.getWidgetId()))) {
			current.removeChild(fragment);
			result.addChild(fragment);
			fragment = null;
		}
	}

	private StringBuilder gridColumnExpression;
	private DataGridBoundColumn currentDataGridBoundColumn = null;
	
	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled) {
		currentDataGridBoundColumn = column;
		String title = column.getTitle();
		String binding = column.getBinding();
		if (binding == null) {
			binding = Bean.BIZ_KEY;
		}
		else {
			StringBuilder sb = new StringBuilder(64);
			sb.append(listBinding).append('.').append(binding);
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, sb.toString());
			if (target != null) {
				Attribute targetAttribute = target.getAttribute();
				if (targetAttribute != null) {
					if (title == null) {
						title = targetAttribute.getDisplayName();
					}
					if (targetAttribute instanceof Association) {
						sb.setLength(0);
						binding = sb.append(binding).append('.').append(Bean.BIZ_KEY).toString();
					}
				}
			}
		}
		current = cr.addDataGridBoundColumn(null,
												current, 
												(AbstractDataWidget) currentGrid,
												column, 
												listVar,
												title, 
												binding, 
												gridColumnExpression);
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled) {
		current = cr.addedDataGridBoundColumn(null, current);
		currentDataGridBoundColumn = null;
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled) {
        current = cr.addDataGridContainerColumn(null, current, (AbstractDataWidget) currentGrid, column);
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled) {
		current = cr.addedDataGridContainerColumn(null, current);
	}

	// A reference to the current widget that is the source of events
	private RenderedComponent eventSource = null;
	
	@Override
	public void visitCheckBox(CheckBox checkBox,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(checkBox);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.checkBox(null, listVar, checkBox, title, required);
		eventSource = c;
		addComponent(title,
						required,
						checkBox.getInvisibleConditionName(), 
						checkBox.showsLabelByDefault(),
						helpText,
						c, 
						checkBox.getPixelWidth(), 
						null,
						null);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitCheckMembership(CheckMembership membership,
	                                    boolean parentVisible,
	                                    boolean parentEnabled) {
//		SmartClientDataGridFieldDefinition def = getFieldDef(membership);
		RenderedComponent c = cr.label(null, "checkMembership"); // TODO check membership
        eventSource = c;
        addToContainer(c, null, null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
	                                    boolean parentVisible,
	                                    boolean parentEnabled) {
	    addedToContainer();
	    eventSource = null;
	}

	@Override
	public void visitColourPicker(ColourPicker colour,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(colour);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.colourPicker(null, listVar, colour, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						colour.getInvisibleConditionName(), 
						colour.showsLabelByDefault(),
						helpText,
						c, 
						colour.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitedColourPicker(ColourPicker colour,
	                                    boolean parentVisible,
	                                    boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitCombo(Combo combo,
	                        boolean parentVisible,
	                        boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(combo);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent s = cr.combo(null, listVar, combo, title, required);
		eventSource = s;
		addComponent(title, 
						required, 
						combo.getInvisibleConditionName(), 
						combo.showsLabelByDefault(),
						helpText,
						s, 
						combo.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitedCombo(Combo combo,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitContentImage(ContentImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(image);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.contentImage(null, listVar, image, title, required);
        addComponent(title, 
        				false, 
        				image.getInvisibleConditionName(), 
        				image.showsLabelByDefault(),
        				helpText,
        				c, 
        				image.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitContentLink(ContentLink link,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(link);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.contentLink(null, listVar, link, title, required);
		addComponent(title, 
						required, 
						link.getInvisibleConditionName(), 
						link.showsLabelByDefault(),
						helpText,
						c, 
						link.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitHTML(HTML html,
                            boolean parentVisible,
                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(html);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.html(null, listVar, html, title, required);
        addComponent(title, 
        				required, 
        				html.getInvisibleConditionName(), 
        				html.showsLabelByDefault(),
        				helpText,
        				c, 
        				html.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		RenderedComponent c = cr.listMembership(null, membership);
		eventSource = c;
		addToContainer(c, membership.getListWidthInPixels(), null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void visitedListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		addedToContainer();
		eventSource = null;
	}

	@Override
	public void visitComparison(Comparison comparison,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
//		SmartClientDataGridFieldDefinition def = getFieldDef(comparison);
		RenderedComponent c = cr.label(null, "comparison"); // TODO comparison
        addToContainer(c, comparison.getPixelWidth(), comparison.getResponsiveWidth(), comparison.getPercentageWidth(), comparison.getInvisibleConditionName());
        addedToContainer();
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup,
	                                    boolean parentVisible,
	                                    boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(lookup);
		SmartClientLookupDefinition ldef = def.getLookup();
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.lookupDescription(null,
													listVar, 
													lookup, 
													title, 
													required,
													BindUtil.unsanitiseBinding(ldef.getDisplayField()),
													ldef.getQuery());
        eventSource = c;
        
        addComponent(title, 
        				required, 
        				lookup.getInvisibleConditionName(), 
        				lookup.showsLabelByDefault(),
        				helpText,
        				c, 
        				lookup.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup,
	                                        boolean parentVisible,
	                                        boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		RenderedComponent c = cr.label(null, "lookup"); // TODO lookup
		addComponent(null, 
						false, 
						lookup.getInvisibleConditionName(), 
						lookup.showsLabelByDefault(),
						null,
						c, 
						null, 
						null, 
						null);
	}

	@Override
	public void visitedLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitPassword(Password password,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(password);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.password(null, listVar, password, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				password.getInvisibleConditionName(), 
        				password.showsLabelByDefault(),
        				helpText,
        				c, 
        				password.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedPassword(Password password,
									boolean parentVisible,
									boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitRadio(Radio radio,
                            boolean parentVisible,
                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(radio);
		String title = def.getTitle();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		boolean required = def.isRequired();
		RenderedComponent c = cr.radio(null, listVar, radio, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						radio.getInvisibleConditionName(), 
						radio.showsLabelByDefault(),
						helpText,
						c, 
						radio.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitedRadio(Radio radio,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitRichText(RichText richText,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(richText);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.richText(null, listVar, richText, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				richText.getInvisibleConditionName(), 
        				richText.showsLabelByDefault(),
        				helpText,
        				c, 
        				richText.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedRichText(RichText richText,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitSlider(Slider slider,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(slider);
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.label(null, "slider"); // TODO slider
	    eventSource = c;
        addComponent(def.getTitle(), 
        				def.isRequired(), 
        				slider.getInvisibleConditionName(), 
        				slider.showsLabelByDefault(),
        				helpText,
        				c, 
        				slider.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedSlider(Slider slider,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitSpinner(Spinner spinner,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(spinner);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.spinner(null, listVar, spinner, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				spinner.getInvisibleConditionName(), 
        				spinner.showsLabelByDefault(),
        				helpText,
        				c, 
        				spinner.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedSpinner(Spinner spinner,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitTextArea(TextArea text,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(text);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		RenderedComponent c = cr.textArea(null, listVar, text, title, required, def.getLength());
        eventSource = c;
        addComponent(title, 
        				required, 
        				text.getInvisibleConditionName(), 
        				text.showsLabelByDefault(),
        				helpText,
        				c, 
        				text.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitedTextArea(TextArea text,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		eventSource = null;
	}

	@Override
	public void visitTextField(TextField text,
								boolean parentVisible,
								boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(text);
		String title = def.getTitle();
		boolean required = def.isRequired();
		String helpText = (def instanceof SmartClientFieldDefinition) ?
							((SmartClientFieldDefinition) def).getHelpText() :
							null;
		Attribute attribute = def.getTarget().getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		TextFormat textFormat = (attribute instanceof Text) ? ((Text) attribute).getFormat() : null;
		Format<?> format = (textFormat == null) ? null : textFormat.getFormat();
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

        RenderedComponent c = cr.text(null,
										listVar, 
										text, 
										title, 
										required,
										def.getLength(),
										converter,
										format);
        eventSource = c;
		addComponent(title, 
						required, 
						text.getInvisibleConditionName(), 
						text.showsLabelByDefault(),
						helpText,
						c, 
						text.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		// do nothing - this is for web 2 ux uis only
	}
	
	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled) {
		eventSource = null;
	}

	private void addToContainer(RenderedComponent component, 
									Integer pixelWidth, 
									Integer responsiveWidth,
									Integer percentageWidth,
									String invisibleConditionName) {
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
		cr.addAjaxBehavior(eventSource, "change", listBinding, listVar, binding, changedActions);
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
		cr.addAjaxBehavior(eventSource, "focus", listBinding, listVar, binding, blurable.getFocusActions());
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
		cr.addAjaxBehavior(eventSource, "blur", listBinding, listVar, binding, blurable.getBlurActions());
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
	public void visitOnPickedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemSelect", listBinding, listVar, lookup.getBinding(), lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		cr.addAjaxBehavior(eventSource, "itemUnselect", listBinding, listVar, lookup.getBinding(), lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup,
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
	public void visitServerSideActionEventAction(ServerSideActionEventAction server,
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
	public void visitCustomAction(ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (RenderedComponent toolbarLayout : toolbarLayouts) {
					toolbarLayout.addChild(cr.action(null,
														listBinding,
														listVar,
														action,
														null,
														action.getDisplayName()));
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
					else {
						String displayName = action.getDisplayName();
						if (displayName == null) {
							displayName = name.getDisplayName();
						}
						toolbarLayout.addChild(cr.action(null,
															listBinding,
															listVar,
															action,
															name,
															displayName));
					}
				}
			}
		}
	}
	
	@Override
	public void visitAddAction(ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Add);
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Remove);
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.ZoomOut);
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Navigate);
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.OK);
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Save);
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Cancel);
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Delete);
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/report/Bum.html?_f=html&_c=<webId>&_id=<id>&wee=poo&_n=Bum&_mod=<module>&_doc=<document>
	 * 
	 * @param action
	 */
	@Override
	public void visitReportAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Report);
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizExport);
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.BizImport);
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Upload);
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		processImplicitAction(action, ImplicitActionName.Download);
	}

	@Override
	public void visitNewAction(ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.New);
	}

	@Override
	public void visitEditAction(ActionImpl action) {
//		processImplicitAction(action, ImplicitActionName.Edit);
	}

	@Override
	public void visitParameter(Parameter parameter,
								boolean parentVisible,
								boolean parentEnabled) {
		// nothing to see here
	}
	
	@Override
	public void visitFilterParameter(FilterParameter parameter,
			boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	private SmartClientDataGridFieldDefinition getFieldDef(InputWidget inputWidget) {
		boolean runtime = (user != null);
		
		SmartClientDataGridFieldDefinition def = null;

		// Document is already set to the child document when instantiating a FacesViewVisitor
		// so there is no need to resolve the view binding within the conversation bean.
		DocumentImpl targetDocument = document;
		ModuleImpl targetModule = module;

		if (listBinding == null) {
			def = SmartClientGenerateUtils.getField(user, customer, targetModule, targetDocument, inputWidget, runtime);
		}
		else {
			if (inputWidget.getBinding() == null) {
				def = SmartClientGenerateUtils.getDataGridField(user, customer, targetModule, targetDocument, inputWidget, listBinding, runtime);
			}
			else {
				TargetMetaData target = Binder.getMetaDataForBinding(customer, targetModule, targetDocument, listBinding);
				targetDocument = (DocumentImpl) module.getDocument(customer, ((Relation) target.getAttribute()).getDocumentName());
				targetModule = (ModuleImpl) customer.getModule(targetDocument.getOwningModuleName());
				def = SmartClientGenerateUtils.getDataGridField(user, customer, targetModule, targetDocument, inputWidget, null, runtime);
			}
		}
		
		String title = (currentFormItem == null) ? null : currentFormItem.getLabel();
		if (title != null) {
			def.setTitle(title);
		}
		Boolean required = (currentFormItem == null) ? null : currentFormItem.getRequired();
		if (required != null) {
			def.setRequired(required.booleanValue());
		}

		return def;
	}
}
