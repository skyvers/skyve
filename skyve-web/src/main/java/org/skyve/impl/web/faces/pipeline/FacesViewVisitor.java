package org.skyve.impl.web.faces.pipeline;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;

import org.primefaces.component.calendar.Calendar;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.generate.SmartClientGenerateUtils.SmartClientDataGridFieldDefinition;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.PickList;
import org.skyve.impl.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.web.faces.converters.date.DD_MMM_YYYY;
import org.skyve.impl.web.faces.converters.date.DD_MM_YYYY;
import org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.impl.web.faces.converters.decimal.Decimal2Integer;
import org.skyve.impl.web.faces.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.impl.web.faces.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.impl.web.faces.converters.decimal.Decimal5Integer;
import org.skyve.impl.web.faces.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.impl.web.faces.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TimeDuration;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.impl.web.faces.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.impl.web.faces.converters.integer.IntegerSeparator;
import org.skyve.impl.web.faces.converters.integer.SimplePercentage;
import org.skyve.impl.web.faces.converters.lang.Decimal10;
import org.skyve.impl.web.faces.converters.lang.Decimal2;
import org.skyve.impl.web.faces.converters.lang.Decimal5;
import org.skyve.impl.web.faces.converters.time.HH24_MI;
import org.skyve.impl.web.faces.converters.time.HH_MI;
import org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY_HH_MI_SS;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

public class FacesViewVisitor extends ViewVisitor {
	private User user;
	private ComponentBuilder cb;
	private LayoutBuilder lb;
	private boolean createView;
	private String widgetId;
	private UIComponent fragment; // if we have a widgetId to render, this holds a reference to that component

	private UIComponent current; // current component being constructed
	private Stack<Container> currentContainers = new Stack<>(); // used to determine how to add widgets to containers
	private UIComponent facesView; // the result of construction
	private UIComponent toolbarLayout; // the toolbar layout

	public FacesViewVisitor(User user, 
							CustomerImpl customer,
							ModuleImpl module,
							DocumentImpl document,
							ViewImpl view,
							String widgetId,
							ComponentBuilder cb,
							LayoutBuilder lb) {
		super(customer, module, document, view);
		this.user = user;
		ViewType viewType = view.getType();
		createView = ViewType.create.equals(viewType);
		this.widgetId = widgetId;
		this.cb = cb;
		this.lb = lb;
	}
	
	public UIComponent getFacesView() {
		return facesView;
	}

	@Override
	public void visitView() {
	    // Ensure visibility is set for both create and edit views
        current = cb.view(createView ? "created" : "notCreated");
        facesView = current;
        
		// Create the toolbar
    	toolbarLayout = lb.toolbarLayout();

        // Add the view layout if defined
    	UIComponent layout = lb.viewLayout();
    	if (layout != null) {
			if (widgetId == null) {
				current.getChildren().add(layout);
			}
	        current = layout;
        }
        
        currentContainers.push(view);
	}

	@Override
	public void visitedView() {
        currentContainers.pop();

        // Add the toolbar if this is a full view render and the toolbar has contents
        if ((widgetId == null) && (! toolbarLayout.getChildren().isEmpty())) {
        	// If we get a toolbar back, add the toolbar layout to it
        	UIComponent toolbar = cb.toolbar();
        	if (toolbar != null) {
            	facesView.getChildren().add(0, toolbar);
            	lb.addToolbarLayout(toolbar, toolbarLayout);
        	}
        	else {
            	facesView.getChildren().add(0, toolbarLayout);
        	}
    	}
	}

//	private TabPane currentTabPane; // for setting disabled tabs
	@Override
	public void visitTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
//		currentTabPane = tabPane;
		UIComponent component = cb.tabPane(tabPane);
        addToContainer(component, 
        				tabPane.getPixelWidth(), 
        				tabPane.getResponsiveWidth(), 
        				tabPane.getPercentageWidth());

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			fragment = component;
		}
	}

	@Override
	public void visitedTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
//		currentTabPane = null;
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}
	
// TODO activeIndex used to select selected tab
	@Override
	public void visitTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
//		tab.getSelectedConditionName();
		UIComponent component = cb.tab(tab);
		lb.addTab(current, component);
		current = component;
		UIComponent layout = lb.tabLayout();
		if (layout != null) {
			current = lb.addTabLayout(component, layout);
		}

		currentContainers.push(tab);
	}

	@Override
	public void visitedTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
		currentContainers.pop();
		current = lb.addedTab(current);
	}

	@Override
	public void visitVBox(VBox vbox,
							boolean parentVisible,
							boolean parentEnabled) {
		// Cater for a border if this thing has a border
		UIComponent border = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			border = cb.border(vbox.getBorderTitle(), vbox.getInvisibleConditionName(), vbox.getPixelWidth());
			addToContainer(border, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth());
		}

		UIComponent layout = lb.vboxLayout(vbox);

		// Cater for border if defined
		if (border != null) {
			lb.addBorderLayout(border, layout);

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth());

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
			current = lb.addedBorderLayout(current);
		}
		addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}

	@Override
	public void visitHBox(HBox hbox,
							boolean parentVisible,
							boolean parentEnabled) {
		// Cater for a border if this thing has a border
		UIComponent border = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			border = cb.border(hbox.getBorderTitle(), hbox.getInvisibleConditionName(), hbox.getPixelWidth());
			addToContainer(border, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth());
		}

		UIComponent layout = lb.hboxLayout(hbox);

		// Cater for border if defined
		if (border != null) {
			lb.addBorderLayout(border, layout);

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth());
			
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
			current = lb.addedBorderLayout(current);
		}
		addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
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
		UIComponent border = null;
		if (Boolean.TRUE.equals(form.getBorder())) {
			border = cb.border(form.getBorderTitle(), form.getInvisibleConditionName(), form.getPixelWidth());
			addToContainer(border, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth());
		}

		UIComponent layout = lb.formLayout(form);

		// Cater for border if defined
		if (border != null) {
			lb.addBorderLayout(border, layout);

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
				fragment = border;
			}
		}
		else {
			addToContainer(layout, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth());

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
			current = lb.addedBorderLayout(current);
		}
		addedToContainer();

		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}

	@Override
	public void visitFormColumn(FormColumn column,
									boolean parentVisible,
									boolean parentEnabled) {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	private UIComponent formRowLayout = null;
	
	@Override
	public void visitFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		formRowLayout = lb.formRowLayout(row);
		if (formRowLayout != null) {
			current = lb.addFormRowLayout(current, formRowLayout);
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
			current = lb.addedFormRowLayout(formRowLayout);
		}
		formRowLayout = null;
	}

	private void addComponent(String widgetLabel,
								boolean widgetRequired,
								String widgetInvisible,
								boolean showsLabelByDefault,
								UIComponent component,
								Integer pixelWidth,
								Integer responsiveWidth,
								Integer percentageWidth) {
		if (component == null) {
			return;
		}

		if (visitingDataGridBoundColumn) { // bound column in a datagrid
			if ((currentGrid instanceof DataGrid) && 
					Boolean.TRUE.equals(((DataGrid) currentGrid).getInline())) {
				current.getChildren().add(component);
			}
		}
		else { // not a bound column in a datagrid
			if (currentFormItem == null) { // not a form item
				if (currentGrid == null) { // not a container column in a datagrid
					// This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, responsiveWidth, percentageWidth);
					addedToContainer();
				}
				else {
					// This must be a data grid container column
					// add a spacer, if required
					List<UIComponent> children = current.getChildren();
					if (! children.isEmpty()) {
						children.add(cb.label(" "));
					}
					children.add(component);
				}
			}
			else { // a form item
				lb.layoutFormItem(current,
									component,
									currentForm, 
									currentFormItem, 
									currentFormColumn,
									widgetLabel,
									widgetRequired,
									widgetInvisible,
									showsLabelByDefault);
			}
		}
	}
	
	@Override
	public void visitButton(Button button,
								boolean parentVisible,
								boolean parentEnabled) {
		org.skyve.metadata.view.Action action = view.getAction(button.getActionName());
		ImplicitActionName name = action.getImplicitName();
		UIComponent c = null;
		if (ImplicitActionName.Report.equals(name)) {
			c = cb.reportButton(button, action);
		}
		else {
			c = cb.actionButton(listBinding, button, action);
		}
	    addComponent(null, 
	    				false, 
	    				action.getInvisibleConditionName(), 
	    				button.showsLabelByDefault(), 
	    				c, 
	    				button.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitGeoLocator(GeoLocator locator,
									boolean parentVisible,
									boolean parentEnabled) {
	    UIComponent l = cb.label("geoLocator"); // TODO geolocator
	    addComponent(null, 
	    				false, 
	    				locator.getInvisibleConditionName(), 
	    				locator.showsLabelByDefault(), 
	    				l, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void visitGeometry(Geometry geometry,
									boolean parentVisible,
									boolean parentEnabled) {
	    UIComponent l = cb.label("geometry"); // TODO geometry
	    addComponent(null, 
	    				false, 
	    				geometry.getInvisibleConditionName(), 
	    				geometry.showsLabelByDefault(), 
	    				l, 
	    				geometry.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitMap(MapDisplay map,
							boolean parentVisible,
							boolean parentEnabled) {
	    UIComponent l = cb.label("map"); // TODO map
	    addComponent(null, 
	    				false, 
	    				map.getInvisibleConditionName(), 
	    				false,
	    				l, 
	    				map.getPixelWidth(), 
	    				map.getResponsiveWidth(),
	    				map.getPercentageWidth());
	}

	@Override
	public void visitDialogButton(DialogButton button,
									boolean parentVisible,
									boolean parentEnabled) {
	    UIComponent bn = cb.label("dialogButton"); // TODO dialog button
	    addComponent(null, 
	    				false, 
	    				button.getInvisibleConditionName(), 
	    				button.showsLabelByDefault(),
	    				bn, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void visitDynamicImage(DynamicImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
		UIComponent i = cb.dynamicImage(image, module.getName(), document.getName());
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						false,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		UIComponent component = cb.spacer(spacer.getPixelWidth(), spacer.getPixelHeight());
		if (component != null) {
			addComponent(null, 
							false, 
							null, 
							spacer.showsLabelByDefault(),
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
		UIComponent i = cb.staticImage(image);
		addComponent(null, 
						false, 
						image.getInvisibleConditionName(), 
						image.showsLabelByDefault(),
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
		UIComponent c = cb.blurb(listBinding, value, binding, blurb);
		addComponent(null, 
						false, 
						null, 
						blurb.showsLabelByDefault(),
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
		final AtomicReference<UIComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			@Override
			@SuppressWarnings("synthetic-access")
			public void processResourceReference(ResourceReference reference) {
				c.set(cb.label("resource link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processReportReference(ReportReference reference) {
				c.set(cb.label("report link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processQueryListViewReference(QueryListViewReference reference) {
				c.set(cb.label("list view link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processImplicitActionReference(ImplicitActionReference reference) {
				c.set(cb.label("implicit action link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processExternalReference(ExternalReference reference) {
				c.set(cb.label("external link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processEditViewReference(EditViewReference reference) {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');

				c.set(cb.outputLink(listBinding, link.getValue(), href.toString(), link.getInvisibleConditionName(), target));
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processDefaultListViewReference(DefaultListViewReference reference) {
				c.set(cb.label("default list view link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processContentReference(ContentReference reference) {
				c.set(cb.label("content link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference) {
				c.set(cb.actionLink(listBinding, link, reference.getActionName()));
			}
		}.process(outerReference);

		addComponent(null, 
						false, 
						link.getInvisibleConditionName(), 
						link.showsLabelByDefault(),
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
		if ((value != null) && value.indexOf('{') > -1) {
			binding = value;
			value = null;
		}
	    UIComponent c = cb.label(listBinding, value, binding, label);
	    addComponent(null, 
	    				false, 
	    				label.getInvisibleConditionName(), 
	    				label.showsLabelByDefault(),
	    				c, 
	    				label.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar,
	                                boolean parentVisible,
	                                boolean parentEnabled) {
	    UIComponent p = cb.label("progressBar"); // TODO progress bar
	    addComponent(null, 
	    				false, 
	    				null, 
	    				progressBar.showsLabelByDefault(),
	    				p, 
	    				progressBar.getPixelWidth(), 
	    				null, 
	    				null);
	}

	private MetaData currentGrid;

	@Override
	public void visitListGrid(ListGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		String queryName = grid.getQueryName();
		String modelName = grid.getModelName();
		String modelDocumentName = null;
		
		ListModel<? extends Bean> model = null;
		if ((queryName == null) && (modelName != null)) {
			model = CORE.getRepository().getListModel(customer, document, modelName);
			modelDocumentName = document.getName();
		}
		else {
			DocumentQueryDefinition query = module.getDocumentQuery(queryName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, queryName);
			}
	        DocumentQueryListModel queryModel = new DocumentQueryListModel();
	        queryModel.setQuery(query);
			modelName = queryName;
	        model = queryModel;
		}
		
		UIComponent l = cb.listGrid(modelDocumentName, modelName, model, true, true, false);
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth());
		currentGrid = grid;
	}

	@Override
	public void visitedListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		currentGrid = null;
		addedToContainer();
	}

	@Override
	public void visitTreeGrid(TreeGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		UIComponent l = cb.label("treeGrid");
		addToContainer(l, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth()); // TODO tree grid
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
	
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Create the datagrid faces component
		UIComponent g = cb.dataGrid(grid);
        addToContainer(g, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth());
		currentGrid = grid;
		listBinding = grid.getBinding();
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			fragment = g;
		}
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Determine the document alias
		String alias = null;
		TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, grid.getBinding());
		if (target != null) {
			Relation targetRelation = (Relation) target.getAttribute();
			if (targetRelation != null) {
				alias = module.getDocument(customer, targetRelation.getDocumentName()).getSingularAlias();
			}
		}

		current = cb.addDataGridActionColumn(current, 
												grid, 
												gridColumnExpression.toString(), 
												alias, 
												Boolean.TRUE.equals(grid.getInline()));
		
	    currentGrid = null;
	    listBinding = null;
	    gridColumnExpression = null;
	    addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}

	private StringBuilder gridColumnExpression;
	private boolean visitingDataGridBoundColumn = false;
	
	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled) {
		visitingDataGridBoundColumn = true;
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
					title = targetAttribute.getDisplayName();
					if (targetAttribute instanceof Association) {
						sb.setLength(0);
						binding = sb.append(binding).append('.').append(Bean.BIZ_KEY).toString();
					}
				}
			}
		}
		current = cb.addDataGridBoundColumn(current, 
												(DataGrid) currentGrid, 
												column, 
												title, 
												binding, 
												gridColumnExpression);
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled) {
		current = cb.addedDataGridBoundColumn(current);
		visitingDataGridBoundColumn = false;
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled) {
        current = cb.addDataGridContainerColumn(current, (DataGrid) currentGrid, column);
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled) {
		current = cb.addedDataGridContainerColumn(current);
	}

	@Override
	public void visitPickList(PickList list,
								boolean parentVisible,
								boolean parentEnabled) {
		UIComponent l = cb.label("pickList");
		addToContainer(l, list.getPixelWidth(), list.getResponsiveWidth(), list.getPercentageWidth()); // TODO picklist
	}

	@Override
	public void visitedPickList(PickList list,
									boolean parentVisible,
									boolean parentEnabled) {
		addedToContainer();
	}

	@Override
	public void visitPickListColumn(PickListColumn column,
										boolean parentVisible,
										boolean parentEnabled) {
		// TODO pick list column
	}

	// A reference to the current widget that is the source of events
	private UIComponentBase eventSource = null;
	
	@Override
	public void visitCheckBox(CheckBox checkBox,
	                            boolean parentVisible,
	                            boolean parentEnabled) {
		SmartClientDataGridFieldDefinition def = getFieldDef(checkBox);
		String title = def.getTitle();
		boolean required = def.isRequired();
		UIComponentBase c = (UIComponentBase) cb.checkBox(listBinding, checkBox, title, required);
		eventSource = c;
		addComponent(title,
						required,
						checkBox.getInvisibleConditionName(), 
						checkBox.showsLabelByDefault(),
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
		SmartClientDataGridFieldDefinition def = getFieldDef(membership);
        UIComponentBase c = (UIComponentBase) cb.label("checkMembership"); // TODO check membership
        eventSource = c;
        addToContainer(c, null, null, null);
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
		UIComponentBase c = (UIComponentBase) cb.colourPicker(listBinding, colour, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						colour.getInvisibleConditionName(), 
						colour.showsLabelByDefault(),
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
		UIComponentBase s = (UIComponentBase) cb.combo(listBinding, combo, title, required);
		eventSource = s;
		addComponent(title, 
						required, 
						combo.getInvisibleConditionName(), 
						combo.showsLabelByDefault(),
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
		UIComponent c = cb.contentImage(listBinding, image, title, required);
        addComponent(title, 
        				false, 
        				image.getInvisibleConditionName(), 
        				image.showsLabelByDefault(),
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
		UIComponent c = cb.contentLink(listBinding, link, title, required);
		addComponent(title, 
						required, 
						link.getInvisibleConditionName(), 
						link.showsLabelByDefault(),
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
		UIComponent c = cb.html(listBinding, html, title, required);
        addComponent(title, 
        				required, 
        				html.getInvisibleConditionName(), 
        				html.showsLabelByDefault(),
        				c, 
        				html.getPixelWidth(), 
        				null, 
        				null);
	}

	@Override
	public void visitListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		UIComponentBase c = (UIComponentBase) cb.listMembership(membership);
		eventSource = c;
		addToContainer(c, membership.getListWidthInPixels(), null, null);
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
		SmartClientDataGridFieldDefinition def = getFieldDef(comparison);
        UIComponent c = cb.label("comparison"); // TODO comparison
        addToContainer(c, comparison.getPixelWidth(), comparison.getResponsiveWidth(), comparison.getPercentageWidth());
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
		UIComponentBase c = (UIComponentBase) cb.lookupDescription(listBinding, 
																	lookup, 
																	title, 
																	required,
																	ldef.getDisplayField().replace('_', '.'),
																	ldef.getQuery());
        eventSource = c;
        
        addComponent(title, 
        				required, 
        				lookup.getInvisibleConditionName(), 
        				lookup.showsLabelByDefault(),
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
		UIComponent c = cb.label("lookup"); // TODO lookup
		addComponent(null, 
						false, 
						null, 
						lookup.showsLabelByDefault(),
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
        UIComponentBase c = (UIComponentBase) cb.password(listBinding, password, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				password.getInvisibleConditionName(), 
        				password.showsLabelByDefault(),
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
		boolean required = def.isRequired();
        UIComponentBase c = (UIComponentBase) cb.radio(listBinding, radio, title, required);
		eventSource = c;
		addComponent(title, 
						required, 
						radio.getInvisibleConditionName(), 
						radio.showsLabelByDefault(),
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
        UIComponentBase c = (UIComponentBase) cb.richText(listBinding, richText, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				richText.getInvisibleConditionName(), 
        				richText.showsLabelByDefault(),
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
        UIComponentBase c = (UIComponentBase) cb.label("slider"); // TODO slider
        eventSource = c;
        addComponent(def.getTitle(), 
        				def.isRequired(), 
        				slider.getInvisibleConditionName(), 
        				slider.showsLabelByDefault(),
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
        UIComponentBase c = (UIComponentBase) cb.spinner(listBinding, spinner, title, required);
        eventSource = c;
        addComponent(title, 
        				required, 
        				spinner.getInvisibleConditionName(), 
        				spinner.showsLabelByDefault(),
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
		UIComponentBase c = (UIComponentBase) cb.textArea(listBinding, text, title, required, def.getLength());
        eventSource = c;
        addComponent(title, 
        				required, 
        				text.getInvisibleConditionName(), 
        				text.showsLabelByDefault(),
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

        UIComponentBase c = (UIComponentBase) cb.text(listBinding, 
        												text, 
        												title, 
        												required,
        												def.getLength(),
        												converter,
        												format,
        												convertConverter(converter, type));
        eventSource = c;
		addComponent(title, 
						required, 
						text.getInvisibleConditionName(), 
						text.showsLabelByDefault(),
						c, 
						text.getPixelWidth(), 
						null, 
						null);
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		// do nothing - this is for web 2 ux uis only
	}

	private static javax.faces.convert.Converter convertConverter(Converter<?> converter, AttributeType type) {
	    javax.faces.convert.Converter result = null;
	    if (converter != null) {
		    String converterName = converter.getClass().getSimpleName();
		    if ("DD_MM_YYYY".equals(converterName)) {
		        result = new DD_MM_YYYY();
		    }
		    else if ("DD_MMM_YYYY".equals(converterName)) {
	            result = new DD_MMM_YYYY();
	        }
	        else if ("DD_MM_YYYY_HH_MI".equals(converterName)) {
	            result = new DD_MM_YYYY_HH_MI();
	        }
	        else if ("DD_MM_YYYY_HH24_MI".equals(converterName)) {
	            result = new DD_MM_YYYY_HH24_MI();
	        }
	        else if ("DD_MMM_YYYY_HH_MI".equals(converterName)) {
	            result = new DD_MMM_YYYY_HH_MI();
	        }
	        else if ("DD_MMM_YYYY_HH24_MI".equals(converterName)) {
	            result = new DD_MMM_YYYY_HH24_MI();
	        }
	        else if ("Decimal2DollarsAndCents".equals(converterName)) {
	            result = new Decimal2DollarsAndCents();
	        }
	        else if ("Decimal5DollarsAndCents".equals(converterName)) {
	            result = new Decimal5DollarsAndCents();
	        }
	        else if ("Decimal2Integer".equals(converterName)) {
	            result = new Decimal2Integer();
	        }
	        else if ("Decimal2IntegerPercentage".equals(converterName)) {
	            result = new Decimal2IntegerPercentage();
	        }
	        else if ("Decimal2OneDecimalPlace".equals(converterName)) {
	            result = new Decimal2OneDecimalPlace();
	        }
	        else if ("Decimal5Integer".equals(converterName)) {
	            result = new Decimal5Integer();
	        }
	        else if ("Decimal5IntegerPercentage".equals(converterName)) {
	            result = new Decimal5IntegerPercentage();
	        }
	        else if ("Decimal5OneDecimalPlace".equals(converterName)) {
	            result = new Decimal5OneDecimalPlace();
	        }
	        else if ("Decimal5TimeDuration".equals(converterName)) {
	            result = new Decimal5TimeDuration();
	        }
	        else if ("Decimal5TwoDecimalPlaces".equals(converterName)) {
	            result = new Decimal5TwoDecimalPlaces();
	        }
	        else if ("Decimal5TwoDecimalPlacesPercentage".equals(converterName)) {
	            result = new Decimal5TwoDecimalPlacesPercentage();
	        }
	        else if ("SimplePercentage".equals(converterName)) {
	            result = new SimplePercentage();
	        }
	        else if ("IntegerSeparator".equals(converterName)) {
	            result = new IntegerSeparator();
	        }
	        else if ("HH_MI".equals(converterName)) {
	            result = new HH_MI();
	        }
	        else if ("HH24_MI".equals(converterName)) {
	            result = new HH24_MI();
	        }
	        else if ("DD_MM_YYYY_HH_MI_SS".equals(converterName)) {
	            result = new DD_MM_YYYY_HH_MI_SS();
	        }
	        else if ("DD_MM_YYYY_HH24_MI_SS".equals(converterName)) {
	            result = new DD_MM_YYYY_HH24_MI_SS();
	        }
	        else if ("DD_MMM_YYYY_HH_MI_SS".equals(converterName)) {
	            result = new DD_MMM_YYYY_HH_MI_SS();
	        }
	        else if ("DD_MMM_YYYY_HH24_MI_SS".equals(converterName)) {
	            result = new DD_MMM_YYYY_HH24_MI_SS();
	        }
	    }
	    else {
	    	// Set default faces numeric converters if none is set
	    	if (AttributeType.decimal2.equals(type)) {
	    		result = new Decimal2();
	    	}
	    	else if (AttributeType.decimal5.equals(type)) {
	    		result = new Decimal5();
	    	}
	    	else if (AttributeType.decimal10.equals(type)) {
	    		result = new Decimal10();
	    	}
	    	else if (AttributeType.integer.equals(type)) {
	    		result = new org.skyve.impl.web.faces.converters.lang.Integer();
	    	}
	    	else if (AttributeType.longInteger.equals(type)) {
	    		result = new org.skyve.impl.web.faces.converters.lang.Long();
	    	}
	    }
	    
	    return result;
	}
	
	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled) {
		eventSource = null;
	}

	private void addToContainer(UIComponent component, 
									Integer pixelWidth, 
									Integer responsiveWidth,
									Integer percentageWidth) {
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();

		current = lb.addToContainer(currentContainer, 
										current, 
										component, 
										pixelWidth, 
										responsiveWidth, 
										percentageWidth);
	}
	
	private void addedToContainer() {
		if (currentContainers.isEmpty()) {
			throw new IllegalStateException("Trying to complete the add to a container but there is nothing in the stack of currentContainers!!");
		}
		Container currentContainer = currentContainers.peek();
		current = lb.addedToContainer(currentContainer, current);
	}
	
	private void addAjaxBehavior(String eventName, List<EventAction> actions) {
		String actionName = null;
		for (EventAction action : actions) {
			if (action instanceof ServerSideActionEventAction) {
				actionName = ((ServerSideActionEventAction) action).getActionName();
				break;
			}
		}
		
		eventSource.addClientBehavior(eventName, cb.ajax(listBinding, actionName));
	}
	
	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		addAjaxBehavior("change", changeable.getChangedActions());
		// Add this special event for date selection on calendar as "changed" doesn't fire on select
		if (eventSource instanceof Calendar) {
			addAjaxBehavior("dateSelect", changeable.getChangedActions());
		}
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
		addAjaxBehavior("focus", blurable.getFocusActions());
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
		addAjaxBehavior("blur", blurable.getFocusActions());
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
		addAjaxBehavior("itemSelect", lookup.getPickedActions());
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
		addAjaxBehavior("itemUnselect", lookup.getClearedActions());
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
	public void visitAction(ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			toolbarLayout.getChildren().add(cb.action(listBinding, action, null, action.getDisplayName()));
		}
	}

	private void processImplicitAction(ActionImpl action, ImplicitActionName name) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (ImplicitActionName.Report.equals(name)) {
				toolbarLayout.getChildren().add(cb.report(action));
			}
			else {
				toolbarLayout.getChildren().add(cb.action(listBinding, action, name, name.getDisplayName()));
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
	public void visitParameter(
			org.skyve.metadata.view.widget.bound.Parameter parameter,
			boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void visitFilterParameter(FilterParameter parameter,
			boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	private SmartClientDataGridFieldDefinition getFieldDef(InputWidget inputWidget) {
		SmartClientDataGridFieldDefinition result = null;

		DocumentImpl targetDocument = document;
		ModuleImpl targetModule = module;
// Document is already set to the child document when instantiating a FacesViewVisitor so there is no need to resolve the view binding
//		if (viewBinding != null) {
//			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, viewBinding);
//			targetDocument = (Document) target.getDocument();
//			targetModule = (Module) customer.getModule(targetDocument.getOwningModuleName());
//		}
		if (listBinding == null) {
			result = SmartClientGenerateUtils.getField(user, customer, targetModule, targetDocument, inputWidget);
		}
		else {
			if (inputWidget.getBinding() == null) {
				result = SmartClientGenerateUtils.getDataGridField(user, customer, targetModule, targetDocument, inputWidget, listBinding);
			}
			else {
				TargetMetaData target = Binder.getMetaDataForBinding(customer, targetModule, targetDocument, listBinding);
				targetDocument = (DocumentImpl) module.getDocument(customer, ((Reference) target.getAttribute()).getDocumentName());
				targetModule = (ModuleImpl) customer.getModule(targetDocument.getOwningModuleName());
				result = SmartClientGenerateUtils.getDataGridField(user, customer, targetModule, targetDocument, inputWidget, null);
			}
		}
		
		String title = (currentFormItem == null) ? null : currentFormItem.getLabel();
		if (title != null) {
			result.setTitle(title);
		}
		Boolean required = (currentFormItem == null) ? null : currentFormItem.getRequired();
		if (required != null) {
			result.setRequired(required.booleanValue());
		}

		return result;
	}
}
