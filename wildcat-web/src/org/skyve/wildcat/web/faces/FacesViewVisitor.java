package org.skyve.wildcat.web.faces;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlOutputLabel;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.component.html.HtmlSelectOneMenu;

import org.primefaces.component.celleditor.CellEditor;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.message.Message;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.row.Row;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;
import org.skyve.wildcat.generate.SmartClientGenerateUtils;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientDataGridFieldDefinition;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientLookupDefinition;
import org.skyve.wildcat.metadata.Container;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.model.document.field.ConvertableField;
import org.skyve.wildcat.metadata.model.document.field.Text;
import org.skyve.wildcat.metadata.model.document.field.TextFormat;
import org.skyve.wildcat.metadata.module.ModuleImpl;
import org.skyve.wildcat.metadata.view.Action;
import org.skyve.wildcat.metadata.view.Inject;
import org.skyve.wildcat.metadata.view.ViewImpl;
import org.skyve.wildcat.metadata.view.ViewVisitor;
import org.skyve.wildcat.metadata.view.container.HBox;
import org.skyve.wildcat.metadata.view.container.Tab;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.VBox;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.container.form.FormColumn;
import org.skyve.wildcat.metadata.view.container.form.FormItem;
import org.skyve.wildcat.metadata.view.container.form.FormRow;
import org.skyve.wildcat.metadata.view.event.Changeable;
import org.skyve.wildcat.metadata.view.event.EventAction;
import org.skyve.wildcat.metadata.view.event.Focusable;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.wildcat.metadata.view.reference.ActionReference;
import org.skyve.wildcat.metadata.view.reference.ContentReference;
import org.skyve.wildcat.metadata.view.reference.DefaultListViewReference;
import org.skyve.wildcat.metadata.view.reference.EditViewReference;
import org.skyve.wildcat.metadata.view.reference.ExternalReference;
import org.skyve.wildcat.metadata.view.reference.ImplicitActionReference;
import org.skyve.wildcat.metadata.view.reference.QueryListViewReference;
import org.skyve.wildcat.metadata.view.reference.ReferenceProcessor;
import org.skyve.wildcat.metadata.view.reference.ReferenceTarget;
import org.skyve.wildcat.metadata.view.reference.ReportReference;
import org.skyve.wildcat.metadata.view.reference.ResourceReference;
import org.skyve.wildcat.metadata.view.widget.Blurb;
import org.skyve.wildcat.metadata.view.widget.Button;
import org.skyve.wildcat.metadata.view.widget.DialogButton;
import org.skyve.wildcat.metadata.view.widget.DynamicImage;
import org.skyve.wildcat.metadata.view.widget.GeoLocator;
import org.skyve.wildcat.metadata.view.widget.Link;
import org.skyve.wildcat.metadata.view.widget.MapDisplay;
import org.skyve.wildcat.metadata.view.widget.Spacer;
import org.skyve.wildcat.metadata.view.widget.StaticImage;
import org.skyve.wildcat.metadata.view.widget.bound.Label;
import org.skyve.wildcat.metadata.view.widget.bound.ProgressBar;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckBox;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.wildcat.metadata.view.widget.bound.input.Combo;
import org.skyve.wildcat.metadata.view.widget.bound.input.Comparison;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentImage;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentLink;
import org.skyve.wildcat.metadata.view.widget.bound.input.Geometry;
import org.skyve.wildcat.metadata.view.widget.bound.input.HTML;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;
import org.skyve.wildcat.metadata.view.widget.bound.input.ListMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.Lookup;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.Password;
import org.skyve.wildcat.metadata.view.widget.bound.input.Radio;
import org.skyve.wildcat.metadata.view.widget.bound.input.RichText;
import org.skyve.wildcat.metadata.view.widget.bound.input.Slider;
import org.skyve.wildcat.metadata.view.widget.bound.input.Spinner;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextArea;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickList;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.DynamicImageServlet;
import org.skyve.wildcat.web.UserAgent.UserAgentType;
import org.skyve.wildcat.web.faces.converters.date.DD_MMM_YYYY;
import org.skyve.wildcat.web.faces.converters.date.DD_MM_YYYY;
import org.skyve.wildcat.web.faces.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.wildcat.web.faces.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.wildcat.web.faces.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.wildcat.web.faces.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.wildcat.web.faces.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.wildcat.web.faces.converters.decimal.Decimal5TimeDuration;
import org.skyve.wildcat.web.faces.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.wildcat.web.faces.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.wildcat.web.faces.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.wildcat.web.faces.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.wildcat.web.faces.converters.integer.SimplePercentage;
import org.skyve.wildcat.web.faces.converters.time.HH24_MI;
import org.skyve.wildcat.web.faces.converters.time.HH_MI;
import org.skyve.wildcat.web.faces.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.wildcat.web.faces.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.wildcat.web.faces.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.wildcat.web.faces.converters.timestamp.DD_MM_YYYY_HH_MI_SS;

// handle domain values correctly
// make associations be auto-completes
// datagrid tag
// listgrid tag
// view titles
// use converters
// use calendars, date, time, date and time, timestamp versions
// make buttons a command button with ajax at least that rerenders the form
public class FacesViewVisitor extends ViewVisitor {
	private User user;
	private ComponentBuilder b;
	private boolean createView;
	private UserAgentType userAgentType;
	private String widgetId;
	private UIComponent fragment; // if we have a widgetId to render, this holds a reference to that component

	
	private UIComponent current; // current component being constructed
	private Stack<Container> currentContainers = new Stack<>(); // used to determine how to add widgets to containers
	private UIComponent facesView; // the result of construction
	private UIComponent toolbar; // the toolbar

	public FacesViewVisitor(User user, 
							CustomerImpl customer,
							ModuleImpl module,
							DocumentImpl document,
							ViewImpl view,
							String managedBeanName,
							UserAgentType userAgentType,
							String widgetId,
							String process,
							String update) {
        super(customer, module, document, view);
        this.user = user;
        ViewType viewType = view.getType();
        createView = ViewType.create.equals(viewType);
        this.userAgentType = userAgentType;
        b = new ComponentBuilder(managedBeanName, process, update);
        this.widgetId = widgetId;
    }
	
	public UIComponent getFacesView() {
		return facesView;
	}

	@Override
	public void visitView() throws MetaDataException {
	    // Ensure visibility is set for both create and edit views
        current = b.panelGroup(true, false, createView ? "created" : "notCreated");
        facesView = current;
        
		// Create the toolbar
    	toolbar = b.panelGroup(true, false, null);

        // Add the panel grid layout for the view container aspect for non-phone agents
    	if (! UserAgentType.phone.equals(userAgentType)) {
	        PanelGrid layout = b.panelGrid(null, Integer.valueOf(100), null, null, null);
			layout.setColumns(1);
			if (widgetId == null) {
				current.getChildren().add(layout);
			}
	        current = layout;
        }
        
        currentContainers.push(view);
	}

	@Override
	public void visitedView() throws MetaDataException {
        currentContainers.pop();

        // Add the toolbar if this is a full view render and the toolbar has contents
        if ((widgetId == null) && (! toolbar.getChildren().isEmpty())) {
            if (UserAgentType.phone.equals(userAgentType)) {
            	facesView.getChildren().add(0, toolbar);
            }
            else {
				Toolbar bar = b.toolbar();
				bar.getFacets().put("left", toolbar);
				facesView.getChildren().add(0, bar);
            }
    	}
	}

//	private TabPane currentTabPane; // for setting disabled tabs
	@Override
	public void visitTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
//		currentTabPane = tabPane;
		UIComponent tabView = UserAgentType.phone.equals(userAgentType) ? 
				                    b.accordionPanel(tabPane.getInvisibleConditionName()) : 
				                    b.tabView(tabPane.getInvisibleConditionName());
        addToContainer(tabView, tabPane.getPixelWidth(), tabPane.getPercentageWidth());

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			fragment = tabView;
		}
	}

	@Override
	public void visitedTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
//		currentTabPane = null;
		completeAddToContainer();

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
							boolean parentEnabled)
	throws MetaDataException {
		tab.getSelectedConditionName();
		org.primefaces.component.tabview.Tab pfTab = b.tab(tab.getTitle(), 
															tab.getDisabledConditionName(), 
															tab.getInvisibleConditionName());
		current.getChildren().add(pfTab);
		if (UserAgentType.phone.equals(userAgentType)) {
			current = pfTab;
		}
		else {
			PanelGrid layout = b.panelGrid(null, Integer.valueOf(100), null, null, null);
			layout.setColumns(1);
			pfTab.getChildren().add(layout);
			current = layout;
		}

		currentContainers.push(tab);
	}

	@Override
	public void visitedTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled)
	throws MetaDataException {
		if (! UserAgentType.phone.equals(userAgentType)) {
			// remove the layout grid
			current = current.getParent();
		}
		current = current.getParent();
		currentContainers.pop();
	}

	@Override
	public void visitVBox(VBox vbox,
							boolean parentVisible,
							boolean parentEnabled)
	throws MetaDataException {
		// Cater for fieldset if this thing has a border
		Panel fs = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			fs = b.panel(vbox.getBorderTitle(), vbox.getInvisibleConditionName(), vbox.getPixelWidth());
			addToContainer(fs, vbox.getPixelWidth(), vbox.getPercentageWidth());
		}

		// VBox is a Panel grid with 1 column
		PanelGrid pg = b.panelGrid(vbox.getPixelWidth(),
									null, // the parent container sets the percentage width
									vbox.getPixelHeight(),
									vbox.getPercentageHeight(),
									vbox.getInvisibleConditionName());
		pg.setColumns(1);
		
		// Cater for fieldset if defined
		if (fs != null) {
			fs.getChildren().add(pg);

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = fs;
			}
		}
		else {
			addToContainer(pg, vbox.getPixelWidth(), vbox.getPercentageWidth());

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = pg;
			}
		}
		current = pg;

		currentContainers.push(vbox);
	}

	@Override
	public void visitedVBox(VBox vbox,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		// Cater for fieldset, if one was added
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			current = current.getParent();
		}
		currentContainers.pop();
		completeAddToContainer();
		
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
							boolean parentEnabled)
	throws MetaDataException {
		// Cater for fieldset if this thing has a border
		Panel fs = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			fs = b.panel(hbox.getBorderTitle(), hbox.getInvisibleConditionName(), hbox.getPixelWidth());
			addToContainer(fs, hbox.getPixelWidth(), hbox.getPercentageWidth());
		}

		// HBox is a Panel grid with n columns
		PanelGrid pg = b.panelGrid(hbox.getPixelWidth(),
									null, // the parent container sets the percentage width
									hbox.getPixelHeight(),
									hbox.getPercentageHeight(),
									hbox.getInvisibleConditionName());
		if (UserAgentType.phone.equals(userAgentType)) { // HBox will layout vertically for phones
			pg.setColumns(1);
		}
		
		// Cater for fieldset if defined
		if (fs != null) {
			fs.getChildren().add(pg);

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = fs;
			}
		}
		else {
			addToContainer(pg, hbox.getPixelWidth(), hbox.getPercentageWidth());
			
			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = pg;
			}
		}
		current = pg;

		currentContainers.push(hbox);
	}

	@Override
	public void visitedHBox(HBox hbox,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		// Cater for fieldset, if one was added
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			current = current.getParent();
		}
		currentContainers.pop();
		completeAddToContainer();
		
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
							boolean parentEnabled)
	throws MetaDataException {
		if (! UserAgentType.phone.equals(userAgentType)) {
			Panel fs = null;
			if (Boolean.TRUE.equals(form.getBorder())) {
				fs = b.panel(form.getBorderTitle(), form.getInvisibleConditionName(), form.getPixelWidth());
				addToContainer(fs, form.getPixelWidth(), form.getPercentageWidth());
			}

			PanelGrid pg = b.panelGrid(form.getPixelWidth(),
										null, // the parent container sets the percentage width
										form.getPixelHeight(), 
										form.getPercentageHeight(),
										form.getInvisibleConditionName());
			if (fs != null) {
				fs.getChildren().add(pg);

				// start rendering if appropriate
				if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
					fragment = fs;
				}
			}
			else {
				addToContainer(pg, form.getPixelWidth(), form.getPercentageWidth());

				// start rendering if appropriate
				if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
					fragment = pg;
				}
			}
			current = pg;
		}
		currentForm = form;
		currentFormColumn = 0;
// TODO form.getDisabledConditionName() form.getLabelDefaultHorizontalAlignment()
	}

	@Override
	public void visitedForm(Form form,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		if (! UserAgentType.phone.equals(userAgentType)) {
			if (Boolean.TRUE.equals(form.getBorder())) {
				current = current.getParent();
			}
			completeAddToContainer();
		}
		currentForm = null;
		
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
									boolean parentEnabled)
	throws MetaDataException {
		// Nothing to do here - for columns are a spec for html tables in this renderer.
	}

	@Override
	public void visitFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		if (! UserAgentType.phone.equals(userAgentType)) {
			Row r = b.row();
			current.getChildren().add(r);
			current = r;
			currentFormColumn = 0;
		}
	}

	private FormItem currentFormItem;
	
	@Override
	public void visitFormItem(FormItem item,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		currentFormItem = item;
	}

	@Override
	public void visitedFormItem(FormItem item,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		currentFormItem = null;
	}

	@Override
	public void visitedFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		if (! UserAgentType.phone.equals(userAgentType)) {
			current = current.getParent();
		}
	}

	private void addComponent(String widgetLabel,
								boolean widgetRequired,
								String widgetInvisible,
								UIComponent component,
								Integer pixelWidth,
								Integer percentageWidth) {
		if (cellEditor == null) { // not a bound column in a datagrid
			if (currentFormItem == null) { // not a form item
				if (currentGrid == null) { // not a container column in a datagrid
					// This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, percentageWidth);
					completeAddToContainer();
				}
				else {
					// This must be a data grid container column

					// add a spacer, if required
					List<UIComponent> children = current.getChildren();
					if (! children.isEmpty()) {
						children.add(b.label(null, null, " ", null));
					}
					children.add(component);
				}
			}
			else { // a form item
				UIComponent columnOrField = null;
				// The label
				if (! Boolean.FALSE.equals(currentFormItem.getShowLabel())) {
					String label = currentFormItem.getLabel();
					if (label == null) {
						label = widgetLabel;
					}
					if (label != null) {
						if (widgetRequired) {
							label = label + " *";
						}
						
						if (UserAgentType.phone.equals(userAgentType)) {
							columnOrField = b.field(null);
							HtmlOutputLabel l = b.label(null, null, label, null);
							columnOrField.getChildren().add(l);
//							Message m = b.message(component.getId());
//							columnOrField.getChildren().add(m);
							current.getChildren().add(columnOrField);
						}
						else {
							List<FormColumn> formColumns = currentForm.getColumns();
							if (currentFormColumn >= formColumns.size()) {
								currentFormColumn = 0;
							}
							FormColumn formColumn = formColumns.get(currentFormColumn++);
							columnOrField = b.column(widgetInvisible, 
														true,
														false,
														formColumn.getPixelWidth(), 
														formColumn.getPercentageWidth(),
														null,
														null);
							current.getChildren().add(columnOrField);
							HtmlPanelGroup pg = b.panelGroup(true, true, widgetInvisible);
							columnOrField.getChildren().add(pg);
							HtmlOutputLabel l = b.label(null, null, label, component.getId());
							pg.getChildren().add(l);
							Message m = b.message(component.getId());
							pg.getChildren().add(m);
						}
					}
				}
				// The field
				if (UserAgentType.phone.equals(userAgentType)) {
					if (columnOrField == null) {
						columnOrField = b.field(null);
						current.getChildren().add(columnOrField);
					}
					columnOrField.getChildren().add(component);
				}
				else {
					List<FormColumn> formColumns = currentForm.getColumns();
					if (currentFormColumn >= formColumns.size()) {
						currentFormColumn = 0;
					}
					FormColumn formColumn = formColumns.get(currentFormColumn++);
					Column col = b.column(widgetInvisible,
											true,
											false,
											formColumn.getPixelWidth(),
											formColumn.getPercentageWidth(),
											currentFormItem.getColspan(),
											currentFormItem.getRowspan());
					current.getChildren().add(col);
					col.getChildren().add(component);
				}
			}
		}
		else { // bound column
	        cellEditor.getFacets().put("input", component);
		}
	}
	
	@Override
	public void visitButton(Button button,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		org.skyve.metadata.view.Action action = view.getAction(button.getActionName());
	    UIComponent c = b.actionButton(action.getDisplayName(),
			                            action.getToolTip(),
			                            action.getImplicitName(),
			                            action.getName(),
			                            listBinding,
			                            button.getPixelWidth(),
			                            button.getPixelHeight(),
			                            action.getClientValidation(),
			                            action.getDisabledConditionName(),
			                            action.getInvisibleConditionName());
	    addComponent(null, false, action.getInvisibleConditionName(), c, button.getPixelWidth(), null);
	}

	@Override
	public void visitGeoLocator(GeoLocator locator,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
	    UIComponent l = b.label(null, null, "geoLocator", null); // TODO geolocator
	    addComponent(null, false, locator.getInvisibleConditionName(), l, null, null);
	}

	@Override
	public void visitGeometry(Geometry geometry,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
	    UIComponent l = b.label(null, null, "geometry", null); // TODO geometry
	    addComponent(null, false, geometry.getInvisibleConditionName(), l, geometry.getPixelWidth(), null);
	}

	@Override
	public void visitMap(MapDisplay map,
							boolean parentVisible,
							boolean parentEnabled)
	throws MetaDataException {
	    UIComponent l = b.label(null, null, "map", null); // TODO map
	    addComponent(null, false, map.getInvisibleConditionName(), l, map.getPixelWidth(), map.getPercentageWidth());
	}

	@Override
	public void visitDialogButton(DialogButton button,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
	    UIComponent bn = b.label(null, null, "dialogButton", null); // TODO dialog button
	    addComponent(null, false, button.getInvisibleConditionName(), bn, null, null);
	}

	@Override
	public void visitDynamicImage(DynamicImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		String name = image.getName();
		Integer pixelWidth = image.getPixelHeight();
		Integer pixelHeight = image.getPixelHeight();
		Integer initialPixelWidth = image.getImageInitialPixelWidth();
		Integer initialPixelHeight = image.getImageInitialPixelHeight();
		
		StringBuilder url = new StringBuilder(128);
		url.append("/images/dynamic.png?").append(AbstractWebContext.DOCUMENT_NAME).append('=');
		url.append(module.getName()).append('.').append(document.getName());
		url.append('&').append(DynamicImageServlet.IMAGE_NAME).append('=').append(name);
		if (pixelWidth != null) {
			url.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(pixelWidth);
		}
		else {
			url.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(initialPixelWidth);
		}
		if (pixelHeight != null) {
			url.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(pixelHeight);
		}
		else {
			url.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(initialPixelHeight);
		}
		url.append('&').append(DynamicImageServlet.IMAGE_WIDTH_ZOOM_NAME).append("=100");
		url.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_ZOOM_NAME).append("=100");
		
		UIComponent i = b.graphicImage(pixelWidth,
    									image.getPercentageWidth(),
    									pixelHeight,
    									image.getPercentageHeight(),
    									url.toString(),
    									image.getInvisibleConditionName());
		addComponent(null, false, image.getInvisibleConditionName(), i, image.getPixelWidth(), image.getPercentageWidth());
	}

	@Override
	public void visitSpacer(Spacer spacer) throws MetaDataException {
		addComponent(null, false, null, b.label(null, null, " ", null), null, null);
	}

	@Override
	public void visitStaticImage(StaticImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		UIComponent i = b.graphicImage(image.getPixelWidth(),
	    								image.getPercentageWidth(),
	    								image.getPixelHeight(),
	    								image.getPercentageHeight(),
	    								"images/" + image.getRelativeFile(),
	    								image.getInvisibleConditionName());
		addComponent(null, false, image.getInvisibleConditionName(), i, image.getPixelWidth(), image.getPercentageWidth());
	}

	@Override
	public void visitBlurb(Blurb blurb,
							boolean parentVisible,
							boolean parentEnabled)
	throws MetaDataException {
		String value = null;
		String binding = null;
		String markup = blurb.getMarkup();
		if (markup.indexOf('{') > -1) {
			binding = markup;
		}
		else {
			value = markup;
		}
		UIComponent c = b.text(value,
								binding, 
								blurb.getTextAlignment(), 
								blurb.getPixelWidth(), 
								blurb.getPixelHeight(), 
								false);
		addComponent(null, false, blurb.getInvisibleConditionName(), c, blurb.getPixelWidth(), null);
	}

	@Override
	public void visitLink(final Link link,
	                        boolean parentVisible,
	                        boolean parentEnabled)
    throws MetaDataException {
		org.skyve.wildcat.metadata.view.reference.Reference outerReference = link.getReference();
		final ReferenceTarget target = link.getTarget();
		final AtomicReference<UIComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			@Override
			public void processResourceReference(ResourceReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			public void processReportReference(ReportReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			public void processQueryListViewReference(QueryListViewReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			public void processImplicitActionReference(ImplicitActionReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			public void processExternalReference(ExternalReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processEditViewReference(EditViewReference reference)
			throws MetaDataException {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');

				c.set(b.outputLink(listBinding, link.getValue(), href.toString(), link.getInvisibleConditionName(), target));
			}
			
			@Override
			public void processDefaultListViewReference(DefaultListViewReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			public void processContentReference(ContentReference reference)
			throws MetaDataException {
				c.set(b.label(null, null, "link", null)); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference)
			throws MetaDataException {
				// TODO do the tooltip and client validation, disabled, invisible thing
				c.set(b.actionLink(link.getValue(),
									null,
									null,
									reference.getActionName(),
									listBinding,
									link.getPixelWidth(),
									null,
									Boolean.FALSE,
									null,
									link.getInvisibleConditionName()));
			}
		}.process(outerReference);

		addComponent(null, false, link.getInvisibleConditionName(), c.get(), link.getPixelWidth(), null);
	}

	@Override
	public void visitLabel(Label label,
	                        boolean parentVisible,
	                        boolean parentEnabled)
    throws MetaDataException {
	    UIComponent c = b.label(listBinding, label.getBinding(), label.getValue(), null);
	    addComponent(null, false, label.getInvisibleConditionName(), c, label.getPixelWidth(), null);
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar,
	                                boolean parentVisible,
	                                boolean parentEnabled)
	throws MetaDataException {
	    UIComponent p = b.label(null, null, "progressBar", null); // TODO progress bar
	    addComponent(null, false, null, p, progressBar.getPixelWidth(), null);
	}

	private MetaData currentGrid;

	@Override
	public void visitListGrid(ListGrid grid,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		HtmlOutputLabel l = b.label(null, null, "listGrid", null);
		addToContainer(l, grid.getPixelWidth(), grid.getPercentageWidth()); // TODO list grid
		currentGrid = grid;
	}

	@Override
	public void visitedListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		currentGrid = null;
		completeAddToContainer();
	}

	private String listBinding;
	
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled)
    throws MetaDataException {
		UIComponent g = UserAgentType.phone.equals(userAgentType) ? 
		                    b.dataList(grid.getBinding(), 
		                    			grid.getTitle(),
		                    			grid.getInvisibleConditionName()) : 
		                    b.dataTable(grid.getBinding(),
		                    				grid.getTitle(),
		                    				grid.getInvisibleConditionName(),
		                    				! Boolean.FALSE.equals(grid.getEditable()),
		                    				UserAgentType.tablet.equals(userAgentType));
        addToContainer(g, grid.getPixelWidth(), grid.getPercentageWidth());
		currentGrid = grid;
		listBinding = grid.getBinding();
		
		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			fragment = g;
		}
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled)
    throws MetaDataException {
		if ((! UserAgentType.tablet.equals(userAgentType)) && (! Boolean.FALSE.equals(grid.getEditable()))) {
			Column col = b.dataTableActionColumn(listBinding, UserAgentType.phone.equals(userAgentType));
			current.getChildren().add(col);
		}
		
	    currentGrid = null;
	    listBinding = null;
		completeAddToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
	}

	private CellEditor cellEditor;

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled)
	throws MetaDataException {
		String title = column.getTitle();
		String binding = column.getBinding();
		if (title == null) {
			StringBuilder sb = new StringBuilder(64);
			sb.append(listBinding);
			if (binding != null) {
				sb.append('.').append(binding);
			}
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, sb.toString());
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					title = attribute.getDisplayName();
				}
			}
		}
		cellEditor = b.cellEditor(listBinding, binding);
		Column col = b.column(listBinding,
								(binding == null) ? Bean.BIZ_KEY : binding,
								title,
	                            column.getAlignment(),
	                            false,
	                            column.getPixelWidth());
		current.getChildren().add(col);
        current = col;
        
        if (UserAgentType.phone.equals(userAgentType) ||
        		(! Boolean.FALSE.equals(((DataGrid) currentGrid).getEditable()))) {
            col.getChildren().add(cellEditor.getFacet("output"));
        }
        else {
	        col.getChildren().add(cellEditor);
        }
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
	                                        boolean parentVisible,
	                                        boolean parentEnabled)
	throws MetaDataException {
		cellEditor = null;
	    current = current.getParent();
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled)
	throws MetaDataException {
        Column col = b.column(listBinding,
        						null,
        						column.getTitle(),
                                column.getAlignment(),
                                false,
                                column.getPixelWidth());
        current.getChildren().add(col);
        current = col;
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
	                                            boolean parentVisible,
	                                            boolean parentEnabled)
	throws MetaDataException {
        current = current.getParent();
	}

	@Override
	public void visitPickList(PickList list,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		HtmlOutputLabel l = b.label(null, null, "pickList", null);
		addToContainer(l, list.getPixelWidth(), list.getPercentageWidth()); // TODO picklist
	}

	@Override
	public void visitedPickList(PickList list,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		completeAddToContainer();
	}

	@Override
	public void visitPickListColumn(PickListColumn column,
										boolean parentVisible,
										boolean parentEnabled)
	throws MetaDataException {
		// TODO pick list column
	}

	// A reference to the current widget that is the source of events
	private UIComponentBase eventSource = null;
	
	@Override
	public void visitCheckBox(CheckBox checkBox,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(checkBox);
/* TODO Why don't tri state checkboxes work???
		UIComponentBase c = Boolean.FALSE.equals(checkBox.getTriState()) ?
								b.checkbox(listBinding,
										checkBox.getBinding(), 
										def.getTitle(),
										def.isRequired(),
										checkBox.getDisabledConditionName()) :
								b.triStateCheckbox(listBinding,
													checkBox.getBinding(), 
													def.getTitle(),
													def.isRequired(),
													checkBox.getDisabledConditionName());
*/
		UIComponentBase c = b.checkbox(listBinding,
										checkBox.getBinding(), 
										def.getTitle(),
										def.isRequired(),
										checkBox.getDisabledConditionName(),
										UserAgentType.phone.equals(userAgentType));
		eventSource = c;
		addComponent(UserAgentType.phone.equals(userAgentType) ? null : def.getTitle(), 
						def.isRequired(),
						checkBox.getInvisibleConditionName(), 
						c, 
						checkBox.getPixelWidth(), 
						null);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitCheckMembership(CheckMembership membership,
	                                    boolean parentVisible,
	                                    boolean parentEnabled)
	throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(membership);
        UIComponentBase c = b.label(null, null, "checkMembership", null); // TODO check membership
        eventSource = c;
        addToContainer(c, null, null);
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
	                                    boolean parentVisible,
	                                    boolean parentEnabled)
	throws MetaDataException {
	    completeAddToContainer();
	    eventSource = null;
	}

	@Override
	public void visitColourPicker(ColourPicker colour,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(colour);
		UIComponentBase c = b.colourPicker(listBinding, 
											colour.getBinding(), 
											def.getTitle(), 
											def.isRequired(), 
											colour.getPixelWidth(),
											! UserAgentType.phone.equals(userAgentType));
		eventSource = c;
		addComponent(def.getTitle(), def.isRequired(), colour.getInvisibleConditionName(), c, colour.getPixelWidth(), null);
	}

	@Override
	public void visitedColourPicker(ColourPicker colour,
	                                    boolean parentVisible,
	                                    boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitCombo(Combo combo,
	                        boolean parentVisible,
	                        boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(combo);
		String binding = combo.getBinding();
		HtmlSelectOneMenu s = b.selectOneMenu(listBinding,
												binding,
					                            def.getTitle(),
					                            def.isRequired(),
					                            combo.getDisabledConditionName(),
					                            null);
		UISelectItems i = b.selectItems(listBinding, binding, true);
		s.getChildren().add(i);
		eventSource = s;
		addComponent(def.getTitle(), def.isRequired(), combo.getInvisibleConditionName(), s, combo.getPixelWidth(), null);
	}

	@Override
	public void visitedCombo(Combo combo,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitContentImage(ContentImage image,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(image);
		UIComponent c = b.contentGraphicImage(image.getPixelWidth(), 
												null, 
												image.getPixelHeight(), 
												null, 
												image.getBinding(), 
												null);
        addComponent(def.getTitle(), false, image.getInvisibleConditionName(), c, image.getPixelWidth(), null);
	}

	@Override
	public void visitContentLink(ContentLink link,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(link);
        UIComponent c = b.label(null, null, "contentLink", null); // TODO content link
        addComponent(def.getTitle(), def.isRequired(), link.getInvisibleConditionName(), c, link.getPixelWidth(), null);
	}

	@Override
	public void visitHTML(HTML html,
                            boolean parentVisible,
                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(html);
        UIComponent c = b.editor(listBinding,
									html.getBinding(),
			                        def.getTitle(),
			                        def.isRequired(),
			                        html.getDisabledConditionName());
        addComponent(def.getTitle(), def.isRequired(), html.getInvisibleConditionName(), c, html.getPixelWidth(), null);
	}

	@Override
	public void visitListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled)
	throws MetaDataException {
		UIComponentBase c = b.label(null, null, "listMembership", null); // TODO complete this
		eventSource = c;
		addToContainer(c, membership.getListWidthInPixels(), null);
	}

	@Override
	public void visitedListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled)
	throws MetaDataException {
		completeAddToContainer();
		eventSource = null;
	}

	@Override
	public void visitComparison(Comparison comparison,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(comparison);
        UIComponent c = b.label(null, null, "comparison", null); // TODO comparison
        addToContainer(c, comparison.getPixelWidth(), comparison.getPercentageWidth());
        completeAddToContainer();
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup,
	                                    boolean parentVisible,
	                                    boolean parentEnabled)
	throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(lookup);
		SmartClientLookupDefinition ldef = def.getLookup();
        UIComponentBase c = b.autoComplete(listBinding,
	    									lookup.getBinding(),
	                                        def.getTitle(),
	                                        def.isRequired(),
	                                        lookup.getDisabledConditionName(),
	                                        ldef.getDisplayField().replace('_', '.'),
	                                        ldef.getQuery(),
	                                        lookup.getPixelWidth(),
	                                        ! UserAgentType.phone.equals(userAgentType));
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), lookup.getInvisibleConditionName(), c, lookup.getPixelWidth(), null);
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup,
	                                        boolean parentVisible,
	                                        boolean parentEnabled)
	throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		UIComponent c = b.label(null, null, "lookup", null); // TODO lookup
		addComponent(null, false, null, c, null, null);
	}

	@Override
	public void visitedLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		// do nothing
	}

	@Override
	public void visitPassword(Password password,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(password);
        UIComponentBase c = b.password(listBinding,
										password.getBinding(), 
				                        def.getTitle(),
				                        def.isRequired(),
				                        password.getDisabledConditionName(),
				                        password.getPixelWidth(),
				                        ! UserAgentType.phone.equals(userAgentType));
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), password.getInvisibleConditionName(), c, password.getPixelWidth(), null);
	}

	@Override
	public void visitedPassword(Password password,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitRadio(Radio radio,
                            boolean parentVisible,
                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(radio);
		String binding = radio.getBinding();
        UIComponentBase c = b.selectOneRadio(listBinding,
												binding,
				                                def.getTitle(),
				                                def.isRequired(),
				                                radio.getDisabledConditionName());
        c.getAttributes().put("binding", radio.getBinding());
        UISelectItems i = b.selectItems(listBinding, binding, false);
		c.getChildren().add(i);
		eventSource = c;
		addComponent(def.getTitle(), def.isRequired(), radio.getInvisibleConditionName(), c, radio.getPixelWidth(), null);
	}

	@Override
	public void visitedRadio(Radio radio,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitRichText(RichText richText,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(richText);
        UIComponentBase c = b.editor(listBinding,
										richText.getBinding(),
				                        def.getTitle(),
				                        def.isRequired(),
				                        richText.getDisabledConditionName());
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), richText.getInvisibleConditionName(), c, richText.getPixelWidth(), null);
	}

	@Override
	public void visitedRichText(RichText richText,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitSlider(Slider slider,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(slider);
        UIComponentBase c = b.label(null, null, "slider", null); // TODO slider
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), slider.getInvisibleConditionName(), c, slider.getPixelWidth(), null);
	}

	@Override
	public void visitedSlider(Slider slider,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitSpinner(Spinner spinner,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(spinner);
        UIComponentBase c = b.spinner(listBinding,
										spinner.getBinding(),
				                        def.getTitle(),
				                        def.isRequired(),
				                        spinner.getDisabledConditionName(),
				                        spinner.getPixelWidth());
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), spinner.getInvisibleConditionName(), c, spinner.getPixelWidth(), null);
	}

	@Override
	public void visitedSpinner(Spinner spinner,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitTextArea(TextArea text,
	                            boolean parentVisible,
	                            boolean parentEnabled)
    throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(text);
        UIComponentBase c = b.textArea(listBinding,
										text.getBinding(),
				                        def.getTitle(),
				                        def.isRequired(),
				                        text.getDisabledConditionName(),
				                        text.getPixelWidth(),
				                        text.getPixelHeight(),
				                        ! UserAgentType.phone.equals(userAgentType));
        eventSource = c;
        addComponent(def.getTitle(), def.isRequired(), text.getInvisibleConditionName(), c, text.getPixelWidth(), null);
	}

	@Override
	public void visitedTextArea(TextArea text,
	                                boolean parentVisible,
	                                boolean parentEnabled)
    throws MetaDataException {
		eventSource = null;
	}

	@Override
	public void visitTextField(TextField text,
								boolean parentVisible,
								boolean parentEnabled)
	throws MetaDataException {
		SmartClientDataGridFieldDefinition def = getFieldDef(text);
		Attribute attribute = def.getTarget().getAttribute();
		AttributeType type = (attribute == null) ? AttributeType.text : attribute.getAttributeType();
		TextFormat format = (attribute instanceof Text) ? ((Text) attribute).getFormat() : null;
		Converter<?> converter = null;
        if (attribute instanceof ConvertableField) {
            converter = ((ConvertableField) attribute).getConverter();
        }
        boolean useCalendar = false;
        if (AttributeType.date.equals(type)) {
        	useCalendar = true;
        	if (converter == null) {
                converter = customer.getDefaultDateConverter();
            }
        }
        if (AttributeType.dateTime.equals(type)) {
            useCalendar = ! UserAgentType.phone.equals(userAgentType);
            if (converter == null) {
                converter = customer.getDefaultDateTimeConverter();
            }
        }
        if (AttributeType.timestamp.equals(type)) {
            useCalendar = ! UserAgentType.phone.equals(userAgentType);
            if (converter == null) {
                converter = customer.getDefaultTimestampConverter();
            }
        }
        if (AttributeType.time.equals(type)) {
            if (converter == null) {
                converter = customer.getDefaultTimeConverter();
            }
        }
        UIComponentBase c = null;
        if (useCalendar) {
            c = b.calendar(listBinding,
            				text.getBinding(),
                            def.getTitle(),
                            def.isRequired(),
                            UserAgentType.phone.equals(userAgentType),
                            text.getDisabledConditionName(),
                            convertConverter(converter));
        }
        else if (format != null) {
            c = b.maskField(listBinding,
					text.getBinding(),
                    def.getTitle(),
                    def.isRequired(),
                    text.getDisabledConditionName(),
                    format,
                    convertConverter(converter),
                    text.getPixelWidth(),
                    ! UserAgentType.phone.equals(userAgentType));
        }
        else {
            c = b.textField(listBinding,
            					text.getBinding(),
                                def.getTitle(),
                                def.isRequired(),
                                text.getDisabledConditionName(),
                                convertConverter(converter),
                                text.getPixelWidth(),
                                ! UserAgentType.phone.equals(userAgentType));
        }
        eventSource = c;
		addComponent(def.getTitle(), def.isRequired(), text.getInvisibleConditionName(), c, text.getPixelWidth(), null);
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled)
	throws MetaDataException {
		// do nothing - this is for web 2 ux uis only
	}

	private static javax.faces.convert.Converter convertConverter(Converter<?> converter) {
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
	        else if ("Decimal5DollarsAndCents".equals(converterName)) {
	            result = new Decimal5DollarsAndCents();
	        }
	        else if ("Decimal2DollarsAndCents".equals(converterName)) {
	            result = new Decimal2DollarsAndCents();
	        }
	        else if ("Decimal5IntegerPercentage".equals(converterName)) {
	            result = new Decimal5IntegerPercentage();
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
	    
	    return result;
	}
	
	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled)
	throws MetaDataException {
		eventSource = null;
	}

	private void addToContainer(UIComponent component, Integer pixelWidth, Integer percentageWidth) {
		if (UserAgentType.phone.equals(userAgentType)) {
			current.getChildren().add(component);
		}
		else if (current instanceof PanelGrid) {
			if (currentContainers.isEmpty()) {
				throw new IllegalStateException("Trying to add to a container but there is nothing in the stack of currentContainers!!");
			}
			Container currentContainer = currentContainers.peek();
			if (currentContainer instanceof HBox) {
				// get the row or add a row if there is none
				Row r = null;
				if (current.getChildCount() == 0) {
					r = b.row();
					current.getChildren().add(r);
				}
				else {
					r = (Row) current.getChildren().get(0);
				}

				// add a column
				Column col = b.column(null, false, true, pixelWidth, percentageWidth, null, null);
				col.getChildren().add(component);
				r.getChildren().add(col);
			}
			else { // every other container is a vertical layout and already has the panel grid columns set to 1
				current.getChildren().add(component);
			}
		}
		else {
			throw new IllegalStateException("Trying to add to a container but the current faces component is not a panel grid!!! - " + current);
		}
		current = component;
	}
	
	private void completeAddToContainer() {
		current = current.getParent(); // account for the component

		// strip off the column and the row for HBox containers
		// All other components of containers are direct children
		if (! UserAgentType.phone.equals(userAgentType)) {
			if (currentContainers.isEmpty()) {
				throw new IllegalStateException("Trying to complete the add to a container but there is nothing in the stack of currentContainers!!");
			}
			Container currentContainer = currentContainers.peek();
			if (currentContainer instanceof HBox) {
				current = current.getParent().getParent();
			}
		}
	}
	
	private void addAjaxBehavior(String eventName, List<EventAction> actions) {
		String actionName = null;
		for (EventAction action : actions) {
			if (action instanceof ServerSideActionEventAction) {
				actionName = ((ServerSideActionEventAction) action).getActionName();
				break;
			}
		}
		
		eventSource.addClientBehavior(eventName, b.ajax(actionName, listBinding));
	}
	
	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		addAjaxBehavior("change", changeable.getChangedActions());
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
												boolean parentVisible, boolean parentEnabled)
	throws MetaDataException {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		addAjaxBehavior("focus", blurable.getFocusActions());
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		addAjaxBehavior("blur", blurable.getFocusActions());
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// nothing to do here
	}

	@Override
	public void visitOnEditedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
	}

	@Override
	public void visitedOnEditedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
	}

	@Override
	public void visitOnAddedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
	}

	@Override
	public void visitedOnAddedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// Cannot edit/zoom in on lookup descriptions in these faces views, so ignore the event
	}

	@Override
	public void visitOnPickedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		addAjaxBehavior("itemSelect", lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		addAjaxBehavior("itemUnselect", lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// nothing to do here
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server,
													boolean parentVisible,
													boolean parentEnabled)
	throws MetaDataException {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled)
	throws MetaDataException {
		// event actions are handled when visiting the action handlers
	}

	@Override
	public void visitAction(Action action) throws MetaDataException {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			CommandButton cb = b.actionButton(action.getDisplayName(),
												action.getToolTip(),
												action.getImplicitName(),
												action.getName(),
												listBinding,
												null,
												null,
												action.getClientValidation(),
												action.getDisabledConditionName(),
												action.getInvisibleConditionName());
			toolbar.getChildren().add(cb);
		}
	}

	private void processImplicitAction(Action action, ImplicitActionName name) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			CommandButton cb = b.actionButton(name.getDisplayName(),
												action.getToolTip(),
												name,
												null,
												listBinding,
												null,
												null,
												action.getClientValidation(),
												action.getDisabledConditionName(),
												action.getInvisibleConditionName());
			toolbar.getChildren().add(cb);
		}
	}
	
	@Override
	public void visitAddAction(Action action) throws MetaDataException {
//		processImplicitAction(action, ImplicitActionName.Add);
	}

	@Override
	public void visitRemoveAction(Action action) throws MetaDataException {
//		if (viewBinding != null) {
			processImplicitAction(action, ImplicitActionName.Remove);
//		}
	}

	@Override
	public void visitZoomOutAction(Action action) throws MetaDataException {
//		if (viewBinding != null) {
			processImplicitAction(action, ImplicitActionName.ZoomOut);
//		}
	}

	@Override
	public void visitNavigateAction(Action action) throws MetaDataException {
//		processImplicitAction(action, ImplicitActionName.Navigate);
	}

	@Override
	public void visitOKAction(Action action) throws MetaDataException {
//		if (viewBinding == null) {
			processImplicitAction(action, ImplicitActionName.OK);
//		}
	}

	@Override
	public void visitSaveAction(Action action) throws MetaDataException {
//		if (viewBinding == null) {
			processImplicitAction(action, ImplicitActionName.Save);
//		}
	}

	@Override
	public void visitCancelAction(Action action) throws MetaDataException {
//		if (viewBinding == null) {
			processImplicitAction(action, ImplicitActionName.Cancel);
//		}
	}

	@Override
	public void visitDeleteAction(Action action) throws MetaDataException {
//		if (viewBinding == null) {
			processImplicitAction(action, ImplicitActionName.Delete);
//		}
	}

	@Override
	public void visitReportAction(Action action) throws MetaDataException {
		processImplicitAction(action, ImplicitActionName.Report);
	}

	@Override
	public void visitBizExportAction(Action action) throws MetaDataException {
		processImplicitAction(action, ImplicitActionName.BizExport);
	}

	@Override
	public void visitBizImportAction(Action action) throws MetaDataException {
		processImplicitAction(action, ImplicitActionName.BizImport);
	}

	@Override
	public void visitUploadAction(Action action) throws MetaDataException {
		processImplicitAction(action, ImplicitActionName.Upload);
	}

	@Override
	public void visitNewAction(Action action) throws MetaDataException {
//		processImplicitAction(action, ImplicitActionName.New);
	}

	@Override
	public void visitEditAction(Action action) throws MetaDataException {
//		processImplicitAction(action, ImplicitActionName.Edit);
	}

	@Override
	public void visitParameter(
			org.skyve.metadata.view.widget.bound.Parameter parameter,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}
	
	private SmartClientDataGridFieldDefinition getFieldDef(InputWidget inputWidget)
	throws MetaDataException {
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
		
		return result;
	}
}
