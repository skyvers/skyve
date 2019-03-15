package org.skyve.impl.web.faces.pipeline;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIOutput;

import org.primefaces.component.calendar.Calendar;
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
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.impl.web.faces.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.impl.web.faces.converters.integer.IntegerSeparator;
import org.skyve.impl.web.faces.converters.integer.LongIntegerSeparator;
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
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

public class FacesViewRenderer extends ViewRenderer {
	private ComponentBuilder cb;
	private LayoutBuilder lb;
	private boolean createView;
	private String widgetId;
	private UIComponent fragment; // if we have a widgetId to render, this holds a reference to that component

	private UIComponent current; // current component being constructed
	private UIComponent facesView; // the result of construction
	private List<UIComponent> toolbarLayouts; // the toolbar layouts

	public FacesViewRenderer(User user,
								Module module,
								Document document,
								View view,
								String widgetId,
								ComponentBuilder cb,
								LayoutBuilder lb) {
		super(user, module, document, view);
		String viewName = view.getName();
		createView = ViewType.create.toString().equals(viewName);
		this.widgetId = widgetId;
		this.cb = cb;
		this.lb = lb;
	}
	
	public UIComponent getFacesView() {
		return facesView;
	}

	@Override
	public void renderView(String title, String icon16x16Url, String icon32x32Url) {
	    // Ensure visibility is set for both create and edit views
        current = cb.view(null, createView ? "created" : "notCreated");
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
	public void renderedView(String title, String icon16x16Url, String icon32x32Url) {
        // Add the toolbar(s) if this is a full view render or
        // a view with a widgetId = actions widgetId
        if ((widgetId == null) || widgetId.equals(view.getActionsWidgetId()))  {
			// Add the toolbar(s) if it/they has/have contents
        	if ((toolbarLayouts != null) && (! toolbarLayouts.isEmpty()) && (! toolbarLayouts.get(0).getChildren().isEmpty())) {
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
	}

	private StringBuilder stickyTabScript = new StringBuilder(128);
	
	@Override
	public void renderTabPane(TabPane tabPane) {
		UIComponent component = cb.tabPane(null, tabPane, module.getName(), document.getName(), stickyTabScript);
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
	public void renderedTabPane(TabPane tabPane) {
		addedToContainer();

        // remember tab unless the tab selection is being controlled by the view.
		UIOutput script = null;
		if ((stickyTabScript.length() > 0) && (tabPane.getSelectedTabIndexBinding() == null)) {
			script = new UIOutput();
			script.setValue(String.format("<script type=\"text/javascript\">%s</script>", stickyTabScript));
        }
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
			if (script != null) {
				facesView.getChildren().add(script);
			}
		}
		else {
			if (script != null) {
				current.getChildren().add(script);
			}
		}
		stickyTabScript.setLength(0);
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
		// Cater for a border if this thing has a border
		UIComponent border = null;
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			border = cb.border(null, borderTitle, vbox.getInvisibleConditionName(), vbox.getPixelWidth());
			addToContainer(border, 
							vbox.getPixelWidth(), 
							vbox.getResponsiveWidth(),
							vbox.getPercentageWidth(),
							vbox.getInvisibleConditionName());
		}

		UIComponent layout = lb.vboxLayout(null, vbox);

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
							vbox.getPercentageWidth(),
							vbox.getInvisibleConditionName());

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;
	}

	@Override
	public void renderedVBox(String borderTitle, VBox vbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(vbox.getBorder())) {
			current = lb.addedBorderLayout(null, current);
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
	public void renderHBox(String borderTitle, HBox hbox) {
		// Cater for a border if this thing has a border
		UIComponent border = null;
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			border = cb.border(null, borderTitle, hbox.getInvisibleConditionName(), hbox.getPixelWidth());
			addToContainer(border, 
							hbox.getPixelWidth(), 
							hbox.getResponsiveWidth(),
							hbox.getPercentageWidth(),
							hbox.getInvisibleConditionName());
		}

		UIComponent layout = lb.hboxLayout(null, hbox);

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
							hbox.getPercentageWidth(),
							hbox.getInvisibleConditionName());
			
			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;
	}

	@Override
	public void renderedHBox(String title, HBox hbox) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(hbox.getBorder())) {
			current = lb.addedBorderLayout(null, current);
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

	@Override
	public void renderForm(String borderTitle, Form form) {
		// Cater for a border if this thing has a border
		UIComponent border = null;
		if (Boolean.TRUE.equals(form.getBorder())) {
			border = cb.border(null, borderTitle, form.getInvisibleConditionName(), form.getPixelWidth());
			addToContainer(border, 
							form.getPixelWidth(), 
							form.getResponsiveWidth(),
							form.getPercentageWidth(),
							form.getInvisibleConditionName());
		}

		UIComponent layout = lb.formLayout(null, form);

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
							form.getPercentageWidth(),
							form.getInvisibleConditionName());

			// start rendering if appropriate
			if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
				fragment = layout;
			}
		}
		current = layout;
// TODO form.getDisabledConditionName() form.getLabelDefaultHorizontalAlignment()
	}

	@Override
	public void renderedForm(String borderTitle, Form form) {
		// Cater for border, if one was added
		if (Boolean.TRUE.equals(form.getBorder())) {
			current = lb.addedBorderLayout(null, current);
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
	public void renderFormItem(String label,
								boolean required,
								String help,
								boolean showLabel,
								FormItem item) {
		// nothing to do here
	}

	@Override
	public void renderedFormItem(String label,
									boolean required,
									String help,
									boolean showLabel,
									FormItem item) {
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
								boolean widgetRequired,
								String widgetInvisible,
								String helpText,
								UIComponent component,
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
				else {  // This must be a container (vbox, hbox etc)
					addToContainer(component, pixelWidth, responsiveWidth, percentageWidth, widgetInvisible);
					addedToContainer();
				}
			}
			else { // a form item
				FormItem formItem = getCurrentFormItem();
				FormColumn formColumn = getCurrentFormColumn();
				if (isCurrentWidgetShowLabel()) {
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
		Form currentForm = getCurrentForm();
		renderButton(action,
						label,
						iconStyleClass,
						toolTip,
						confirmationText,
						button,
						(currentForm == null) ? null : currentForm.getDisabledConditionName());
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
		renderButton(action, label, iconStyleClass, toolTip, confirmationText, button, null);
	}
	
	private void renderButton(Action action,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
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
									label,
									iconStyleClass,
									toolTip,
									confirmationText,
									button,
									formDisabledConditionName,
									action,
									module.getName(),
									document.getName());
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
	    				false, 
	    				action.getInvisibleConditionName(), 
	    				null,
	    				c, 
	    				button.getPixelWidth(), 
	    				null, 
	    				null);
	}

	@Override
	public void renderFormGeoLocator(GeoLocator locator) {
		renderGeoLocator(locator);
	}

	@Override
	public void renderGeoLocator(GeoLocator locator) {
	    UIComponent l = cb.label(null, "geoLocator"); // TODO geolocator
	    addComponent(null, 
	    				false, 
	    				locator.getInvisibleConditionName(), 
	    				null,
	    				l, 
	    				null, 
	    				null, 
	    				null);
	}

	@Override
	public void renderMap(MapDisplay map) {
	    UIComponent l = cb.label(null, "map"); // TODO map
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
	public void renderBoundColumnGeometry(Geometry geometry) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void renderFormGeometry(Geometry geometry) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void renderFormDialogButton(String label, DialogButton button) {
		renderDialogButton(label, button);
	}

	@Override
	public void renderDialogButton(String label, DialogButton button) {
	    UIComponent bn = cb.label(null, "dialogButton"); // TODO dialog button
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
	public void renderFormSpacer(Spacer spacer) {
		renderSpacer(spacer);
	}

	@Override
	public void renderSpacer(Spacer spacer) {
		UIComponent component = cb.spacer(null, spacer);
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
	public void renderStaticImage(String fileUrl, StaticImage image) {
		UIComponent i = cb.staticImage(null, fileUrl, image);
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
						false, 
						image.getInvisibleConditionName(), 
						null,
						i, 
						image.getPixelWidth(), 
						image.getResponsiveWidth(),
						image.getPercentageWidth());
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
		final AtomicReference<UIComponent> c = new AtomicReference<>();
		new ReferenceProcessor() {
			@Override
			@SuppressWarnings("synthetic-access")
			public void processResourceReference(ResourceReference reference) {
				c.set(cb.label(null, "resource link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processReportReference(ReportReference reference) {
				c.set(cb.label(null, "report link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processQueryListViewReference(QueryListViewReference reference) {
				c.set(cb.label(null, "list view link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processImplicitActionReference(ImplicitActionReference reference) {
				c.set(cb.label(null, "implicit action link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processExternalReference(ExternalReference reference) {
				c.set(cb.label(null, "external link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processEditViewReference(EditViewReference reference) {
				StringBuilder href = new StringBuilder(128);
				href.append("./?a=").append(WebAction.e.toString()).append("&m=").append(reference.getModuleName());
				href.append("&d=").append(reference.getDocumentName()).append("&i={").append(reference.getBinding()).append('}');

				c.set(cb.outputLink(dataWidgetVar, value, href.toString(), link.getInvisibleConditionName(), target));
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processDefaultListViewReference(DefaultListViewReference reference) {
				c.set(cb.label(null, "default list view link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processContentReference(ContentReference reference) {
				c.set(cb.label(null, "content link")); // TODO link
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void processActionReference(ActionReference reference) {
				final TargetMetaData listTarget = BindUtil.getMetaDataForBinding(customer, module, document, dataWidgetBinding);

				final Document listDocument;
				// Figure out the document type of the relation.
				if (listTarget.getAttribute() instanceof Relation) {
					final String documentName = ((Relation) listTarget.getAttribute()).getDocumentName();
					listDocument = module.getDocument(customer, documentName);
				}
				else {
					listDocument = listTarget.getDocument();
				}

				final ViewType[] viewTypesToSearch = new ViewType[] { ViewType.edit, ViewType.create };
				Action action = null;
				for (ViewType viewType : viewTypesToSearch) {
					final View listDocumentView = listDocument.getView(cb.userAgentType.name(), customer, viewType.name());
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
					c.set(cb.actionLink(null, dataWidgetBinding, dataWidgetVar, value, link, action));
				}
				else {
					c.set(cb.actionLink(null, dataWidgetBinding, dataWidgetVar, value, link, reference.getActionName()));
				}
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
		UIComponent c = cb.blurb(null, dataWidgetVar, value, binding, blurb);
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
	public void renderFormLabel(String value, Label label) {
		renderLabel(value, label);
	}

	@Override
	public void renderContainerColumnLabel(String value, Label label) {
		renderLabel(value, label);
	}

	@Override
	public void renderLabel(String value, Label label) {
		String ultimateValue = label.getValue();
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
	    UIComponent c = cb.label(null, dataWidgetVar, ultimateValue, binding, label);
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
	    UIComponent p = cb.label(null, "progressBar"); // TODO progress bar
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
		UIComponent l = cb.listGrid(null,
										module.getName(),
										getCurrentListWidgetModelDocumentName(),
										getCurrentListWidgetModelName(),
										getCurrentListWidgetModel(),
										title,
										grid,
										user.canCreateDocument(getCurrentListWidgetDrivingDocument()),
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
		UIComponent r = cb.listRepeater(null,
											getCurrentListWidgetModelDocumentName(), 
											getCurrentListWidgetModelName(), 
											getCurrentListWidgetModel(), 
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
		UIComponent l = cb.label(null, "treeGrid");
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
		UIComponent g = cb.dataGrid(null, dataWidgetVar, ordered, title, grid);
        addToContainer(g, grid.getPixelWidth(), grid.getResponsiveWidth(), grid.getPercentageWidth(), grid.getInvisibleConditionName());
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			fragment = g;
		}
	}

	@Override
	public void renderedDataGrid(String title, DataGrid grid) {
		renderedDataWidget(grid);
	}

	@Override
	public void renderDataRepeater(String title, DataRepeater repeater) {
		// Create the data repeater faces component
		dataWidgetBinding = repeater.getBinding();
		dataWidgetVar = BindUtil.sanitiseBinding(dataWidgetBinding) + "Row";
		UIComponent r = cb.dataRepeater(null, dataWidgetVar, title, repeater);
        addToContainer(r, repeater.getPixelWidth(), repeater.getResponsiveWidth(), repeater.getPercentageWidth(), repeater.getInvisibleConditionName());
		gridColumnExpression = new StringBuilder(512);

		// start rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(repeater.getWidgetId()))) {
			fragment = r;
		}
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
				alias = module.getDocument(customer, targetRelation.getDocumentName()).getSingularAlias();
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
													Boolean.TRUE.equals(grid.getInline()));
		}
	    dataWidgetBinding = null;
	    dataWidgetVar = null;
	    gridColumnExpression = null;
	    addedToContainer();
		
		// stop rendering if appropriate
		if ((widgetId != null) && (widgetId.equals(widget.getWidgetId()))) {
			current.getChildren().remove(fragment);
			fragment.setParent(null);
			facesView.getChildren().add(fragment);
			fragment = null;
		}
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
		current = cb.addDataGridBoundColumn(null,
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
		current = cb.addedDataGridBoundColumn(null, current);
	}

	@Override
	public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderDataGridContainerColumn(title, column);
	}

	@Override
	public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
        current = cb.addDataGridContainerColumn(null, current, getCurrentDataWidget(), title, column);
	}

	@Override
	public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
		renderedDataGridContainerColumn(title, column);
	}

	@Override
	public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
		current = cb.addedDataGridContainerColumn(null, current);
	}

	// A reference to the current widget that is the source of events
	private UIComponentBase eventSource = null;
	
	@Override
	public void renderBoundColumnCheckBox(CheckBox checkBox) {
		renderFormCheckBox(checkBox);
	}

	@Override
	public void renderFormCheckBox(CheckBox checkBox) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		UIComponentBase c = (UIComponentBase) cb.checkBox(null,
															dataWidgetVar,
															checkBox,
															(currentForm == null) ? null : currentForm.getDisabledConditionName(),
															title, required);
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
        UIComponentBase c = (UIComponentBase) cb.label(null, "checkMembership"); // TODO check membership
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
		Form currentForm = getCurrentForm();
		UIComponentBase c = (UIComponentBase) cb.colourPicker(null,
																dataWidgetVar,
																colour,
																(currentForm == null) ? null : currentForm.getDisabledConditionName(),
																title, required);
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
		Form currentForm = getCurrentForm();
		UIComponentBase s = (UIComponentBase) cb.combo(null,
														dataWidgetVar,
														combo,
														(currentForm == null) ? null : currentForm.getDisabledConditionName(),
														title, required);
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
		Form currentForm = getCurrentForm();
		UIComponent c = cb.contentImage(null,
											dataWidgetVar,
											image,
											(currentForm == null) ? null : currentForm.getDisabledConditionName(),
											title,
											required);
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
	public void renderBoundColumnContentLink(String value, ContentLink link) {
		renderFormContentLink(value, link);
	}

	@Override
	public void renderFormContentLink(String value, ContentLink link) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
		UIComponent c = cb.contentLink(null,
										dataWidgetVar,
										link,
										(currentForm == null) ? null : currentForm.getDisabledConditionName(),
										title,
										required);
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
		Form currentForm = getCurrentForm();
		UIComponent c = cb.html(null,
									dataWidgetVar,
									html,
									(currentForm == null) ? null : currentForm.getDisabledConditionName(),
									title,
									required);
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
		UIComponentBase c = (UIComponentBase) cb.listMembership(null, membership);
		eventSource = c;
		addToContainer(c, membership.getListWidthInPixels(), null, null, membership.getInvisibleConditionName());
	}

	@Override
	public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
		addedToContainer();
		eventSource = null;
	}

	@Override
	public void renderComparison(Comparison comparison) {
        UIComponent c = cb.label(null, "comparison"); // TODO comparison
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
		Form currentForm = getCurrentForm();
		UIComponentBase c = (UIComponentBase) cb.lookupDescription(null,
																	dataWidgetVar, 
																	lookup, 
																	title, 
																	(currentForm == null) ? null : currentForm.getDisabledConditionName(),
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
	public void renderFormLookup(MetaDataQueryDefinition query, boolean canCreate, boolean canUpdate, Lookup lookup) {
		UIComponent c = cb.label(null, "lookup"); // TODO lookup
		addComponent(null, 
						false, 
						lookup.getInvisibleConditionName(), 
						null,
						c, 
						null, 
						null, 
						null);
	}

	@Override
	public void renderedFormLookup(MetaDataQueryDefinition query, boolean canCreate, boolean canUpdate, Lookup lookup) {
		// do nothing
	}

	@Override
	public void renderBoundColumnPassword(Password password) {
		renderFormPassword(password);
	}

	@Override
	public void renderFormPassword(Password password) {
		String title = getCurrentWidgetLabel();
		boolean required = isCurrentWidgetRequired();
		Form currentForm = getCurrentForm();
        UIComponentBase c = (UIComponentBase) cb.password(null,
        													dataWidgetVar,
        													password,
        													(currentForm == null) ? null : currentForm.getDisabledConditionName(),
        													title,
        													required);
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
		Form currentForm = getCurrentForm();
        UIComponentBase c = (UIComponentBase) cb.radio(null,
        												dataWidgetVar,
        												radio,
        												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
        												title,
        												required);
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
		Form currentForm = getCurrentForm();
        UIComponentBase c = (UIComponentBase) cb.richText(null,
        													dataWidgetVar,
        													text,
        													(currentForm == null) ? null : currentForm.getDisabledConditionName(),
        													title,
        													required);
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
		UIComponentBase c = (UIComponentBase) cb.label(null, "slider"); // TODO slider
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
		Form currentForm = getCurrentForm();
		UIComponentBase c = (UIComponentBase) cb.spinner(null,
        													dataWidgetVar,
        													spinner,
        													(currentForm == null) ? null : currentForm.getDisabledConditionName(),
        													title,
        													required);
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
		Form currentForm = getCurrentForm();
		UIComponentBase c = (UIComponentBase) cb.textArea(null,
															dataWidgetVar,
															text,
															(currentForm == null) ? null : currentForm.getDisabledConditionName(),
															title,
															required,
															length);
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
		Form currentForm = getCurrentForm();
        UIComponentBase c = (UIComponentBase) cb.text(null,
        												dataWidgetVar, 
        												text, 
        												(currentForm == null) ? null : currentForm.getDisabledConditionName(),
        												title, 
        												required,
        												length,
        												converter,
        												format,
        												convertConverter(converter, type));
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
	public void renderedBoundColumnTextField(TextField text) {
		renderedFormTextField(text);
	}

	@Override
	public void renderedFormTextField(TextField text) {
		eventSource = null;
	}

	private static javax.faces.convert.Converter convertConverter(Converter<?> converter, AttributeType type) {
	    javax.faces.convert.Converter result = null;
	    if (converter != null) {
		    String converterName = converter.getClass().getSimpleName();
		    if ("DD_MM_YYYY".equals(converterName)) {
		    	if (AttributeType.date.equals(type)) {
		    		result = new DD_MM_YYYY();
		    	}
		    	else if (AttributeType.dateTime.equals(type)) {
		    		result = new org.skyve.impl.web.faces.converters.datetime.DD_MM_YYYY();
		    	}
		    	else if (AttributeType.timestamp.equals(type)) {
		    		result = new org.skyve.impl.web.faces.converters.timestamp.DD_MM_YYYY();
		    	}
		    }
		    else if ("DD_MMM_YYYY".equals(converterName)) {
		    	if (AttributeType.date.equals(type)) {
		    		result = new DD_MMM_YYYY();
		    	}
		    	else if (AttributeType.dateTime.equals(type)) {
		    		result = new org.skyve.impl.web.faces.converters.datetime.DD_MMM_YYYY();
		    	}
		    	else if (AttributeType.timestamp.equals(type)) {
		    		result = new org.skyve.impl.web.faces.converters.timestamp.DD_MMM_YYYY();
		    	}
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
			else if ("Decimal2DollarsAndCentsAbsolute".equals(converterName)) {
				result = new Decimal2DollarsAndCentsAbsolute();
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
			else if ("LongIntegerSeparator".equals(converterName)) {
				result = new LongIntegerSeparator();
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
	
	@Override
	public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
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
				for (UIComponent toolbarLayout : toolbarLayouts) {
					toolbarLayout.getChildren().add(cb.action(null,
																dataWidgetBinding,
																dataWidgetVar,
																action,
																null,
																action.getDisplayName()));
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

	private void processImplicitAction(ActionImpl action, ImplicitActionName name) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			if (toolbarLayouts != null) {
				for (UIComponent toolbarLayout : toolbarLayouts) {
					if (ImplicitActionName.Report.equals(name)) {
						toolbarLayout.getChildren().add(cb.report(null, action));
					}
					else if (ImplicitActionName.Download.equals(name)) {
						toolbarLayout.getChildren().add(cb.download(null,
																		action,
																		module.getName(),
																		document.getName()));
					}
					else if (ImplicitActionName.Upload.equals(name)) {
						toolbarLayout.getChildren().add(cb.upload(null, action));
					}
					else {
						String displayName = action.getDisplayName();
						if (displayName == null) {
							displayName = name.getDisplayName();
						}
						toolbarLayout.getChildren().add(cb.action(null,
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
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		String binding = changeable.getBinding();
		List<EventAction> changedActions = changeable.getChangedActions();
		cb.addAjaxBehavior(eventSource, "change", dataWidgetBinding, dataWidgetVar, binding, changedActions);
		// Add this special event for date selection on calendar as "changed" doesn't fire on select
		if (eventSource instanceof Calendar) {
			cb.addAjaxBehavior(eventSource, "dateSelect", dataWidgetBinding, dataWidgetVar, binding, changedActions);
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
	public void visitOnPickedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		cb.addAjaxBehavior(eventSource, "itemSelect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		cb.addAjaxBehavior(eventSource, "itemUnselect", dataWidgetBinding, dataWidgetVar, lookup.getBinding(), lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
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
}
