package org.skyve.impl.sail.mock;

import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

import javax.el.ExpressionFactory;
import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.NavigationHandler;
import javax.faces.application.StateManager;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.component.UIOutput;
import javax.faces.component.UIParameter;
import javax.faces.component.UISelectItems;
import javax.faces.component.behavior.Behavior;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputHidden;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGrid;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.MethodBinding;
import javax.faces.el.PropertyResolver;
import javax.faces.el.ReferenceSyntaxException;
import javax.faces.el.ValueBinding;
import javax.faces.el.VariableResolver;
import javax.faces.event.ActionListener;
import javax.faces.validator.Validator;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.barchart.BarChart;
import org.primefaces.component.breadcrumb.BreadCrumb;
import org.primefaces.component.button.Button;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.datepicker.DatePicker;
import org.primefaces.component.defaultcommand.DefaultCommand;
import org.primefaces.component.dialog.Dialog;
import org.primefaces.component.donutchart.DonutChart;
import org.primefaces.component.texteditor.TextEditor;
import org.primefaces.component.fieldset.Fieldset;
import org.primefaces.component.fileupload.FileUpload;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.linechart.LineChart;
import org.primefaces.component.menuitem.UIMenuItem;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.outputpanel.OutputPanel;
import org.primefaces.component.overlaypanel.OverlayPanel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.password.Password;
import org.primefaces.component.picklist.PickList;
import org.primefaces.component.piechart.PieChart;
import org.primefaces.component.polarareachart.PolarAreaChart;
import org.primefaces.component.progressbar.ProgressBar;
import org.primefaces.component.radarchart.RadarChart;
import org.primefaces.component.remotecommand.RemoteCommand;
import org.primefaces.component.row.Row;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectmanycheckbox.SelectManyCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.signature.Signature;
import org.primefaces.component.spacer.Spacer;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.steps.Steps;
import org.primefaces.component.sticky.Sticky;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;
import org.skyve.impl.web.faces.components.Conversation;
import org.skyve.impl.web.faces.components.ListGrid;
import org.skyve.impl.web.faces.components.Map;
import org.skyve.impl.web.faces.components.SetUxUi;
import org.skyve.impl.web.faces.components.View;

@SuppressWarnings("deprecation") // This super class has a whole bunch of deprecated shit in it
public class MockApplication extends Application {
	private ExpressionFactory ef = new MockExpressionFactory();
	
	@Override
	public ExpressionFactory getExpressionFactory() {
    	return ef;
    }
    
	@Override
	public UIComponent createComponent(String componentType) throws FacesException {
		if (AutoComplete.COMPONENT_TYPE.equals(componentType)) {
			return new AutoComplete();
		}
		else if (AccordionPanel.COMPONENT_TYPE.equals(componentType)) {
			return new AccordionPanel();
		}
		else if (BarChart.COMPONENT_TYPE.equals(componentType)) {
			return new BarChart();
		}
		else if (BreadCrumb.COMPONENT_TYPE.equals(componentType)) {
			return new BreadCrumb();
		}
		else if (Button.COMPONENT_TYPE.equals(componentType)) {
			return new Button();
		}
		else if (ColorPicker.COMPONENT_TYPE.equals(componentType)) {
			return new ColorPicker();
		}
		else if (Column.COMPONENT_TYPE.equals(componentType)) {
			return new Column();
		}
		else if (CommandButton.COMPONENT_TYPE.equals(componentType)) {
			return new CommandButton();
		}
		else if (CommandLink.COMPONENT_TYPE.equals(componentType)) {
			return new CommandLink();
		}
		else if (Conversation.COMPONENT_TYPE.equals(componentType)) {
			return new Conversation();
		}
		else if (DataList.COMPONENT_TYPE.equals(componentType)) {
			return new DataList();
		}
		else if (DataTable.COMPONENT_TYPE.equals(componentType)) {
			return new DataTable() {
				@Override
				protected FacesContext getFacesContext() {
			    	// work around FacesContext.getCurrentInstance().
					return new MockFacesContext();
				}
			};
		}
		else if (DatePicker.COMPONENT_TYPE.equals(componentType)) {
			return new DatePicker();
		}
		else if (DefaultCommand.COMPONENT_TYPE.equals(componentType)) {
			return new DefaultCommand();
		}
		else if (Dialog.COMPONENT_TYPE.equals(componentType)) {
			return new Dialog();
		}
		else if (DonutChart.COMPONENT_TYPE.equals(componentType)) {
			return new DonutChart();
		}
		else if (TextEditor.COMPONENT_TYPE.equals(componentType)) {
			return new TextEditor();
		}
		else if (Fieldset.COMPONENT_TYPE.equals(componentType)) {
			return new Fieldset();
		}
		else if (FileUpload.COMPONENT_TYPE.equals(componentType)) {
			return new FileUpload();
		}
		else if (GraphicImage.COMPONENT_TYPE.equals(componentType)) {
			return new GraphicImage();
		}
		else if (HtmlForm.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlForm();
		}
		else if (HtmlInputHidden.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlInputHidden();
		}
		else if (HtmlInputText.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlInputText();
		}
		else if (HtmlOutputLink.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlOutputLink();
		}
		else if (HtmlOutputText.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlOutputText();
		}
		else if (HtmlPanelGrid.COMPONENT_TYPE.equals(componentType)) {
			return new HtmlPanelGrid() {
			    @Override
				public String getClientId() {
			    	// work around FacesContext.getCurrentInstance().
			    	return getId();
			    }
			};
		}
		else if (HtmlPanelGroup.COMPONENT_TYPE.equals(componentType)) {
			// Override getClientId() as Map components need DIV client IDs to attach Javascript to.
			return new HtmlPanelGroup() {
				@Override
				public String getClientId() {
					return "MOCKED:" + getId();
				}
			};
		}
		else if (InputMask.COMPONENT_TYPE.equals(componentType)) {
			return new InputMask();
		}
		else if (InputText.COMPONENT_TYPE.equals(componentType)) {
			return new InputText();
		}
		else if (InputTextarea.COMPONENT_TYPE.equals(componentType)) {
			return new InputTextarea();
		}
		else if (LineChart.COMPONENT_TYPE.equals(componentType)) {
			return new LineChart();
		}
		else if (ListGrid.COMPONENT_TYPE.equals(componentType)) {
			return new ListGrid();
		}
		else if (Map.COMPONENT_TYPE.equals(componentType)) {
			return new Map();
		}
		else if (Message.COMPONENT_TYPE.equals(componentType)) {
			return new Message();
		}
		else if (OutputLabel.COMPONENT_TYPE.equals(componentType)) {
			return new OutputLabel();
		}
		else if (OutputPanel.COMPONENT_TYPE.equals(componentType)) {
			return new OutputPanel();
		}
		else if (OverlayPanel.COMPONENT_TYPE.equals(componentType)) {
			return new OverlayPanel();
		}
		else if (Panel.COMPONENT_TYPE.equals(componentType)) {
			return new Panel();
		}
		else if (PanelGrid.COMPONENT_TYPE.equals(componentType)) {
			return new PanelGrid();
		}
		else if (Password.COMPONENT_TYPE.equals(componentType)) {
			return new Password();
		}
		else if (PickList.COMPONENT_TYPE.equals(componentType)) {
			return new PickList();
		}
		else if (PieChart.COMPONENT_TYPE.equals(componentType)) {
			return new PieChart();
		}
		else if (PolarAreaChart.COMPONENT_TYPE.equals(componentType)) {
			return new PolarAreaChart();
		}
		else if (ProgressBar.COMPONENT_TYPE.equals(componentType)) {
			return new ProgressBar();
		}
		else if (RadarChart.COMPONENT_TYPE.equals(componentType)) {
			return new RadarChart();
		}
		else if (RemoteCommand.COMPONENT_TYPE.equals(componentType)) {
			return new RemoteCommand();
		}
		else if (Row.COMPONENT_TYPE.equals(componentType)) {
			return new Row();
		}
		else if (SelectOneMenu.COMPONENT_TYPE.equals(componentType)) {
			return new SelectOneMenu();
		}
		else if (SelectOneRadio.COMPONENT_TYPE.equals(componentType)) {
			return new SelectOneRadio();
		}
		else if (SelectBooleanCheckbox.COMPONENT_TYPE.equals(componentType)) {
			return new SelectBooleanCheckbox();
		}
		else if (SelectManyCheckbox.COMPONENT_TYPE.equals(componentType)) {
			return new SelectManyCheckbox();
		}
		else if (SetUxUi.COMPONENT_TYPE.equals(componentType)) {
			return new SetUxUi();
		}
		else if (Spacer.COMPONENT_TYPE.equals(componentType)) {
			return new Spacer();
		}
		else if (Spinner.COMPONENT_TYPE.equals(componentType)) {
			return new Spinner();
		}
		else if (Steps.COMPONENT_TYPE.equals(componentType)) {
			return new Steps();
		}
		else if (Sticky.COMPONENT_TYPE.equals(componentType)) {
			return new Sticky();
		}
		else if (Tab.COMPONENT_TYPE.equals(componentType)) {
			return new Tab();
		}
		else if (TabView.COMPONENT_TYPE.equals(componentType)) {
			return new TabView();
		}
		else if (TextEditor.COMPONENT_TYPE.equals(componentType)) {
			return new TextEditor();
		}
		else if (Toolbar.COMPONENT_TYPE.equals(componentType)) {
			return new Toolbar();
		}
		else if (TriStateCheckbox.COMPONENT_TYPE.equals(componentType)) {
			return new TriStateCheckbox();
		}
		else if (UIForm.COMPONENT_TYPE.equals(componentType)) {
			return new UIForm();
		}
		else if (UIMenuItem.COMPONENT_TYPE.equals(componentType)) {
			return new UIMenuItem();
		}
		else if (UIOutput.COMPONENT_TYPE.equals(componentType)) {
			return new UIOutput();
		}
		else if (UIParameter.COMPONENT_TYPE.equals(componentType)) {
			return new UIParameter();
		}
		else if (UISelectItems.COMPONENT_TYPE.equals(componentType)) {
			return new UISelectItems();
		}
		else if (View.COMPONENT_TYPE.equals(componentType)) {
			return new View();
		}
		else if (Signature.COMPONENT_TYPE.equals(componentType)) {
			return new Signature();
		}
		throw new FacesException("MockApplication.createComponent() does not cater for componentType " + componentType);
	}

	@Override
	public Behavior createBehavior(String behaviorId) throws FacesException {
		if (AjaxBehavior.BEHAVIOR_ID.equals(behaviorId)) {
			return new AjaxBehavior();
		}
		else if (ConfirmBehavior.BEHAVIOR_ID.equals(behaviorId)) {
			return new ConfirmBehavior();
		}
		throw new FacesException("MockApplication.createBehavior() does not cater for behaviorId " + behaviorId);
	}
	
	@Override
	public ActionListener getActionListener() {
		return null;
	}

	@Override
	public void setActionListener(ActionListener listener) {
		// nothing to see here
	}

	@Override
	public Locale getDefaultLocale() {
		return null;
	}

	@Override
	public void setDefaultLocale(Locale locale) {
		// nothing to see here
	}

	@Override
	public String getDefaultRenderKitId() {
		return null;
	}

	@Override
	public void setDefaultRenderKitId(String renderKitId) {
		// nothing to see here
	}

	@Override
	public String getMessageBundle() {
		return null;
	}

	@Override
	public void setMessageBundle(String bundle) {
		// nothing to see here
	}

	@Override
	public NavigationHandler getNavigationHandler() {
		return null;
	}

	@Override
	public void setNavigationHandler(NavigationHandler handler) {
		// nothing to see here
	}

	@Override
	public PropertyResolver getPropertyResolver() {
		return null;
	}

	@Override
	public void setPropertyResolver(PropertyResolver resolver) {
		// nothing to see here
	}

	@Override
	public VariableResolver getVariableResolver() {
		return null;
	}

	@Override
	public void setVariableResolver(VariableResolver resolver) {
		// nothing to see here
	}

	@Override
	public ViewHandler getViewHandler() {
		return null;
	}

	@Override
	public void setViewHandler(ViewHandler handler) {
		// nothing to see here
	}

	@Override
	public StateManager getStateManager() {
		return null;
	}

	@Override
	public void setStateManager(StateManager manager) {
		// nothing to see here
	}

	@Override
	public void addComponent(String componentType, String componentClass) {
		// nothing to see here
	}

	@Override
	public UIComponent createComponent(ValueBinding componentBinding, FacesContext context, String componentType)
	throws FacesException {
		return null;
	}

	@Override
	public Iterator<String> getComponentTypes() {
		return null;
	}

	@Override
	public void addConverter(String converterId, String converterClass) {
		// nothing to see here
	}

	@Override
	public void addConverter(Class<?> targetClass, String converterClass) {
		// nothing to see here
	}

	@Override
	public Converter createConverter(String converterId) {
		return null;
	}

	@Override
	public Converter createConverter(Class<?> targetClass) {
		return null;
	}

	@Override
	public Iterator<String> getConverterIds() {
		return null;
	}

	@Override
	public Iterator<Class<?>> getConverterTypes() {
		return null;
	}

	@Override
	public MethodBinding createMethodBinding(String ref, Class<?>[] params) throws ReferenceSyntaxException {
		return null;
	}

	@Override
	public Iterator<Locale> getSupportedLocales() {
		return null;
	}

	@Override
	public void setSupportedLocales(Collection<Locale> locales) {
		// nothing to see here
	}

	@Override
	public void addValidator(String validatorId, String validatorClass) {
		// nothing to see here
	}

	@Override
	public Validator createValidator(String validatorId) throws FacesException {
		return null;
	}

	@Override
	public Iterator<String> getValidatorIds() {
		return null;
	}

	@Override
	public ValueBinding createValueBinding(String ref) throws ReferenceSyntaxException {
		return null;
	}
}
