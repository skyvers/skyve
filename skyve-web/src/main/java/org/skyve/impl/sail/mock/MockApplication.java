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
import javax.faces.component.UIOutput;
import javax.faces.component.UISelectItems;
import javax.faces.component.behavior.Behavior;
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
import org.primefaces.component.button.Button;
import org.primefaces.component.column.Column;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.selectonemenu.SelectOneMenu;

@SuppressWarnings("deprecation") // This super class has a whole bunch of deprecated shit in it
public class MockApplication extends Application {
	private ExpressionFactory ef = new MockExpressionFactory();
	
	@Override
	public ExpressionFactory getExpressionFactory() {
    	return ef;
    }
    
	@Override
	public UIComponent createComponent(String componentType) throws FacesException {
		if (Button.COMPONENT_TYPE.equals(componentType)) {
			return new Button();
		}
		else if (Column.COMPONENT_TYPE.equals(componentType)) {
			return new Column();
		}
		else if (DataTable.COMPONENT_TYPE.equals(componentType)) {
			return new DataTable();
		}
		else if (SelectOneMenu.COMPONENT_TYPE.equals(componentType)) {
			return new SelectOneMenu();
		}
		else if (UIOutput.COMPONENT_TYPE.equals(componentType)) {
			return new UIOutput();
		}
		else if (UISelectItems.COMPONENT_TYPE.equals(componentType)) {
			return new UISelectItems();
		}
		throw new FacesException("MockApplication.createComponent() does not cater for componentType " + componentType);
	}

	@Override
	public Behavior createBehavior(String behaviorId) throws FacesException {
		if (AjaxBehavior.BEHAVIOR_ID.equals(behaviorId)) {
			return new AjaxBehavior();
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
