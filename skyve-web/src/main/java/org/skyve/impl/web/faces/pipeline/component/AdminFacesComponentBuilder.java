package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;

import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;

public class AdminFacesComponentBuilder extends NoOpComponentBuilder {
	private static final String BUTTON_STYLE_CLASS = "btn-primary";
	@Override
	public UIComponent action(UIComponent component, String listBinding, String listVar, Action action, ImplicitActionName name, String title) {
		if (component != null) {
			((CommandButton) component).setStyleClass(BUTTON_STYLE_CLASS);
		}
		
		return component;
	}

	@Override
	public UIComponent actionButton(UIComponent component, String listBinding, String listVar, Button button, Action action) {
		if (component != null) {
			((CommandButton) component).setStyleClass(BUTTON_STYLE_CLASS);
		}
		
		return component;
	}
	
	@Override
	public UIComponent downloadButton(UIComponent component, Button button, Action action, String moduleName, String documentName) {
		if (component != null) {
			((CommandButton) component).setStyleClass(BUTTON_STYLE_CLASS);
		}
		
		return component;
	}
	
	@Override
	public UIComponent reportButton(UIComponent component, Button button, Action action) {
		if (component != null) {
			((CommandButton) component).setStyleClass(BUTTON_STYLE_CLASS);
		}
		
		return component;
	}
}
