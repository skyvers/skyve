package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.List;

import org.primefaces.component.autoupdate.AutoUpdateListener;
import org.primefaces.component.outputpanel.OutputPanel;
import org.skyve.util.Util;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.FacesException;
import jakarta.faces.application.Application;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlForm;
import jakarta.faces.context.FacesContext;

@FacesComponent(CSRFForm.COMPONENT_TYPE)
public class CSRFForm extends HtmlForm {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.CSRFForm";

	public CSRFForm() {
		// Default to not be a naming container so that csrfToken goes down with that parameter name.
		setPrependId(false);
	}

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		List<UIComponent> children = getChildren();
		if (children.isEmpty() || (! (children.get(0) instanceof OutputPanel))) {
			FacesContext fc = FacesContext.getCurrentInstance();
			Application a = fc.getApplication();
			ExpressionFactory ef = a.getExpressionFactory();
			ELContext elc = fc.getELContext();
	
	    	final String managedBeanName = Util.processStringValue((String) getAttributes().get("managedBean"));
	    	if (managedBeanName == null) {
	    		throw new FacesException("managedBean attribute is required");
	    	}
	    	
			// Add hidden csrfToken
			// <p:outputPanel id="<formId>_csrfToken" style="display:none" layout="inline" deferred="false">
	    	// 		<input type="hidden" name="csrfToken" value="#{<managedBeanName>.csrfToken}"/>
			// 		<p:autoUpdate />
			// </p:outputPanel>
	    	OutputPanel panel = (OutputPanel) a.createComponent(OutputPanel.COMPONENT_TYPE);
	    	panel.setId(getId() + "_csrfToken");
	    	panel.setStyle("display:none");
	    	panel.setLayout("inline");
	    	panel.setDeferred(false);
	    	UIOutput text = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
	    	text.setValueExpression("value", ef.createValueExpression(elc, "<input type=\"hidden\" name=\"csrfToken\" value=\"#{" + managedBeanName + ".csrfToken}\"/>", String.class));
	    	panel.getChildren().add(text);
	        AutoUpdateListener.subscribe(panel);
	
			children.add(0, panel);
		}
		super.encodeBegin(context);
	}
}
