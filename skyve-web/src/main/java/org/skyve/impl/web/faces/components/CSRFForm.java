package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.List;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.faces.application.Application;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputHidden;
import javax.faces.context.FacesContext;

import org.primefaces.component.autoupdate.AutoUpdateListener;

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
		if (children.isEmpty() || (! "csrfToken".equals(children.get(0).getId()))) {
			FacesContext fc = FacesContext.getCurrentInstance();
			Application a = fc.getApplication();
			ExpressionFactory ef = a.getExpressionFactory();
			ELContext elc = fc.getELContext();
	
	    	final String managedBeanName = (String) getAttributes().get("managedBean");
	
			// Add hidden csrfToken
			// <h:inputHidden id="csrfToken" value="#{skyve.csrfToken}">
			// 		<p:autoUpdate />
			// </h:inputHidden>
			HtmlInputHidden hidden = (HtmlInputHidden) a.createComponent(HtmlInputHidden.COMPONENT_TYPE);
			hidden.setId("csrfToken");
			hidden.setValueExpression("value", ef.createValueExpression(elc, "#{" + managedBeanName + ".csrfToken}", String.class));
	        AutoUpdateListener.subscribe(hidden);
	
			children.add(0, hidden);
		}
		super.encodeBegin(context);
	}
}
