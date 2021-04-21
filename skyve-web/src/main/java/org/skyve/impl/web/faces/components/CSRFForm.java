package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.List;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;

import org.primefaces.component.autoupdate.AutoUpdateListener;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.util.Util;

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
		if (children.isEmpty() || (! (children.get(0) instanceof HtmlOutputText))) {
			FacesContext fc = FacesContext.getCurrentInstance();
			Application a = fc.getApplication();
			ExpressionFactory ef = a.getExpressionFactory();
			ELContext elc = fc.getELContext();
	
	    	final String managedBeanName = Util.processStringValue((String) getAttributes().get("managedBean"));
	    	if (managedBeanName == null) {
	    		throw new FacesException("managedBean attribute is required");
	    	}
	    	
	    	FacesView<?> managedBean = null;
			// Do nothing if this is being executed through SAIL
	    	if (FacesContext.getCurrentInstance() != null) {
				managedBean = FacesUtil.getManagedBean(managedBeanName);
			}

			// Add hidden csrfToken
			// <h:outputText id="#{skyve.nextId()}" style="display:none" escape="false" value="&lt;input type=&quot;hidden&quot; name=&quot;csrfToken&quot; value=&quot;#{fspView.csrfToken}&quot;/&gt;">
			// 		<p:autoUpdate />
			// </h:outputText>
	    	HtmlOutputText text = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
	    	if (managedBean != null) {
	    		text.setId(managedBean.nextId());
	    	}
	    	text.setStyle("display:none");
	    	text.setEscape(false);
	    	text.setValueExpression("value", ef.createValueExpression(elc, "<input type=\"hidden\" name=\"csrfToken\" value=\"#{" + managedBeanName + ".csrfToken}\"/>", String.class));
	        AutoUpdateListener.subscribe(text);
	
			children.add(0, text);
		}
		super.encodeBegin(context);
	}
}
