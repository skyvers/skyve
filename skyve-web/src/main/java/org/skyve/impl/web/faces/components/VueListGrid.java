package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Map;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.context.FacesContext;

@FacesComponent(VueListGrid.COMPONENT_TYPE)
public class VueListGrid extends UIOutput {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.VueListGrid";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();

		// NB:- I might need to let this be evaluated always since this just isnt structural but may have expressions that need re-evaluating
		// TODO Need to try this out.
		if ((getValue() == null) || Boolean.TRUE.toString().equals(attributes.get("dynamic"))) {
			final String moduleName = (String) attributes.get("module");
			final String queryName = (String) attributes.get("query");
			final String documentName = (String) attributes.get("document");
			final String modelName = (String) attributes.get("model");
			Object createRenderedAttribute = attributes.get("createRendered");
			final Boolean createRendered = Boolean.valueOf((createRenderedAttribute == null) || 
															String.valueOf(true).equals(createRenderedAttribute) || // literal "true"
															Boolean.TRUE.equals(createRenderedAttribute)); // evaluated EL expression
			Object createDisabledAttribute = attributes.get("createDisabled");
			final boolean createDisabled = String.valueOf(true).equals(createDisabledAttribute) || // literal "true"
											Boolean.TRUE.equals(createDisabledAttribute); // evaluated EL Expression
			Object zoomRenderedAttribute = attributes.get("zoomRendered");
			final Boolean zoomRendered = Boolean.valueOf((zoomRenderedAttribute == null) ||
															String.valueOf(true).equals(zoomRenderedAttribute) || // literal "true"
															Boolean.TRUE.equals(zoomRenderedAttribute)); // evaluated EL expression
			Object zoomDisabledAttribute = attributes.get("zoomDisabled");
			final boolean zoomDisabled = String.valueOf(true).equals(zoomDisabledAttribute) || // literal "true"
											Boolean.TRUE.equals(zoomDisabledAttribute); // evaluated EL expression
			Object filterRenderedAttribute = attributes.get("filterRendered");
			final Boolean filterRendered = Boolean.valueOf((filterRenderedAttribute == null) ||
															String.valueOf(true).equals(filterRenderedAttribute) || // literal "true"
															Boolean.TRUE.equals(filterRenderedAttribute)); // evaluated EL expression

			FacesContext fc = FacesContext.getCurrentInstance();
			Application a = fc.getApplication();
			ExpressionFactory ef = a.getExpressionFactory();
			ELContext elc = fc.getELContext();
			
			String guts = "<script>alert('test')</script>";
			setValueExpression("value", ef.createValueExpression(elc, guts, String.class));
		}
	}
}
