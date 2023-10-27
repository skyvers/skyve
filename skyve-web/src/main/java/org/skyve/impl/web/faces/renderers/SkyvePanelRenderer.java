package org.skyve.impl.web.faces.renderers;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.panel.PanelRenderer;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.beans.FacesView;

public class SkyvePanelRenderer extends PanelRenderer {
	/**
	 * Set the collapsed state from the session attribute (if set) and then call super.
	 */
	@Override
	public void encodeBegin(FacesContext context, UIComponent component) throws IOException {
		UIViewRoot uiViewRoot = context.getViewRoot();
		if (uiViewRoot != null) {
			String managedBeanName = (String) uiViewRoot.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			if (managedBeanName != null) {
				FacesView<?> view	= FacesUtil.getManagedBean(managedBeanName);
				if (view != null) {
					view.setCollapsedFromSession((Panel) component);
				}
			}
		}

		super.encodeBegin(context, component);
	}
}
