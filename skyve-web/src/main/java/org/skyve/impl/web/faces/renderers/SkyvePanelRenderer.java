package org.skyve.impl.web.faces.renderers;

import java.io.IOException;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.panel.PanelRenderer;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.views.FacesView;

import jakarta.annotation.Nonnull;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.FacesContext;

public class SkyvePanelRenderer extends PanelRenderer {
	/**
	 * Set the collapsed state from the session attribute (if set) and then call super.
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void encodeBegin(@SuppressWarnings("null") @Nonnull FacesContext context,
							@SuppressWarnings("null") @Nonnull UIComponent component) throws IOException {
		UIViewRoot uiViewRoot = context.getViewRoot();
		if (uiViewRoot != null) {
			String managedBeanName = (String) uiViewRoot.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			if (managedBeanName != null) {
				FacesView view	= (FacesView) FacesUtil.getNamed(managedBeanName);
				view.setCollapsedFromSession((Panel) component);
			}
		}

		super.encodeBegin(context, component);
	}
}
