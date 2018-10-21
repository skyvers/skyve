package org.skyve.impl.web.faces.renderers;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.primefaces.component.tabview.TabView;

/**
 * If there is no server side tab selected (capture tab changes in browser session storage),
 * set no active tab as this will be set client-side through the browsers.
 * See TabularComponentRenderer.
 * 
 * @author mike
 */
public class TabViewRenderer extends org.primefaces.component.tabview.TabViewRenderer {
	@Override
	public void encodeEnd(FacesContext context, UIComponent component) throws IOException {
		TabView tabView = (TabView) component;
		String onTabChange = tabView.getOnTabChange();
		if ((onTabChange != null) && onTabChange.startsWith("sessionStorage")) { // set client-side
			int activeIndex = tabView.getActiveIndex();
			tabView.setActiveIndex(-1);
			super.encodeEnd(context, component);
			tabView.setActiveIndex(activeIndex);
		}
		else {
			super.encodeEnd(context, component);
		}
	}
}
