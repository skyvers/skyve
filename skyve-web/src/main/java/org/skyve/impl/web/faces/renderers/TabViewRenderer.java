package org.skyve.impl.web.faces.renderers;

import java.io.IOException;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.skyve.impl.web.AbstractWebContext;

public class TabViewRenderer extends org.primefaces.component.tabview.TabViewRenderer {
	@Override
	@SuppressWarnings("resource")
	protected void encodeTabContent(FacesContext context, Tab tab, int index, boolean active, boolean dynamic)
	throws IOException {
		ResponseWriter writer = context.getResponseWriter();
/* Replace this line
        String styleClass = active ? TabView.ACTIVE_TAB_CONTENT_CLASS : TabView.INACTIVE_TAB_CONTENT_CLASS;
 * with this */
		String styleClass = TabView.INACTIVE_TAB_CONTENT_CLASS;
		Map<String, Object> attributes = tab.getParent().getAttributes();
		String moduleName = (String) attributes.get(AbstractWebContext.MODULE_NAME);
		String documentName = (String) attributes.get(AbstractWebContext.DOCUMENT_NAME);
		if ((moduleName == null) || (documentName == null)) {
			super.encodeTabContent(context, tab, index, active, dynamic);
			return;
		}
/* end of replacement */
		writer.startElement("div", null);
		writer.writeAttribute("id", tab.getClientId(context), null);
		writer.writeAttribute("class", styleClass, null);
		writer.writeAttribute("role", "tabpanel", null);
		writer.writeAttribute("aria-hidden", String.valueOf(!active), null);
		writer.writeAttribute("data-index", Integer.valueOf(index), null);

		if (dynamic) {
			if (active) {
				tab.encodeAll(context);
				tab.setLoaded(true);
			}
		}
		else {
			tab.encodeAll(context);
		}

		writer.endElement("div");
	}
}
