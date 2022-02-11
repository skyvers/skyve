package org.skyve.impl.web.faces.components;

import java.io.IOException;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.util.Util;

@FacesComponent(Map.COMPONENT_TYPE)
public class Map extends HtmlPanelGroup {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.Map";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		if (getChildCount() == 0) {
			java.util.Map<String, Object> attributes = getAttributes();
			final String moduleName = (String) attributes.get("module");
			final String queryName = (String) attributes.get("query");
			final String modelName = (String) attributes.get("model");
			final String geometryBinding = (String) attributes.get("geometryBinding");
			final String managedBeanName = (String) attributes.get("managedBean");

			String classString = (String) attributes.get("componentBuilderClass");
			ComponentBuilder tempComponentBuilder = null;
			try {
				if (classString == null) {
					tempComponentBuilder = new SkyveComponentBuilderChain();
				}
				else {
					Class<?> type = Class.forName(classString);
					tempComponentBuilder = (ComponentBuilder) type.getDeclaredConstructor().newInstance();
				}
			}
			catch (Exception e) {
				throw new IOException("Cannot instantiate the component builder " + classString, e);
			}
			final ComponentBuilder componentBuilder = tempComponentBuilder;

			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					componentBuilder.setManagedBeanName(managedBeanName);
			    	componentBuilder.setUserAgentType(UserAgent.getType((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest()));

			    	Map.this.getChildren().add(generate(moduleName,
															queryName,
															geometryBinding,
															modelName,
															componentBuilder));
				    
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}		

	public static UIComponent generate(String moduleName,
										String queryName,
										String geometryBinding,
										String modelName,
										ComponentBuilder componentBuilder) {

    	if (modelName != null) {
			return componentBuilder.map(null, new MapDisplay(), modelName);
		}
		return componentBuilder.map(null, new MapDisplay(), moduleName, queryName, geometryBinding);
	}
}
