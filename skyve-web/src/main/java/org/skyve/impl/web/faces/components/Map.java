package org.skyve.impl.web.faces.components;

import java.io.IOException;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.map.DocumentQueryMapModel;
import org.skyve.metadata.view.model.map.MapModel;
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
			final String documentName = (String) attributes.get("document");
			final String modelName = (String) attributes.get("model");
			final String geometryBinding = (String) attributes.get("geometryBinding");
			final String managedBeanName = (String) attributes.get("managedBean");

			String classString = (String) attributes.get("componentBuilderClass");
			ComponentBuilder tempComponentBuilder = null;
			try {
				tempComponentBuilder = (classString != null) ?
										(ComponentBuilder) Class.forName(classString).newInstance() :
										new SkyveComponentBuilderChain();
			}
			catch (Exception e) {
				throw new IOException("Cannot instantiate the component builder " + classString, e);
			}
			final ComponentBuilder componentBuilder = tempComponentBuilder;

			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					FacesContext fc = FacesContext.getCurrentInstance();
					final UserAgentType userAgentType = (UserAgentType) fc.getExternalContext().getRequestMap().get(FacesUtil.USER_AGENT_TYPE_KEY);

					Map.this.getChildren().add(generate(moduleName,
															documentName,
															queryName,
															modelName,
															managedBeanName,
															userAgentType,
															componentBuilder));
				    
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}		

	public static UIComponent generate(String moduleName,
										String documentName,
										String queryName,
										String modelName,
										String managedBeanName,
										UserAgentType userAgentType,
										ComponentBuilder componentBuilder) {
		MapModel<? extends Bean> model = null;
		String name = null;

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		if (queryName != null) {
			MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
			if (query == null) {
				query = module.getDocumentDefaultQuery(customer, queryName);
			}
			DocumentQueryMapModel<Bean> queryModel = new DocumentQueryMapModel<>(query);
			model = queryModel;
			name = queryName;
		}
		else {
			Document document = module.getDocument(customer, documentName);
			model = CORE.getRepository().getMapModel(customer, document, modelName, true);
			name = modelName;
		}

		componentBuilder.setManagedBeanName(managedBeanName);
    	componentBuilder.setUserAgentType(userAgentType);

		MapDisplay map = new MapDisplay();

    	return componentBuilder.map(null, null);
	}
}
