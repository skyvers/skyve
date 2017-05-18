package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Util;

@FacesComponent(ListGrid.COMPONENT_TYPE)
public class ListGrid extends HtmlPanelGroup {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.ListGrid";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		if (getChildCount() == 0) {
			Map<String, Object> attributes = getAttributes();
			final String moduleName = (String) attributes.get("module");
			final String queryName = (String) attributes.get("query");
			final String documentName = (String) attributes.get("document");
			final String modelName = (String) attributes.get("model");
			final String managedBeanName = (String) attributes.get("managedBean");
			Object canCreateAttribute = attributes.get("canCreate");
			final boolean canCreate = (canCreateAttribute == null) || "true".equals(canCreateAttribute);
			final boolean paginator = "true".equals(attributes.get("paginator"));
	    	String classString = (String) attributes.get("componentBuilderClass");
	    	ComponentBuilder tempComponentBuilder = null;
	    	try {
	    		tempComponentBuilder = (classString != null) ? 
	    								(ComponentBuilder) Class.forName(classString).newInstance() :
									new SkyveComponentBuilder();
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the component builder " + classString, e);
	    	}
	    	final ComponentBuilder componentBuilder = tempComponentBuilder;

			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					ListModel<? extends Bean> model = null;
					String name = null;

					User user = CORE.getUser();
					Customer customer = user.getCustomer();
					Module module = customer.getModule(moduleName);
					if (queryName != null) {
						DocumentQueryDefinition query = module.getDocumentQuery(queryName);
						if (query == null) {
							query = module.getDocumentDefaultQuery(customer, queryName);
						}
						DocumentQueryListModel queryModel = new DocumentQueryListModel();
						queryModel.setQuery(query);
						model = queryModel;
						name = queryName;
					}
					else {
						Document document = module.getDocument(customer, documentName);
						model = CORE.getRepository().getListModel(customer, document, modelName);
						name = modelName;
					}
					
					FacesContext fc = FacesContext.getCurrentInstance();
					final UserAgentType userAgentType = (UserAgentType) fc.getExternalContext().getRequestMap().get(FacesUtil.USER_AGENT_TYPE_KEY);
					componentBuilder.setManagedBeanName(managedBeanName);
		        	componentBuilder.setUserAgentType(userAgentType);
				    
				    UIComponent grid = componentBuilder.listGrid(documentName, 
				    												name, 
				    												model, 
				    												null,
				    												canCreate, 
				    												paginator, 
				    												true);
				    ListGrid.this.getChildren().add(grid);
				    
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}
}
