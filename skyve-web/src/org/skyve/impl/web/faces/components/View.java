package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.FacesComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.FacesViewVisitor;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.ResponsiveComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;

@FacesComponent(View.COMPONENT_TYPE) 
public class View extends HtmlPanelGroup {
    @SuppressWarnings("hiding")
    public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.View";

    @Override
    public void encodeBegin(FacesContext context) throws IOException {
    	if (getChildCount() == 0) {
 			Map<String, Object> attributes = getAttributes();
			final String moduleName = (String) attributes.get("module");
			final String documentName = (String) attributes.get("document");
	    	final String managedBeanName = (String) attributes.get("managedBean");
	    	final String widgetId = (String) attributes.get("widgetId");
	    	final String process = (String) attributes.get("process");
	    	final String update = (String) attributes.get("update");
	    	String typeString = (String) attributes.get("type");
	    	final UserAgentType type = (typeString == null) ? null : UserAgentType.valueOf(typeString);
	    	String classString = (String) attributes.get("componentBuilderClass");
	    	ComponentBuilder tempComponentBuilder = null;
	    	try {
	    		tempComponentBuilder = (classString != null) ? 
	    								(ComponentBuilder) Class.forName(classString).newInstance() :
									new ResponsiveComponentBuilder();
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the component builder " + classString, e);
	    	}
	    	classString = (String) attributes.get("layoutBuilderClass");
	    	LayoutBuilder tempLayoutBuilder = null;
	    	try {
	    		tempLayoutBuilder = (classString != null) ? 
										(LayoutBuilder) Class.forName(classString).newInstance() :
										new ResponsiveLayoutBuilder();
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the layout builder " + classString, e);
	    	}
	    	final ComponentBuilder componentBuilder = tempComponentBuilder;
	    	final LayoutBuilder layoutBuilder = tempLayoutBuilder;
	    	
	    	if (UtilImpl.FACES_TRACE) {
	   			UtilImpl.LOGGER.info(String.format("View - GENERATE moduleName=%s : documentName=%s : " + 
	   													"managedBeanName=%s : type=%s : widgetId=%s" +
	   													" : process=%s : update=%s : managedBeanName=%s : " + 
	   													"componentBuilderClass=%s : layoutBuilderClass=%s",
			   										moduleName, 
			   										documentName, 
			   										managedBeanName, 
			   										type, 
			   										widgetId, 
			   									 	process, 
			   										update, 
			   										managedBeanName, 
			   										componentBuilder.getClass().getName(),
			   										layoutBuilder.getClass().getName()));
	   		}

	    	FacesContext fc = FacesContext.getCurrentInstance();
	    	final UxUi uxui = (UxUi) fc.getExternalContext().getRequestMap().get(FacesUtil.UX_UI_KEY);
	    	
	    	new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					User user = CORE.getUser();
			        Customer customer = user.getCustomer();
			        Module module = customer.getModule(moduleName);
			        Document document = module.getDocument(customer, documentName); // FacesActions.getTargetDocumentForViewBinding(customer, module, facesView);
			        Repository repository = CORE.getRepository();

		        	componentBuilder.setManagedBeanName(managedBeanName);
		        	componentBuilder.setProcess(process);
		        	componentBuilder.setUpdate(update);
		        	componentBuilder.setUserAgentType(type);
		        	layoutBuilder.setManagedBeanName(managedBeanName);
		        	layoutBuilder.setProcess(process);
		        	layoutBuilder.setUpdate(update);
		        	layoutBuilder.setUserAgentType(type);

			        FacesViewVisitor fvv = null;
			        org.skyve.metadata.view.View view = repository.getView(uxui.getName(), customer, document, ViewType.edit);
			        if (view != null) {
			        	fvv = new FacesViewVisitor(user,
													(CustomerImpl) customer,
													(ModuleImpl) module, 
													(DocumentImpl) document,
													(ViewImpl) view,
													widgetId,
													componentBuilder,
													layoutBuilder);
	                    fvv.visit();
	                    View.this.getChildren().add(fvv.getFacesView());
	                }
	                view = repository.getView(uxui.getName(), customer, document, ViewType.create);
	                if (view != null) {
	                    fvv = new FacesViewVisitor(user,
		                                              (CustomerImpl) customer,
		                                              (ModuleImpl) module, 
		                                              (DocumentImpl) document,
		                                              (ViewImpl) view,
		                                              widgetId,
		                                              componentBuilder,
		                                              layoutBuilder);
	                    fvv.visit();
	                    View.this.getChildren().add(fvv.getFacesView());
	                }
	                
	                return null;
				}
			}.execute();
			
			if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());
		}

		super.encodeBegin(context);
    }
}