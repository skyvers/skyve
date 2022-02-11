package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.impl.web.faces.pipeline.FacesViewRenderer;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;

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
	    	classString = (String) attributes.get("layoutBuilderClass");
	    	LayoutBuilder tempLayoutBuilder = null;
	    	try {
	    		if (classString == null) {
	    			tempLayoutBuilder = new ResponsiveLayoutBuilder();
	    		}
	    		else {
	    			Class<?> type = Class.forName(classString);
		    		tempLayoutBuilder = (LayoutBuilder) type.getDeclaredConstructor().newInstance();
	    		}
	    	}
	    	catch (Exception e) {
	    		throw new IOException("Cannot instantiate the layout builder " + classString, e);
	    	}
	    	final ComponentBuilder componentBuilder = tempComponentBuilder;
	    	final LayoutBuilder layoutBuilder = tempLayoutBuilder;
	    	
	    	if (UtilImpl.FACES_TRACE) {
	   			UtilImpl.LOGGER.info(String.format("View - GENERATE moduleName=%s : documentName=%s : " + 
	   													"managedBeanName=%s : widgetId=%s" +
	   													" : process=%s : update=%s : managedBeanName=%s : " + 
	   													"componentBuilderClass=%s : layoutBuilderClass=%s",
			   										moduleName, 
			   										documentName, 
			   										managedBeanName, 
			   										widgetId, 
			   									 	process, 
			   										update, 
			   										managedBeanName, 
			   										componentBuilder.getClass().getName(),
			   										layoutBuilder.getClass().getName()));
	   		}

	    	FacesContext fc = FacesContext.getCurrentInstance();
	    	Map<String, Object> requestMap = fc.getExternalContext().getRequestMap();
	    	UxUi uxui = (UxUi) requestMap.get(AbstractWebContext.UXUI);
	    	UserAgentType userAgentType = (UserAgentType) requestMap.get(AbstractWebContext.USER_AGENT_TYPE_KEY);
	    	if ((uxui == null) || (userAgentType == null)) {
	    		FacesView<?> fv = FacesUtil.getManagedBean(managedBeanName);
	    		if (uxui == null) {
	    			uxui = fv.getUxUi();
	    		}
	    		if (userAgentType == null) {
	    			userAgentType = fv.getUserAgentType();
	    		}
	    	}
	    	
	    	final String uxuiName = uxui.getName();
	    	final UserAgentType finalUAT = userAgentType;
	    	new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					List<UIComponent> views = View.generate(moduleName,
																documentName,
																widgetId,
																managedBeanName,
																uxuiName,
																finalUAT,
																process,
																update,
																componentBuilder,
																layoutBuilder);
					View.this.getChildren().addAll(views);
					
	                return null;
				}
			}.execute();
			
			if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());
		}

		super.encodeBegin(context);
    }
    
    public static List<UIComponent> generate(String moduleName,
				    							String documentName,
				    							String widgetId,
				    							String managedBeanName,
				    							String uxui,
				    							UserAgentType userAgentType,
				    							String process,
				    							String update,
				    							ComponentBuilder componentBuilder,
				    							LayoutBuilder layoutBuilder) {
    	List<UIComponent> result = new ArrayList<>(2);
    	
    	User user = CORE.getUser();
    	Customer customer = user.getCustomer();
        Module module = customer.getModule(moduleName);
        Document document = module.getDocument(customer, documentName); // FacesActions.getTargetDocumentForViewBinding(customer, module, facesView);

        componentBuilder.setManagedBeanName(managedBeanName);
    	componentBuilder.setProcess(process);
    	componentBuilder.setUpdate(update);
    	componentBuilder.setUserAgentType(userAgentType);
    	layoutBuilder.setManagedBeanName(managedBeanName);
    	layoutBuilder.setProcess(process);
    	layoutBuilder.setUpdate(update);
    	layoutBuilder.setUserAgentType(userAgentType);

    	FacesViewRenderer fvr = null;
        org.skyve.metadata.view.View view = document.getView(uxui, customer, ViewType.edit.toString());
        if (view != null) {
        	fvr = new FacesViewRenderer(user,
										module, 
										document,
										view,
										widgetId,
										componentBuilder,
										layoutBuilder);
        	fvr.visit();
            result.add(fvr.getFacesView());
        }
        // Get the create view and add (so long as we didn't get the edit view back)
        view = document.getView(uxui, customer, ViewType.create.toString());
        if ((view != null) && ViewType.create.toString().equals(view.getName())) {
        	fvr = new FacesViewRenderer(user,
                                          module, 
                                          document,
                                          view,
                                          widgetId,
                                          componentBuilder,
                                          layoutBuilder);
        	fvr.visit();
            result.add(fvr.getFacesView());
        }
        
        return result;
    }
}