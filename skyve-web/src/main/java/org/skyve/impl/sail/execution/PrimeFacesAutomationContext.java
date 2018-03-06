package org.skyve.impl.sail.execution;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.impl.web.faces.components.ListGrid;
import org.skyve.impl.web.faces.components.View;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;

public class PrimeFacesAutomationContext extends AutomationContext {
	private ComponentCollector componentCollector;
	private UIComponent component;
	
	public PrimeFacesAutomationContext() {
		// nothing to see here
	}
	
	public PrimeFacesAutomationContext(PrimeFacesAutomationContext context) {
		super(context);
		componentCollector = context.componentCollector;
		component = context.component;
	}
	
	List<UIComponent> getFacesComponents(String identifier) {
		return componentCollector.getFacesComponents(identifier);
	}

	List<Object> getSkyveWidgets(String identifier) {
		return componentCollector.getSkyveWidgets(identifier);
	}

	public void generate(PushListContext push, ComponentBuilder componentBuilder) {
		String moduleName = push.getModuleName();
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();

		componentCollector = new ComponentCollector(this, push);
		ComponentBuilderChain chain = new ComponentBuilderChain(componentBuilder, componentCollector);
	
		component = ListGrid.generate(moduleName, 
										documentName, 
										queryName, 
										modelName, 
										Boolean.TRUE,
										false,
										Boolean.TRUE,
										false,
										"skyve",
										getUserAgentType(),
										chain);
	}
	
	public void generate(PushEditContext push,
							ComponentBuilder componentBuilder,
							LayoutBuilder layoutBuilder) {
		componentCollector = new ComponentCollector(this, push);
		ComponentBuilderChain chain = new ComponentBuilderChain(componentBuilder, componentCollector);
		
		List<UIComponent> editAndCreateView = View.generate(push.getModuleName(), 
																push.getDocumentName(), 
																null,
																"skyve", 
																push.getUxui(), 
																push.getUserAgentType(),
																null,
																null,
																chain,
																layoutBuilder);
		if ((editAndCreateView.size() > 1) && Boolean.TRUE.equals(push.getCreate())) {
			component = editAndCreateView.get(1);
		}
		else {
			component = editAndCreateView.get(0);
		}		
	}
}
