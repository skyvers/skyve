package org.skyve.impl.sail.execution;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

public class ExecutionDelegate {
	public static PrimeFacesAutomationContext newContext(PushListContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		String moduleName = push.getModuleName();
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();
		
		if (queryName != null) {
			MetaDataQueryDefinition q = m.getMetaDataQuery(queryName);
			if (q == null) {
				q = m.getDocumentDefaultQuery(c, documentName);
			}
			m = q.getOwningModule();
			newContext.setModuleName(m.getName());
			newContext.setDocumentName(q.getDocumentName());
		}
		else if (documentName != null) {
			Document d = m.getDocument(c, documentName);
			if (modelName != null) {
				d = d.getListModel(c, modelName, false).getDrivingDocument();
			}
			else {
				push.setQueryName(documentName);
			}
			newContext.setModuleName(d.getOwningModuleName());
			newContext.setDocumentName(d.getName());
		}
		else {
			throw new MetaDataException("NavigateList must have module and one of (query, document, document & mode)l");
		}

		newContext.setViewType(ViewType.list);
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());
		
		return newContext;
	}
	
	public static PrimeFacesAutomationContext newContext(PushEditContext push) {
		PrimeFacesAutomationContext newContext = new PrimeFacesAutomationContext();
		newContext.setModuleName(push.getModuleName());
		newContext.setDocumentName(push.getDocumentName());
		if (Boolean.TRUE.equals(push.getCreateView())) {
			newContext.setViewType(ViewType.create);
		}
		else {
			newContext.setViewType(ViewType.edit);
		}
		newContext.setUxui(push.getUxui());
		newContext.setUserAgentType(push.getUserAgentType());

		return newContext;
	}
	
	public static void executeTestDataEnter(TestDataEnter testDataEnter,
												PrimeFacesAutomationContext context,
												Executor executor) {
		String moduleName = context.getModuleName();
		String documentName = context.getDocumentName();
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		
		Bean bean = null;
		String fixture = testDataEnter.getFixture();
		if (fixture == null) {
			bean = new DataBuilder().fixture(FixtureType.sail).build(d);
		}
		else {
			bean = new DataBuilder().fixture(fixture).build(d);
		}
		
        ViewImpl view = (ViewImpl) d.getView(context.getUxui(), c, context.getViewType().toString());
		TestDataEnterViewVisitor visitor = new TestDataEnterViewVisitor((CustomerImpl) c,
																			(ModuleImpl) m,
																			(DocumentImpl) d,
																			view,
																			bean);
		visitor.visit();
		
		for (Step steps : visitor.getScalarSteps()) {
			steps.execute(executor);
		}
	}
}
