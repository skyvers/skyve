package org.skyve.impl.sail.execution;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.user.User;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

@Deprecated(forRemoval = true)
public class ExecutionDelegate {
	
	public static void executeTestDataEnter(TestDataEnter testDataEnter, PrimeFacesAutomationContext context, Executor executor) {
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
		
		final String uxui = context.getUxui();
		ViewImpl view = (ViewImpl) d.getView(uxui, c, context.getViewType().toString());
		TestDataEnterViewVisitor visitor = new TestDataEnterViewVisitor(
				(CustomerImpl) c,
				(ModuleImpl) m,
				(DocumentImpl) d,
				view,
				uxui,
				bean);
		visitor.visit();
		
		for (Step steps : visitor.getScalarSteps()) {
			steps.execute(executor, ExecutionOptions.defaultOptions());
		}
	}
}
