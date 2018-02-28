package org.skyve.impl.tools.test.sail.generate;

import java.util.Map.Entry;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.TestSuite;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

public class Generator {
	public static final void main(String[] args) throws Exception {
		if (args.length != 1) {
			System.err.println("args are <customerName>");
			System.exit(1);
		}

System.out.println(visitModules(args[0]));
	}

	public static String visitModules(String customerName) throws Exception {
		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/dtf/skyve/skyve-ee/javaee/skyve.ear/apps.jar/";
		AbstractRepository.set(new LocalDesignRepository());
		Customer customer = AbstractRepository.get().getCustomer(customerName);

		TestSuite result = new TestSuite();
		
		// visit each customer module
		for (Module module : customer.getModules()) {
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				DocumentRef documentRef = entry.getValue();
				if (documentRef.getOwningModuleName().equals(module.getName())) {
					Document document = module.getDocument(customer, entry.getKey());
					View createView = document.getView("desktop", customer, ViewType.create.toString());
					View editView = document.getView("desktop", customer, ViewType.edit.toString());
					if (createView != editView) {
						new GenerateViewVisitor((CustomerImpl) customer,
													(ModuleImpl) module,
													(DocumentImpl) document,
													(ViewImpl) createView,
													result).visit();
					}
					new GenerateViewVisitor((CustomerImpl) customer,
												(ModuleImpl) module,
												(DocumentImpl) document,
												(ViewImpl) editView,
												result).visit();
				}
			}
		}
		
		return XMLUtil.marshalWAIL(result);
	}
}
