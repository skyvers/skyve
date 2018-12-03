package org.skyve.impl.generate.pwa.react;

import java.util.Map;

import org.skyve.impl.generate.pwa.PWAViewVisitor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

public class ReactViewVisitor extends PWAViewVisitor {
	protected Map<String, String> imports;

	public ReactViewVisitor(Customer customer,
								Module module,
								Document document,
								View view,
								String widgetId,
								Map<String, String> imports) {
		super(customer, module, document, view, widgetId);
		this.imports = imports;
	}
}
