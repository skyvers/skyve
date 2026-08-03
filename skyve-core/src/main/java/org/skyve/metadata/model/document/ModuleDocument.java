package org.skyve.metadata.model.document;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;

/**
 * A compile-time reference to a document declared in a module - implemented by the generated
 * per-module document enums (e.g. <code>AdminDocument.CONTACT</code>) so Skyve APIs can accept
 * a single self-describing token in place of a module name and document name String pair.
 */
public interface ModuleDocument {
	/**
	 * The name of the module that declares (or references) the document.
	 *
	 * @return the module name; never {@code null}
	 */
	String moduleName();

	/**
	 * The document name within the module.
	 *
	 * @return the document name; never {@code null}
	 */
	String documentName();

	/**
	 * Resolves this reference to the document metadata through the current customer,
	 * so per-customer overrides are honoured.
	 *
	 * @return the document metadata; never {@code null}
	 */
	default Document toDocument() {
		Customer customer = CORE.getCustomer();
		return customer.getModule(moduleName()).getDocument(customer, documentName());
	}
}
