package org.skyve.metadata.model.document;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.ModuleMember;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a document declared in a module - implemented by the generated
 * per-module document enums (e.g. <code>AdminDocument.CONTACT</code>) so Skyve APIs can accept
 * a single self-describing token in place of a module name and document name String pair.
 * <p/>
 * A document the module references from another module
 * (<code>&lt;document ref="Contact" moduleRef="admin" /&gt;</code>) is addressed through the
 * referencing module, exactly as the String form <code>("whosin", "Contact")</code> is, so
 * {@link #moduleName()} is the referencing module while
 * {@link #toDocument()}<code>.getOwningModuleName()</code> is the module that defines it.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleDocument extends ModuleMember {
	/**
	 * The document name within the module.
	 *
	 * @return the document name
	 */
	@Nonnull String documentName();

	/**
	 * Resolves this reference to the document metadata through the current user's customer, so
	 * per-customer overrides are honoured.
	 *
	 * @return the document metadata
	 * @throws org.skyve.metadata.MetaDataException if the module has no such document for the
	 * 				customer
	 */
	default @Nonnull Document toDocument() {
		return toDocument(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the document metadata through the given customer, so
	 * per-customer overrides are honoured.
	 *
	 * @param customer	the customer to resolve the document for
	 * @return the document metadata
	 * @throws org.skyve.metadata.MetaDataException if the module has no such document for the
	 * 				customer
	 */
	default @Nonnull Document toDocument(@Nonnull Customer customer) {
		return toModule(customer).getDocument(customer, documentName());
	}
}
