package modules.whosin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum WhosinDocument implements ModuleDocument {
	/** The "Contact" document. */
	CONTACT("Contact"),
	/** The "MyStatus" document. */
	MY_STATUS("MyStatus"),
	/** The "Office" document. */
	OFFICE("Office"),
	/** The "Staff" document. */
	STAFF("Staff"),
	/** The "StaffQualification" document. */
	STAFF_QUALIFICATION("StaffQualification");

	private final String name;

	private WhosinDocument(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "whosin";
	}

	@Override
	public String documentName() {
		return name;
	}
}
