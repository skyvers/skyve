package modules.whosin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the querys declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum WhosinQuery implements ModuleQuery {
	/** The "qStaff" query. */
	Q_STAFF("qStaff"),
	/** The "qStaffForOffice" query. */
	Q_STAFF_FOR_OFFICE("qStaffForOffice");

	private final String name;

	private WhosinQuery(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "whosin";
	}

	@Override
	public String queryName() {
		return name;
	}
}
