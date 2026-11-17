package modules.whosin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the metadata queries declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum WhosinQuery implements ModuleQuery {
	/** The "qStaff" metadata query. */
	Q_STAFF("qStaff"),
	/** The "qStaffForOffice" metadata query. */
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
