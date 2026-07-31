package modules.whosin.domain;

import jakarta.annotation.Generated;

/**
 * The metadata names declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public final class Whosin {
	/** The name of the whosin module. */
	public static final String MODULE_NAME = "whosin";

	/** The role names declared in the whosin module. */
	public static final class Roles {
		/** The "Manager" role name. */
		public static final String MANAGER = "Manager";
		/** The "StaffMember" role name. */
		public static final String STAFF_MEMBER = "StaffMember";

		private Roles() {
			// prevent instantiation
		}
	}

	/** The document names declared in the whosin module. */
	public static final class Documents {
		/** The "Contact" document name. */
		public static final String CONTACT = "Contact";
		/** The "MyStatus" document name. */
		public static final String MY_STATUS = "MyStatus";
		/** The "Office" document name. */
		public static final String OFFICE = "Office";
		/** The "Staff" document name. */
		public static final String STAFF = "Staff";
		/** The "StaffQualification" document name. */
		public static final String STAFF_QUALIFICATION = "StaffQualification";

		private Documents() {
			// prevent instantiation
		}
	}

	/** The query names declared in the whosin module. */
	public static final class Queries {
		/** The "qStaff" query name. */
		public static final String Q_STAFF = "qStaff";
		/** The "qStaffForOffice" query name. */
		public static final String Q_STAFF_FOR_OFFICE = "qStaffForOffice";

		private Queries() {
			// prevent instantiation
		}
	}

	private Whosin() {
		// prevent instantiation
	}
}
