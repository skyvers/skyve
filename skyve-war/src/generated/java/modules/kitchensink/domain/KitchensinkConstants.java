package modules.kitchensink.domain;

import jakarta.annotation.Generated;

/**
 * The metadata names declared in the kitchensink module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public final class KitchensinkConstants {
	/** The name of the kitchensink module. */
	public static final String MODULE_NAME = "kitchensink";

	/** The role names declared in the kitchensink module. */
	public static final class Roles {
		/** The "dev" role name. */
		public static final String DEV = "dev";

		private Roles() {
			// prevent instantiation
		}
	}

	/** The document names declared in the kitchensink module. */
	public static final class Documents {
		/** The "ContainerGrid" document name. */
		public static final String CONTAINER_GRID = "ContainerGrid";
		/** The "DataRepeater" document name. */
		public static final String DATA_REPEATER = "DataRepeater";
		/** The "EscapingFixture" document name. */
		public static final String ESCAPING_FIXTURE = "EscapingFixture";
		/** The "InlineGrid" document name. */
		public static final String INLINE_GRID = "InlineGrid";
		/** The "KitchenSink" document name. */
		public static final String KITCHEN_SINK = "KitchenSink";
		/** The "ListAttributes" document name. */
		public static final String LIST_ATTRIBUTES = "ListAttributes";
		/** The "LookupDescription" document name. */
		public static final String LOOKUP_DESCRIPTION = "LookupDescription";
		/** The "OrderedGrid" document name. */
		public static final String ORDERED_GRID = "OrderedGrid";
		/** The "UploadFixture" document name. */
		public static final String UPLOAD_FIXTURE = "UploadFixture";
		/** The "UploadFixtureGridRow" document name. */
		public static final String UPLOAD_FIXTURE_GRID_ROW = "UploadFixtureGridRow";

		private Documents() {
			// prevent instantiation
		}
	}

	/** The query names declared in the kitchensink module. */
	public static final class Queries {
		/** The "qEscapingFixture" query name. */
		public static final String Q_ESCAPING_FIXTURE = "qEscapingFixture";

		private Queries() {
			// prevent instantiation
		}
	}

	private KitchensinkConstants() {
		// prevent instantiation
	}
}
