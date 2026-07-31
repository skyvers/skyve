package modules.test.domain;

import jakarta.annotation.Generated;

/**
 * The metadata names declared in the test module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public final class Test {
	/** The name of the test module. */
	public static final String MODULE_NAME = "test";

	/** The document names declared in the test module. */
	public static final class Documents {
		/** The "AllAttributesDynamicEmbedded" document name. */
		public static final String ALL_ATTRIBUTES_DYNAMIC_EMBEDDED = "AllAttributesDynamicEmbedded";
		/** The "AllAttributesDynamicPersistent" document name. */
		public static final String ALL_ATTRIBUTES_DYNAMIC_PERSISTENT = "AllAttributesDynamicPersistent";
		/** The "AllAttributesDynamicPersistentDynamicChild" document name. */
		public static final String ALL_ATTRIBUTES_DYNAMIC_PERSISTENT_DYNAMIC_CHILD = "AllAttributesDynamicPersistentDynamicChild";
		/** The "AllAttributesEmbedded" document name. */
		public static final String ALL_ATTRIBUTES_EMBEDDED = "AllAttributesEmbedded";
		/** The "AllAttributesPersistent" document name. */
		public static final String ALL_ATTRIBUTES_PERSISTENT = "AllAttributesPersistent";
		/** The "AllAttributesRequiredPersistent" document name. */
		public static final String ALL_ATTRIBUTES_REQUIRED_PERSISTENT = "AllAttributesRequiredPersistent";
		/** The "AllDynamicAttributesPersistent" document name. */
		public static final String ALL_DYNAMIC_ATTRIBUTES_PERSISTENT = "AllDynamicAttributesPersistent";
		/** The "AllDynamicAttributesPersistentDynamicChild" document name. */
		public static final String ALL_DYNAMIC_ATTRIBUTES_PERSISTENT_DYNAMIC_CHILD = "AllDynamicAttributesPersistentDynamicChild";
		/** The "AnyBase" document name. */
		public static final String ANY_BASE = "AnyBase";
		/** The "AnyDerived1" document name. */
		public static final String ANY_DERIVED1 = "AnyDerived1";
		/** The "AnyDerived2" document name. */
		public static final String ANY_DERIVED2 = "AnyDerived2";
		/** The "ArcOneToMany" document name. */
		public static final String ARC_ONE_TO_MANY = "ArcOneToMany";
		/** The "ArcOneToOne" document name. */
		public static final String ARC_ONE_TO_ONE = "ArcOneToOne";
		/** The "DeleteDuringPostDelete" document name. */
		public static final String DELETE_DURING_POST_DELETE = "DeleteDuringPostDelete";
		/** The "DynamicMappedExtension" document name. */
		public static final String DYNAMIC_MAPPED_EXTENSION = "DynamicMappedExtension";
		/** The "DynamicMappedSubclassed" document name. */
		public static final String DYNAMIC_MAPPED_SUBCLASSED = "DynamicMappedSubclassed";
		/** The "Hierarchical" document name. */
		public static final String HIERARCHICAL = "Hierarchical";
		/** The "InjectedDocument" document name. */
		public static final String INJECTED_DOCUMENT = "InjectedDocument";
		/** The "InverseManyToManyPersistent" document name. */
		public static final String INVERSE_MANY_TO_MANY_PERSISTENT = "InverseManyToManyPersistent";
		/** The "InverseOneToManyPersistent" document name. */
		public static final String INVERSE_ONE_TO_MANY_PERSISTENT = "InverseOneToManyPersistent";
		/** The "InverseOneToOnePersistent" document name. */
		public static final String INVERSE_ONE_TO_ONE_PERSISTENT = "InverseOneToOnePersistent";
		/** The "MappedBase" document name. */
		public static final String MAPPED_BASE = "MappedBase";
		/** The "MappedExtensionJoinedStrategy" document name. */
		public static final String MAPPED_EXTENSION_JOINED_STRATEGY = "MappedExtensionJoinedStrategy";
		/** The "MappedExtensionSingleStrategy" document name. */
		public static final String MAPPED_EXTENSION_SINGLE_STRATEGY = "MappedExtensionSingleStrategy";
		/** The "MappedExtensionUniqueJoinedStrategy" document name. */
		public static final String MAPPED_EXTENSION_UNIQUE_JOINED_STRATEGY = "MappedExtensionUniqueJoinedStrategy";
		/** The "MappedExtensionUniqueSingleStrategy" document name. */
		public static final String MAPPED_EXTENSION_UNIQUE_SINGLE_STRATEGY = "MappedExtensionUniqueSingleStrategy";
		/** The "MappedSubclassedJoinedStrategy" document name. */
		public static final String MAPPED_SUBCLASSED_JOINED_STRATEGY = "MappedSubclassedJoinedStrategy";
		/** The "MappedSubclassedSingleStrategy" document name. */
		public static final String MAPPED_SUBCLASSED_SINGLE_STRATEGY = "MappedSubclassedSingleStrategy";
		/** The "MappedSubclassedUniqueJoinedStrategy" document name. */
		public static final String MAPPED_SUBCLASSED_UNIQUE_JOINED_STRATEGY = "MappedSubclassedUniqueJoinedStrategy";
		/** The "MappedSubclassedUniqueSingleStrategy" document name. */
		public static final String MAPPED_SUBCLASSED_UNIQUE_SINGLE_STRATEGY = "MappedSubclassedUniqueSingleStrategy";
		/** The "Reachability" document name. */
		public static final String REACHABILITY = "Reachability";
		/** The "UniqueConstraintMultipleNavigable" document name. */
		public static final String UNIQUE_CONSTRAINT_MULTIPLE_NAVIGABLE = "UniqueConstraintMultipleNavigable";
		/** The "UniqueConstraintNonNullable" document name. */
		public static final String UNIQUE_CONSTRAINT_NON_NULLABLE = "UniqueConstraintNonNullable";
		/** The "UniqueConstraintNullable" document name. */
		public static final String UNIQUE_CONSTRAINT_NULLABLE = "UniqueConstraintNullable";
		/** The "UniqueConstraintOptimisation" document name. */
		public static final String UNIQUE_CONSTRAINT_OPTIMISATION = "UniqueConstraintOptimisation";
		/** The "UniqueConstraintPersistent" document name. */
		public static final String UNIQUE_CONSTRAINT_PERSISTENT = "UniqueConstraintPersistent";

		private Documents() {
			// prevent instantiation
		}
	}

	/** The query names declared in the test module. */
	public static final class Queries {
		/** The "qH" query name. */
		public static final String Q_H = "qH";
		/** The "qHPoly" query name. */
		public static final String Q_H_POLY = "qHPoly";
		/** The "qMB" query name. */
		public static final String Q_MB = "qMB";
		/** The "qMEJS" query name. */
		public static final String Q_MEJS = "qMEJS";
		/** The "qMEJSNotPoly" query name. */
		public static final String Q_MEJS_NOT_POLY = "qMEJSNotPoly";
		/** The "qMESS" query name. */
		public static final String Q_MESS = "qMESS";
		/** The "qMSJS" query name. */
		public static final String Q_MSJS = "qMSJS";
		/** The "qMSSS" query name. */
		public static final String Q_MSSS = "qMSSS";
		/** The "qMetaDataQueryColumnBinding" query name. */
		public static final String Q_META_DATA_QUERY_COLUMN_BINDING = "qMetaDataQueryColumnBinding";
		/** The "qMetaDataQueryFromAndFilterBinding" query name. */
		public static final String Q_META_DATA_QUERY_FROM_AND_FILTER_BINDING = "qMetaDataQueryFromAndFilterBinding";
		/** The "qExpressionQuery" query name. */
		public static final String Q_EXPRESSION_QUERY = "qExpressionQuery";
		/** The "qAssociations" query name. */
		public static final String Q_ASSOCIATIONS = "qAssociations";
		/** The "qRDBMSDynamic" query name. */
		public static final String Q_RDBMS_DYNAMIC = "qRDBMSDynamic";

		private Queries() {
			// prevent instantiation
		}
	}

	private Test() {
		// prevent instantiation
	}
}
