package modules.test.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the queries declared in the test module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum TestQuery implements ModuleQuery {
	/** The "qH" query. */
	Q_H("qH"),
	/** The "qHPoly" query. */
	Q_H_POLY("qHPoly"),
	/** The "qMB" query. */
	Q_MB("qMB"),
	/** The "qMEJS" query. */
	Q_MEJS("qMEJS"),
	/** The "qMEJSNotPoly" query. */
	Q_MEJS_NOT_POLY("qMEJSNotPoly"),
	/** The "qMESS" query. */
	Q_MESS("qMESS"),
	/** The "qMSJS" query. */
	Q_MSJS("qMSJS"),
	/** The "qMSSS" query. */
	Q_MSSS("qMSSS"),
	/** The "qMetaDataQueryColumnBinding" query. */
	Q_META_DATA_QUERY_COLUMN_BINDING("qMetaDataQueryColumnBinding"),
	/** The "qMetaDataQueryFromAndFilterBinding" query. */
	Q_META_DATA_QUERY_FROM_AND_FILTER_BINDING("qMetaDataQueryFromAndFilterBinding"),
	/** The "qExpressionQuery" query. */
	Q_EXPRESSION_QUERY("qExpressionQuery"),
	/** The "qAssociations" query. */
	Q_ASSOCIATIONS("qAssociations"),
	/** The "qRDBMSDynamic" query. */
	Q_RDBMS_DYNAMIC("qRDBMSDynamic");

	private final String name;

	private TestQuery(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "test";
	}

	@Override
	public String queryName() {
		return name;
	}
}
