package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.dialect.Oracle10gDialect;
import org.hibernate.usertype.UserType;
import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.SpatialFunction;
import org.hibernatespatial.geodb.GeoDBGeometryUserType;

public class OracleNoOpSpatialDialect extends Oracle10gDialect implements SpatialDialect {
	private static final long serialVersionUID = 1L;

	@Override
	public String getSpatialRelateSQL(String columnName, int spatialRelation) {
		return "";
	}

	@Override
	public String getSpatialFilterExpression(String columnName) {
		return "";
	}

	@Override
	public UserType getGeometryUserType() {
        return new GeoDBGeometryUserType(); // this is from H2 and is bogus
	}

	@Override
	public String getSpatialAggregateSQL(String columnName, int aggregation) {
		return "";
	}

	@Override
	public String getDWithinSQL(String columnName) {
		return "";
	}

	@Override
	public String getHavingSridSQL(String columnName) {
		return "";
	}

	@Override
	public String getIsEmptySQL(String columnName, boolean isEmpty) {
		return "";
	}

	@Override
	public String getDbGeometryTypeName() {
		return "";
	}

	@Override
	public boolean isTwoPhaseFiltering() {
		return false;
	}

	@Override
	public boolean supportsFiltering() {
		return false;
	}

	@Override
	public boolean supports(SpatialFunction function) {
		return false;
	}
}
