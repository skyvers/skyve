package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.SQLException;
import java.util.Iterator;

import org.geolatte.geom.ByteOrder;
import org.geolatte.geom.codec.Wkb;
import org.geolatte.geom.codec.WkbEncoder;
import org.geolatte.geom.jts.JTS;
import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.postgis.PGGeometryTypeDescriptor;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.internal.StandardIndexExporter;
import org.hibernate.tool.schema.spi.Exporter;
import org.locationtech.jts.geom.Geometry;
import org.postgresql.util.PGobject;

/**
 * Provides PostgreSQL/PostGIS-specific dialect behaviour used by Skyve spatial persistence.
 */
public final class PostgreSQLSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = 614578841629775970L;

	private JTSGeometryType geometryType = new JTSGeometryType(PGGeometryTypeDescriptor.INSTANCE_WKB_2);
	// This is used at startup (hopefully before any Serialization)
	private transient StandardIndexExporter indexExporter;

	/**
	 * Creates the delegate for a concrete Hibernate dialect and configures custom index DDL.
	 *
	 * @param dialect The Hibernate dialect backing this delegate.
	 */
	public PostgreSQLSpatialDialectDelegate(Dialect dialect) {
		// We override index exporter for PostGreSQL so that we can specify lower() functional indexes
		// using text pattern index operations (except for PKs which can be case sensitive)
		indexExporter = new StandardIndexExporter(dialect) {
			
			/**
			 * Builds index DDL and rewrites single-column character indexes to use
			 * {@code lower(...)} with {@code text_pattern_ops} for case-insensitive
			 * prefix matching.
			 *
			 * @param index The Hibernate index metadata.
			 * @param metadata The mapping metadata used for SQL type inspection.
			 * @return SQL statements to create the index.
			 */
			@Override
			public String[] getSqlCreateStrings(Index index, Metadata metadata) {
				@SuppressWarnings("deprecation")
				String[] result = super.getSqlCreateStrings(index, metadata);

				Iterator<Column> i = index.getColumnIterator();
				Column column = i.next();
				if (! i.hasNext()) { // 1 column - we can do something
					int typeCode = column.getSqlTypeCode(metadata);
					if (DDLDelegate.isChar(typeCode)) {
						String create = result[0];
						int openingBraceIndex = create.indexOf('(');
						create = new StringBuilder(create.length() + 25)
										.append(create.substring(0, openingBraceIndex + 1))
										.append("lower(")
										.append(create.substring(openingBraceIndex + 1, create.length() - 1))
										.append(") text_pattern_ops)")
										.toString();
						result[0] = create;
					}
				}

				return result;
			}
		};
	}
	
	/**
	 * Returns the JDBC SQL type code used for persisted geometry values.
	 *
	 * @return The SQL type code for PostGIS geometry.
	 */
	@Override
	public int getGeometrySqlType() {
		return PGGeometryTypeDescriptor.INSTANCE_WKB_2.getSqlType();
	}

	/**
	 * Returns the Hibernate geometry type bound to the PostGIS descriptor.
	 *
	 * @return The configured {@link JTSGeometryType}.
	 */
	@Override
	public JTSGeometryType getGeometryType() {
		return geometryType;
	}

	/**
	 * Converts a JTS geometry into the PostgreSQL persisted representation.
	 *
	 * @param geometry The geometry to persist.
	 * @return A {@link PGobject} containing EWKB hex text.
	 */
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		final WkbEncoder encoder = Wkb.newEncoder(Wkb.Dialect.POSTGIS_EWKB_1);
		final String hexString = encoder.encode(JTS.from(geometry), ByteOrder.NDR).toString();
		final PGobject result = new PGobject();
		result.setType("geometry");
		try {
			result.setValue(hexString);
		}
		catch (SQLException e) {
			throw new IllegalArgumentException("Could not set valueon PGObject of " + hexString, e);
		}
		return result;
	}

	/**
	 * Converts a PostgreSQL geometry value back into a JTS geometry.
	 *
	 * @param geometry The database geometry payload.
	 * @return The decoded JTS geometry, or {@code null}.
	 */
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		org.geolatte.geom.Geometry<?> result = PGGeometryTypeDescriptor.INSTANCE_WKB_2.toGeometry(geometry);
		return (result == null) ? null : JTS.to(result);
	}

	/**
	 * Determines whether Hibernate must emit an alter-column change for this column.
	 *
	 * @param column The mapped column definition.
	 * @param columnInfo The live database column information.
	 * @return {@code true} when a column change statement is required.
	 */
	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		return DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}

	/**
	 * Returns the vendor-specific SQL fragment used to modify a column.
	 *
	 * @return The PostgreSQL column-modify fragment.
	 */
	@Override
	public String getModifyColumnString() {
		return "alter column";
	}

	/**
	 * Identifies the backing RDBMS for this dialect delegate.
	 *
	 * @return {@link RDBMS#postgresql}.
	 */
	@Override
	public RDBMS getRDBMS() {
		return RDBMS.postgresql;
	}
	
	/**
	 * Returns the index exporter configured for PostgreSQL functional index generation.
	 *
	 * @return The {@link Exporter} used to emit index DDL.
	 */
	Exporter<Index> getIndexExporter() {
		return indexExporter;
	}
}
