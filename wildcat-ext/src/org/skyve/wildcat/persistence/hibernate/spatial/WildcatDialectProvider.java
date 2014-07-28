package org.skyve.wildcat.persistence.hibernate.spatial;

import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.spi.SpatialDialectProvider;
import org.skyve.wildcat.util.UtilImpl;

public class WildcatDialectProvider implements SpatialDialectProvider {
	@Override
	public SpatialDialect createSpatialDialect(String dialect) {
		try {
			ClassLoader loader = Thread.currentThread().getContextClassLoader();
			return (SpatialDialect) loader.loadClass(dialect).newInstance();
		}
		catch (Exception e) {
			throw new IllegalStateException("Cannot create spatial dialect " + dialect, e);
		}
	}

	@Override
	public SpatialDialect getDefaultDialect() {
		return createSpatialDialect(UtilImpl.DIALECT);
	}

	@Override
	public String[] getSupportedDialects() {
		return new String[] {UtilImpl.DIALECT};
	}
}
