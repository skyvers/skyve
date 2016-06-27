package org.skyve.impl.persistence.hibernate.spatial;

import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.spi.SpatialDialectProvider;
import org.skyve.impl.util.UtilImpl;

public class SkyveDialectProvider implements SpatialDialectProvider {
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
		return createSpatialDialect(UtilImpl.DATA_STORE.getDialectClassName());
	}

	@Override
	public String[] getSupportedDialects() {
		return new String[] {UtilImpl.DATA_STORE.getDialectClassName()};
	}
}
