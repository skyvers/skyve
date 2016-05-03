/*
 * $Id$
 *
 * This file is part of Hibernate Spatial, an extension to the
 * hibernate ORM solution for geographic data.
 *
 * Copyright 2010 Geodan IT b.v.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * For more information, visit: http://www.hibernatespatial.org/
 */
package org.hibernatespatial.geodb;

import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.spi.SpatialDialectProvider;

/**
 * GeoDB (H2 database with HatBox spatial extension) Dialect Provider.
 *
 * @author Jan Boonen, Geodan IT b.v.
 */
public class DialectProvider implements SpatialDialectProvider {

    /* (non-Javadoc)
      * @see org.hibernatespatial.spi.SpatialDialectProvider#createSpatialDialect(java.lang.String)
      */

    public SpatialDialect createSpatialDialect(String dialect) {
        if (dialect.equals(GeoDBDialect.class.getCanonicalName())
                || dialect.equals("org.hibernate.dialect.H2SQLDialect")
                || dialect.equals("H2"))
            return new GeoDBDialect();
        else
            return null;
    }

    /* (non-Javadoc)
      * @see org.hibernatespatial.spi.SpatialDialectProvider#getDefaultDialect()
      */

    public SpatialDialect getDefaultDialect() {
        return new GeoDBDialect();
    }

    /* (non-Javadoc)
      * @see org.hibernatespatial.spi.SpatialDialectProvider#getSupportedDialects()
      */

    public String[] getSupportedDialects() {
        return new String[]{GeoDBDialect.class.getCanonicalName()};
    }

}
