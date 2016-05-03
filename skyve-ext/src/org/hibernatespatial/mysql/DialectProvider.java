/**
 * $Id: DialectProvider.java 298 2011-03-12 15:29:54Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the 
 * hibernate ORM solution for geographic data. 
 *
 * Copyright © 2007 Geovise BVBA
 * Copyright © 2007 K.U. Leuven LRD, Spatial Applications Division, Belgium
 *
 * This work was partially supported by the European Commission, 
 * under the 6th Framework Programme, contract IST-2-004688-STP.
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
package org.hibernatespatial.mysql;

import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.spi.SpatialDialectProvider;

/**
 * MySQL DialectProvider
 *
 * @author Karel Maesen
 */
public class DialectProvider implements SpatialDialectProvider {

    /*
      * (non-Javadoc)
      *
      * @see org.hibernatespatial.spi.SpatialDialectProvider#createSpatialDialect(java.lang.String,
      *      java.util.Map)
      */
    public SpatialDialect createSpatialDialect(String dialect) {
        if (dialect.equals(MySQLSpatialInnoDBDialect.class.getCanonicalName())
                || dialect.equals("org.hibernate.dialect.MySQLInnoDBDialect")
                || dialect.equals("org.hibernate.dialect.MySQL5InnoDBDialect"))
            return new MySQLSpatialInnoDBDialect();
        else
            return null;
    }

    /*
      * (non-Javadoc)
      *
      * @see org.hibernatespatial.spi.SpatialDialectProvider#getDefaultDialect()
      */
    public SpatialDialect getDefaultDialect() {
        return new MySQLSpatialInnoDBDialect();
    }

    /*
      * (non-Javadoc)
      *
      * @see org.hibernatespatial.spi.SpatialDialectProvider#getSupportedDialects()
      */
    public String[] getSupportedDialects() {
        return new String[]{MySQLSpatialInnoDBDialect.class.getCanonicalName()};
    }

}
