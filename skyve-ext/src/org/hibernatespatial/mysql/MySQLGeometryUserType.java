/**
 * $Id: MySQLGeometryUserType.java 190 2010-03-21 21:35:27Z maesenka $
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

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ByteOrderValues;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKBWriter;
import org.hibernatespatial.AbstractDBGeometryType;
import org.hibernatespatial.HBSpatialExtension;

import java.sql.Connection;
import java.sql.Types;

/**
 * Specific <code>GeometryType</code> for MySQL geometry type
 *
 * @author Karel Maesen
 */
public class MySQLGeometryUserType extends AbstractDBGeometryType {

    private static final int SRIDLEN = 4;

    private static final int[] geometryTypes = new int[]{Types.ARRAY};

    public int[] sqlTypes() {
        return geometryTypes;
    }

    /**
     * Converts the native geometry object to a JTS <code>Geometry</code>.
     *
     * @param object native database geometry object (depends on the JDBC spatial
     *               extension of the database)
     * @return JTS geometry corresponding to geomObj.
     */
    public Geometry convert2JTS(Object object) {
        if (object == null) {
            return null;
        }
        byte[] data = (byte[]) object;
        byte[] wkb = new byte[data.length - SRIDLEN];
        System.arraycopy(data, SRIDLEN, wkb, 0, wkb.length);
        int srid = 0;
        // WKB in MySQL Spatial is always little endian.
        srid = data[3] << 24 | (data[2] & 0xff) << 16 | (data[1] & 0xff) << 8
                | (data[0] & 0xff);
        Geometry geom = null;
        try {
            WKBReader reader = new WKBReader();
            geom = reader.read(wkb);
        } catch (Exception e) {
            throw new RuntimeException(
                    "Couldn't parse incoming MySQL Spatial data.");
        }
        geom.setSRID(srid);
        return geom;
    }

    /**
     * Converts a JTS <code>Geometry</code> to a native geometry object.
     *
     * @param jtsGeom    JTS Geometry to convert
     * @param connection the current database connection
     * @return native database geometry object corresponding to jtsGeom.
     */
    public Object conv2DBGeometry(Geometry jtsGeom, Connection connection) {
        if (jtsGeom.isEmpty()) return null;
        jtsGeom = forceGeometryCollection(jtsGeom);
        int srid = jtsGeom.getSRID();

        WKBWriter writer = new WKBWriter(2,
                ByteOrderValues.LITTLE_ENDIAN);
        byte[] wkb = writer.write(jtsGeom);

        byte[] byteArr = new byte[wkb.length + SRIDLEN];
        byteArr[3] = (byte) ((srid >> 24) & 0xFF);
        byteArr[2] = (byte) ((srid >> 16) & 0xFF);
        byteArr[1] = (byte) ((srid >> 8) & 0xFF);
        byteArr[0] = (byte) (srid & 0xFF);
        System.arraycopy(wkb, 0, byteArr, SRIDLEN, wkb.length);
        return byteArr;
    }

    private Geometry forceGeometryCollection(Geometry jtsGeom) {
        if (jtsGeom.isEmpty()) return createEmptyGeometryCollection(jtsGeom);
        if (jtsGeom instanceof GeometryCollection) {
            GeometryCollection gc = (GeometryCollection) jtsGeom;
            Geometry[] components = new Geometry[gc.getNumGeometries()];
            for (int i = 0; i < gc.getNumGeometries(); i++) {
                Geometry component = gc.getGeometryN(i);
                if (component.isEmpty()) {
                    components[i] = jtsGeom.getFactory().createGeometryCollection(null);
                } else {
                    components[i] = component;
                }
            }
            Geometry geometryCollection = jtsGeom.getFactory().createGeometryCollection(components);
            geometryCollection.setSRID(jtsGeom.getSRID());
            return geometryCollection;
        }
        return jtsGeom;
    }

    private Geometry createEmptyGeometryCollection(Geometry jtsGeom) {
        GeometryFactory factory = jtsGeom.getFactory();
        if (factory == null) {
            factory = HBSpatialExtension.getDefaultGeomFactory();
        }
        Geometry empty = factory.createGeometryCollection(null);
        empty.setSRID(jtsGeom.getSRID());
        return empty;
    }

}
