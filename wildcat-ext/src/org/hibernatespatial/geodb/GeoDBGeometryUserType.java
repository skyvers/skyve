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

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBConstants;

import geodb.GeoDB;
import org.hibernate.HibernateException;
import org.hibernatespatial.AbstractDBGeometryType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Blob;
import java.sql.Connection;
import java.sql.Types;

/**
 * @author Jan Boonen, Geodan IT b.v.
 */
public class GeoDBGeometryUserType extends AbstractDBGeometryType {

	private static final long serialVersionUID = -8929343431050580222L;

	private static final Logger LOGGER = LoggerFactory.getLogger(GeoDBGeometryUserType.class);

    private static final int[] geometryTypes = new int[]{Types.ARRAY};
    
    private GeometryFactory gf = new GeometryFactory();

    /*
      * (non-Javadoc)
      *
      * @see org.hibernatespatial.AbstractDBGeometryType#sqlTypes()
      */

    public int[] sqlTypes() {
        return geometryTypes;
    }

    /*
      * (non-Javadoc)
      *
      * @see
      * org.hibernatespatial.AbstractDBGeometryType#convert2JTS(java.lang.Object)
      */

    @Override
    public Geometry convert2JTS(Object object) {
        if (object == null)
            return null;
        try {
            if (object instanceof Blob) {
                return GeoDB.gFromWKB(toByteArray((Blob) object));
            } else if (object instanceof byte[]) {
                return geometryFromByteArray((byte[]) object);//GeoDB.gFromWKB((byte[])object);/
            } else if(object instanceof Envelope) {
        		return gf.toGeometry((Envelope) object);
        	} else {
                throw new IllegalArgumentException(
                        "Can't convert database object of type "
                                + object.getClass().getCanonicalName());
            }
        } catch (Exception e) {
            LOGGER.warn("Could not convert database object to a JTS Geometry.");
            throw new HibernateException(e);
        }
    }

	/**
	 * Convert a WKB or EWKB byte array to a {@link Geometry}. To figure out
	 * whether the byte array is either a WKB or EWK the following checks are
	 * performed: <br/>
	 * <ol>
	 * <li>If the first byte is not 0 or 1, the geometry must be an EWKB (with
	 * the first 32 bytes as a bounding box)</li>
	 * <li>Otherwise, the the byte array is parsed as a WKB</li>
	 * <li>When a parse error occurs it is assumed that the byte array is a EWKB
	 * (with the first byte accidentally a 0 or 1), and parsed as EWKB</li>
	 * </ol>
	 * 
	 * @param object
	 * @return
	 */
	private Geometry geometryFromByteArray(byte[] bytes) {
		/*
		 * wkbXDR = 0, // Big Endian 
		 * wkbNDR = 1 // Little Endian
		 */
		if (bytes[0] != WKBConstants.wkbNDR && bytes[0] != WKBConstants.wkbXDR) {
			// process as EWKB
			return GeoDB.gFromEWKB(bytes);
		} else {
			// process as WKB
			try {
				return GeoDB.gFromWKB(bytes);
			} catch (RuntimeException e) {
				// note: ParseException is wrapped in a RuntimeException in
				// geodb.GeoDB
				if (e.getCause() != null
						&& e.getCause() instanceof ParseException) {
					// retry as EWKB, this should rarely happen, but may occur
					// when the first byte of the EWKB bounding-box is '0'.
					// this should not be considered as a error
					LOGGER.debug(
							"Caught parse exception while parsing byte array as WKB. Retrying as EWKB.",
							e);
					return GeoDB.gFromEWKB(bytes);
				} else {
					// this is an other exception, simply re-throw
					throw e;
				}
			}
		}
	}

    /*
      * (non-Javadoc)
      *
      * @seeorg.hibernatespatial.AbstractDBGeometryType#conv2DBGeometry(com.
      * vividsolutions.jts.geom.Geometry, java.sql.Connection)
      */

    @Override
    public Object conv2DBGeometry(Geometry jtsGeom, Connection connection) {
        try {
            return GeoDB.gToWKB(jtsGeom);
        }
        catch (Exception e) {
            LOGGER.warn("Could not convert JTS Geometry to a database object.");
            e.printStackTrace();
            return null;
        }
    }

    private byte[] toByteArray(Blob blob) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buf = new byte[1024];

        InputStream in = null;
        try {
            in = blob.getBinaryStream();
            int n = 0;
            while ((n = in.read(buf)) >= 0) {
                baos.write(buf, 0, n);

            }
        } catch (Exception e) {
            LOGGER.warn("Could not convert database BLOB object to binary stream.");
            e.printStackTrace();
        }
        finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                LOGGER.warn("Could not close binary stream.");
                e.printStackTrace();
            }
        }

        return baos.toByteArray();
    }

}
