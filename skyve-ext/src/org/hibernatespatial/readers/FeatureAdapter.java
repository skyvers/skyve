/*
 * $Id: FeatureAdapter.java 165 2010-03-11 20:27:08Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the
 * hibernate ORM solution for geographic data.
 *
 * Copyright © 2007-2010 Geovise BVBA
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
package org.hibernatespatial.readers;

import org.hibernate.EntityMode;
import org.hibernate.metadata.ClassMetadata;
import org.hibernate.property.Getter;
import org.hibernate.util.ReflectHelper;
import org.hibernatespatial.helper.FinderException;
import org.hibernatespatial.helper.GeometryPropertyFinder;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * Adapts arbitrary objects to the {@link Feature} interface using dynamic proxying.
 *
 * @author Karel Maesen
 */
public class FeatureAdapter {

    public static Feature adapt(Object o, ClassMetadata cf) {

        return (Feature) Proxy.newProxyInstance(o.getClass().getClassLoader(),
                new Class[]{Feature.class},
                new FeatureInvocationHandler(o, cf));
    }

    static private class FeatureInvocationHandler implements InvocationHandler {
        private final Object target;
        private final ClassMetadata metadata;
        private Method targetGeomGetter;
        private Method targetIdGetter;


        private static final Method geomGetter;
        private static final Method idGetter;
        private static final Method attrGetter;

        private static GeometryPropertyFinder geomPropertyFinder = new GeometryPropertyFinder();

        static {
            Class featureIntf = Feature.class;
            try {
                geomGetter = featureIntf.getDeclaredMethod("getGeometry", new Class[]{});
                idGetter = featureIntf.getDeclaredMethod("getId", new Class[]{});
                attrGetter = featureIntf.getDeclaredMethod("getAttribute", new Class[]{String.class});
            } catch (Exception e) {
                throw new RuntimeException("Probable programming Error", e);
            }
        }

        private FeatureInvocationHandler(Object o, ClassMetadata meta) {
            //TODO check if this is sufficiently general. What if not a POJO?
            if (meta.getMappedClass(EntityMode.POJO) != o.getClass()) {
                throw new RuntimeException("Metadata and POJO class do not cohere");
            }
            this.target = o;
            this.metadata = meta;
        }

        public Object invoke(Object proxy, Method method, Object[] args)
                throws Throwable {
            Method m = getTargetGetter(method, args);

            if (m == null) {
                return method.invoke(this.target, args);
            } else {
                return m.invoke(this.target);
            }
        }

        private Method getTargetGetter(Method invokedMethod, Object[] args) {
            try {
                if (invokedMethod.equals(geomGetter)) {
                    if (this.targetGeomGetter == null) {
                        this.targetGeomGetter = getGeomGetter();
                    }
                    return this.targetGeomGetter;
                } else if (invokedMethod.equals(idGetter)) {
                    if (this.targetIdGetter == null) {
                        this.targetIdGetter = getIdGetter();
                    }
                    return this.targetIdGetter;
                } else if (invokedMethod.equals(attrGetter)) {
                    String property = (String) args[0];
                    return getGetterFor(property);
                } else {
                    return null;
                }
            } catch (Exception e) {
                throw new RuntimeException("Problem getting suitable target method for method:  " + invokedMethod.getName(), e);
            }

        }

        private Method getGetterFor(String property) {
            Class cl = this.metadata.getMappedClass(EntityMode.POJO);
            Getter getter = ReflectHelper.getGetter(cl, property);
            return getter.getMethod();
        }

        private Method getGeomGetter() {
            try {
                String prop = getGeometryPropertyName();
                return getGetterFor(prop);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        public String getGeometryPropertyName() throws FinderException {
            return this.geomPropertyFinder.find(this.metadata);
        }

        public Method getIdGetter() {
            String prop = this.metadata.getIdentifierPropertyName();
            return getGetterFor(prop);
        }

    }


}

