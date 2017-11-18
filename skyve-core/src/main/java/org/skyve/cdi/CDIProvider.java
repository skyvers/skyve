package org.skyve.cdi;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;

import javax.inject.Inject;

import org.apache.commons.lang3.reflect.FieldUtils;

public class CDIProvider {

	/**
	 * Assigns null to any instance fields marked with @{@link Inject} 
	 * @param instance
	 * @return the instance
	 */
	public static <T> T clearInjectedFields(T instance) {
		if (instance == null) {
			return null;
		}
		Set<Field> fields = findFields(instance.getClass(), Inject.class);
		for (Field field : fields) {
			try {
				FieldUtils.writeField(field, instance, null, true);
			} catch (IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		}
		return instance;		
	}
	
	/**
	 * Find all fields with an annotation with a matching name. We match on name rather than
	 * class to handle different runtime versions of an annotation. 
	 * @param clazz 
	 * @param annotation 
	 * @return fields with specified annotation in class and all super classes
	 */
	private static Set<Field> findFields(Class<?> clazz, Class<? extends Annotation> annotation) {
	    Set<Field> set = new HashSet<>();
	    Class<?> c = clazz;
	    while (c != null) {
	        for (Field field : c.getDeclaredFields()) {
	        	for (Annotation a : field.getDeclaredAnnotations()) {
	        		if (a.annotationType().getName().equals(annotation.getName())) {
	        			set.add(field);
	        			break;
	        		}
	            }
	        }
	        c = c.getSuperclass();
	    }
	    return set;
	}
}
